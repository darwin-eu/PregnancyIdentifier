# exportPregnanciesVerbose.R
#
# Does exactly what PregnancyIdentifier::exportPregnancies() does, but logs every
# single step with start/finish timestamps and elapsed seconds, so you can see
# precisely which step is slow or where it hangs.
#
# It does NOT re-implement the export logic: it calls the package's own internal
# export* helpers one at a time (via the package namespace), in the same order and
# with the same arguments as exportPregnancies(), so the output CSVs are identical.
# The only addition is a timing wrapper around each step.
#
# Behaviour notes:
#   * Steps marked [DB] issue read-only queries to Databricks (the usual suspects
#     for slowness/hangs). Steps marked [loc] are pure local R on the ~1.68M-row
#     episodes data frame.
#   * A step that ERRORS is logged and the script continues to the next step, so
#     you still get a full timing picture and all the CSVs that can be written.
#   * A step that HANGS will log ">>> START ..." and nothing after it — that line
#     is your culprit.
#   * START lines are flushed immediately so you see progress live.
#   * A summary table is written to exportFolder/export_timings.csv at the end.
#
# Required env vars: DATABRICKS_HTTPPATH, DATABRICKS_CDM_SCHEMA,
#                    DATABRICKS_SCRATCH_SCHEMA

library(PregnancyIdentifier)

# ---- Fill these in ----------------------------------------------------------
outputFolder <- "..."    # folder with final_pregnancy_episodes.rds + runStart.csv
exportFolder <- "..."    # where the export CSVs are written
cdmName      <- "BIFAP"
minCellCount <- 5L
makeZip      <- TRUE      # run zipExportFolder() at the end

# ---- Connect (fresh connection — old ones go stale after hours) -------------
con <- DBI::dbConnect(
  odbc::databricks(),
  httpPath = Sys.getenv("DATABRICKS_HTTPPATH"),
  useNativeQuery = FALSE
)
on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

cdm <- CDMConnector::cdmFromCon(
  con         = con,
  cdmSchema   = Sys.getenv("DATABRICKS_CDM_SCHEMA"),
  writeSchema = Sys.getenv("DATABRICKS_SCRATCH_SCHEMA"),
  cdmName     = cdmName
)

dir.create(exportFolder, showWarnings = FALSE, recursive = TRUE)

# ---- Logger + timing wrapper ------------------------------------------------
logger <- log4r::logger(
  threshold = "INFO",
  appenders = list(
    log4r::console_appender(),
    log4r::file_appender(file.path(exportFolder, "export_verbose_log.txt"))
  )
)
logLine <- function(msg) { log4r::info(logger, msg); try(flush.console(), silent = TRUE) }

timings <- list()
# `expr` is lazily evaluated; it runs (once) only inside the tryCatch below.
timed <- function(label, isDb, expr) {
  logLine(sprintf(">>> START  [%s] %s", if (isDb) "DB " else "loc", label))
  t0 <- Sys.time()
  outcome <- tryCatch(
    { force(expr); list(ok = TRUE, err = NA_character_) },
    error = function(e) list(ok = FALSE, err = conditionMessage(e))
  )
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  logLine(sprintf("%s %s  (%.1f s)%s",
                  if (outcome$ok) "<<< DONE   " else "<<< FAILED ",
                  label, dt, if (outcome$ok) "" else paste0(" :: ", outcome$err)))
  timings[[length(timings) + 1L]] <<- data.frame(
    step = label, seconds = round(dt, 1), ok = outcome$ok, error = outcome$err,
    stringsAsFactors = FALSE
  )
  invisible(outcome$ok)
}

# Internal export* helpers live in the package namespace (not exported).
P <- asNamespace("PregnancyIdentifier")

logLine(sprintf("==== exportPregnanciesVerbose START | cdm=%s | export=%s ====", cdmName, exportFolder))

# ---- Preamble (matches exportPregnancies lines 31-59) -----------------------
runStart <- utils::read.csv(file.path(outputFolder, "runStart.csv"))$start

snap <- NULL
timed("snapshot(cdm)", TRUE, { snap <<- CDMConnector::snapshot(cdm) })
utils::write.csv(snap, file.path(exportFolder, "cdm_source.csv"), row.names = FALSE)

res <- NULL
timed("readRDS(final_pregnancy_episodes.rds)", FALSE, {
  res <<- readRDS(file.path(outputFolder, "final_pregnancy_episodes.rds"))
})
names(res) <- tolower(names(res))
if (!"merge_pregnancy_start" %in% names(res) && "final_episode_start_date" %in% names(res)) {
  res$merge_pregnancy_start <- res$final_episode_start_date
}
logLine(sprintf("episodes loaded: %s rows, %s cols",
                format(nrow(res), big.mark = ","), ncol(res)))

timed("copy raw artifacts", FALSE, {
  for (f in c("hip_concept_counts.csv", "pps_concept_counts.csv", "esd_concept_counts.csv", "log.txt", "attrition.csv")) {
    src <- file.path(outputFolder, f)
    if (file.exists(src)) file.copy(src, file.path(exportFolder, f), overwrite = TRUE)
  }
})

pkgVersion <- as.character(utils::packageVersion("PregnancyIdentifier"))

# ---- The 19 export steps, in the exact order exportPregnancies runs them ----
# args mirror exportPregnancies(); [DB] = touches Databricks, [loc] = local only.
timed("exportAgeSummary",              TRUE,  P$exportAgeSummary(res, cdm, exportFolder, snap, runStart, pkgVersion, minCellCount))
timed("exportPrecisionDays",           FALSE, P$exportPrecisionDays(res, exportFolder, snap, runStart, pkgVersion))
timed("exportPrecisionDaysDenominators", FALSE, P$exportPrecisionDaysDenominators(res, exportFolder, snap, runStart, pkgVersion))
timed("exportEpisodeFrequency",        FALSE, P$exportEpisodeFrequency(res, exportFolder, snap, runStart, pkgVersion, minCellCount))
timed("exportPregnancyFrequency",      FALSE, P$exportPregnancyFrequency(res, exportFolder, snap, runStart, pkgVersion, minCellCount))
timed("exportEpisodeFrequencySummary", FALSE, P$exportEpisodeFrequencySummary(res, exportFolder, snap, runStart, pkgVersion))
timed("exportGestationalAgeSummary",   FALSE, P$exportGestationalAgeSummary(res, exportFolder, snap, runStart, pkgVersion))
timed("exportGestationalAgeCounts",    FALSE, P$exportGestationalAgeCounts(res, exportFolder, snap, runStart, pkgVersion))
timed("exportGestationalWeeksCounts",  FALSE, P$exportGestationalWeeksCounts(res, exportFolder, snap, runStart, pkgVersion, minCellCount))
timed("exportGestationalDurationCounts", FALSE, P$exportGestationalDurationCounts(res, exportFolder, snap, runStart, pkgVersion))
timed("exportTimeTrends",              FALSE, P$exportTimeTrends(res, exportFolder, snap, runStart, pkgVersion))
timed("exportObservationPeriodRange",  TRUE,  P$exportObservationPeriodRange(res, cdm, exportFolder, snap, runStart, pkgVersion))
timed("exportPregnancyOverlapCounts",  FALSE, P$exportPregnancyOverlapCounts(res, exportFolder, snap, runStart, pkgVersion))
timed("exportMissingDates",            FALSE, P$exportMissingDates(res, exportFolder, snap, runStart, pkgVersion))
timed("exportReversedDatesCounts",     FALSE, P$exportReversedDatesCounts(res, exportFolder, snap, runStart, pkgVersion))
timed("exportOutcomeCategoriesCounts", FALSE, P$exportOutcomeCategoriesCounts(res, exportFolder, snap, runStart, pkgVersion))
timed("exportDeliveryModeSummary",     FALSE, P$exportDeliveryModeSummary(res, exportFolder, snap, runStart, pkgVersion))
timed("exportConceptTimingCheck",      TRUE,  P$exportConceptTimingCheck(cdm, res, exportFolder, snap, runStart, pkgVersion))
timed("exportCleanupQualityCheck",     FALSE, P$exportCleanupQualityCheck(res, exportFolder, snap, runStart, pkgVersion))

if (isTRUE(makeZip)) {
  timed("zipExportFolder", FALSE, PregnancyIdentifier::zipExportFolder(exportFolder))
}

# ---- Timing summary ---------------------------------------------------------
timingDf <- do.call(rbind, timings)
timingDf <- timingDf[order(-timingDf$seconds), ]
utils::write.csv(timingDf, file.path(exportFolder, "export_timings.csv"), row.names = FALSE)

logLine("==== TIMING SUMMARY (slowest first) ====")
for (i in seq_len(nrow(timingDf))) {
  logLine(sprintf("  %7.1f s  %s%s", timingDf$seconds[i], timingDf$step[i],
                  if (!timingDf$ok[i]) "  [FAILED]" else ""))
}
nFailed <- sum(!timingDf$ok)
logLine(sprintf("==== exportPregnanciesVerbose DONE | total %.1f s | %d step(s) failed ====",
                sum(timingDf$seconds), nFailed))
message("Done. Per-step timings written to ", file.path(exportFolder, "export_timings.csv"))
