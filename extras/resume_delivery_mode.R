# resume_delivery_mode.R
#
# In-session patch + checkpointing for runEsd() — applied without reinstalling
# the package. Source this in the R session before calling runEsd().
#
# What it does
# ------------
# 1. Fixes the addDeliveryMode bug (Spark ODBC returns DATE as <character>;
#    we coerce to Date before the local left_join).
# 2. Adds disk checkpointing to the four expensive steps inside runEsd:
#       getTimingConcepts                  (~35 hr)
#       episodesWithGestationalTimingInfo  (~1.7 hr)
#       mergedEpisodesWithMetadata         (~7 min)
#       addDeliveryMode  (deliveryResult after collect, ~52 hr of intersect)
#    On a future run, any step whose checkpoint exists is loaded from disk
#    and skipped. Recovery from a mid-run failure goes from days to minutes.
# 3. Wires the checkpoints into runEsd() via assignInNamespace() — no reinstall.
#
# Usage
# -----
#   options(PregnancyIdentifier.checkpointDir = outputFolder)
#   source("extras/resume_delivery_mode.R")
#   options(error = function() {
#     dump.frames("last_dump", to.file = TRUE)
#     cat("\n[error] frames dumped to last_dump.rda\n")
#   })
#   PregnancyIdentifier::runEsd(
#     cdm = cdm, outputFolder = outputFolder,
#     startDate = startDate, endDate = endDate,
#     logger = logger, debugMode = FALSE, conformToValidation = FALSE
#   )
#
# To force a fresh run of one or more steps, delete the corresponding files
# from outputFolder (or call clearEsdCheckpoints() below).

library(PregnancyIdentifier)

# ---- 0. Checkpoint plumbing ------------------------------------------------

.chkptPath <- function(name) {
  dir <- getOption("PregnancyIdentifier.checkpointDir", default = NULL)
  if (is.null(dir)) return(NULL)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, paste0("chkpt_", name, ".rds"))
}

# Run `thunk()`, persisting the result keyed by `name`. If a checkpoint exists,
# load it and skip the work entirely. Pass logger = NULL to silence logging.
.checkpointed <- function(name, thunk, logger = NULL) {
  path <- .chkptPath(name)
  if (is.null(path)) return(thunk())
  if (file.exists(path)) {
    if (!is.null(logger)) log4r::info(logger, sprintf("[checkpoint] load  %s", path))
    return(readRDS(path))
  }
  result <- thunk()
  saveRDS(result, path)
  if (!is.null(logger)) log4r::info(logger, sprintf("[checkpoint] save  %s", path))
  result
}

clearEsdCheckpoints <- function(outputFolder, which = NULL) {
  all <- c("timing_concepts", "esd_df", "merged_pre_delivery", "delivery_result")
  if (is.null(which)) which <- all
  for (n in which) {
    p <- file.path(outputFolder, paste0("chkpt_", n, ".rds"))
    if (file.exists(p)) {
      file.remove(p)
      message("[checkpoint] removed ", p)
    }
  }
  invisible(NULL)
}

# ---- 1. Capture originals BEFORE patching ----------------------------------
# Closures will reference these captured copies, not the namespace, so the
# wrappers can call the originals without recursing into themselves.

.orig_getTimingConcepts                  <- PregnancyIdentifier:::getTimingConcepts
.orig_episodesWithGestationalTimingInfo  <- PregnancyIdentifier:::episodesWithGestationalTimingInfo
.orig_mergedEpisodesWithMetadata         <- PregnancyIdentifier:::mergedEpisodesWithMetadata

# ---- 2. Checkpointed wrappers for upstream steps ---------------------------

getTimingConcepts_chkpt <- function(cdm, startDate, endDate, hippsEpisodes, logger) {
  .checkpointed(
    "timing_concepts",
    function() .orig_getTimingConcepts(
      cdm = cdm, startDate = startDate, endDate = endDate,
      hippsEpisodes = hippsEpisodes, logger = logger
    ),
    logger = logger
  )
}

episodesWithGestationalTimingInfo_chkpt <- function(timingConceptsDf, logger) {
  .checkpointed(
    "esd_df",
    function() .orig_episodesWithGestationalTimingInfo(timingConceptsDf, logger = logger),
    logger = logger
  )
}

mergedEpisodesWithMetadata_chkpt <- function(episodesWithGestationalTimingInfoDf,
                                             hippsEpisodes, termMaxMin, logger) {
  .checkpointed(
    "merged_pre_delivery",
    function() .orig_mergedEpisodesWithMetadata(
      episodesWithGestationalTimingInfoDf = episodesWithGestationalTimingInfoDf,
      hippsEpisodes = hippsEpisodes,
      termMaxMin    = termMaxMin,
      logger        = logger
    ),
    logger = logger
  )
}

# ---- 3. addDeliveryMode: bug fix + internal checkpoint ---------------------
# The expensive part is the concept intersect + collect (~52 hr). We checkpoint
# `deliveryResult` AFTER collect, so a failure on the local join (or anywhere
# downstream) means the next run loads from RDS instead of redoing intersect.

addDeliveryMode_fixed <- function(cdm, df, logger, intersectWindow = c(-30, 30)) {
  checkmate::assertClass(logger, "logger", null.ok = FALSE)
  dfColNames <- colnames(df)
  colnames(df) <- tolower(dfColNames)

  minimalCohort <- df %>%
    dplyr::select("person_id", "inferred_episode_end") %>%
    dplyr::filter(!is.na(.data$inferred_episode_end)) %>%
    dplyr::distinct()
  if (nrow(minimalCohort) > 0) {
    minimalCohort <- PregnancyIdentifier:::validateDeliveryModeCohort(
      minimalCohort, logger = logger
    )
  }

  deliveryResult <- .checkpointed(
    "delivery_result",
    function() {
      tableName <- "esd_delivery_mode_cohort"
      cdm <- omopgenerics::insertTable(
        cdm = cdm, name = tableName, table = minimalCohort,
        overwrite = TRUE, temporary = FALSE
      )
      conceptSet <- suppressMessages(
        omopgenerics::importConceptSetExpression(
          path = system.file(
            package = "PregnancyIdentifier",
            "concepts/delivery_mode", mustWork = TRUE
          )
        )
      )
      conceptSet <- suppressMessages(
        omopgenerics::validateConceptSetArgument(conceptSet, cdm = cdm)
      )
      names(conceptSet) <- unlist(lapply(names(conceptSet), FUN = function(name) {
        unlist(strsplit(name, "^\\d+-"))[2]
      }))
      out <- suppressWarnings(
        cdm[[tableName]] %>%
          PatientProfiles::addConceptIntersectFlag(
            conceptSet = conceptSet,
            indexDate  = "inferred_episode_end",
            window     = intersectWindow,
            nameStyle  = "{concept_name}_{window_name}"
          ) %>%
          PatientProfiles::addConceptIntersectCount(
            conceptSet = conceptSet,
            indexDate  = "inferred_episode_end",
            window     = intersectWindow,
            nameStyle  = "{concept_name}_{window_name}_count"
          ) %>%
          dplyr::collect()
      )
      # Spark ODBC returns DATE as <character>; coerce so the local join works.
      out %>% dplyr::mutate(inferred_episode_end = as.Date(.data$inferred_episode_end))
    },
    logger = logger
  )

  # Defensive coercion in case the checkpoint was created before this fix
  # was added (e.g. from a Path-A recovery dump that wasn't coerced).
  deliveryResult <- deliveryResult %>%
    dplyr::mutate(inferred_episode_end = as.Date(.data$inferred_episode_end))

  deliveryCols <- setdiff(names(deliveryResult), c("person_id", "inferred_episode_end"))
  mergedDf <- df %>%
    dplyr::left_join(
      deliveryResult %>%
        dplyr::select("person_id", "inferred_episode_end", dplyr::all_of(deliveryCols)),
      by = c("person_id", "inferred_episode_end")
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(deliveryCols), ~ dplyr::coalesce(.x, 0)))
  mergedDf <- dplyr::as_tibble(mergedDf)
  colnames(mergedDf)[1:length(dfColNames)] <- dfColNames
  mergedDf
}

# ---- 4. Install all wrappers into the namespace ----------------------------

assignInNamespace("getTimingConcepts",                 getTimingConcepts_chkpt,                 ns = "PregnancyIdentifier")
assignInNamespace("episodesWithGestationalTimingInfo", episodesWithGestationalTimingInfo_chkpt, ns = "PregnancyIdentifier")
assignInNamespace("mergedEpisodesWithMetadata",        mergedEpisodesWithMetadata_chkpt,        ns = "PregnancyIdentifier")
assignInNamespace("addDeliveryMode",                   addDeliveryMode_fixed,                   ns = "PregnancyIdentifier")

message("[patch] PregnancyIdentifier internals replaced in-session:")
message("        getTimingConcepts, episodesWithGestationalTimingInfo,")
message("        mergedEpisodesWithMetadata, addDeliveryMode (with bug fix).")
message("[patch] Checkpoint dir: ",
        getOption("PregnancyIdentifier.checkpointDir", "<unset — set options(PregnancyIdentifier.checkpointDir = outputFolder)>"))
