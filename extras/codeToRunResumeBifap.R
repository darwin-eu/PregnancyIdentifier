# codeToRunResumeBifap.R
#
# Resume BIFAP from AFTER ESD, WITHOUT uploading the pregnancy-episode table.
#
# Uploading final_pregnancy_episodes.rds to Databricks over SQL is a bad idea: the
# per-row INSERT path is slow AND it lands as many tiny files, which then makes
# every downstream query slow. Instead, the data partner (Ana) converts
# final_pregnancy_episodes.rds to parquet, uploads it, and COPY INTOs a proper
# Delta table. This script consumes that pre-loaded table by name and runs the
# remaining study steps:
#
#   Step 6  incidence / prevalence / characteristics  (reads the Delta table; no upload)
#   Step 7  exportPregnancies                         (reads the local RDS; no upload)
#
# It does NOT modify the PregnancyIdentifier package. The Step 6 logic is a faithful
# port of PregnancyIdentifier:::computeIncidencePrevalence() with the RDS-read +
# insertTable replaced by a reference to the existing Delta table (and it does NOT
# drop that table at the end). If the package's computeIncidencePrevalence changes,
# re-sync this port.
#
# Required env vars: DATABRICKS_HTTPPATH, DATABRICKS_CDM_SCHEMA,
#                    DATABRICKS_SCRATCH_SCHEMA

library(dplyr)            # provides %>% and the .data pronoun used below
library(PregnancyIdentifier)

# ---- Fill these in ----------------------------------------------------------
# Fully-qualified location of the pre-loaded Delta table that Ana created from
# final_pregnancy_episodes.rds. Accepts "catalog.schema.table", "schema.table",
# or a bare "table" (resolved in the connection's current schema). May also be
# overridden with the DATABRICKS_EPISODE_TABLE env var.
episodeTableLocation <- Sys.getenv(
  "DATABRICKS_EPISODE_TABLE",
  unset = "hive_metastore.omop_2024_results.identified_pregnancies"
)

outputFolder <- "..."    # folder containing final_pregnancy_episodes.rds (for export)
exportFolder <- "..."    # where incidence/prevalence/characteristics CSVs are written
cdmName      <- "BIFAP"
minCellCount <- 5L

# ---- Connect ----------------------------------------------------------------
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

logger <- PregnancyIdentifier::makeLogger(outputFolder)
logMsg <- function(msg) log4r::info(logger, msg)

# ---- Reference the pre-loaded episode table (no upload) ---------------------
makeTableRef <- function(location) {
  if (inherits(location, c("Id", "ident", "ident_q"))) return(location)
  parts <- strsplit(location, ".", fixed = TRUE)[[1]]
  switch(as.character(length(parts)),
    "1" = parts[[1]],
    "2" = dbplyr::in_schema(parts[[1]], parts[[2]]),
    "3" = dbplyr::in_catalog(parts[[1]], parts[[2]], parts[[3]]),
    stop("episodeTableLocation must be 'table', 'schema.table', or 'catalog.schema.table'")
  )
}

ipTbl <- dplyr::tbl(con, makeTableRef(episodeTableLocation))

requiredCols <- c(
  "person_id", "merge_episode_number", "merge_pregnancy_start",
  "hip_end_date", "pps_end_date", "final_episode_start_date",
  "final_outcome_category"
)
missingCols <- setdiff(requiredCols, tolower(colnames(ipTbl)))
if (length(missingCols) > 0) {
  stop("Episode table ", episodeTableLocation,
       " is missing required column(s): ", paste(missingCols, collapse = ", "))
}
nEpisodes <- ipTbl %>% dplyr::ungroup() %>% dplyr::count() %>% dplyr::pull("n")
logMsg(sprintf("Using pre-loaded episode table: %s (%s rows)",
               episodeTableLocation, format(nEpisodes, big.mark = ",")))
# Sanity check: TOTAL = 1,682,637.

# =============================================================================
# Step 6 — incidence / prevalence / characteristics
# (port of PregnancyIdentifier:::computeIncidencePrevalence, table-backed)
# =============================================================================
dir.create(exportFolder, showWarnings = FALSE, recursive = TRUE)

# -- Exclude persons with >1 observation period per episode -------------------
toExclude <- cdm$observation_period %>%
  dplyr::right_join(ipTbl, by = "person_id") %>%
  dplyr::group_by(.data$person_id, .data$merge_episode_number) %>%
  dplyr::count() %>%
  dplyr::filter(.data$n > 1) %>%
  dplyr::collect()
logMsg(sprintf("Excluding %s persons with multiple observation periods", nrow(toExclude)))

# -- Distinct outcome categories (excluding the multi-OP persons) -------------
ipLocal <- ipTbl %>%
  dplyr::select("person_id", "final_outcome_category") %>%
  dplyr::collect() %>%
  dplyr::filter(!.data$person_id %in% toExclude$person_id)
outcome_categories <- sort(unique(
  ipLocal$final_outcome_category[!is.na(ipLocal$final_outcome_category)]
))

# -- Add age at pregnancy start -----------------------------------------------
cdm$res_age <- cdm$person %>%
  dplyr::select("person_id", "gender_concept_id", "birth_datetime") %>%
  dplyr::right_join(ipTbl, by = "person_id") %>%
  PatientProfiles::addAge(indexDate = "final_episode_start_date") %>%
  dplyr::rename(age_pregnancy_start = "age") %>%
  dplyr::compute(name = "res_age")

# -- Build HIPPS cohort table -------------------------------------------------
make_window_cohorts <- function(base_tbl, end_date_col, base_id) {
  all_outcomes <- base_tbl %>%
    dplyr::select(
      subject_id = "person_id",
      cohort_start_date = "merge_pregnancy_start",
      cohort_end_date = dplyr::all_of(end_date_col),
      "final_outcome_category"
    ) %>%
    dplyr::mutate(cohort_definition_id = base_id)

  outcome_cohorts <- lapply(seq_along(outcome_categories), function(i) {
    oc <- outcome_categories[i]
    all_outcomes %>%
      dplyr::filter(.data$final_outcome_category == oc) %>%
      dplyr::mutate(cohort_definition_id = base_id + i)
  })

  Reduce(dplyr::union_all, outcome_cohorts, init = all_outcomes)
}

n_outcomes    <- length(outcome_categories)
stride        <- 1L + n_outcomes
hipp_base_id  <- 1L
pps_base_id   <- 1L + stride
hipps_base_id <- 1L + 2L * stride

hipps_src <- cdm$res_age %>%
  dplyr::mutate(
    hipps_end_date = dplyr::case_when(
      is.na(.data$hip_end_date) ~ .data$pps_end_date,
      .default = .data$hip_end_date
    )
  )

combined <- make_window_cohorts(cdm$res_age, "hip_end_date", hipp_base_id) %>%
  dplyr::union_all(make_window_cohorts(cdm$res_age, "pps_end_date", pps_base_id)) %>%
  dplyr::union_all(make_window_cohorts(hipps_src, "hipps_end_date", hipps_base_id))

window_names <- c("hipp", "pps", "hipps")
cohort_set_ref <- do.call(rbind, lapply(seq_along(window_names), function(w) {
  base <- 1L + (w - 1L) * stride
  data.frame(
    cohort_definition_id = c(base, base + seq_len(n_outcomes)),
    cohort_name = c(window_names[w], paste0(window_names[w], "_", tolower(outcome_categories))),
    stringsAsFactors = FALSE
  )
}))

cdm$hipps_cohort_table <- combined %>%
  dplyr::mutate(
    cohort_start_date = as.Date(.data$cohort_start_date),
    cohort_end_date = as.Date(.data$cohort_end_date)
  ) %>%
  dplyr::filter(.data$cohort_start_date < .data$cohort_end_date) %>%
  omopgenerics::newCohortTable(cohortSetRef = cohort_set_ref, .softValidation = TRUE) %>%
  PatientProfiles::addInObservation(indexDate = "cohort_start_date", nameStyle = "start_in_obs") %>%
  PatientProfiles::addInObservation(indexDate = "cohort_end_date", nameStyle = "end_in_obs") %>%
  dplyr::filter(.data$start_in_obs == 1, .data$end_in_obs == 1) %>%
  dplyr::compute(name = "hipps_cohort_table") %>%
  omopgenerics::newCohortTable(cohortSetRef = cohort_set_ref, .softValidation = TRUE)

# -- Denominator --------------------------------------------------------------
ages   <- cdm$res_age %>% dplyr::pull(.data$age_pregnancy_start)
minAge <- max(min(ages, na.rm = TRUE), 0L)
maxAge <- max(max(ages, na.rm = TRUE), 0L)

logMsg("Generating denominator cohorts")
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm,
  name = "hipps_denom",
  sex = c("Female", "Both"),
  ageGroup = list(
    `0 to 150`   = c(0L, 150L),
    `min to max` = c(minAge, maxAge),
    `12 to 55`   = c(12L, 55L),
    `<35`        = c(0L, 34L),
    `>=35`       = c(35L, 150L)
  )
)

# -- Incidence ----------------------------------------------------------------
logMsg("Estimating incidence")
incRes <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "hipps_denom",
  outcomeTable = "hipps_cohort_table",
  interval = c("overall", "years"),
  outcomeWashout = 0,
  repeatedEvents = TRUE
)

# -- Period prevalence --------------------------------------------------------
logMsg("Building prevalence cohort table")
cdm$hipps_prev_cohort <- cdm$hipps_cohort_table %>%
  dplyr::mutate(
    cohort_start_date = !!CDMConnector::dateadd("cohort_end_date", -1L, interval = "day")
  ) %>%
  dplyr::compute(name = "hipps_prev_cohort") %>%
  omopgenerics::newCohortTable(cohortSetRef = cohort_set_ref, .softValidation = TRUE)

logMsg("Estimating period prevalence")
prevRes <- IncidencePrevalence::estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "hipps_denom",
  outcomeTable = "hipps_prev_cohort",
  interval = c("overall", "years")
)

# -- Characteristics ----------------------------------------------------------
logMsg("Summarising cohort characteristics")
charRes <- cdm$hipps_cohort_table %>%
  PatientProfiles::addAge(
    ageGroup = list(
      `0 - 9`   = c(0, 9),
      `10 - 19` = c(10, 19),
      `20 - 29` = c(20, 29),
      `30 - 39` = c(30, 39),
      `40 - 49` = c(40, 49),
      `>=50`    = c(50, 150)
    )
  ) %>%
  CohortCharacteristics::summariseCharacteristics(
    strata = list("age_group", "final_outcome_category")
  )

# -- Export the summarised results -------------------------------------------
logMsg("Exporting incidence, prevalence, and characteristics")
omopgenerics::exportSummarisedResult(
  incRes, minCellCount = minCellCount,
  fileName = file.path(exportFolder, "{date}_{cdm_name}_incidence.csv")
)
omopgenerics::exportSummarisedResult(
  prevRes, minCellCount = minCellCount,
  fileName = file.path(exportFolder, "{date}_{cdm_name}_prevalence.csv")
)
omopgenerics::exportSummarisedResult(
  charRes, minCellCount = minCellCount,
  fileName = file.path(exportFolder, "{date}_{cdm_name}_characteristics.csv")
)

# NOTE: deliberately NOT dropping the episode table — it belongs to the data partner.

# =============================================================================
# Step 7 — export aggregated patient summaries (reads the local RDS; no upload)
# =============================================================================
logMsg("Running exportPregnancies")
PregnancyIdentifier::exportPregnancies(
  cdm          = cdm,
  outputFolder = outputFolder,
  exportFolder = exportFolder,
  minCellCount = minCellCount
)

PregnancyIdentifier::zipExportFolder(exportFolder)

logMsg("Resume run complete")
message("Done. Results are in: ", exportFolder)
