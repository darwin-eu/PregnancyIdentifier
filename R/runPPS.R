# Copyright (c) 2024 Louisa Smith
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Run the PPS Algorithm to Identify Pregnancy Episodes
#'
#' This function executes the Pregnancy Progression Signature (PPS) algorithm against a Common Data Model (CDM) instance.
#' It inserts the required PPS concept lookup table, extracts gestational timing evidence (e.g., gestational weeks, trimesters) across OMOP clinical domains,
#' assembles person-level gestational timing records, and writes intermediate episode and summary files to the specified output directory.
#'
#' @param cdm A `cdm_reference` object; must include all OMOP tables and structure needed for pregnancy concept search.
#' @param outputDir Character. Directory path where intermediate and output RDS files will be saved.
#' @param startDate Date (`Date(1)`). Earliest clinical date to be considered for gestational timing evidence (default: `"1900-01-01"`).
#' @param endDate Date (`Date(1)`). Latest clinical date to be considered for gestational timing evidence (default: `Sys.Date()`).
#' @param logger Optional `log4r` logger object for emitting information and debug messages.
#'
#' @return Returns the input `cdm_reference` invisibly, possibly modified with intermediate tables in its environment.
#'         Main results are side effects: RDS files with person-level gestational timing episodes and summary statistics are written to `outputDir`.
#' @export
runPps <- function(cdm,
                   outputDir,
                   startDate = as.Date("1900-01-01"),
                   endDate   = Sys.Date(),
                   logger = NULL) {

  # ---- validation ----
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertCharacter(outputDir, len = 1)
  checkmate::assertDate(startDate)
  checkmate::assertDate(endDate)
  checkmate::assertClass(logger, "logger", null.ok = TRUE)

  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

  # ----------------------------------------------------------
  # Insert PPS concept lookup table
  # ----------------------------------------------------------
  logInfo(logger, "Inserting PPS concepts")

  ppsConcepts <- readxl::read_excel(
    system.file("concepts", "PPS_concepts.xlsx", package = "PregnancyIdentifier")
  ) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(domain_concept_id = as.integer(domain_concept_id))

  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "preg_pps_concepts",
    table = ppsConcepts
  )

  logInfo(logger, "START Running PPS")

  # ----------------------------------------------------------
  # Pull PPS concepts from OMOP domain tables
  # ----------------------------------------------------------
  cdm <- inputGtConcepts(
    cdm = cdm,
    startDate = startDate,
    endDate = endDate,
    logger = logger
  )

  # ----------------------------------------------------------
  # Get gestational timing information for each person
  # ----------------------------------------------------------
  logInfo(logger, "Get gestational timing information")
  ppsEpisodes <- getPpsEpisodes(cdm, outputDir)

  # ----------------------------------------------------------
  # Get min and max dates for each episode
  # ----------------------------------------------------------
  logInfo(logger, "Get min and max dates for episodes")
  ppsMinMax <- getEpisodeMaxMinDates(ppsEpisodes)

  saveRDS(ppsEpisodes, file.path(outputDir, "pps_gest_timing_episodes.rds"))
  saveRDS(ppsMinMax,  file.path(outputDir, "pps_min_max_episodes.rds"))

  # Drop temporary source tables
  omopgenerics::dropSourceTable(cdm, "input_gt_concepts_df")
}

# ============================================================
# Pull PPS concepts from OMOP domain tables
# ============================================================

# Pulls concepts from a single OMOP table and normalizes column names so that
# all downstream logic can operate on a consistent schema.
pullPpsDomain <- function(cdm,
                          tableName,
                          outputName,
                          dateColumn,
                          conceptIdColumn,
                          startDate,
                          endDate,
                          logger) {

  logInfo(logger, sprintf("Pulling data from %s", tableName))

  cdm[[outputName]] <- cdm[[tableName]] %>%
    dplyr::filter(
      .data[[dateColumn]] >= startDate,
      .data[[dateColumn]] <= endDate
    ) %>%
    dplyr::transmute(
      person_id,
      domain_concept_start_date = .data[[dateColumn]],
      domain_concept_id         = .data[[conceptIdColumn]]
    ) %>%
    dplyr::inner_join(cdm$preg_pps_concepts, by = "domain_concept_id") %>%
    dplyr::distinct() %>%
    dplyr::compute()

  cdm
}

inputGtConcepts <- function(cdm, startDate, endDate, logger) {

  # Specification of all OMOP tables containing pregnancy-related concepts
  domainSpecs <- tibble::tribble(
    ~table_name,             ~output_name, ~date_column,              ~concept_id_column,
    "condition_occurrence",  "c_o",         "condition_start_date",    "condition_concept_id",
    "procedure_occurrence",  "p_o",         "procedure_date",          "procedure_concept_id",
    "observation",           "o_df",        "observation_date",        "observation_concept_id",
    "measurement",           "m_df",        "measurement_date",        "measurement_concept_id",
    "visit_occurrence",      "v_o",         "visit_start_date",        "visit_concept_id"
  )

  # Pull concepts from each domain table
  for (i in seq_len(nrow(domainSpecs))) {
    cdm <- pullPpsDomain(
      cdm               = cdm,
      tableName         = domainSpecs$table_name[[i]],
      outputName        = domainSpecs$output_name[[i]],
      dateColumn        = domainSpecs$date_column[[i]],
      conceptIdColumn   = domainSpecs$concept_id_column[[i]],
      startDate         = startDate,
      endDate           = endDate,
      logger            = logger
    )
  }

  # Union all concepts into a single table
  cdm$input_gt_concepts_df <- purrr::reduce(
    cdm[domainSpecs$output_name],
    dplyr::union_all
  ) %>%
    dplyr::compute()

  # Remove intermediate tables
  omopgenerics::dropSourceTable(cdm, domainSpecs$output_name)
}

# ============================================================
# Episode construction logic
# ============================================================

# For the below:
#   t = time (actual observed difference between records)
#   c = concept (expected difference based on clinician knowledge)
#
# A record is considered compatible with another record if the observed
# difference in months falls within the expected gestational window.
.agrees <- function(df, later, earlier, slackMonths = 2) {

  # Observed time difference (months)
  deltaMonths <- as.numeric(
    difftime(
      df$domain_concept_start_date[later],
      df$domain_concept_start_date[earlier],
      units = "days"
    )
  ) / 30

  # Expected min / max month differences between concepts
  maxExpected <- df$max_month[later] - df$min_month[earlier] + slackMonths
  minExpected <- df$min_month[later] - df$max_month[earlier] - slackMonths

  deltaMonths <= maxExpected && deltaMonths >= minExpected
}

recordsComparison <- function(personDf, i) {

  # ----------------------------------------------------------
  # Compare current record to all previous records
  # ----------------------------------------------------------
  for (j in seq_len(i - 1)) {
    if (.agrees(personDf, i, i - j)) {
      return(TRUE) # return early — agreement only needs to occur once
    }
  }

  # ----------------------------------------------------------
  # Compare records surrounding record i
  # This handles cases where record i is an outlier
  # ----------------------------------------------------------
  left  <- i - 1
  right <- nrow(personDf) - i

  if (left == 0 || right == 0) return(FALSE)

  for (l in seq_len(left)) {
    for (r in seq_len(right)) {
      if (.agrees(personDf, i + r, i - l)) {
        return(TRUE)
      }
    }
  }

  FALSE
}

assignEpisodes <- function(personDf, ...) {

  if (nrow(personDf) == 1) {
    personDf$person_episode_number <- 1L
    return(personDf)
  }

  # Treat the first record as belonging to the first episode
  episodeNumber <- integer(nrow(personDf))
  episodeNumber[1] <- 1L

  for (i in 2:nrow(personDf)) {

    # Time difference (months) between consecutive records
    deltaMonths <- as.numeric(
      difftime(
        personDf$domain_concept_start_date[i],
        personDf$domain_concept_start_date[i - 1],
        units = "days"
      )
    ) / 30

    # Determine whether concepts are temporally compatible
    agreement <- recordsComparison(personDf, i)

    # Start a new episode if:
    #   - concepts are incompatible and gap > 1 month (retry period), OR
    #   - gap > 10 months (definitely a new pregnancy)
    startNewEpisode <-
      (!agreement && deltaMonths > 1) ||
      (deltaMonths > 10)

    episodeNumber[i] <- episodeNumber[i - 1] + as.integer(startNewEpisode)
  }

  # ----------------------------------------------------------
  # Remove implausibly long episodes (> 12 months)
  # Pregnancy should last ~9–10 months, plus some documentation noise
  # ----------------------------------------------------------
  spans <- tapply(
    personDf$domain_concept_start_date,
    episodeNumber,
    function(d)
      as.numeric(difftime(max(d), min(d), units = "days")) / 30
  )

  invalidEpisodes <- as.integer(names(spans)[spans > 12])
  episodeNumber[episodeNumber %in% invalidEpisodes] <- 0L

  # Renumber remaining episodes sequentially
  validEpisodes <- sort(unique(episodeNumber[episodeNumber != 0L]))
  episodeNumber <- ifelse(
    episodeNumber == 0L,
    0L,
    match(episodeNumber, validEpisodes)
  )

  personDf$person_episode_number <- episodeNumber
  personDf
}

# ============================================================
# Episode extraction + summarisation
# ============================================================

getPpsEpisodes <- function(cdm, outputDir) {

  # Identify all patients with pregnancy-related concepts
  cdm$patients_with_preg_concepts <- cdm$input_gt_concepts_df %>%
    dplyr::filter(!is.na(domain_concept_start_date)) %>%
    dplyr::compute("patients_with_preg_concepts")

  # Save concept frequency counts for QA
  cdm$patients_with_preg_concepts %>%
    dplyr::group_by(domain_concept_id, domain_concept_name) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    utils::write.csv(
      file.path(outputDir, "pps_concept_counts.csv"),
      row.names = FALSE
    )

  # ----------------------------------------------------------
  # Restrict to women of reproductive age
  # ----------------------------------------------------------
  patientsDf <- cdm$patients_with_preg_concepts %>%
    dplyr::inner_join(
      cdm$person %>%
        dplyr::select(
          "person_id",
          "gender_concept_id",
          "year_of_birth",
          "month_of_birth",
          "day_of_birth"
        ),
      by = "person_id"
    ) %>%
    dplyr::mutate(
      day_of_birth   = dplyr::coalesce(.data$day_of_birth, 1L),
      month_of_birth = dplyr::coalesce(.data$month_of_birth, 1L),
      date_of_birth  = as.Date(paste0(.data$year_of_birth, "-", .data$month_of_birth, "-", .data$day_of_birth)),
      age =
        !!CDMConnector::datediff(
          "date_of_birth",
          "domain_concept_start_date",
          "day"
        ) / 365
    ) %>%
    dplyr::filter(
      .data$gender_concept_id == 8532,
      .data$age >= 15,
      .data$age < 56
    ) %>%
    dplyr::collect(page_size = 50000) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$domain_concept_start_date)

  if (nrow(patientsDf) == 0) return(patientsDf)

  patientsDf %>%
    dplyr::group_modify(assignEpisodes)
}

getEpisodeMaxMinDates <- function(ppsEpisodesDf) {

  # If no episode numbers exist, fall back to person-level episodes
  if (!"person_episode_number" %in% names(ppsEpisodesDf)) {
    ppsEpisodesDf$person_episode_number <-
      if (nrow(ppsEpisodesDf) > 0) ppsEpisodesDf$person_id else integer(0)
  }

  ppsEpisodesDf %>%
    dplyr::filter(!is.na(.data$person_episode_number)) %>%
    dplyr::group_by(.data$person_id, .data$person_episode_number) %>%
    dplyr::summarise(
      episode_min_date = min(.data$domain_concept_start_date, na.rm = TRUE),
      episode_max_date = max(.data$domain_concept_start_date, na.rm = TRUE),
      episode_max_date_plus_two_months = lubridate::add_with_rollback(.data$episode_max_date, lubridate::period(months = 2)),
      n_gt_concepts = dplyr::n_distinct(.data$domain_concept_id),
      .groups = "drop"
    )
}
