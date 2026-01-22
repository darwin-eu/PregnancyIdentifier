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
inputGtConcepts <- function(cdm, startDate, endDate, logger) {

  domainSpecs <- tibble::tribble(
    ~table_name,            ~date_column,            ~concept_id_column,
    "condition_occurrence", "condition_start_date",  "condition_concept_id",
    "procedure_occurrence", "procedure_date",        "procedure_concept_id",
    "observation",          "observation_date",      "observation_concept_id",
    "measurement",          "measurement_date",      "measurement_concept_id",
    "visit_occurrence",     "visit_start_date",      "visit_concept_id"
  )

  pulls <- purrr::map(seq_len(nrow(domainSpecs)), \(i) {
    spec <- domainSpecs[i, ]

    tableName <- spec$table_name[[1]]
    dateCol   <- spec$date_column[[1]]
    cidCol    <- spec$concept_id_column[[1]]

    logInfo(logger, sprintf("Pulling data from %s", tableName))

    cdm[[tableName]] %>%
      dplyr::filter(.data[[dateCol]] >= startDate, .data[[dateCol]] <= endDate) %>%
      dplyr::transmute(
        person_id,
        domain_concept_start_date = .data[[dateCol]],
        domain_concept_id         = .data[[cidCol]]
      ) %>%
      dplyr::inner_join(cdm$preg_pps_concepts, by = "domain_concept_id") %>%
      dplyr::distinct()
  })

  cdm$input_gt_concepts_df <- purrr::reduce(pulls, dplyr::union_all) %>%
    dplyr::filter(!is.na(domain_concept_start_date)) %>%
    dplyr::compute()

  cdm
}

getPpsEpisodes <- function(cdm, outputDir, slackMonths = 2) {

  # ------------------------------------------------------------------
  # Episode logic (per-person):
  # - Each record is a pregnancy-related concept with an observed date.
  # - Concepts have expected gestational windows (min_month/max_month).
  # - Two records "agree" if their observed spacing (months) is consistent
  #   with what we'd expect given their gestational windows (± slack).
  #
  # Episodes are created by scanning records in date order:
  # - By default, record 1 starts episode 1.
  # - For each subsequent record i:
  #     * If it "fits" with earlier context, keep same episode.
  #     * Otherwise, start a new episode only if there is a meaningful gap:
  #         - gap > 1 month AND no agreement (to allow short "retry"/noise)
  #         - OR gap > 10 months (definitely a different pregnancy)
  #
  # After assignment:
  # - Drop implausibly long episodes (> 12 months) as invalid (set to 0).
  # - Renumber remaining episodes sequentially (1..K), keep 0 as invalid.
  # ------------------------------------------------------------------

  agrees <- function(df, later, earlier) {
    # Observed spacing in months (approx)
    delta <- as.numeric(difftime(df$domain_concept_start_date[later],
                                 df$domain_concept_start_date[earlier],
                                 units = "days")) / 30

    # Expected spacing bounds derived from gestational windows:
    # latest plausible month for "later" minus earliest plausible month for "earlier"
    maxExp <- df$max_month[later] - df$min_month[earlier] + slackMonths
    # earliest plausible month for "later" minus latest plausible month for "earlier"
    minExp <- df$min_month[later] - df$max_month[earlier] - slackMonths

    delta >= minExp && delta <= maxExp
  }

  hasAgreement <- function(df, i) {
    if (i <= 1) return(TRUE)

    # Reasoning: record i can stay in the current episode if it is compatible
    # with at least one earlier record in the same person's timeline.
    if (any(purrr::map_lgl(seq_len(i - 1), \(j) agrees(df, i, j)))) return(TRUE)

    # Reasoning: record i itself may be an "outlier" (bad date/code),
    # so also allow keeping the episode if there exists *any* pair
    # of records that bracket i (some left record and some right record)
    # that agree with each other—suggesting the pregnancy is continuous
    # and i is just noise.
    left  <- seq_len(i - 1)
    right <- (i + 1):nrow(df)
    if (length(right) == 0) return(FALSE)

    any(purrr::map_lgl(right, \(r) any(purrr::map_lgl(left, \(l) agrees(df, r, l)))))
  }

  assignEpisodes <- function(df, ...) {
    n <- nrow(df)
    if (n <= 1) {
      # Single record => trivially one episode
      return(dplyr::mutate(df, person_episode_number = 1L))
    }

    # Consecutive gaps in months:
    # used as a pragmatic "break" signal when compatibility is absent.
    gaps <- c(0, as.numeric(diff(df$domain_concept_start_date)) / 30)

    # Start a new episode at record i if:
    # - No agreement with pregnancy context AND gap is meaningfully large (> 1 mo),
    #   which avoids splitting episodes due to short documentation retries/noise.
    # - OR gap is huge (> 10 mo), which is taken as definitive evidence of a new
    #   pregnancy regardless of concept compatibility.
    starts <- purrr::map_lgl(seq_len(n), \(i)
                             i > 1 && ( (!hasAgreement(df, i) && gaps[i] > 1) || gaps[i] > 10 )
    )

    # Episode number increments each time we "start" a new episode
    ep <- 1L + cumsum(starts)

    # Post-filter: pregnancies shouldn't span > 12 months (9–10 expected, + noise).
    # Mark those episodes invalid by setting episode number to 0.
    spans <- tapply(df$domain_concept_start_date, ep, \(d)
                    as.numeric(difftime(max(d), min(d), units = "days")) / 30
    )
    invalid <- as.integer(names(spans)[spans > 12])
    ep[ep %in% invalid] <- 0L

    # Renumber valid episodes to be consecutive (1..K); keep 0 for invalid
    valid <- sort(unique(ep[ep != 0L]))
    ep <- ifelse(ep == 0L, 0L, match(ep, valid))

    dplyr::mutate(df, person_episode_number = ep)
  }

  # ------------------------------------------------------------------
  # Episode extraction pipeline
  # ------------------------------------------------------------------

  # Restrict to women of reproductive age and bring to R for per-person iteration
  patientsDf <- cdm$input_gt_concepts_df %>%
    dplyr::inner_join(
      dplyr::select(cdm$person, "person_id", "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth"),
      by = "person_id"
    ) %>%
    dplyr::mutate(
      day_of_birth   = dplyr::coalesce(.data$day_of_birth, 1L),
      month_of_birth = dplyr::coalesce(.data$month_of_birth, 1L),
      date_of_birth  = as.Date(paste0(.data$year_of_birth, "-", .data$month_of_birth, "-", .data$day_of_birth)),
      age = !!CDMConnector::datediff("date_of_birth", "domain_concept_start_date", "day") / 365
    ) %>%
    dplyr::filter(.data$gender_concept_id == 8532, .data$age >= 15, .data$age < 56) %>%
    dplyr::collect()

  # QA: write concept frequencies
  patientsDf %>%
    dplyr::count(domain_concept_id, domain_concept_name, name = "n") %>%
    utils::write.csv(file.path(outputDir, "pps_concept_counts.csv"), row.names = FALSE)

  if (nrow(patientsDf) == 0) return(patientsDf)

  patientsDf %>%
    tidyr::nest(.by = person_id) %>%
    dplyr::mutate(data = purrr::map(data, assignEpisodes)) %>%
    tidyr::unnest(data)
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
