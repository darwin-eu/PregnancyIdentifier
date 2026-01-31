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
#' @param debugMode (`Logical`) Should intermediat datasets be written to the output folder for debugging? TRUE or FALSE (default)
#'
#' @return Returns the input `cdm_reference` invisibly, possibly modified with intermediate tables in its environment.
#'         Main results are side effects: RDS files with person-level gestational timing episodes and summary statistics are written to `outputDir`.
#' @export
runPps <- function(cdm,
                   outputDir,
                   startDate = as.Date("1900-01-01"),
                   endDate   = Sys.Date(),
                   logger = NULL,
                   debugMode = FALSE) {

  # ---- validation ----
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertCharacter(outputDir, len = 1)
  checkmate::assertDate(startDate)
  checkmate::assertDate(endDate)
  checkmate::assertClass(logger, "logger", null.ok = TRUE)

  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

  logInfo(logger, "START Running PPS")

  # ----------------------------------------------------------
  # Get gestational timing information for each person
  # ----------------------------------------------------------
  logInfo(logger, "Get gestational timing information")
  ppsEpisodes <- getPpsEpisodes(cdm, outputDir)

  # ----------------------------------------------------------
  # Get min and max dates and outcomes for each episode
  # ----------------------------------------------------------
  logInfo(logger, "Get min and max dates for episodes")
  ppsMinMax <- getEpisodeMaxMinDates(ppsEpisodes)

  ppsWithOutcomes <- outcomesPerEpisode(ppsMinMax, ppsEpisodes, cdm, logger) %>%
    addOutcomes(ppsMinMax)

  if (nrow(ppsWithOutcomes) == 0) {
    ppsWithOutcomes <- emptyPpsEpisodes()
  }
  if (debugMode) {
    saveRDS(
      if (nrow(ppsEpisodes) == 0) emptyPpsGestTiming() else ppsEpisodes,
      file.path(outputDir, "pps_gest_timing_episodes.rds")
    )
    saveRDS(
      if (nrow(ppsMinMax) == 0) emptyPpsMinMax() else ppsMinMax,
      file.path(outputDir, "pps_min_max_episodes.rds")
    )
  }
  saveRDS(ppsWithOutcomes, file.path(outputDir, "pps_episodes.rds"))

  cdm
}

getPpsEpisodes <- function(cdm, outputDir, slackMonths = 2) {

  # ------------------------------------------------------------------
  # Episode logic (per-person):
  # - Each record is a pregnancy-related concept with an observed date.
  # - Concepts have expected gestational windows (pps_min_month/pps_max_month).
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
    delta <- as.numeric(difftime(df$pps_concept_start_date[later],
                                 df$pps_concept_start_date[earlier],
                                 units = "days")) / 30

    # Expected spacing bounds derived from gestational windows:
    # latest plausible month for "later" minus earliest plausible month for "earlier"
    maxExp <- df$pps_max_month[later] - df$pps_min_month[earlier] + slackMonths
    # earliest plausible month for "later" minus latest plausible month for "earlier"
    minExp <- df$pps_min_month[later] - df$pps_max_month[earlier] - slackMonths

    delta >= minExp && delta <= maxExp
  }

  hasAgreement <- function(df, i) {
    if (i <= 1) return(TRUE)

    # Reasoning: record i can stay in the current episode if it is compatible
    # with at least one earlier record in the same person's timeline.
    # i is the current record index. j iterates over all previous records (seq_len(i-1))
    if (any(purrr::map_lgl(seq_len(i-1), \(j) agrees(df, i, j)))) return(TRUE)

    # Reasoning: record i itself may be an "outlier" (bad date/code),
    # so also allow keeping the episode if there exists *any* pair
    # of records that bracket i (some earlier record and some later record)
    # that agree with each other—suggesting the pregnancy is continuous
    # and i is just noise.
    earlier  <- seq_len(i - 1)
    later <- (i + 1):nrow(df)
    if (length(later) == 0) return(FALSE)

    # For each later record, is there any earlier record that agrees?
    # If there is any agreement between any later record and any earlier record, return TRUE => same episode
    any(purrr::map_lgl(later, \(r) any(purrr::map_lgl(earlier, \(l) agrees(df, r, l)))))
  }

  assignEpisodes <- function(df) {
    n <- nrow(df)
    if (n <= 1) {
      # Single record => trivially one episode
      return(dplyr::mutate(df, person_episode_number = 1L))
    }

    # Consecutive gaps in months:
    # used as a pragmatic "break" signal when compatibility is absent.
    gaps <- c(0, as.numeric(diff(df$pps_concept_start_date)) / 30)

    # Start a new episode at record i if:
    # - No agreement with pregnancy context AND gap is meaningfully large (> 1 mo),
    #   which avoids splitting episodes due to short documentation retries/noise.
    # - OR gap is huge (> 10 mo), which is taken as definitive evidence of a new
    #   pregnancy regardless of concept compatibility.
    starts <- purrr::map_lgl(seq_len(n), \(i) i > 1 && ((!hasAgreement(df, i) && gaps[i] > 1) || gaps[i] > 10))

    # Episode number increments each time we "start" a new episode
    ep <- 1L + cumsum(starts)

    # Post-filter: pregnancies shouldn't span > 12 months (9–10 expected, + noise).
    # Mark those episodes invalid by setting episode number to 0.
    spans <- tapply(df$pps_concept_start_date, ep, \(d)
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

  patientsDf <- cdm$preg_pps_records %>%
   dplyr::collect()

  # QA: write concept frequencies
  patientsDf %>%
    dplyr::count(.data$pps_concept_id, .data$pps_concept_name, name = "n") %>%
    utils::write.csv(file.path(outputDir, "pps_concept_counts.csv"), row.names = FALSE)

  if (nrow(patientsDf) == 0) return(patientsDf)

  patientsDf %>%
    tidyr::nest(.by = person_id) %>%
    dplyr::mutate(data = purrr::map(.data$data, assignEpisodes)) %>%
    tidyr::unnest("data")
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
      episode_min_date = min(.data$pps_concept_start_date, na.rm = TRUE),
      episode_max_date = max(.data$pps_concept_start_date, na.rm = TRUE),
      episode_max_date_plus_two_months = lubridate::add_with_rollback(.data$episode_max_date, lubridate::period(months = 2)),
      n_gt_concepts = dplyr::n_distinct(.data$pps_concept_id),
      .groups = "drop"
    )
}

# ---- Outcomes for PPS episodes ----------------------------------------------

getOutcomeDate <- function(x, outcome) {
  # outcomes_list contains strings like "YYYY-MM-DD,concept_id,CATEGORY"
  # return the first date matching CATEGORY, else NA
  hit <- grep(outcome, x)
  if (length(hit)) strsplit(x[hit[1]], ",")[[1]][1] else NA_character_
}

outcomesPerEpisode <- function(ppsMinMaxDf, ppsEpisodesDf, cdm, logger) {
  # Outcomes are inferred for PPS episodes within an episode-specific window:
  # - lookback: 14 days before episode_max_date
  # - lookahead: earliest of:
  #   (a) the day before the next episode starts, or
  #   (b) the latest plausible pregnancy outcome date based on the last concept
  #
  # Within that window, choose one outcome using the Matcho hierarchy.

  # Ensure ppsEpisodesDf has person_episode_number
  if (!"person_episode_number" %in% names(ppsEpisodesDf)) {
    ppsEpisodesDf$person_episode_number <- if (nrow(ppsEpisodesDf)) 1L else integer(0)
  }

  pregnantDates <- ppsMinMaxDf %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$person_episode_number, .data$episode_min_date, .by_group = TRUE) %>%
    dplyr::mutate(
      # next episode bounds the lookahead to avoid borrowing outcomes from later pregnancies
      next_closest_episode_date = dplyr::coalesce(
        dplyr::lead(.data$episode_min_date) - lubridate::days(1),
        lubridate::ymd("2999-01-01")
      ),
      episode_max_date_minus_lookback_window = .data$episode_max_date - lubridate::days(14)
    ) %>%
    dplyr::ungroup()

  # Determine the last plausible pregnancy outcome date based on the last concept
  # months_to_add = 11 - pps_min_month, then add to the last concept date.
  maxPregnancyDateDf <- ppsEpisodesDf %>%
    dplyr::group_by(.data$person_id, .data$person_episode_number) %>%
    dplyr::arrange(
      dplyr::desc(.data$pps_concept_start_date),
      dplyr::desc(.data$pps_max_month),
      dplyr::desc(.data$pps_min_month),
      .by_group = TRUE
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      .data$person_id, .data$person_episode_number,
      max_pregnancy_date = lubridate::add_with_rollback(
        .data$pps_concept_start_date,
        lubridate::period(months = 11L - as.integer(.data$pps_min_month))
      )
    )

  pregnantDates <- pregnantDates %>%
    dplyr::left_join(maxPregnancyDateDf, by = c("person_id", "person_episode_number")) %>%
    dplyr::mutate(
      # final lookahead is the earliest of (next episode) and (max plausible outcome date)
      episode_max_date_plus_lookahead_window =
        pmin(.data$next_closest_episode_date, .data$max_pregnancy_date, na.rm = TRUE)
    )

  # Pull outcome concepts (LB/SB/ECT/SA/AB/DELIV) that fall within each episode window
  pregRelatedConcepts <- cdm$preg_hip_records %>%
    dplyr::filter(.data$category %in% c("LB", "SB", "DELIV", "ECT", "AB", "SA")) %>%
    dplyr::collect() %>%
    dplyr::inner_join(
      pregnantDates,
      by = dplyr::join_by(
        person_id,
        dplyr::between(
          visit_date,
          episode_max_date_minus_lookback_window,
          episode_max_date_plus_lookahead_window
        )
      ),
      relationship = "many-to-many"
    )

  outcomesListDf <- pregRelatedConcepts %>%
    dplyr::mutate(lst = paste(.data$visit_date, ",", .data$concept_id, ",", .data$category)) %>%
    dplyr::group_by(
      .data$person_id, .data$person_episode_number, .data$episode_min_date, .data$episode_max_date,
      .data$episode_max_date_minus_lookback_window, .data$episode_max_date_plus_lookahead_window,
      .data$n_gt_concepts
    ) %>%
    dplyr::summarise(outcomes_list = list(sort(unique(.data$lst))), .groups = "drop") %>%
    dplyr::filter(purrr::map_lgl(.data$outcomes_list, ~ length(.x) > 0))

  # Matcho hierarchy (priority order) for PPS outcomes
  outcome_order <- c("LB", "SB", "ECT", "SA", "AB", "DELIV")
  date_cols <- paste0(tolower(outcome_order), "_delivery_date")

  outcomesListDf %>%
    dplyr::mutate(
      # Extract first matching date per outcome category
      !!!rlang::set_names(
        purrr::map(outcome_order, \(cat)
                   rlang::expr(purrr::map_chr(.data$outcomes_list, getOutcomeDate, !!cat))
        ),
        date_cols
      ),
      # Choose one category/date by hierarchy
      algo2_category = dplyr::case_when(
        !is.na(.data$lb_delivery_date)    ~ "LB",
        !is.na(.data$sb_delivery_date)    ~ "SB",
        !is.na(.data$ect_delivery_date)   ~ "ECT",
        !is.na(.data$sa_delivery_date)    ~ "SA",
        !is.na(.data$ab_delivery_date)    ~ "AB",
        !is.na(.data$deliv_delivery_date) ~ "DELIV"
      ),
      algo2_outcome_date = lubridate::ymd(dplyr::case_when(
        !is.na(.data$lb_delivery_date)    ~ .data$lb_delivery_date,
        !is.na(.data$sb_delivery_date)    ~ .data$sb_delivery_date,
        !is.na(.data$ect_delivery_date)   ~ .data$ect_delivery_date,
        !is.na(.data$sa_delivery_date)    ~ .data$sa_delivery_date,
        !is.na(.data$ab_delivery_date)    ~ .data$ab_delivery_date,
        !is.na(.data$deliv_delivery_date) ~ .data$deliv_delivery_date
      ))
    )
}

addOutcomes <- function(outcomesDf, ppsMinMaxDf) {
  # Attach inferred PPS outcomes back onto the PPS episode min/max table
  ppsMinMaxDf %>%
    dplyr::left_join(
      outcomesDf %>%
        dplyr::select(
          "person_id", "person_episode_number", "episode_min_date",
          "algo2_category", "algo2_outcome_date", "n_gt_concepts"
        ),
      by = c("person_id", "person_episode_number", "episode_min_date", "n_gt_concepts")
    )
}
