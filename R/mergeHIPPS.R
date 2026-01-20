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

# From: https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/algorithm/Merge_HIPPS_episodes.R

#' Merge HIP and PPS episodes into a unified HIPPS episode table
#'
#' Reads previously-saved HIP and PPS outputs from `outputDir`, infers pregnancy outcomes
#' for PPS episodes using a lookback/lookahead window around each PPS episode end,
#' merges HIP and PPS episodes based on temporal overlap, resolves many-to-many overlaps
#' by selecting the best-matching pairs, and returns a single per-person episode table
#' with standardized columns used downstream by ESD and reporting.
#'
#' Key steps:
#' \itemize{
#'   \item Load HIP episodes, PPS episode rows, and PPS episode min/max summary tables
#'   \item Infer PPS outcomes from `cdm$initial_pregnant_cohort_df` within an episode-specific window
#'   \item Merge HIP + PPS episodes by overlap; flag episodes involved in many-to-many overlaps
#'   \item Remove duplicated overlap matches (retain best matches by end-date proximity and episode plausibility)
#'   \item Add standardized columns and per-person episode ordering
#' }
#'
#' @param cdm (`cdm_reference`) CDM reference containing required tables
#'   (`initial_pregnant_cohort_df`, `pps_concepts`, etc.).
#' @param outputDir (`character(1)`) Directory containing intermediate RDS artifacts:
#'   `PPS_min_max_episodes.rds`, `PPS_gest_timing_episodes.rds`, `HIP_episodes.rds`.
#' @param logger (`logger`) log4r logger.
#'
#' @return Invisibly returns `NULL` and writes `HIPPS_episodes.rds` to `outputDir`.
#' @export
mergeHips <- function(cdm, outputDir, logger) {
  log4r::info(logger, "Merging HIP and PPS into HIPPS")

  ppsMinMaxDf   <- readRDS(file.path(outputDir, "PPS_min_max_episodes.rds"))
  ppsEpisodesDf <- readRDS(file.path(outputDir, "PPS_gest_timing_episodes.rds"))
  hipDf         <- readRDS(file.path(outputDir, "HIP_episodes.rds"))

  outcomesDf <- outcomesPerEpisode(
    ppsMinMaxDf = ppsMinMaxDf,
    ppsEpisodesDf = ppsEpisodesDf,
    cdm = cdm,
    logger = logger
  )

  ppsWithOutcomesDf <- addOutcomes(outcomesDf, ppsMinMaxDf)

  mergedDf <- mergeEpisodes(hipDf, ppsWithOutcomesDf, logger = logger) |>
    dedupeMergedEpisodes(logger = logger) |>
    addMergedEpisodeDetails()

  saveRDS(mergedDf, file.path(outputDir, "HIPPS_episodes.rds"))
  invisible(NULL)
}

# ---- Outcomes for PPS episodes ------------------------------------------------

getOutcomeDate <- function(x, outcome) {
  hit <- grep(outcome, x)
  if (length(hit) > 0) strsplit(x[hit], ",")[[1]][1] else NA
}

outcomesPerEpisode <- function(ppsMinMaxDf, ppsEpisodesDf, cdm, logger) {
  # Get outcomes for Algorithm 2 (PPS):
  # Outcomes are collected from a 'lookback to lookahead window', which is the
  # episode max date minus 14d to the earliest out of i) the next closest
  # episode start date or ii) a number of months of length (10 months - the
  # earliest concept month that could relate to the end of the episode)
  #
  # To assign a measure of support for outcomes identified in HIP episodes, we
  # inferred pregnancy outcomes in the PPS by checking for any outcome within
  # a window of 14 days before the episode end to up to the earliest date from
  # either (1) the next episode start date, or (2) up to 10 months minus the
  # last record’s expected minimum month after the start of the episode. We
  # then selected the outcome based on Matcho et al’s outcome hierarchy assessment

  pregnantDates <- ppsMinMaxDf |>
    dplyr::group_by(.data$person_id) |>
    dplyr::arrange(.data$person_episode_number, .data$episode_min_date, .by_group = TRUE) |>
    dplyr::mutate(
      next_closest_episode_date = dplyr::lead(.data$episode_min_date) - lubridate::days(1),
      previous_episode_date = dplyr::lag(.data$episode_max_date) + lubridate::days(1)
    ) |>
    dplyr::ungroup()

  # Ensure ppsEpisodesDf has person_episode_number (preserve original behavior)
  if (nrow(ppsEpisodesDf) == 0) {
    ppsEpisodesDf <- dplyr::mutate(ppsEpisodesDf, person_episode_number = integer(0))
  } else if (!"person_episode_number" %in% names(ppsEpisodesDf)) {
    ppsEpisodesDf <- dplyr::mutate(ppsEpisodesDf, person_episode_number = 1)
  }

  # Determine the max lookahead date per episode based on the last concept:
  # months_to_add = 11 - min_month, then add to last concept date.
  maxPregnancyDateDf <- ppsEpisodesDf |>
    dplyr::group_by(.data$person_id, .data$person_episode_number) |>
    dplyr::arrange(
      dplyr::desc(.data$domain_concept_start_date),
      dplyr::desc(.data$max_month),
      dplyr::desc(.data$min_month),
      .by_group = TRUE
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      months_to_add = 11L - as.integer(.data$min_month),
      max_pregnancy_date = lubridate::`%m+%`(.data$domain_concept_start_date, lubridate::months(.data$months_to_add))
    ) |>
    dplyr::select("person_id", "person_episode_number", "max_pregnancy_date")

  pregnantDates <- pregnantDates |>
    dplyr::left_join(maxPregnancyDateDf, by = c("person_id", "person_episode_number")) |>
    dplyr::mutate(
      # if there's no next episode impute a distant time
      next_closest_episode_date = dplyr::if_else(
        is.na(.data$next_closest_episode_date),
        lubridate::ymd("2999-01-01"),
        .data$next_closest_episode_date
      ),
      # choose the earliest out of the next episode and the last time we'd expect an outcome
      episode_max_date_plus_lookahead_window = pmin(.data$next_closest_episode_date, .data$max_pregnancy_date, na.rm = TRUE),
      # two weeks before we saw the last concept
      episode_max_date_minus_lookback_window = .data$episode_max_date - lubridate::days(14)
    )

  # begin searching for outcomes within the relevant lookback and lookahead dates
  pregRelatedConcepts <- cdm$initial_pregnant_cohort_df |>
    dplyr::filter(.data$category %in% c("LB", "SB", "DELIV", "ECT", "AB", "SA")) |>
    dplyr::collect() |>
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

  outcomesListDf <- pregRelatedConcepts |>
    dplyr::mutate(lst = paste(.data$visit_date, ",", .data$concept_id, ",", .data$category)) |>
    dplyr::group_by(
      .data$person_id, .data$person_episode_number, .data$episode_min_date, .data$episode_max_date,
      .data$episode_max_date_minus_lookback_window, .data$episode_max_date_plus_lookahead_window,
      .data$n_gt_concepts
    ) |>
    dplyr::summarise(outcomes_list = list(unique(.data$lst)), .groups = "drop") |>
    dplyr::mutate(
      outcomes_list = purrr::discard(.data$outcomes_list, purrr::is_empty),
      outcomes_list = purrr::map(.data$outcomes_list, sort)
    )

  # Matcho hierarchy (priority order) for algorithm 2 outcomes
  outcomeOrder <- c("LB", "SB", "ECT", "SA", "AB", "DELIV")
  dateCols <- paste0(outcomeOrder, "_delivery_date")

  outcomesListDf |>
    dplyr::mutate(
      # Extract first matching date for each outcome category
      !!!rlang::set_names(
        purrr::map(
          outcomeOrder,
          ~ rlang::expr(purrr::map_chr(.data$outcomes_list, getOutcomeDate, !!.x))
        ),
        dateCols
      ),
      algo2_category = dplyr::case_when(
        !is.na(.data$LB_delivery_date)    ~ "LB",
        !is.na(.data$SB_delivery_date)    ~ "SB",
        !is.na(.data$ECT_delivery_date)   ~ "ECT",
        !is.na(.data$SA_delivery_date)    ~ "SA",
        !is.na(.data$AB_delivery_date)    ~ "AB",
        !is.na(.data$DELIV_delivery_date) ~ "DELIV"
      ),
      algo2_outcome_date = dplyr::case_when(
        !is.na(.data$LB_delivery_date)    ~ .data$LB_delivery_date,
        !is.na(.data$SB_delivery_date)    ~ .data$SB_delivery_date,
        !is.na(.data$ECT_delivery_date)   ~ .data$ECT_delivery_date,
        !is.na(.data$SA_delivery_date)    ~ .data$SA_delivery_date,
        !is.na(.data$AB_delivery_date)    ~ .data$AB_delivery_date,
        !is.na(.data$DELIV_delivery_date) ~ .data$DELIV_delivery_date
      ),
      algo2_outcome_date = lubridate::ymd(.data$algo2_outcome_date)
    )
}

addOutcomes <- function(outcomesDf, ppsMinMaxDf) {
  outDf <- outcomesDf |>
    dplyr::select(
      .data$person_id, .data$person_episode_number, .data$episode_min_date,
      .data$algo2_category, .data$algo2_outcome_date, .data$n_gt_concepts
    )

  ppsMinMaxDf |>
    dplyr::left_join(outDf, by = c("person_id", "person_episode_number", "episode_min_date", "n_gt_concepts"))
}

# ---- Merge + dedupe HIP and PPS episodes -------------------------------------

mergeEpisodes <- function(hipDf, ppsWithOutcomesDf, logger) {
  # Merge episodes by checking for any overlap of episodes between the two algorithms.
  #
  # algo1 = HIP episodes
  # algo2 = PPS episodes
  #
  # The following is for checking overlap:
  # - complete overlap
  # - algo1 contains algo2
  # - algo2 contains algo1
  # - start in algo1 is within algo2
  # - start in algo2 is within algo1
  # - end in algo1 is within algo2
  # - end in algo2 is within algo1

  algo1Df <- hipDf |>
    dplyr::rename(
      pregnancy_start = "estimated_start_date",
      pregnancy_end   = "visit_date",
      first_gest_date = "gest_date"
    ) |>
    dplyr::mutate(
      algo1_id = paste(.data$person_id, .data$episode, "1", sep = "_"),
      pregnancy_start = as.Date(.data$pregnancy_start),
      pregnancy_end   = as.Date(.data$pregnancy_end)
    )

  algo2Df <- ppsWithOutcomesDf |>
    dplyr::mutate(
      algo2_id = paste(.data$person_id, .data$person_episode_number, "2", sep = "_"),
      episode_min_date = as.Date(.data$episode_min_date),
      episode_max_date_plus_two_months = as.Date(.data$episode_max_date_plus_two_months)
    )

  allEpisodes <- algo1Df |>
    dplyr::full_join(
      algo2Df,
      by = dplyr::join_by(
        person_id,
        dplyr::overlaps(pregnancy_start, pregnancy_end, episode_min_date, episode_max_date_plus_two_months)
      )
    ) |>
    dplyr::mutate(
      merged_episode_start = as.Date(pmin(.data$first_gest_date, .data$episode_min_date, .data$pregnancy_end)),
      merged_episode_end   = as.Date(pmax(.data$episode_max_date, .data$pregnancy_end)),
      merged_episode_length = as.numeric(difftime(.data$merged_episode_end, .data$merged_episode_start, units = "days")) / 30.25
    )

  # flag duplicates (many-to-many overlap matches)
  addDupFlag <- function(df, idCol, outCol) {
    df |>
      dplyr::group_by(.data[[idCol]]) |>
      dplyr::mutate(
        !!rlang::sym(outCol) := dplyr::if_else(is.na(.data[[idCol]])[1], NA, as.integer(dplyr::n() > 1))
      ) |>
      dplyr::ungroup()
  }
  allEpisodes <- allEpisodes |>
    addDupFlag("algo1_id", "algo1_dup") |>
    addDupFlag("algo2_id", "algo2_dup")

  # overlap diagnostics
  countDistinct <- function(df, ...) df |> dplyr::distinct(...) |> dplyr::tally() |> dplyr::pull(.data$n)

  log4r::info(logger, sprintf("Total initial number of episodes for HIP: %s", countDistinct(algo1Df, .data$person_id, .data$episode)))
  log4r::info(logger, sprintf("Total initial number of episodes for PPS: %s", countDistinct(algo2Df, .data$person_id, .data$person_episode_number)))
  log4r::info(logger, sprintf("Count of HIP episodes that overlap multiple PPS episodes: %s", countDistinct(dplyr::filter(allEpisodes, .data$algo1_dup != 0), .data$algo1_id)))
  log4r::info(logger, sprintf("Count of PPS episodes that overlap multiple HIP episodes: %s", countDistinct(dplyr::filter(allEpisodes, .data$algo2_dup != 0), .data$algo2_id)))
  log4r::info(logger, sprintf("Total number of HIP episodes after merging: %s", countDistinct(allEpisodes, .data$algo1_id) - 1))
  log4r::info(logger, sprintf("Total number of PPS episodes after merging: %s", countDistinct(allEpisodes, .data$algo2_id) - 1))

  allEpisodes
}

dedupeMergedEpisodes <- function(mergedDf, logger) {
  # Remove any episodes that overlap with more than one episode.
  #
  # Strategy (matches original behavior):
  # - Keep non-duplicated pairs (or one-sided episodes) as-is
  # - For duplicated matches, repeatedly pick "best" matches:
  #   * minimize end-date difference between HIP and PPS
  #   * prefer PPS episodes with outcomes (deprioritize missing algo2_category)
  #   * prefer plausible PPS episode length (<= 310 days), and among ties prefer longer (new_date_diff)
  # - Repeat several rounds to break ties (A..E in original); implement the same repeated selection via a loop.

  baseKeep <- mergedDf |>
    dplyr::filter(
      (.data$algo1_dup == 0 & .data$algo2_dup == 0) |
        (.data$algo1_dup == 0 & is.na(.data$algo2_dup)) |
        (is.na(.data$algo1_dup) & .data$algo2_dup == 0)
    )

  dupDf <- mergedDf |>
    dplyr::filter(
      (.data$algo1_dup == 1 & !is.na(.data$algo2_id)) |
        (.data$algo2_dup == 1 & !is.na(.data$algo1_id))
    )

  scoreAlgo1 <- function(df) {
    df |>
      dplyr::mutate(
        date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
        # deprioritize algo2 without outcomes
        date_diff = ifelse(is.na(.data$algo2_category), 10000, .data$date_diff),
        new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
        new_date_diff = ifelse(is.na(.data$algo2_category) | .data$new_date_diff > 310, -1, .data$date_diff)
      )
  }
  scoreAlgo2 <- function(df) {
    df |>
      dplyr::mutate(
        date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
        new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
        new_date_diff = ifelse(.data$new_date_diff > 310, -1, .data$date_diff)
      )
  }

  pickBest <- function(df, side = c("algo1", "algo2"), withTiesMax = TRUE) {
    side <- match.arg(side)
    if (side == "algo1") {
      df |>
        dplyr::filter(.data$algo1_dup == 1 & !is.na(.data$algo2_id)) |>
        scoreAlgo1() |>
        dplyr::group_by(.data$algo1_id) |>
        dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) |>
        dplyr::slice_max(.data$new_date_diff, n = 1, with_ties = withTiesMax) |>
        dplyr::ungroup()
    } else {
      df |>
        dplyr::filter(.data$algo2_dup == 1 & !is.na(.data$algo1_id)) |>
        scoreAlgo2() |>
        dplyr::group_by(.data$algo2_id) |>
        dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) |>
        dplyr::slice_max(.data$new_date_diff, n = 1, with_ties = withTiesMax) |>
        dplyr::ungroup()
    }
  }

  recomputeDup <- function(df) {
    df |>
      dplyr::select(-dplyr::contains("date_diff"), -dplyr::contains("dup")) |>
      dplyr::distinct() |>
      dplyr::group_by(.data$algo1_id) |>
      dplyr::mutate(algo1_dup = dplyr::if_else(is.na(.data$algo1_id)[1], NA, as.integer(dplyr::n() > 1))) |>
      dplyr::ungroup() |>
      dplyr::group_by(.data$algo2_id) |>
      dplyr::mutate(algo2_dup = dplyr::if_else(is.na(.data$algo2_id)[1], NA, as.integer(dplyr::n() > 1))) |>
      dplyr::ungroup()
  }

  keepNonDupPairs <- function(df) {
    df |>
      dplyr::filter(
        !(.data$algo1_dup == 1 & !is.na(.data$algo2_id)) &
          !(.data$algo2_dup == 1 & !is.na(.data$algo1_id))
      )
  }

  # First round uses the original A behavior (with_ties TRUE for slice_max)
  best <- dplyr::bind_rows(
    pickBest(dupDf, "algo1", withTiesMax = TRUE),
    pickBest(dupDf, "algo2", withTiesMax = TRUE)
  ) |>
    recomputeDup()

  keeps <- list(keepNonDupPairs(best))

  # Subsequent rounds (B..E): slice_max(..., with_ties = FALSE) to break ties
  for (i in 1:4) {
    best <- dplyr::bind_rows(
      pickBest(best, "algo1", withTiesMax = FALSE),
      pickBest(best, "algo2", withTiesMax = FALSE)
    ) |>
      recomputeDup()

    keeps[[length(keeps) + 1]] <- keepNonDupPairs(best)
  }

  allRows <- dplyr::bind_rows(baseKeep, keeps) |>
    dplyr::distinct() |>
    dplyr::group_by(.data$algo1_id) |>
    dplyr::mutate(algo1_dup = dplyr::if_else(is.na(.data$algo1_id)[1], NA, as.integer(dplyr::n() > 1))) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$algo2_id) |>
    dplyr::mutate(algo2_dup = dplyr::if_else(is.na(.data$algo2_id)[1], NA, as.integer(dplyr::n() > 1))) |>
    dplyr::ungroup()

  undupedCounts <- allRows |>
    dplyr::count(
      no_algo1 = is.na(.data$algo1_id), .data$algo1_dup,
      no_algo2 = is.na(.data$algo2_id), .data$algo2_dup
    )

  log4r::info(logger, sprintf(
    "Count of duplicated algorithm 1 episodes: %s",
    dupDf |>
      dplyr::filter(.data$algo1_dup != 0) |>
      dplyr::distinct(.data$algo1_id) |>
      dplyr::tally() |>
      dplyr::pull(.data$n)
  ))
  log4r::info(logger, sprintf(
    "Count of duplicated algorithm 2 episodes: %s",
    dupDf |>
      dplyr::filter(.data$algo2_dup != 0) |>
      dplyr::distinct(.data$algo2_id) |>
      dplyr::tally() |>
      dplyr::pull(.data$n)
  ))
  log4r::info(logger, sprintf(
    "count of unduplicated episodes with both: %s",
    undupedCounts |>
      dplyr::filter(!.data$no_algo1, !.data$no_algo2, .data$algo1_dup == 0, .data$algo2_dup == 0) |>
      dplyr::pull(.data$n)
  ))
  log4r::info(logger, sprintf(
    "count of unduplicated algorithm 1 episodes: %s",
    undupedCounts |>
      dplyr::filter(!.data$no_algo1, .data$no_algo2, .data$algo1_dup == 0) |>
      dplyr::pull(.data$n)
  ))
  log4r::info(logger, sprintf(
    "count of unduplicated algorithm 2 episodes: %s",
    undupedCounts |>
      dplyr::filter(.data$no_algo1, !.data$no_algo2, .data$algo2_dup == 0) |>
      dplyr::pull(.data$n)
  ))
  log4r::info(logger, sprintf("total unduplicated episodes: %s", nrow(allRows)))

  # Recalculate merged dates, episode number, and episode length
  allRows |>
    dplyr::select(dplyr::any_of(c(
      "algo1_id", "algo2_id", "person_id", "pregnancy_end", "pregnancy_start",
      "first_gest_date", "category", "episode_min_date", "episode_max_date",
      "algo1_dup", "algo2_dup", "algo2_category", "algo2_outcome_date"
    ))) |>
    dplyr::mutate(
      merged_episode_start = as.Date(pmin(.data$first_gest_date, .data$episode_min_date, .data$pregnancy_end, na.rm = TRUE)),
      merged_episode_end   = as.Date(pmax(.data$episode_max_date, .data$pregnancy_end, na.rm = TRUE))
    ) |>
    dplyr::group_by(.data$person_id) |>
    dplyr::arrange(.data$merged_episode_start, .by_group = TRUE) |>
    dplyr::mutate(
      episode_num = dplyr::row_number(),
      merged_episode_length = as.numeric(difftime(.data$merged_episode_end, .data$merged_episode_start, units = "days")) / 30.25
    ) |>
    dplyr::ungroup()
}

# ---- Standardize columns + flags ---------------------------------------------

addMergedEpisodeDetails <- function(mergedDf) {
  # Add demographic details for each patient.
  # ADD: assign PPS episodes without outcomes to PREG

  mergedDf <- mergedDf |>
    dplyr::mutate(
      algo2_category = dplyr::if_else(!is.na(.data$algo2_id) & is.na(.data$algo2_category), "PREG", .data$algo2_category),
      algo2_outcome_date = dplyr::if_else(
        !is.na(.data$algo2_id) & is.na(.data$algo2_outcome_date),
        .data$episode_max_date,
        .data$algo2_outcome_date
      )
    ) |>
    dplyr::rename(
      HIP_end_date = "pregnancy_end",
      HIP_outcome_category = "category",
      PPS_outcome_category = "algo2_category",
      PPS_end_date = "algo2_outcome_date",
      recorded_episode_start = "merged_episode_start",
      recorded_episode_end   = "merged_episode_end",
      recorded_episode_length = "merged_episode_length"
    ) |>
    dplyr::mutate(
      # add columns marking if episode was identified by either algorithm
      HIP_flag = dplyr::if_else(!is.na(.data$algo1_id), 1, 0),
      PPS_flag = dplyr::if_else(!is.na(.data$algo2_id), 1, 0),
      PPS_outcome_category = dplyr::if_else(
        .data$PPS_flag == 1 & is.na(.data$PPS_outcome_category),
        "PREG",
        .data$PPS_outcome_category
      )
    ) |>
    dplyr::group_by(.data$person_id) |>
    dplyr::arrange(.data$recorded_episode_start, .by_group = TRUE) |>
    dplyr::mutate(episode_number = dplyr::row_number()) |>
    dplyr::ungroup()

  mergedDf
}
