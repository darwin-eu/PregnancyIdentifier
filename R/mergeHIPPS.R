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
#' Reads previously-saved HIP and PPS outputs from `outputDir`,
#' merges HIP and PPS episodes based on temporal overlap, resolves many-to-many overlaps
#' by selecting the best-matching pairs, and returns a single per-person episode table
#' with standardized columns used downstream by ESD and reporting.
#'
#' Key steps:
#' \itemize{
#'   \item Load HIP episodes and PPS episodes from `outputDir`
#'   \item Merge HIP + PPS episodes by overlap; flag episodes involved in many-to-many overlaps
#'   \item Resolve many-to-many overlaps by selecting best-matching pairs (retain best matches by end-date proximity and episode plausibility)
#'   \item Add standardized columns and per-person episode ordering; write `hipps_episodes.rds`
#' }
#'
#' @param outputDir (`character(1)`) Directory containing intermediate RDS artifacts:
#'   `pps_episodes.rds`, `hip_episodes.rds` (and optionally `pps_min_max_episodes.rds`, `pps_gest_timing_episodes.rds` from PPS debug).
#' @param logger (`logger`) log4r logger.
#'
#' @return Invisibly returns `NULL` and writes `hipps_episodes.rds` to `outputDir`.
#' @export
mergeHipps <- function(outputDir, logger) {
  checkmate::assertDirectoryExists(outputDir)
  checkmate::assertFileExists(file.path(outputDir, "pps_episodes.rds"))
  checkmate::assertFileExists(file.path(outputDir, "hip_episodes.rds"))
  checkmate::assertClass(logger, "logger")

  log4r::info(logger, "Merging HIP and PPS into HIPPS")

  ppsWithOutcomesDf <- readRDS(file.path(outputDir, "pps_episodes.rds"))
  hipDf <- readRDS(file.path(outputDir, "hip_episodes.rds"))

  mergedDf <- mergeEpisodes(hipDf, ppsWithOutcomesDf, logger) %>%
    dedupeMergedEpisodes(logger) %>%
    addMergedEpisodeDetails()

  if (nrow(mergedDf) == 0) {
    mergedDf <- emptyHippsEpisodes()
  }
  saveRDS(mergedDf, file.path(outputDir, "hipps_episodes.rds"))
  invisible(NULL)
}


# ---- Merge HIP and PPS episodes ---------------------------------------------

mergeEpisodes <- function(hipDf, ppsWithOutcomesDf, logger) {
  # Join episodes within person if intervals overlap:
  # HIP: [pregnancy_start, pregnancy_end]
  # PPS: [episode_min_date, episode_max_date_plus_two_months]  (extension aids matching)

  algo1Df <- hipDf %>%
    dplyr::rename(
      pregnancy_start = estimated_start_date,
      pregnancy_end   = visit_date,
      first_gest_date = gest_date
    ) %>%
    dplyr::mutate(
      algo1_id = paste(.data$person_id, .data$episode, "1", sep = "_"),
      pregnancy_start = as.Date(.data$pregnancy_start),
      pregnancy_end   = as.Date(.data$pregnancy_end)
    )

  algo2Df <- ppsWithOutcomesDf %>%
    dplyr::mutate(
      algo2_id = paste(.data$person_id, .data$person_episode_number, "2", sep = "_"),
      episode_min_date = as.Date(.data$episode_min_date),
      episode_max_date_plus_two_months = as.Date(.data$episode_max_date_plus_two_months)
    )

  allEpisodes <- algo1Df %>%
    dplyr::full_join(
      algo2Df,
      by = dplyr::join_by(
        person_id,
        pregnancy_start <= episode_max_date_plus_two_months,
        pregnancy_end   >= episode_min_date
      )
    ) %>%
    dplyr::mutate(
      # conservative merged interval spanning evidence from both algorithms
      merged_episode_start = as.Date(pmin(.data$first_gest_date, .data$episode_min_date, .data$pregnancy_start, na.rm = TRUE)),
      merged_episode_end   = as.Date(pmax(.data$episode_max_date, .data$pregnancy_end, na.rm = TRUE)),
      merged_episode_length =
        as.numeric(difftime(.data$merged_episode_end, .data$merged_episode_start, units = "days")) / 30.25
    )

  addDupFlag <- function(df, id) {
    # flag many-to-many matches so they can be resolved downstream
    df %>%
      dplyr::group_by(.data[[id]]) %>%
      dplyr::mutate("{id}_dup" := dplyr::if_else(is.na(.data[[id]])[1], NA_integer_, as.integer(dplyr::n() > 1))) %>%
      dplyr::ungroup()
  }

  allEpisodes <- allEpisodes %>%
    addDupFlag("algo1_id") %>%
    addDupFlag("algo2_id")

  countDistinct <- function(df, ...) df %>% dplyr::distinct(...) %>% dplyr::tally() %>% dplyr::pull(.data$n)
  log4r::info(logger, sprintf("Total initial HIP episodes: %s", countDistinct(algo1Df, .data$person_id, .data$episode)))
  log4r::info(logger, sprintf("Total initial PPS episodes: %s", countDistinct(algo2Df, .data$person_id, .data$person_episode_number)))
  log4r::info(logger, sprintf("HIP episodes overlapping multiple PPS episodes: %s",
                              countDistinct(dplyr::filter(allEpisodes, .data$algo1_id_dup == 1), .data$algo1_id)))
  log4r::info(logger, sprintf("PPS episodes overlapping multiple HIP episodes: %s",
                              countDistinct(dplyr::filter(allEpisodes, .data$algo2_id_dup == 1), .data$algo2_id)))

  allEpisodes
}

# ---- Resolve many-to-many overlaps -------------------------------------------

dedupeMergedEpisodes <- function(mergedDf, logger) {
  # Keep:
  # - one-to-one matches
  # - one-sided episodes (HIP-only or PPS-only)
  #
  # For many-to-many overlap candidates, select best matches by:
  # 1) minimizing |HIP_end - PPS_end|
  # 2) preferring PPS episodes with an inferred outcome
  # 3) preferring plausible PPS duration (<= 310 days), then longer duration (tie-break)

  baseKeep <- mergedDf %>%
    dplyr::filter(
      (.data$algo1_id_dup == 0 & .data$algo2_id_dup == 0) |
        (.data$algo1_id_dup == 0 & is.na(.data$algo2_id_dup)) |
        (is.na(.data$algo1_id_dup) & .data$algo2_id_dup == 0)
    )

  dupDf <- mergedDf %>%
    dplyr::filter(
      (.data$algo1_id_dup == 1 & !is.na(.data$algo2_id)) |
        (.data$algo2_id_dup == 1 & !is.na(.data$algo1_id))
    )

  score <- function(df, penalizeMissingOutcome = FALSE) {
    df %>%
      dplyr::mutate(
        date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
        date_diff = if (penalizeMissingOutcome)
          ifelse(is.na(.data$algo2_category), 10000, .data$date_diff) else .data$date_diff,
        pps_days = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
        # mark implausible PPS episodes so they lose tie-breaks
        pps_days = ifelse(.data$pps_days > 310, -1, .data$pps_days)
      )
  }

  pickBest <- function(df, id, penalizeMissingOutcome, withTiesMax) {
    df %>%
      score(penalizeMissingOutcome) %>%
      dplyr::group_by(.data[[id]]) %>%
      dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) %>%
      dplyr::slice_max(.data$pps_days,  n = 1, with_ties = withTiesMax) %>%
      dplyr::ungroup()
  }

  recomputeDup <- function(df) {
    df %>%
      dplyr::select(-dplyr::any_of(c("date_diff", "pps_days")), -dplyr::ends_with("_dup")) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$algo1_id) %>%
      dplyr::mutate(algo1_id_dup = dplyr::if_else(is.na(.data$algo1_id)[1], NA_integer_, as.integer(dplyr::n() > 1))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$algo2_id) %>%
      dplyr::mutate(algo2_id_dup = dplyr::if_else(is.na(.data$algo2_id)[1], NA_integer_, as.integer(dplyr::n() > 1))) %>%
      dplyr::ungroup()
  }

  # Resolve duplicates in rounds (keeps original "A..E" repeated tie-breaking idea)
  best <- dplyr::bind_rows(
    pickBest(dupDf %>% dplyr::filter(.data$algo1_id_dup == 1), "algo1_id", penalizeMissingOutcome = TRUE,  withTiesMax = TRUE),
    pickBest(dupDf %>% dplyr::filter(.data$algo2_id_dup == 1), "algo2_id", penalizeMissingOutcome = FALSE, withTiesMax = TRUE)
  ) %>%
    recomputeDup()

  keeps <- list(best %>% dplyr::filter(!(.data$algo1_id_dup == 1 & !is.na(.data$algo2_id)) &
                                        !(.data$algo2_id_dup == 1 & !is.na(.data$algo1_id))))

  for (i in 1:4) {
    best <- dplyr::bind_rows(
      pickBest(best %>% dplyr::filter(.data$algo1_id_dup == 1), "algo1_id", penalizeMissingOutcome = TRUE,  withTiesMax = FALSE),
      pickBest(best %>% dplyr::filter(.data$algo2_id_dup == 1), "algo2_id", penalizeMissingOutcome = FALSE, withTiesMax = FALSE)
    ) %>%
      recomputeDup()

    keeps[[length(keeps) + 1]] <- best %>%
      dplyr::filter(!(.data$algo1_id_dup == 1 & !is.na(.data$algo2_id)) &
                      !(.data$algo2_id_dup == 1 & !is.na(.data$algo1_id)))
  }

  allRows <- dplyr::bind_rows(baseKeep, keeps) %>%
    dplyr::distinct()

  log4r::info(logger, sprintf("Total unduplicated merged rows: %s", nrow(allRows)))

  # Recompute merged dates and order episodes within person
  allRows %>%
    dplyr::mutate(
      merged_episode_start = as.Date(pmin(.data$first_gest_date, .data$episode_min_date, .data$pregnancy_start, na.rm = TRUE)),
      merged_episode_end   = as.Date(pmax(.data$episode_max_date, .data$pregnancy_end, na.rm = TRUE))
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$merged_episode_start, .by_group = TRUE) %>%
    dplyr::mutate(
      episode_num = dplyr::row_number(),
      merged_episode_length = as.numeric(difftime(.data$merged_episode_end, .data$merged_episode_start, units = "days")) / 30.25
    ) %>%
    dplyr::ungroup()
}

# ---- Standardize columns + flags --------------------------------------------

addMergedEpisodeDetails <- function(mergedDf) {
  # Standardize outcome fields and indicate whether each merged episode was found
  # by HIP, PPS, or both. PPS episodes without outcomes are retained as "PREG".

  mergedDf %>%
    dplyr::mutate(
      algo2_category = dplyr::if_else(!is.na(.data$algo2_id) & is.na(.data$algo2_category), "PREG", .data$algo2_category),
      algo2_outcome_date = dplyr::if_else(
        !is.na(.data$algo2_id) & is.na(.data$algo2_outcome_date),
        .data$episode_max_date,
        .data$algo2_outcome_date
      )
    ) %>%
    dplyr::rename(
      hip_end_date = "pregnancy_end",
      hip_outcome_category = "category",
      pps_outcome_category = "algo2_category",
      pps_end_date = "algo2_outcome_date",
      recorded_episode_start = "merged_episode_start",
      recorded_episode_end   = "merged_episode_end",
      recorded_episode_length = "merged_episode_length"
    ) %>%
    dplyr::mutate(
      hip_flag = dplyr::if_else(!is.na(.data$algo1_id), 1L, 0L),
      pps_flag = dplyr::if_else(!is.na(.data$algo2_id), 1L, 0L)
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$recorded_episode_start, .by_group = TRUE) %>%
    dplyr::mutate(episode_number = dplyr::row_number()) %>%
    dplyr::ungroup()
}
