# Copyright (c) 2024 Louisa Smith
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
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
#'   `pps_episodes.rds`, `hip_episodes.rds`
#' @param logger (`logger`) log4r logger created with `makeLogger()`.
#'
#' @return Invisibly returns `NULL` and writes `hipps_episodes.rds` to `outputDir`.
#' @export
mergeHipps <- function(outputDir, logger) {
  checkmate::assertDirectoryExists(outputDir)
  checkmate::assertFileExists(file.path(outputDir, "pps_episodes.rds"))
  checkmate::assertFileExists(file.path(outputDir, "hip_episodes.rds"))
  checkmate::assertClass(logger, "logger", null.ok = FALSE)

  log4r::info(logger, "Merging HIP and PPS into HIPPS")

  ppsWithOutcomesDf <- readRDS(file.path(outputDir, "pps_episodes.rds"))
  hipDf <- readRDS(file.path(outputDir, "hip_episodes.rds"))

  mergedDf <- mergeEpisodes(hipDf, ppsWithOutcomesDf, logger) %>%
    dedupeMergedEpisodes(logger) %>%
    addMergedEpisodeDetails()

  if (nrow(mergedDf) == 0) {
    mergedDf <- emptyHippsEpisodes()
  }
  mergedDf <- mergedDf %>%
    dplyr::select(
      "person_id",
      "merge_episode_number",
      "merge_episode_start",
      "merge_episode_end",
      "hip_end_date",
      "pps_end_date",
      "hip_outcome_category",
      "pps_outcome_category",
      "hip_flag",
      "pps_flag",
      "merge_episode_length",
      dplyr::any_of(c(
        "merge_pregnancy_start", "merge_first_gest_date", "pps_episode_min_date", "pps_episode_max_date",
        "pps_episode_max_date_plus_two_months", "hip_episode", "pps_episode_number",
        "hip_episode_id", "pps_episode_id"
      ))
    )
  saveRDS(mergedDf, file.path(outputDir, "hipps_episodes.rds"))
  invisible(NULL)
}


# ---- Merge HIP and PPS episodes ---------------------------------------------

mergeEpisodes <- function(hipDf, ppsWithOutcomesDf, logger) {
  # Join episodes within person if intervals overlap:
  # HIP: [pregnancy_start, pregnancy_end]
  # PPS: [pps_episode_min_date, pps_episode_max_date_plus_two_months]  (extension aids matching)

  hipEpisodesDf <- hipDf %>%
    dplyr::rename(
      pregnancy_start = "hip_pregnancy_start",
      pregnancy_end   = "hip_pregnancy_end",
      first_gest_date = "hip_first_gest_date"
    ) %>%
    dplyr::mutate(
      hip_episode_id = paste(.data$person_id, .data$hip_episode, "hip", sep = "_"),
      pregnancy_start = as.Date(.data$pregnancy_start),
      pregnancy_end   = as.Date(.data$pregnancy_end)
    )

  ppsEpisodesDf <- ppsWithOutcomesDf %>%
    dplyr::mutate(
      pps_episode_id = paste(.data$person_id, .data$pps_episode_number, "pps", sep = "_"),
      pps_episode_min_date = as.Date(.data$pps_episode_min_date),
      pps_episode_max_date_plus_two_months = as.Date(.data$pps_episode_max_date_plus_two_months)
    )

  allEpisodes <- hipEpisodesDf %>%
    dplyr::full_join(
      ppsEpisodesDf,
      by = dplyr::join_by(
        "person_id",
        "pregnancy_start" <= "pps_episode_max_date_plus_two_months",
        "pregnancy_end"   >= "pps_episode_min_date"
      )
    ) %>%
    dplyr::mutate(
      # conservative merged interval spanning evidence from both algorithms
      merged_episode_start = as.Date(pmin(.data$first_gest_date, .data$pps_episode_min_date, .data$pregnancy_start, na.rm = TRUE)),
      merged_episode_end   = as.Date(pmax(.data$pps_episode_max_date, .data$pregnancy_end, na.rm = TRUE)),
      merged_episode_length =
        as.numeric(difftime(.data$merged_episode_end, .data$merged_episode_start, units = "days")) / 30.25
    )

  addDupFlag <- function(df, id) {
    # flag many-to-many matches so they can be resolved downstream
    dupCol <- paste0("duplicated_", id)
    df %>%
      dplyr::group_by(.data[[id]]) %>%
      dplyr::mutate("{dupCol}" := dplyr::if_else(is.na(.data[[id]])[1], NA_integer_, as.integer(dplyr::n() > 1))) %>%
      dplyr::ungroup()
  }

  allEpisodes <- allEpisodes %>%
    addDupFlag("hip_episode_id") %>%
    addDupFlag("pps_episode_id")

  countDistinct <- function(df, ...) df %>% dplyr::distinct(...) %>% dplyr::tally() %>% dplyr::pull(.data$n)
  log4r::info(logger, sprintf("Total initial HIP episodes: %s", countDistinct(hipEpisodesDf, .data$person_id, .data$hip_episode)))
  log4r::info(logger, sprintf("Total initial PPS episodes: %s", countDistinct(ppsEpisodesDf, .data$person_id, .data$pps_episode_number)))
  log4r::info(logger, sprintf("HIP episodes overlapping multiple PPS episodes: %s",
                              countDistinct(dplyr::filter(allEpisodes, .data$duplicated_hip_episode_id == 1), .data$hip_episode_id)))
  log4r::info(logger, sprintf("PPS episodes overlapping multiple HIP episodes: %s",
                              countDistinct(dplyr::filter(allEpisodes, .data$duplicated_pps_episode_id == 1), .data$pps_episode_id)))

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
      (.data$duplicated_hip_episode_id == 0 & .data$duplicated_pps_episode_id == 0) |
        (.data$duplicated_hip_episode_id == 0 & is.na(.data$duplicated_pps_episode_id)) |
        (is.na(.data$duplicated_hip_episode_id) & .data$duplicated_pps_episode_id == 0)
    )

  dupDf <- mergedDf %>%
    dplyr::filter(
      (.data$duplicated_hip_episode_id == 1 & !is.na(.data$pps_episode_id)) |
        (.data$duplicated_pps_episode_id == 1 & !is.na(.data$hip_episode_id))
    )

  score <- function(df, penalizeMissingOutcome = FALSE) {
    df %>%
      dplyr::mutate(
        date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$pps_episode_max_date, units = "days"))),
        date_diff = if (penalizeMissingOutcome)
          ifelse(is.na(.data$pps_outcome_category), 10000, .data$date_diff) else .data$date_diff,
        pps_days = abs(as.numeric(difftime(.data$pps_episode_max_date, .data$pps_episode_min_date, units = "days"))),
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
      dplyr::select(-dplyr::any_of(c("date_diff", "pps_days", "duplicated_hip_episode_id", "duplicated_pps_episode_id"))) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$hip_episode_id) %>%
      dplyr::mutate(duplicated_hip_episode_id = dplyr::if_else(is.na(.data$hip_episode_id)[1], NA_integer_, as.integer(dplyr::n() > 1))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$pps_episode_id) %>%
      dplyr::mutate(duplicated_pps_episode_id = dplyr::if_else(is.na(.data$pps_episode_id)[1], NA_integer_, as.integer(dplyr::n() > 1))) %>%
      dplyr::ungroup()
  }

  # Helper: count unduplicated rows by type (both, HIP-only, PPS-only) and still-duplicate counts
  countByType <- function(df) {
    list(
      n_both     = sum(!is.na(df$hip_episode_id) & !is.na(df$pps_episode_id)),
      n_hip_only = sum(!is.na(df$hip_episode_id) &  is.na(df$pps_episode_id)),
      n_pps_only = sum( is.na(df$hip_episode_id) & !is.na(df$pps_episode_id)),
      n_total    = nrow(df)
    )
  }
  logDedupRound <- function(roundLabel, bestDf, keptDf) {
    nDupHip <- if (nrow(bestDf) > 0)
      dplyr::n_distinct(bestDf$hip_episode_id[bestDf$duplicated_hip_episode_id == 1], na.rm = TRUE) else 0L
    nDupPps <- if (nrow(bestDf) > 0)
      dplyr::n_distinct(bestDf$pps_episode_id[bestDf$duplicated_pps_episode_id == 1], na.rm = TRUE) else 0L
    log4r::info(logger, sprintf("  [%s] Still duplicated: HIP episodes=%s, PPS episodes=%s; rows=%s",
                                roundLabel, nDupHip, nDupPps, nrow(bestDf)))
    if (nrow(keptDf) > 0) {
      cts <- countByType(keptDf)
      log4r::info(logger, sprintf("  [%s] Unduplicated this round: both=%s, HIP-only=%s, PPS-only=%s; total=%s",
                                  roundLabel, cts$n_both, cts$n_hip_only, cts$n_pps_only, cts$n_total))
    }
  }

  log4r::info(logger, "Deduplication: base rows (one-to-one and one-sided) by type:")
  baseCts <- countByType(baseKeep)
  log4r::info(logger, sprintf("  base: both=%s, HIP-only=%s, PPS-only=%s; total=%s",
                              baseCts$n_both, baseCts$n_hip_only, baseCts$n_pps_only, baseCts$n_total))
  log4r::info(logger, sprintf("  duplicate candidates (many-to-many): %s rows", nrow(dupDf)))

  # Resolve duplicates in rounds (keeps original "A..E" repeated tie-breaking idea)
  best <- dplyr::bind_rows(
    pickBest(dupDf %>% dplyr::filter(.data$duplicated_hip_episode_id == 1), "hip_episode_id", penalizeMissingOutcome = TRUE,  withTiesMax = TRUE),
    pickBest(dupDf %>% dplyr::filter(.data$duplicated_pps_episode_id == 1), "pps_episode_id", penalizeMissingOutcome = FALSE, withTiesMax = TRUE)
  ) %>%
    recomputeDup()

  keeps <- list(best %>% dplyr::filter(!(.data$duplicated_hip_episode_id == 1 & !is.na(.data$pps_episode_id)) &
                                        !(.data$duplicated_pps_episode_id == 1 & !is.na(.data$hip_episode_id))))
  logDedupRound("round 0 (initial pick)", best, keeps[[1]])

  maxRounds <- 10L
  i <- 0L
  while (i < maxRounds) {
    nDupHip <- dplyr::n_distinct(best$hip_episode_id[best$duplicated_hip_episode_id == 1], na.rm = TRUE)
    nDupPps <- dplyr::n_distinct(best$pps_episode_id[best$duplicated_pps_episode_id == 1], na.rm = TRUE)
    if (nDupHip == 0L && nDupPps == 0L) break

    i <- i + 1L
    best <- dplyr::bind_rows(
      pickBest(best %>% dplyr::filter(.data$duplicated_hip_episode_id == 1), "hip_episode_id", penalizeMissingOutcome = TRUE,  withTiesMax = FALSE),
      pickBest(best %>% dplyr::filter(.data$duplicated_pps_episode_id == 1), "pps_episode_id", penalizeMissingOutcome = FALSE, withTiesMax = FALSE)
    ) %>%
      recomputeDup()

    keeps[[length(keeps) + 1]] <- best %>%
      dplyr::filter(!(.data$duplicated_hip_episode_id == 1 & !is.na(.data$pps_episode_id)) &
                      !(.data$duplicated_pps_episode_id == 1 & !is.na(.data$hip_episode_id)))
    logDedupRound(sprintf("round %s", i), best, keeps[[length(keeps)]])
  }

  nRemainHip <- if (nrow(best) > 0) dplyr::n_distinct(best$hip_episode_id[best$duplicated_hip_episode_id == 1], na.rm = TRUE) else 0L
  nRemainPps <- if (nrow(best) > 0) dplyr::n_distinct(best$pps_episode_id[best$duplicated_pps_episode_id == 1], na.rm = TRUE) else 0L
  if (nRemainHip > 0L || nRemainPps > 0L) {
    log4r::warn(logger, sprintf(
      "Deduplication hit max rounds (%s) with duplicates still remaining: HIP episodes=%s, PPS episodes=%s, rows=%s (these rows are dropped from the merged output)",
      maxRounds, nRemainHip, nRemainPps, nrow(best)
    ))
  }

  allRows <- dplyr::bind_rows(baseKeep, keeps) %>%
    dplyr::distinct()

  finalCts <- countByType(allRows)
  log4r::info(logger, sprintf("Total unduplicated merged rows: %s (both=%s, HIP-only=%s, PPS-only=%s)",
                              finalCts$n_total, finalCts$n_both, finalCts$n_hip_only, finalCts$n_pps_only))

  # Recompute merged dates and order episodes within person
  allRows %>%
    dplyr::mutate(
      merged_episode_start = as.Date(pmin(.data$first_gest_date, .data$pps_episode_min_date, .data$pregnancy_start, na.rm = TRUE)),
      merged_episode_end   = as.Date(pmax(.data$pps_episode_max_date, .data$pregnancy_end, na.rm = TRUE))
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
      pps_outcome_category = dplyr::if_else(!is.na(.data$pps_episode_id) & is.na(.data$pps_outcome_category), "PREG", .data$pps_outcome_category),
      pps_outcome_date = dplyr::if_else(
        !is.na(.data$pps_episode_id) & is.na(.data$pps_outcome_date),
        .data$pps_episode_max_date,
        .data$pps_outcome_date
      )
    ) %>%
    dplyr::rename(
      hip_end_date = "pregnancy_end",
      pps_end_date = "pps_outcome_date",
      merge_episode_start = "merged_episode_start",
      merge_episode_end   = "merged_episode_end",
      merge_episode_length = "merged_episode_length",
      merge_pregnancy_start = "pregnancy_start",
      merge_first_gest_date = "first_gest_date"
    ) %>%
    dplyr::mutate(
      hip_flag = dplyr::if_else(!is.na(.data$hip_episode_id), 1L, 0L),
      pps_flag = dplyr::if_else(!is.na(.data$pps_episode_id), 1L, 0L)
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$merge_episode_start, .by_group = TRUE) %>%
    dplyr::mutate(merge_episode_number = dplyr::row_number()) %>%
    dplyr::ungroup()
}
