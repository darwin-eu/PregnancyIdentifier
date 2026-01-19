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

#' mergeHip
#'
#' @param cdm (`cdm_reference`)
#' @param outputDir (`character(1)`)
#' @param logger (`logger`) Logger object
#'
#' @return `NULL`
#' @export
mergeHips <- function(cdm, outputDir, logger) {
  log4r::info(logger, "Merging HIP and PPS into HIPPS")
  # collect outcomes for PPS algorithm from lookahead window

  # PPS_episodes_df -> min max
  # get_PPS_episodes_df -> episode

  PPSMinMax <- readRDS(file.path(outputDir, "PPS_min_max_episodes.rds"))
  PPSEpisode <- readRDS(file.path(outputDir, "PPS_gest_timing_episodes.rds"))
  HIP <- readRDS(file.path(outputDir, "HIP_episodes.rds"))

  outcomes_per_episode_df <- outcomes_per_episode(PPSMinMax, PPSEpisode, cdm)

  # add outcomes to PPS episodes
  PPS_episodes_with_outcomes_df <- add_outcomes(outcomes_per_episode_df, PPSMinMax)

  # merge HIPS and PPS episodes
  final_merged_episodes_df <- final_merged_episodes(HIP, PPS_episodes_with_outcomes_df, logger = logger)

  # remove any duplicated episodes
  final_merged_episodes_no_duplicates_df <- final_merged_episodes_no_duplicates(final_merged_episodes_df, logger = logger)

  # add (some) demographic details
  final_merged_episode_detailed_df <- final_merged_episode_detailed(final_merged_episodes_no_duplicates_df)

  saveRDS(final_merged_episode_detailed_df, file.path(outputDir, "HIPPS_episodes.rds"))
}

get_outcome_date <- function(x, outcome) {
  if (length(grep(outcome, x)) > 0) {
    strsplit(x[grep(outcome, x)], ",")[[1]][1]
  } else {
    NA
  }
}

outcomes_per_episode <- function(PPS_episodes_df, get_PPS_episodes_df, cdm, logger) {
  # Get outcomes for Algorithm 2:
  # Outcomes are collected from a 'lookback to lookahead window', which is the
  # episode max date minus 14d to the earliest out of i) the next closest
  # episode start date or ii) a number of months of length (10 months - the
  # earliest concept month that could relate to the end of the episode)

  # To assign a measure of support for outcomes identified in HIP episodes, we
  # inferred pregnancy outcomes in the PPS by checking for any outcome within
  # a window of 14 days before the episode end to up to the earliest date from
  # either (1) the next episode start date, or (2) up to 10 months minus the
  # last record’s expected minimum month after the start of the episode. We
  # then selected the outcome based on Matcho et al’s outcome hierarchy
  # assessment

  pregnant_dates <- PPS_episodes_df %>%
    dplyr::group_by(person_id) %>%
    dplyr::arrange(.data$person_episode_number, .data$episode_min_date) %>%
    # not gestational week 1, just when the first concept appears
    dplyr::mutate(
      next_closest_episode_date = dplyr::lead(.data$episode_min_date) - lubridate::days(1),
      previous_episode_date = dplyr::lag(.data$episode_max_date) + lubridate::days(1)
    ) %>%
    dplyr::ungroup()

  if (nrow(get_PPS_episodes_df) == 0) {
    get_PPS_episodes_df <- get_PPS_episodes_df %>%
      dplyr::mutate(person_episode_number = integer(0))
  } else {
    if (!"person_episode_number" %in% names(get_PPS_episodes_df)) {
      get_PPS_episodes_df <- get_PPS_episodes_df %>%
        dplyr::mutate(person_episode_number = 1)
    }
  }

  # get the max number of months to look ahead from the episode itself, in a new column called 'max_pregnancy_date'
  # do this by saving the concept date relating to the last episode concept
  # (if multiple on the same date then the one containing max month),
  # of which the min month is used out of the tuple of month values where necessary and subtracted from 10,
  # this then is added onto the concept date to get 'max_pregnancy_date'
  tmp_preg_episode_concept_GA <- get_PPS_episodes_df %>%
    dplyr::group_by(.data$person_id, .data$person_episode_number) %>%
    dplyr::arrange(dplyr::desc(.data$domain_concept_start_date), dplyr::desc(.data$max_month), dplyr::desc(.data$min_month)) %>%
    # choose the last concept date and the greatest possible gestational age at that time
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # the next pregnancy could occur (10 months + buffer)
      months_to_add = 11L - as.integer(.data$min_month),
      # last time we would expect an outcome
      max_pregnancy_date = lubridate::`%m+%`(.data$domain_concept_start_date, months(.data$months_to_add))
    ) %>%
    dplyr::select("person_id", "person_episode_number", "max_pregnancy_date")

  pregnant_dates <- pregnant_dates %>%
    dplyr::left_join(
      tmp_preg_episode_concept_GA,
      by = c("person_id", "person_episode_number")
  ) %>%
    dplyr::mutate(
      # if there's no next episode impute a distant time
      next_closest_episode_date = dplyr::if_else(
        is.na(next_closest_episode_date),
        lubridate::ymd("2999-01-01"),
        .data$next_closest_episode_date
      ),
      # choose the earliest out of the next episode and the last time we'd expect an outcome
      episode_max_date_plus_lookahead_window = pmin(.data$next_closest_episode_date, .data$max_pregnancy_date, na.rm = TRUE),
      # two weeks before we saw the last concept
      episode_max_date_minus_lookback_window = .data$episode_max_date - lubridate::days(14)
    )

  # begin searching for outcomes within the relevant lookback and lookahead dates
  preg_related_concepts <- cdm$initial_pregnant_cohort_df %>%
    dplyr::filter(.data$category %in% c("LB", "SB", "DELIV", "ECT", "AB", "SA")) %>%
    dplyr::collect() %>%
    dplyr::inner_join(
      pregnant_dates,
      by = dplyr::join_by(
        "person_id",
        between(
          # the max lookahead window stops at the next episode... but only if it's in PPS
          # if there were multiple
          # outcomes and the first date wasn't chosen, there will be problems...
          visit_date, episode_max_date_minus_lookback_window,
          episode_max_date_plus_lookahead_window
        )
      ),
      relationship = "many-to-many"
    )

  preg_related_concepts_lst <- preg_related_concepts %>%
    dplyr::mutate(lst = paste(visit_date, ",", concept_id, ",", category)) %>%
    dplyr::group_by(
      .data$person_id, .data$person_episode_number, .data$episode_min_date, .data$episode_max_date,
      .data$episode_max_date_minus_lookback_window, .data$episode_max_date_plus_lookahead_window,
      .data$n_gt_concepts
    ) %>%
    dplyr::summarise(
      outcomes_list = list(unique(lst)), .groups = "drop"
    )

  df1 <- preg_related_concepts_lst %>%
    dplyr::mutate(outcomes_list = purrr::discard(outcomes_list, purrr::is_empty)) %>%
    dplyr::mutate(outcomes_list = purrr::map(outcomes_list, sort))

  df1_outcomes <- df1 %>%
    dplyr::mutate(
      LB_delivery_date = purrr::map_chr(outcomes_list, get_outcome_date, "LB"),
      SB_delivery_date = purrr::map_chr(outcomes_list, get_outcome_date, "SB"),
      ECT_delivery_date = purrr::map_chr(outcomes_list, get_outcome_date, "ECT"),
      SA_delivery_date = purrr::map_chr(outcomes_list, get_outcome_date, "SA"),
      AB_delivery_date = purrr::map_chr(outcomes_list, get_outcome_date, "AB"),
      DELIV_delivery_date = purrr::map_chr(outcomes_list, get_outcome_date, "DELIV"),
      algo2_category = dplyr::case_when(
        !is.na(LB_delivery_date) ~ "LB",
        !is.na(SB_delivery_date) ~ "SB",
        !is.na(ECT_delivery_date) ~ "ECT",
        !is.na(SA_delivery_date) ~ "SA",
        !is.na(AB_delivery_date) ~ "AB",
        !is.na(DELIV_delivery_date) ~ "DELIV"
      ),
      algo2_outcome_date = dplyr::case_when(
        !is.na(LB_delivery_date) ~ LB_delivery_date,
        !is.na(SB_delivery_date) ~ SB_delivery_date,
        !is.na(ECT_delivery_date) ~ ECT_delivery_date,
        !is.na(SA_delivery_date) ~ SA_delivery_date,
        !is.na(AB_delivery_date) ~ AB_delivery_date,
        !is.na(DELIV_delivery_date) ~ DELIV_delivery_date
      ),
      algo2_outcome_date = lubridate::ymd(algo2_outcome_date)
    )

  return(df1_outcomes)
}

add_outcomes <- function(outcomes_per_episode_df, PPS_episodes_df) {
  out_df <- outcomes_per_episode_df %>%
    dplyr::select(
      "person_id", "person_episode_number", "episode_min_date", "algo2_category",
      "algo2_outcome_date", "n_gt_concepts"
    )

  df <- PPS_episodes_df %>%
    dplyr::left_join(
      out_df,
      by = c("person_id", "person_episode_number", "episode_min_date", "n_gt_concepts")
    )

  return(df)
}

final_merged_episodes <- function(HIP_episodes_local_df, PPS_episodes_with_outcomes_df, logger) {
  # Merge episodes by checking for any overlap of episodes between the two algorithms.
  #
  # algo1 = HIP episodes
  # algo2 = PPS episodes

  # The following is for checking overlap:
  # - complete overlap
  # - algo1 contains algo2
  # - algo2 contains algo1
  # - start in algo1 is within algo2
  # - start in algo1 is within algo2
  # - start in algo2 is within algo1
  # - end in algo1 is within algo2
  # - end in algo2 is within algo1

  algo1_pregnancy <- HIP_episodes_local_df %>%
    dplyr::rename(
      pregnancy_start = "estimated_start_date",
      pregnancy_end = "visit_date",
      first_gest_date = "gest_date"
    ) %>%
    dplyr::mutate(
      algo1_id = paste(.data$person_id, .data$episode, "1", sep = "_"),
      pregnancy_start = as.Date(.data$pregnancy_start),
      pregnancy_end = as.Date(.data$pregnancy_end)
    )

  algo2 <- PPS_episodes_with_outcomes_df %>%
    dplyr::mutate(
      algo2_id = paste(.data$person_id, .data$person_episode_number, "2", sep = "_"),
      episode_min_date = as.Date(.data$episode_min_date),
      episode_max_date_plus_two_months = as.Date(.data$episode_max_date_plus_two_months)
    )

  all_episodes <- algo1_pregnancy %>%
    dplyr::full_join(
      algo2,
      by = dplyr::join_by(
        person_id,
        overlaps(pregnancy_start, pregnancy_end, episode_min_date, episode_max_date_plus_two_months)
      )
    ) %>%
    dplyr::mutate(
      merged_episode_start = as.Date(pmin(.data$first_gest_date, .data$episode_min_date, .data$pregnancy_end)),
      merged_episode_end = as.Date(pmax(.data$episode_max_date, .data$pregnancy_end)),
      merged_episode_length = as.numeric(difftime(.data$merged_episode_end, .data$merged_episode_start, units = "days")) / 30.25
    )

  # check for duplicated algorithm 1 episodes
  # these are HIPPS episodes that overlap multiple PPS episodes
  all_episodes <- all_episodes %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::mutate(
      algo1_dup = dplyr::if_else(is.na(.data$algo1_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup()

  # Check for duplicated algorithm 2 episodes
  all_episodes <- all_episodes %>%
    dplyr::group_by(algo2_id) %>%
    dplyr::mutate(
      algo2_dup = dplyr::if_else(is.na(algo2_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup()

  # check overlap
  log4r::info(logger, sprintf(
    "Total initial number of episodes for HIP: %s",
    algo1_pregnancy %>%
      dplyr::distinct(.data$person_id, .data$episode) %>%
      dplyr::tally() %>%
      dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "Total initial number of episodes for PPS: %s",
    algo2 %>%
      dplyr::distinct(.data$person_id, .data$person_episode_number) %>%
      dplyr::tally() %>%
      dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "Count of HIP episodes that overlap multiple PPS episodes: %s",
    all_episodes %>%
      dplyr::filter(.data$algo1_dup != 0) %>%
      dplyr::distinct(.data$algo1_id) %>%
      dplyr::tally() %>%
      dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "Count of PPS episodes that overlap multiple HIP episodes: %s",
    all_episodes %>%
      dplyr::filter(.data$algo2_dup != 0) %>%
      dplyr::distinct(.data$algo2_id) %>%
      dplyr::tally() %>%
      dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "Total number of HIP episodes after merging: %s",
    all_episodes %>%
      dplyr::distinct(.data$algo1_id) %>%
      dplyr::tally() %>%
      dplyr::pull(.data$n) - 1 # don't count NA
  ))

  log4r::info(logger, sprintf(
      "Total number of PPS episodes after merging: %s",
      all_episodes %>%
        dplyr::distinct(.data$algo2_id) %>%
        dplyr::tally() %>%
        dplyr::pull(.data$n) - 1 # don't count NA
  ))

  return(all_episodes)
}

final_merged_episodes_no_duplicates <- function(final_merged_episodes_df, logger) {
  # Remove any episodes that overlap with more than one episode.

  # 1. Keep algorithm 1 episodes with an end date closest to algorithm 2's end
  # date. Starting with duplicated algorithm 1 episodes, find the date
  # difference in days between each algorithm's end date. Find the minimum
  # date difference in days. If an algorithm 1 episode date difference in days
  # does not equal the minimum date difference in days, flag that episode for
  # removal by converting algorithm 1 episode info to null.

  # 2. Any remaining duplicated algorithm 1 episodes may have more than one
  # algorithm 2 episodes with the same date difference in days. Calculate the
  # length of algorithm 2 episodes and keep only the longest algorithm 2
  # episode. For any algorithm 2 episode that doesn't meet this criteria, both
  # the algorithm 1 and 2 episode info are converted to null.

  # 3. Next repeat the same process described in Step 1 for duplicated
  # algorithm 2 episodes.

  no_dup_df <- final_merged_episodes_df %>%
    dplyr::filter(
      (.data$algo1_dup == 0 & .data$algo2_dup == 0)
      | (.data$algo1_dup == 0 & is.na(.data$algo2_dup))
      | (is.na(.data$algo1_dup) & .data$algo2_dup == 0)
    )

  best_algo1A <- final_merged_episodes_df %>%
    dplyr::filter(.data$algo1_dup == 1 & !is.na(.data$algo2_id)) %>%
    dplyr::mutate(
      date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
      # deprioritize algo2 without outcomces
      date_diff = ifelse(is.na(.data$algo2_category), 10000, .data$date_diff),
      new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
      new_date_diff = ifelse(is.na(.data$algo2_category) | .data$new_date_diff > 310, -1, .data$date_diff)
    ) %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(.data$new_date_diff, n = 1) %>%
    dplyr::ungroup()

  best_algo2A <- final_merged_episodes_df %>%
    dplyr::filter(.data$algo2_dup == 1 & !is.na(.data$algo1_id)) %>%
    dplyr::mutate(
      date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
      new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
      new_date_diff = ifelse(.data$new_date_diff > 310, -1, date_diff)
    ) %>%
    dplyr::group_by(.data$algo2_id) %>%
    dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(.data$new_date_diff, n = 1) %>%
    dplyr::ungroup()

  best_bothA <- best_algo1A %>%
    dplyr::bind_rows(best_algo2A) %>%
    dplyr::select(-contains("date_diff"), -contains("dup")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::mutate(
      algo1_dup = dplyr::if_else(is.na(.data$algo1_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$algo2_id) %>%
    dplyr::mutate(
      algo2_dup = dplyr::if_else(is.na(.data$algo2_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup()

  keepA <- best_bothA %>%
    dplyr::filter(
      !(.data$algo1_dup == 1 & !is.na(.data$algo2_id))
      & !(.data$algo2_dup == 1 & !is.na(.data$algo1_id))
    )

  best_algo1B <- best_bothA %>%
    dplyr::filter(.data$algo1_dup == 1 & !is.na(.data$algo2_id)) %>%
    dplyr::mutate(
      date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
      # deprioritize algo2 without outcomces
      date_diff = ifelse(is.na(.data$algo2_category), 10000, .data$date_diff),
      new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
      new_date_diff = ifelse(is.na(.data$algo2_category) | .data$new_date_diff > 310, -1, .data$date_diff)
    ) %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(.data$new_date_diff, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  best_algo2B <- best_bothA %>%
    dplyr::filter(.data$algo2_dup == 1 & !is.na(.data$algo1_id)) %>%
    dplyr::mutate(
      date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
      new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
      new_date_diff = ifelse(.data$new_date_diff > 310, -1, .data$date_diff)
    ) %>%
    dplyr::group_by(.data$algo2_id) %>%
    dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(.data$new_date_diff, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  best_bothB <- best_algo1B %>%
    dplyr::bind_rows(best_algo2B) %>%
    dplyr::select(-contains("date_diff"), -contains("dup")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::mutate(
      algo1_dup = dplyr::if_else(is.na(.data$algo1_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$algo2_id) %>%
    dplyr::mutate(
      algo2_dup = dplyr::if_else(is.na(.data$algo2_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup()

  keepB <- best_bothB %>%
    dplyr::filter(
      !(.data$algo1_dup == 1 & !is.na(.data$algo2_id))
      & !(.data$algo2_dup == 1 & !is.na(.data$algo1_id))
    )

  # check to make sure this was sufficient
  # nrow(best_bothB) == nrow(keepB)

  best_algo1C <- best_bothB %>%
    dplyr::filter(.data$algo1_dup == 1 & !is.na(.data$algo2_id)) %>%
    dplyr::mutate(
      date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
      # deprioritize algo2 without outcomces
      date_diff = ifelse(is.na(.data$algo2_category), 10000, .data$date_diff),
      new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
      new_date_diff = ifelse(is.na(.data$algo2_category) | .data$new_date_diff > 310, -1, .data$date_diff)
    ) %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(.data$new_date_diff, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  best_algo2C <- best_bothB %>%
    dplyr::filter(.data$algo2_dup == 1 & !is.na(.data$algo1_id)) %>%
    dplyr::mutate(
      date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
      new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
      new_date_diff = ifelse(.data$new_date_diff > 310, -1, .data$date_diff)
    ) %>%
    dplyr::group_by(.data$algo2_id) %>%
    dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(.data$new_date_diff, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  best_bothC <- best_algo1C %>%
    dplyr::bind_rows(best_algo2C) %>%
    dplyr::select(-contains("date_diff"), -contains("dup")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::mutate(
      algo1_dup = dplyr::if_else(is.na(.data$algo1_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$algo2_id) %>%
    dplyr::mutate(
      algo2_dup = dplyr::if_else(is.na(.data$algo2_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup()

  keepC <- best_bothC %>%
    dplyr::filter(
      !(algo1_dup == 1 & !is.na(algo2_id))
      & !(algo2_dup == 1 & !is.na(algo1_id))
    )

  # check to make sure this was sufficient
  # nrow(best_bothC) == nrow(keepC)

  best_algo1D <- best_bothC %>%
    dplyr::filter(.data$algo1_dup == 1 & !is.na(.data$algo2_id)) %>%
    dplyr::mutate(
      date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
      # deprioritize algo2 without outcomces
      date_diff = ifelse(is.na(.data$algo2_category), 10000, .data$date_diff),
      new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
      new_date_diff = ifelse(is.na(.data$algo2_category) | .data$new_date_diff > 310, -1, .data$date_diff)
    ) %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(.data$new_date_diff, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  best_algo2D <- best_bothC %>%
    dplyr::filter(.data$algo2_dup == 1 & !is.na(.data$algo1_id)) %>%
    dplyr::mutate(
      date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
      new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
      new_date_diff = ifelse(.data$new_date_diff > 310, -1, .data$date_diff)
    ) %>%
    dplyr::group_by(.data$algo2_id) %>%
    dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(.data$new_date_diff, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  best_bothD <- best_algo1D %>%
    dplyr::bind_rows(best_algo2D) %>%
    dplyr::select(-dplyr::contains("date_diff"), -dplyr::contains("dup")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::mutate(
      algo1_dup = dplyr::if_else(is.na(.data$algo1_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$algo2_id) %>%
    dplyr::mutate(
      algo2_dup = dplyr::if_else(is.na(.data$algo2_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup()

  keepD <- best_bothD %>%
    dplyr::filter(
      !(.data$algo1_dup == 1 & !is.na(.data$algo2_id))
      & !(.data$algo2_dup == 1 & !is.na(.data$algo1_id))
    )

  # check to make sure this was sufficient
  # nrow(best_bothD) == nrow(keepD)

  best_algo1E <- best_bothD %>%
    dplyr::filter(.data$algo1_dup == 1 & !is.na(.data$algo2_id)) %>%
    dplyr::mutate(
      date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
      # deprioritize algo2 without outcomces
      date_diff = ifelse(is.na(.data$algo2_category), 10000, .data$date_diff),
      new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
      new_date_diff = ifelse(is.na(.data$algo2_category) | .data$new_date_diff > 310, -1, .data$date_diff)
    ) %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(.data$new_date_diff, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  best_algo2E <- best_bothD %>%
    dplyr::filter(.data$algo2_dup == 1 & !is.na(.data$algo1_id)) %>%
    dplyr::mutate(
      date_diff = abs(as.numeric(difftime(.data$pregnancy_end, .data$episode_max_date, units = "days"))),
      new_date_diff = abs(as.numeric(difftime(.data$episode_max_date, .data$episode_min_date, units = "days"))),
      new_date_diff = ifelse(.data$new_date_diff > 310, -1, .data$date_diff)
    ) %>%
    dplyr::group_by(.data$algo2_id) %>%
    dplyr::slice_min(.data$date_diff, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(.data$new_date_diff, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  best_bothE <- best_algo1E %>%
    dplyr::bind_rows(best_algo2E) %>%
    dplyr::select(-contains("date_diff"), -contains("dup")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::mutate(
      algo1_dup = dplyr::if_else(is.na(.data$algo1_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$algo2_id) %>%
    dplyr::mutate(
      algo2_dup = dplyr::if_else(is.na(.data$algo2_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup()

  keepE <- best_bothE %>%
    dplyr::filter(
      !(.data$algo1_dup == 1 & !is.na(.data$algo2_id))
      & !(.data$algo2_dup == 1 & !is.na(.data$algo1_id))
    )

  # check to make sure this was sufficient
  # nrow(best_bothE) == nrow(keepE)

  all_rows <- no_dup_df %>%
    dplyr::bind_rows(keepA, keepB, keepC, keepD, keepE) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$algo1_id) %>%
    dplyr::mutate(
      algo1_dup = dplyr::if_else(is.na(algo1_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$algo2_id) %>%
    dplyr::mutate(
      algo2_dup = dplyr::if_else(is.na(.data$algo2_id)[1], NA, as.integer(dplyr::n() > 1))
    ) %>%
    dplyr::ungroup()

  # check
  unduped_counts <- all_rows %>%
    dplyr::count(
      no_algo1 = is.na(.data$algo1_id), .data$algo1_dup,
      no_algo2 = is.na(.data$algo2_id), .data$algo2_dup
    )

  dup_df <- final_merged_episodes_df %>%
    dplyr::filter(
      (.data$algo1_dup == 1 & !is.na(.data$algo2_id))
      | (.data$algo2_dup == 1 & !is.na(.data$algo1_id))
    )


  log4r::info(logger, sprintf(
    "Count of duplicated algorithm 1 episodes: %s",
    dup_df %>%
      dplyr::filter(.data$algo1_dup != 0) %>%
      dplyr::distinct(.data$algo1_id) %>%
      dplyr::tally() %>%
      dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "Count of duplicated algorithm 2 episodes: %s",
    dup_df %>%
      dplyr::filter(.data$algo2_dup != 0) %>%
      dplyr::distinct(.data$algo2_id) %>%
      dplyr::tally() %>%
      dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "count of unduplicated episodes with both: %s",
    unduped_counts %>%
      dplyr::filter(!.data$no_algo1, !.data$no_algo2, .data$algo1_dup == 0, .data$algo2_dup == 0) %>%
      dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "count of unduplicated algorithm 1 episodes: %s",
    unduped_counts %>%
      dplyr::filter(!.data$no_algo1, .data$no_algo2, .data$algo1_dup == 0) %>%
      dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "count of unduplicated algorithm 2 episodes: %s",
    unduped_counts %>%
      dplyr::filter(.data$no_algo1, !.data$no_algo2, .data$algo2_dup == 0) %>%
      dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "total unduplicated episodes: %s",
    nrow(all_rows)
  ))

  # recalculate merged dates, episode number, and episode length
  final_df <- all_rows %>%
    dplyr::select(any_of(c(
      "algo1_id", "algo2_id", "person_id", "pregnancy_end", "pregnancy_start",
      "first_gest_date", "category", "episode_min_date", "episode_max_date",
      "algo1_dup", "algo2_dup", "algo2_category", "algo2_outcome_date"
    ))) %>%
    dplyr::mutate(
      merged_episode_start = as.Date(pmin(.data$first_gest_date, .data$episode_min_date, .data$pregnancy_end, na.rm = TRUE)),
      merged_episode_end = as.Date(pmax(.data$episode_max_date, .data$pregnancy_end, na.rm = TRUE))
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$merged_episode_start) %>%
    dplyr::mutate(
      episode_num = dplyr::row_number(),
      merged_episode_length = as.numeric(difftime(.data$merged_episode_end, .data$merged_episode_start, units = "days")) / 30.25
    ) %>%
    dplyr::ungroup()

  return(final_df)
}

final_merged_episode_detailed <- function(final_merged_episodes_no_duplicates_df) {
  # Add demographic details for each patient.

  # ADD: assign PPS episodes without outcomes to PREG
  df <- final_merged_episodes_no_duplicates_df %>%
    dplyr::mutate(
      algo2_category = dplyr::if_else(
        !is.na(.data$algo2_id) & is.na(.data$algo2_category), "PREG", .data$algo2_category
      ),
      algo2_outcome_date = dplyr::if_else(
        !is.na(.data$algo2_id) & is.na(.data$algo2_outcome_date),
        .data$episode_max_date, .data$algo2_outcome_date
      )
    )

  df <- df %>%
    dplyr::rename(
      HIP_end_date = "pregnancy_end",
      HIP_outcome_category = "category",
      PPS_outcome_category = "algo2_category",
      PPS_end_date = "algo2_outcome_date",
      recorded_episode_start = "merged_episode_start",
      recorded_episode_end = "merged_episode_end",
      recorded_episode_length = "merged_episode_length"
    )

  # add columns marking if episode was identified either algorithm
  df <- df %>%
    dplyr::mutate(
      HIP_flag = dplyr::if_else(!is.na(.data$algo1_id), 1, 0),
      PPS_flag = dplyr::if_else(!is.na(.data$algo2_id), 1, 0),
      # add PPS outcome category for those without them
      PPS_outcome_category = dplyr::if_else(
        PPS_flag == 1 & is.na(PPS_outcome_category),
        "PREG",
        .data$PPS_outcome_category
      )
    )

  final_df <- df %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$recorded_episode_start) %>%
    dplyr::mutate(episode_number = dplyr::row_number()) %>%
    dplyr::ungroup()

  return(final_df)
}
