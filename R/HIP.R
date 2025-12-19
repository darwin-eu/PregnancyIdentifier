cdmTableExists <- function(cdm, tableName, attach = TRUE) {
  DBI::dbExistsTable(conn = attr(cdm, "dbcon"), name = "initial_pregnant_cohort_df")
}

#' runHip
#'
#' Runs the HIP
#'
#' @param cdm (`cdm_reference`) A CDM-Reference object from CDMConnector.
#' @param outputDir (`character(1)`) Output directory to write output to.
#' @param startDate (`Date(1)`: `as.Date("1900-01-01"`) Start date of data to use. By default 1900-01-01
#' @param endDate (`Date(1)`: `Sys.Date()`) End date of data to use. By default today.
#' @param logger (`logger`) Logger object.
#' @param justGestation (`logical(1)`: `TRUE`) Should episodes that only have gestational concepts be concidered?
#' @param ... Dev params
#'
#' @returns cdm object
#'
#' @export
runHip <- function(cdm, outputDir, startDate = as.Date("1900-01-01"), endDate = Sys.Date(), justGestation = TRUE, logger, ...) {
  log4r::info(logger, "START Running HIP")

  dots <- list(...)
  dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
  ## Outcome-based episodes

  # get initial cohort based on hip_concepts
  # this returns a dataset with person_id, concept_id, visit_date, domain, etc.
  # for all the the HIP concepts that for women who were 15-55
  cdm <- cdm %>%
    initial_pregnant_cohort(startDate = startDate, endDate = endDate, continue = dots$continue)

  if (getTblRowCount(cdm$initial_pregnant_cohort_df) == 0) {
    log4r::warn("No records after initializing pregnant cohort")
    return(cdm)
  }

  # get outcome visits from Matcho et al.
  # this function takes the HIP concepts matching the category of interest
  # calculates days between each visit and selects the first episode
  # and any episodes that are separated by at least that many days

  # TODO: Make param for categories
  categories <- list(
    c("AB", "SA"),
    "DELIV",
    "ECT",
    "SB",
    "LB"
  )

  for (i in seq_len(length(categories))) {
    log4r::info(logger, sprintf("Running Category: %s [%s/%s]", paste(categories[[i]], collapse = ", "), i, length(categories)))
    cdm <- final_visits(
      cdm,
      categories = categories[[i]],
      tableName = sprintf("final_%s_visits_df", tolower(paste(categories[[i]], collapse = "_"))),
      logger = logger
    )
  }

  # add stillbirth episodes to livebirth episodes
  # after making sure they are sufficiently spaced
  log4r::info(logger, "Adding Still Birth")
  cdm <- add_stillbirth(cdm)

  # add ectopic episodes to previous
  log4r::info(logger, "Adding Ectopic")
  cdm <- add_ectopic(cdm)

  # add abortion episodes to previous
  log4r::info(logger, "Adding Abortion")
  cdm <- add_abortion(cdm)

  # add delivery-only episodes to previous
  log4r::info(logger, "Adding Delivery")
  cdm <- add_delivery(cdm, logger = logger)

  # calculate start of pregnancies based on outcomes
  # min start date = latest possible start date if shortest term
  # max start date = earliest possible start date if max term
  cdm <- calculate_start(cdm)

  ## Gestation-based episodes

  # now go back to the initial set of concepts and find ones with gestation weeks
  cdm <- gestation_visits(cdm)

  # identify the start of episodes based on the difference in time between gestational age-related concepts
  # and the actual difference in days
  cdm <- gestation_episodes(cdm)

  # get various mins and maxes of gestational age and dates
  cdm <- get_min_max_gestation(cdm)

  ## Combine gestation-based and outcome-based episodes

  # add gestation episodes to outcome episodes
  cdm <- add_gestation(cdm, justGestation = justGestation, logger = logger)

  # clean episodes by removing duplicate episodes and reclassifying outcome-based episodes
  cdm <- clean_episodes(cdm, logger = logger)

  # remove any episodes that overlap and keep only the latter episode if the previous episode is PREG
  cdm <- remove_overlaps(cdm, logger = logger)

  # keep subset of columns with episode start and end as well as category
  cdm <- final_episodes(cdm)

  # find the first gestation record within an episode and calculate the episode length
  # based on the date of the first gestation record and the visit date
  cdm <- final_episodes_with_length(cdm)

  cdm$hip_episodes %>%
    dplyr::collect() %>%
    saveRDS(file.path(outputDir, "HIP_episodes.rds"))

  return(cdm)
}

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

# From: https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/algorithm/HIP_algorithm_functions.R

initial_pregnant_cohort <- function(cdm, startDate = as.Date("1900-01-01"), endDate = Sys.Date(), continue = FALSE) {
  if (cdmTableExists(cdm, "initial_pregnant_cohort_df") & continue) {
    cdm <- CDMConnector::readSourceTable(cdm = cdm, name = "initial_pregnant_cohort_df")
    return(cdm)
  }

  cdm$observation_df <- cdm$observation %>%
    dplyr::filter(
      .data$observation_date >= startDate,
      .data$observation_date <= endDate
    ) %>%
    dplyr::select(
      "person_id",
      concept_id = "observation_concept_id",
      visit_date = "observation_date",
      "value_as_number"
    ) %>%
    dplyr::inner_join(cdm$hip_concepts, by = "concept_id") %>%
    dplyr::compute()

  cdm$measurement_df <- cdm$measurement %>%
    dplyr::filter(
      .data$measurement_date >= startDate,
      .data$measurement_date <= endDate
    ) %>%
    dplyr::select(
      "person_id",
      concept_id = "measurement_concept_id",
      visit_date = "measurement_date",
      "value_as_number"
    ) %>%
    dplyr::inner_join(cdm$hip_concepts, by = "concept_id") %>%
    dplyr::compute()

  cdm$procedure_df <- cdm$procedure_occurrence %>%
    dplyr::filter(
      .data$procedure_date >= startDate,
      .data$procedure_date <= endDate
    ) %>%
    dplyr::select(
      "person_id",
      concept_id = "procedure_concept_id",
      visit_date = "procedure_date"
    ) %>%
    dplyr::inner_join(cdm$hip_concepts, by = "concept_id") %>%
    dplyr::compute()

  # filter condition table
  cdm$condition_df <- cdm$condition_occurrence %>%
    dplyr::filter(
      .data$condition_start_date >= startDate,
      .data$condition_end_date <= endDate
    ) %>%
    dplyr::select(
      "person_id",
      concept_id = "condition_concept_id",
      visit_date = "condition_start_date"
    ) %>%
    dplyr::inner_join(cdm$hip_concepts, by = "concept_id") %>%
    dplyr::compute()

  # combine tables
  all_dfs <- list(cdm$measurement_df, cdm$procedure_df, cdm$observation_df, cdm$condition_df)
  cdm$union_df <- purrr::reduce(all_dfs, dplyr::union_all) %>%
    dplyr::compute()

  # get unique person ids for women of reproductive age
  cdm$person_df <- cdm$person %>%
    dplyr::filter(
      # 45878463: Female
      # 46273637: Intersex
      # 45880669: Male
      # 1177221: I prefer not to answer
      # 903096: Skip
      # 4124462: None
      # TODO: Add option to specify specific column and/or concept ID(s)
      .data$gender_concept_id == 8532
      # .data$sex_at_birth_concept_id != 45880669
      # the majority of the people in the other non-Female or Male categories
      # also report female gender
    ) %>%
    dplyr::mutate(
      day_of_birth = as.integer(dplyr::if_else(is.na(.data$day_of_birth), 1L, .data$day_of_birth)),
      month_of_birth = as.integer(dplyr::if_else(is.na(.data$month_of_birth), 1L, .data$month_of_birth)),
      date_of_birth = as.Date(paste0(as.character(as.integer(.data$year_of_birth)), "-", as.character(as.integer(.data$month_of_birth)), "-", as.character(as.integer(.data$day_of_birth))))
    ) %>%
    dplyr::select("person_id", "date_of_birth") %>%
    dplyr::compute()

  # keep only person_ids of women of reproductive age at some visit
  cdm$initial_pregnant_cohort_df <- cdm$union_df %>%
    dplyr::inner_join(cdm$person_df, by = "person_id") %>%
    dplyr::mutate(
      date_diff = !!CDMConnector::datediff("date_of_birth", "visit_date", interval = "day")
    ) %>%
    dplyr::mutate(
      age = .data$date_diff / 365
    ) %>%
    # TODO: Add param for upper and lower bounds
    dplyr::filter(.data$age >= 15) %>%
    dplyr::filter(.data$age < 56) %>%
    dplyr::distinct() %>%
    dplyr::compute(name = "initial_pregnant_cohort_df", temporary = FALSE, overwrite = TRUE)

  return(cdm)
}

# Note here that for SA and AB, if there is an episode that contains concepts for both,
# only one will be (essentially randomly) chosen
final_visits <- function(cdm, categories, tableName, logger) {
  cdm$temp_df <- cdm$initial_pregnant_cohort_df %>%
    dplyr::filter(category %in% categories) %>%
    # only keep one obs per person-date -- they're all in the same category
    # select(person_id, visit_date, category) %>%
    dplyr::group_by(person_id, visit_date) %>%
    # slicing by minimum concept id just a choice to make the code work
    # could have also done something like filter(row_number() == 1) but doesn't
    # work on databases
    dplyr::slice_min(order_by = concept_id, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    # distinct(person_id, visit_date, .keep_all = TRUE) %>% stopped working?!
    dplyr::group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    # Create a new column called "days" that calculates the number of days between each visit for each person.
    dplyr::mutate(prev_visit_date = lag(.data$visit_date)) %>%
    dplyr::mutate(days = !!CDMConnector::datediff(start = "prev_visit_date", end = "visit_date", interval = "day")) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # get minimum days between outcomes
  category <- categories[1]
  min_day <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == category & .data$outcome_preg_category == category) %>%
    dplyr::pull(.data$min_days)

  # identify first visit for each pregnancy episode
  cdm$first_df <- cdm$temp_df %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::slice_min(.data$visit_date) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  cdm$other_df <- cdm$temp_df %>%
    dplyr::filter(.data$days >= min_day) %>%
    dplyr::compute()

  cdm$all_df <- dplyr::union_all(cdm$first_df, cdm$other_df) %>%
    dplyr::distinct() %>%
    dplyr::compute()

  log4r::info(logger, sprintf(
    "  * Preliminary total number of episodes: %s",
    getTblRowCount(cdm$all_df)
  ))

  cdm[[tableName]] <- cdm$all_df %>%
    dplyr::compute(name = tableName, temporary = FALSE, overwrite = TRUE)
  return(cdm)
}

add_stillbirth <- function(cdm) {
  # Add stillbirth visits to livebirth visits table.

  # get minimum days between outcomes
  before_min <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "LB" & .data$outcome_preg_category == "SB") %>%
    dplyr::pull(.data$min_days)

  after_min <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "SB" & .data$outcome_preg_category == "LB") %>%
    dplyr::pull(.data$min_days)


  # pull out the stillbirth episodes again, but first figure out if it's plausible
  # that they happened relative to a live birth
  cdm$final_temp_df <- cdm$final_sb_visits_df %>%
    dplyr::union_all(cdm$final_lb_visits_df) %>%
    dplyr::select(-dplyr::any_of(c("gest_value", "value_as_number"))) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$visit_date) %>%
    dplyr::mutate(
      # get previous category if available
      previous_category = dplyr::lag(.data$category),
      prev_visit = dplyr::lag(visit_date),
      next_category = dplyr::lead(category),
      next_visit = dplyr::lead(visit_date)
    ) %>%
    dplyr::mutate(
      # get difference in days with previous episode start date
      after_days = !!CDMConnector::datediff("prev_visit", "visit_date", "day"),
      # and next episode start date
      before_days = !!CDMConnector::datediff("visit_date", "next_visit", "day")
    ) %>%
    dplyr::filter(.data$category == "SB") %>%
    dplyr::filter(
      # it's the only episode
      (is.na(.data$before_days) & is.na(.data$after_days))
      # the previous category was a stillbirth and there's no next category
      # (those were already checked)
      |(.data$previous_category != "LB" & is.na(.data$next_category))
      # same but opposite
      |(.data$next_category != "LB" & is.na(.data$previous_category))
      |(.data$previous_category != "LB" & .data$next_category != "LB")
      # the last episode was a live birth and this one happens after the minimum
      |(.data$previous_category == "LB" & .data$after_days >= before_min & is.na(.data$next_category))
      # the next episode is a live birth and happens after the minimum
      |(.data$next_category == "LB" & .data$before_days >= after_min & is.na(.data$previous_category))
      # or surrounded by two live births spaced sufficiently
      |(.data$next_category == "LB" & .data$before_days >= after_min & .data$previous_category == "LB" & .data$after_days >= before_min)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # combine with livebirth table and drop columns
  cdm$add_stillbirth_df <- cdm$final_lb_visits_df %>%
    dplyr::union_all(cdm$final_temp_df) %>%
    dplyr::select(-"previous_category", -"next_category", -"before_days", -"after_days") %>%
    dplyr::distinct() %>%
    dplyr::compute()
  return(cdm)
}

add_ectopic <- function(cdm) {
  # get minimum days between outcomes
  # minimum number of days that ECT can follow LB and SB; LB and SB have the same days
  before_min <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "LB" & .data$outcome_preg_category == "ECT") %>%
    dplyr::pull(.data$min_days)
  # minimum number of days that LB can follow ECT
  after_min_lb <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "ECT" & .data$outcome_preg_category == "LB") %>%
    dplyr::pull(.data$min_days)
  # minimum number of days that SB can follow ECT
  after_min_sb <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "ECT" & .data$outcome_preg_category == "SB") %>%
    dplyr::pull(.data$min_days)

  # get difference in days with subsequent visit
  cdm$final_temp_df <- cdm$add_stillbirth_df %>%
    dplyr::union_all(
      cdm$final_ect_visits_df %>%
        dplyr::select(-any_of(c("gest_value", "value_as_number")))
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$visit_date) %>%
    dplyr::mutate(
      # get previous category if available
      previous_category = dplyr::lag(.data$category),
      next_category = dplyr::lead(category),
      prev_visit = dplyr::lag(visit_date),
      next_visit = dplyr::lead(visit_date)
    ) %>%
    dplyr::mutate(
      # get difference in days with previous episode start date
      after_days = !!CDMConnector::datediff("prev_visit", "visit_date", "day"),
      # and next episode start date
      before_days = !!CDMConnector::datediff("visit_date", "next_visit", "day")
    ) %>%
    # filter to ectopic visits
    # keep visits with days containing null values - indicates single event
    # keep visits not preceded by or followed by LB or SB
    # record is preceded by livebirth/stillbirth only
    # record is followed by livebirth only
    # record is followed by stillbirth only
    # record is followed by LB and preceded by livebirth/stillbirth
    # record is followed by SB and preceded by livebirth/stillbirth
    dplyr::filter(.data$category == "ECT") %>%
    dplyr::filter(
      # it's the only episode
      (is.na(.data$before_days) & is.na(.data$after_days)) |
        # the previous category was ectopic and there's no next category
        # (those were already checked)
        # or some configuration
        (!.data$previous_category %in% c("LB", "SB") & is.na(.data$next_category)) |
        (!.data$next_category %in% c("LB", "SB") & is.na(.data$previous_category)) |
        (!.data$previous_category %in% c("LB", "SB") & !.data$next_category %in% c("LB", "SB")) |
        # the last episode was a delivery and this one happens after the minimum
        (.data$previous_category %in% c("LB", "SB") & .data$after_days >= before_min & is.na(.data$next_category)) |
        # there was no previous category and the next live birth happens after the minimum
        (.data$next_category == "LB" & .data$before_days >= after_min_lb & is.na(.data$previous_category)) |
        (.data$next_category == "SB" & .data$before_days >= after_min_sb & is.na(.data$previous_category)) |
        # surrounded by each, appropriately spaced
        (.data$next_category == "LB" & .data$before_days >= after_min_lb & .data$previous_category %in% c("LB", "SB") & .data$after_days >= before_min) |
        (.data$next_category == "SB" & .data$before_days >= after_min_sb & .data$previous_category %in% c("LB", "SB") & .data$after_days >= before_min)
    ) %>%
    dplyr::ungroup()

  cdm$add_ectopic_df <- cdm$add_stillbirth_df %>%
    dplyr::union_all(cdm$final_temp_df) %>%
    dplyr::select(-"previous_category", -"next_category", -"before_days", -"after_days") %>%
    dplyr::distinct() %>%
    dplyr::compute()

  return(cdm)
}

add_abortion <- function(cdm) {
  # Add abortion visits - SA and AB are treated the same.

  # get minimum days between outcomes
  # minimum number of days that ECT can follow LB and SB; LB and SB have the same days
  before_min_lb <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "LB" & .data$outcome_preg_category == "AB") %>%
    dplyr::pull(.data$min_days)

  before_min_ect <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "ECT" & .data$outcome_preg_category == "AB") %>%
    dplyr::pull(.data$min_days)
  # minimum number of days that LB can follow ECT
  after_min_lb <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "AB" & .data$outcome_preg_category == "LB") %>%
    dplyr::pull(.data$min_days)
  # minimum number of days that SB can follow ECT
  after_min_sb <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "AB" & .data$outcome_preg_category == "SB") %>%
    dplyr::pull(.data$min_days)

  after_min_ect <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "AB" & .data$outcome_preg_category == "ECT") %>%
    dplyr::pull(.data$min_days)

  # get difference in days with subsequent visit
  cdm$final_temp_df <- cdm$add_ectopic_df %>%
    dplyr::union_all(
      cdm$final_ab_sa_visits_df %>%
        dplyr::select(-any_of(c("gest_value", "value_as_number")))
    ) %>%
    dplyr::mutate(
      temp_category = ifelse(.data$category == "SA", "AB", .data$category)
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$visit_date) %>%
    dplyr::mutate(
      # get previous category if available
      previous_category = dplyr::lag(.data$temp_category),
      next_category = dplyr::lead(.data$temp_category),
      prev_visit = dplyr::lag(visit_date),
      next_visit = dplyr::lead(visit_date)
    ) %>%
    dplyr::mutate(
      # get difference in days with previous episode start date
      after_days = !!CDMConnector::datediff("prev_visit", "visit_date", "day"),
      # and next episode start date
      before_days = !!CDMConnector::datediff("visit_date", "next_visit", "day")
    ) %>%
    dplyr::filter(.data$temp_category == "AB") %>%
    dplyr::filter(
      # don't have to worry about limits
      (is.na(.data$before_days) & is.na(.data$after_days)) |
        (!.data$previous_category %in% c("LB", "SB", "ECT") & is.na(.data$next_category)) |
        (!.data$next_category %in% c("LB", "SB", "ECT") & is.na(.data$previous_category)) |
        (!.data$previous_category %in% c("LB", "SB", "ECT") & !.data$next_category %in% c("LB", "SB", "ECT")) |

        # the last episode was a delivery and this one happens after the minimum
        (.data$previous_category %in% c("LB", "SB") & .data$after_days >= before_min_lb & is.na(.data$next_category)) |
        (.data$next_category == "LB" & .data$before_days >= after_min_lb & is.na(.data$previous_category)) |
        (.data$next_category == "SB" & .data$before_days >= after_min_sb & is.na(.data$previous_category)) |
        (.data$next_category == "LB" & .data$previous_category %in% c("LB", "SB") & .data$before_days >= after_min_lb & .data$after_days >= before_min_lb) |
        (.data$next_category == "SB" & .data$previous_category %in% c("LB", "SB") & .data$before_days >= after_min_sb & .data$after_days >= before_min_lb) |
        (.data$previous_category == "ECT" & .data$after_days >= before_min_ect & is.na(.data$next_category)) |
        (.data$next_category == "ECT" & .data$before_days >= after_min_ect & is.na(.data$previous_category)) |
        (.data$next_category == "ECT" & .data$previous_category == "ECT" & .data$before_days >= after_min_ect & .data$after_days >= before_min_ect) |
        (.data$next_category == "ECT" & .data$previous_category %in% c("LB", "SB") & .data$before_days >= after_min_ect & .data$after_days >= before_min_lb) |
        (.data$next_category == "LB" & .data$previous_category == "ECT" & .data$before_days >= after_min_lb & .data$after_days >= before_min_ect) |
        (.data$next_category == "SB" & .data$previous_category == "ECT" & .data$before_days >= after_min_sb & .data$after_days >= before_min_ect)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  cdm$add_abortion_df <- cdm$add_ectopic_df %>%
    dplyr::union_all(cdm$final_temp_df) %>%
    dplyr::select(-"previous_category", -"next_category", -"before_days", -"after_days", -"temp_category") %>%
    dplyr::distinct() %>%
    dplyr::compute()
  return(cdm)
}

add_delivery <- function(cdm, logger) {
  #  Add delivery record only visits

  # get minimum days between outcomes
  # minimum number of days that DELIV can follow LB and SB; LB and SB have the same days
  before_min_lb <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "LB" & .data$outcome_preg_category == "DELIV") %>%
    dplyr::pull(.data$min_days)

  before_min_ect <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "ECT" & .data$outcome_preg_category == "DELIV") %>%
    dplyr::pull(.data$min_days)
  # minimum number of days that LB can follow
  # to add: if there's LB or SB outcome before then, they should be given this delivery date
  after_min_lb <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "DELIV" & .data$outcome_preg_category == "LB") %>%
    dplyr::pull(.data$min_days)
  # minimum number of days that SB can follow
  after_min_sb <- cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == "DELIV" & .data$outcome_preg_category == "SB") %>%
    dplyr::pull(.data$min_days)

  after_min_ect <- cdm$matcho_outcome_limits %>%
    dplyr::filter(first_preg_category == "DELIV" & outcome_preg_category == "ECT") %>%
    dplyr::pull(min_days)

  # get difference in days with subsequent visit
  cdm$final_temp_df <- cdm$add_abortion_df %>%
    dplyr::union_all(
      cdm$final_deliv_visits_df %>%
        dplyr::select(-any_of(c("gest_value", "value_as_number")))
    ) %>%
    dplyr::mutate(temp_category = ifelse(.data$category == "SA", "AB", .data$category)) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$visit_date) %>%
    dplyr::mutate(
      # get previous category if available
      previous_category = lag(temp_category),
      prev_visit = lag(visit_date),
      # get difference in days with previous episode start date
      next_category = lead(temp_category),
      next_visit = lead(visit_date)
      # and next episode start date
    ) %>%
    dplyr::mutate(
      after_days = !!CDMConnector::datediff("prev_visit", "visit_date", "day"),
      before_days = !!CDMConnector::datediff("visit_date", "next_visit", "day")
    ) %>%
    dplyr::compute()

  # have the deliveries and all othe others
  # add this: want to move the LB or SB date earlier if there's an earlier delivery date
  cdm$add_abortion_df_rev <- cdm$final_temp_df %>%
    dplyr::mutate(
      visit_date = dplyr::if_else(
        !is.na(.data$previous_category)
        & .data$previous_category == "DELIV"
        & .data$category %in% c("LB", "SB") & .data$after_days < after_min_sb,
        .data$prev_visit,
        .data$visit_date
      )
    ) %>%
    dplyr::filter(category != "DELIV") %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  cdm$final_temp_df <- cdm$final_temp_df %>%
    dplyr::filter(.data$category == "DELIV") %>%
    dplyr::filter(
      # don't need to worry about timing
      (is.na(.data$before_days) & is.na(.data$after_days)) |
        (!.data$previous_category %in% c("LB", "SB", "ECT", "AB") & is.na(.data$next_category)) |
        (!.data$next_category %in% c("LB", "SB", "ECT", "AB") & is.na(.data$previous_category)) |
        (!.data$previous_category %in% c("LB", "SB", "ECT", "AB") & !.data$next_category %in% c("LB", "SB", "ECT", "AB")) |
        # timing
        (.data$previous_category %in% c("LB", "SB") & .data$after_days >= before_min_lb & is.na(.data$next_category)) |
        (.data$next_category == "LB" & .data$before_days >= after_min_lb & is.na(.data$previous_category)) |
        (.data$next_category == "SB" & .data$before_days >= after_min_sb & is.na(.data$previous_category)) |
        (.data$next_category == "LB" & .data$previous_category %in% c("LB", "SB") & .data$before_days >= after_min_lb & .data$after_days >= before_min_lb) |
        (.data$next_category == "SB" & .data$previous_category %in% c("LB", "SB") & .data$before_days >= after_min_sb & .data$after_days >= before_min_lb) |
        (.data$previous_category %in% c("ECT", "AB") & .data$after_days >= before_min_ect & is.na(.data$next_category)) |
        (.data$next_category %in% c("ECT", "AB") & .data$before_days >= after_min_ect & is.na(.data$previous_category)) |
        (.data$next_category %in% c("ECT", "AB") & .data$previous_category %in% c("ECT", "AB") & .data$before_days >= after_min_ect & .data$after_days >= before_min_ect) |
        (.data$next_category %in% c("ECT", "AB") & .data$previous_category %in% c("LB", "SB") & .data$before_days >= after_min_ect & .data$after_days >= before_min_lb) |
        (.data$next_category == "LB" & .data$previous_category %in% c("ECT", "AB") & .data$before_days >= after_min_lb & .data$after_days >= before_min_ect) |
        (.data$next_category == "SB" & .data$previous_category %in% c("ECT", "AB") & .data$before_days >= after_min_sb & .data$after_days >= before_min_ect)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  cdm$add_delivery_df <- cdm$add_abortion_df_rev %>%
    dplyr::union_all(cdm$final_temp_df) %>%
    dplyr::select(-"previous_category", -"next_category", -"before_days", -"after_days", -"temp_category") %>%
    dplyr::distinct() %>%
    dplyr::compute()

  counts <- cdm$add_delivery_df %>%
    dplyr::group_by(.data$category) %>%
    dplyr::summarise(n = as.integer(dplyr::n())) %>%
    dplyr::collect()

  for (i in seq_len(nrow(counts))) {
    log4r::info(logger, sprintf("%s: %s", counts[i, "category"], counts[i, "n"]))
  }
  return(cdm)
}

calculate_start <- function(cdm) {
  # Estimate start of pregnancies based on outcome type.

  # join tables
  cdm$calculate_start_df <- cdm$add_delivery_df %>%
    dplyr::left_join(cdm$matcho_term_durations, by = "category") %>%
    dplyr::mutate(
      min_term = dplyr::if_else(is.na(.data$min_term), NA_integer_, as.integer(.data$min_term)),
      max_term = dplyr::if_else(is.na(.data$max_term), NA_integer_, as.integer(.data$max_term))
    ) %>%
    # based only on the outcome, when did pregnancy start
    # calculate latest start start date
    dplyr::mutate(
      min_start_date = as.Date(!!CDMConnector::dateadd(date = "visit_date", number = "-min_term", interval = "day")),
      # calculate earliest start date
      max_start_date = as.Date(!!CDMConnector::dateadd(date = "visit_date", number = "-max_term", interval = "day"))
    ) %>%
    dplyr::compute()

  return(cdm)
}

gestation_visits <- function(cdm) {
  # Filter to visits with gestation period.
  # Additional gestation concepts to use:
  # 3002209 - Gestational age Estimated
  # 3048230 - Gestational age in weeks
  # 3012266 - Gestational age

  # Get records with gestation period
  gest_df <- cdm$initial_pregnant_cohort_df %>%
    dplyr::filter(!is.na(.data$gest_value))

  # Get records with gestational age in weeks
  cdm$gestation_visits_df <- cdm$initial_pregnant_cohort_df %>%
    dplyr::filter(
      .data$concept_id %in% c(3002209, 3048230, 3012266),
      !is.na(.data$value_as_number),
      # also filter out 0 -- this is an error
      .data$value_as_number > 0, .data$value_as_number <= 44
    ) %>%
    dplyr::mutate(gest_value = as.integer(.data$value_as_number)) %>%
    dplyr::union_all(gest_df) %>%
    dplyr::compute()

  return(cdm)
}

gestation_episodes <- function(cdm, min_days = 70, buffer_days = 28) {
  # minimum number of days to be new distinct episode
  # number of days to use as a buffer

  # Define pregnancy episode per patient by gestational records.
  #
  # Any record with a negative change or no change in the gestational age in
  # weeks from the previous record is flagged as the start of a potential
  # episode. This record is then checked if there is at least a separation of
  # 70 days from the previous record. The number of days, 70, was determined
  # by taking the minimum outcome limit in days from Matcho et al. (56) and
  # adding a buffer of 14 days. If the record is not at least 70 days from the
  # previous record, it is no longer flagged as the start of an episode.
  #
  # For all records with a positive change in the gestational age in weeks
  # from the previous record is then checked if the date difference in days is
  # greater than the difference in days between the record's gestational age
  # in weeks and the previous record's gestational age in weeks with a buffer
  # of 28 days. The buffer of 28 days was determined by taking the minimum
  # retry period in days from Matcho et al. (14) and adding 14 days as a
  # buffer. If the date difference in days is greater than the difference in
  # days between the record's gestational age in weeks and the previous
  # record's gestational age in weeks with the buffer, then this record is
  # flagged as a start of a new episode.

  # filter out any empty visit dates
  cdm$gestation_episodes_df <- cdm$gestation_visits_df %>%
    dplyr::filter(
      !is.na(.data$visit_date),
      # remove any records with incorrect gestational weeks (i.e. 9999999)
      .data$gest_value > 0 & .data$gest_value <= 44
    ) %>%
    # keep max gest_value if two or more gestational records share same date
    dplyr::group_by(.data$person_id, .data$visit_date) %>%
    dplyr::mutate(gest_week = max(.data$gest_value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    # filter out rows that are not the max gest_value at visit_date
    dplyr::filter(.data$gest_value == .data$gest_week) %>%
    # add column for gestation period in days
    dplyr::mutate(gest_day = .data$gest_week * 7) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$visit_date) %>%
    dplyr::mutate(
      # get previous gestation week
      prev_week = dplyr::lag(.data$gest_week, 1),
      # get previous date
      prev_date = dplyr::lag(.data$visit_date, 1),
      # calculate difference between gestation weeks
      week_diff = .data$gest_week - .data$prev_week,
      # calculate number of days between gestation weeks with buffer
      day_diff = .data$week_diff * 7 + buffer_days,
    ) %>%
    dplyr::mutate(
      # get difference in days between visit date and previous date
      date_diff = !!CDMConnector::datediff("prev_date", "visit_date", "day"),
      # check if any negative or zero number in week_diff column corresponds to a new pregnancy episode
      # assume it does if the difference in actual dates is larger than the minimum
      # change to 1 (arbitrary positive number) if not;
      # new_diff = 1 if the next obs has lower gest week and the difference in dates
      # is smaller than the minimum number of days between pregnancies
      # week_diff is negative if at a lower gestational age now
      new_diff = dplyr::if_else(.data$date_diff < min_days & .data$week_diff <= 0, 1, .data$week_diff),
      # check if any positive number in week_diff column (so at a higher gestational age next time)
      # has a date_diff >= day_diff
      # that means that the difference in time is greater than the difference
      # in gestational age + buffer
      # may correspond to new pregnancy episode, if so change to -1 (negative number)
      new_diff2 = dplyr::if_else(.data$date_diff >= .data$day_diff & .data$week_diff > 0, -1, .data$new_diff),
      # create new columns, index and episode; any zero or negative number in newdiff2 column indicates a new episode
      index = dplyr::row_number(),
      # count as an episodes if first row or
      # difference in gest age is negative and date_diff large enough for it to be a new pregnancy or
      # difference in gest age is positive but that difference is larger than the difference in dates
      episode = as.integer(cumsum(ifelse(.data$new_diff2 <= 0 | .data$index == 1, 1, 0))),
      episode_chr = as.character(.data$episode) # for grouping
    ) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  return(cdm)
}

get_min_max_gestation <- function(cdm) {
  # Get the min and max gestational age in weeks and the corresponding visit
  # dates per pregnancy episode.
  #
  # Also get the first and last visits and their corresponding gestational age
  # in weeks per pregnancy episode.
  #
  # For minimum gestational age in weeks, the first and last occurrence and
  # their dates were obtained.

  ############ First Visit Date ############

  # identify first visit for each pregnancy episode
  # and get max gestation week at first visit date
  cdm$new_first_df <- cdm$gestation_episodes_df %>%
    dplyr::group_by(.data$person_id, .data$episode) %>%
    dplyr::slice_min(.data$visit_date, n = 1) %>%
    dplyr::summarise(first_gest_week = max(.data$gest_week, na.rm = TRUE), .groups = "drop") %>%
    dbplyr::window_order() %>%
    dplyr::compute()

  ############ Min Gestation Week ############

  # identify minimum gestation week for each pregnancy episode
  cdm$temp_min_df <- cdm$gestation_episodes_df %>%
    dplyr::group_by(.data$person_id, .data$episode) %>%
    dplyr::slice_min(.data$gest_week, n = 1) %>%
    dplyr::mutate(min_gest_week = .data$gest_week) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # get range of time when that gestational week was recorded
  # get first occurrence of min gestation week
  cdm$new_min_df <- cdm$temp_min_df %>%
    dplyr::group_by(.data$person_id, .data$episode, .data$min_gest_week) %>%
    dplyr::summarize(min_gest_date = min(.data$visit_date, na.rm = TRUE), .groups = "drop") %>%
    dbplyr::window_order() %>%
    dplyr::compute()

  # get last occurrence of min gestation week
  cdm$second_min_df <- cdm$temp_min_df %>%
    dplyr::group_by(.data$person_id, .data$episode, .data$gest_week) %>% # = min_gest_week
    dplyr::summarize(min_gest_date_2 = max(.data$visit_date, na.rm = TRUE), .groups = "drop") %>%
    dbplyr::window_order() %>%
    dplyr::compute()

  ############ End Visit Date ############

  # identify end visit for each pregnancy episode
  # keep in mind this could be a month after pregnancy actually ended...
  cdm$temp_end_df <- cdm$gestation_episodes_df %>%
    dplyr::group_by(.data$person_id, .data$episode) %>%
    dplyr::slice_max(.data$visit_date, n = 1) %>%
    dplyr::mutate(end_gest_date = .data$visit_date) %>%
    dplyr::compute()

  # get max gestation week at end visit date
  cdm$new_end_df <- cdm$temp_end_df %>%
    dplyr::group_by(.data$person_id, .data$episode, .data$end_gest_date) %>%
    dplyr::summarise(end_gest_week = max(.data$gest_week, na.rm = TRUE), .groups = "drop") %>%
    dbplyr::window_order() %>%
    dplyr::compute()

  ############ Max Gestation Week ############

  # identify max gestation week for each pregnancy episode
  cdm$temp_max_df <- cdm$gestation_episodes_df %>%
    dplyr::group_by(.data$person_id, .data$episode) %>%
    dplyr::slice_max(.data$gest_week, n = 1) %>%
    dplyr::mutate(max_gest_week = .data$gest_week) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # get first occurrence of max gestation week
  cdm$new_max_df <- cdm$temp_max_df %>%
    dplyr::group_by(.data$person_id, .data$episode, .data$max_gest_week) %>%
    dplyr::summarize(max_gest_date = min(.data$visit_date, na.rm = TRUE), .groups = "drop") %>%
    dbplyr::window_order() %>%
    dplyr::compute()

  # max_gest_date can be later than min_gest_date_2 (only one GA but multiple dates)

  ############ Join tables ############

  # join first and end tables
  cdm$get_min_max_gestation_df <- cdm$new_first_df %>%
    dplyr::inner_join(cdm$new_end_df, by = c("person_id", "episode")) %>%
    dplyr::inner_join(cdm$new_min_df, by = c("person_id", "episode")) %>%
    dplyr::inner_join(cdm$second_min_df, by = c("person_id", "episode")) %>%
    dplyr::inner_join(cdm$new_max_df, by = c("person_id", "episode")) %>%
    dplyr::compute()

  return(cdm)
}

### START HERE
add_gestation <- function(cdm, buffer_days = 28, justGestation = TRUE, logger) {
  # Add gestation-based episodes. Any gestation-based episode that overlaps with an outcome-based
  # episode is removed as a distinct episode.
  # add unique id for each outcome visit
  cdm$calculate_start_df <- cdm$calculate_start_df %>%
    # visit date is the first outcome date for the hierarchically chosen outcome
    dplyr::mutate(visit_id = paste0(as.character(.data$person_id), as.character(.data$visit_date))) %>%
    dplyr::compute()

  # add unique id for each gestation visit
  cdm$get_min_max_gestation_df <- cdm$get_min_max_gestation_df %>%
    # max gest date is the first occurrence of the maximum gestational week
    dplyr::mutate(
      gest_id = paste0(as.character(.data$person_id), as.character(.data$max_gest_date))
    ) %>%
    dplyr::compute() %>%
    dplyr::mutate(
      # add column for gestation period in days for largest gestation week on record
      max_gest_day = (.data$max_gest_week * 7),
      # add column for gestation period in days for smallest gestation week on record
      min_gest_day = (.data$min_gest_week * 7),
    ) %>%
    dplyr::compute() %>%
    dplyr::mutate(
      # get date of estimated start date based on max gestation week on record
      # max_gest_date is the first occurrence of the maximum gestational week
      max_gest_start_date = !!CDMConnector::dateadd(date = "max_gest_date", number = "-max_gest_day", interval = "day"),
      # get date of estimated start date based on min gestation week on record
      # min_gest_date is the first occurence of the min gestational week
      min_gest_start_date = !!CDMConnector::dateadd(date = "min_gest_date", number = "-min_gest_day", interval = "day"),
      # which one is earlier
      max_gest_start_date_further = dplyr::if_else(
        .data$max_gest_start_date > .data$min_gest_start_date,
        .data$min_gest_start_date, .data$max_gest_start_date
      ),
      # and which one is later
      min_gest_start_date = dplyr::if_else(
        max_gest_start_date > min_gest_start_date,
        max_gest_start_date, min_gest_start_date
      ),
      # so max_gest_start_date will always be earlier
      max_gest_start_date = .data$max_gest_start_date_further
    ) %>%
    dplyr::compute() %>%
    dplyr::mutate(
      # get difference in days between estimated start dates
      gest_start_date_diff = !!CDMConnector::datediff("min_gest_start_date", "max_gest_start_date", "day")
    ) %>%
    dplyr::compute()

  # join both tables to find overlaps
  cdm$both_df <- cdm$calculate_start_df %>%
    dplyr::inner_join(
      cdm$get_min_max_gestation_df,
      by = dplyr::join_by(person_id, overlaps(
        max_start_date, visit_date,
        max_gest_start_date, max_gest_date
      ))
    ) %>%
    # Check for any gestation-based episodes that overlap with more than one outcome-based
    # episode and keep only those episodes where the gestation-based end date is closest to the
    # outcome date.
    # dplyr::mutate(days_diff = as.numeric(difftime(visit_date, max_gest_date))) %>%
    dplyr::mutate(
      # add -- these are changed anyway so if there are multiple similar overlaps, choose
      # the one with the better term duration
      # visit date should be the first visit date at which there's an outcome
      gest_at_outcome = !!CDMConnector::datediff("max_gest_start_date", "visit_date", "day")
    ) %>%
    dplyr::mutate(
      # we want it to be under the max
      is_under_max = ifelse(.data$gest_at_outcome <= .data$max_term, 1, 0),
      # and over the min, ie both = 1
      is_over_min = ifelse(.data$gest_at_outcome >= .data$min_term, 1, 0)
    ) %>%
    dplyr::mutate(
      days_diff = !!CDMConnector::datediff("max_gest_date", "visit_date", "day"),
      days_diff = dplyr::if_else(.data$is_over_min == 1 | .data$is_under_max == 1 | .data$days_diff < -buffer_days, 10000, .data$days_diff)
    ) %>%
    dplyr::group_by(visit_id) %>%
    dplyr::slice_min(order_by = abs(.data$days_diff), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$gest_id) %>%
    dplyr::slice_min(order_by = abs(.data$days_diff), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # only outcome-based episodes
  cdm$just_outcome_df <- cdm$calculate_start_df %>%
    dplyr::anti_join(
      cdm$both_df %>%
        dplyr::select("visit_id"),
      by = "visit_id"
    ) %>%
    dplyr::compute()

  cdm$just_gestation_df <- cdm$get_min_max_gestation_df %>%
    dplyr::anti_join(
      cdm$both_df %>%
        dplyr::select("gest_id"),
      by = "gest_id"
    ) %>%
    dplyr::mutate(
      category = "PREG",
      # visit date becomes
      visit_date = .data$max_gest_date
    ) %>%
    dplyr::compute()

  # only gestation-based episodes
  tblList <- if (justGestation) {
    list(
      cdm$both_df,
      cdm$just_outcome_df,
      cdm$just_gestation_df
    )
  } else {
    list(
      cdm$both_df,
      cdm$just_outcome_df
    )
  }

  cdm$add_gestation_df <- purrr::reduce(
    tblList,
    dplyr::union_all
  ) %>%
    dplyr::select(-dplyr::all_of("episode")) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$visit_date) %>%
    dplyr::mutate(episode = row_number()) %>%
    dplyr::ungroup() %>%
    # recalculate since I overwrote
    dplyr::mutate(days_diff = !!CDMConnector::datediff("max_gest_date", "visit_date", "day")) %>%
    dplyr::compute()

  counts <- cdm$add_gestation_df %>%
    dplyr::count(
      gestation_based = !is.na(.data$gest_id),
      outcome_based = !is.na(.data$visit_id)
  ) %>%
    dplyr::collect()

  log4r::info(logger, sprintf(
    "Total number of outcome-based episodes: %s",
    dplyr::tally(cdm$calculate_start_df) %>% dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "Total number of gestation-based episodes: %s",
    dplyr::tally(cdm$get_min_max_gestation_df) %>% dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "Total number of only outcome-based episodes after merging: %s",
    counts %>% dplyr::filter(!gestation_based, outcome_based) %>% dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "Total number of only gestation-based episodes after merging: %s",
    counts %>% dplyr::filter(gestation_based, !outcome_based) %>% dplyr::pull(.data$n)
  ))

  log4r::info(logger, sprintf(
    "Total number of episodes with both after merging: %s",
    counts %>% dplyr::filter(gestation_based, outcome_based) %>% dplyr::pull(.data$n)
  ))

  return(cdm)
}

clean_episodes <- function(cdm, buffer_days = 28, logger) {
  # Clean up episodes by removing duplicate episodes and reclassifying outcome-based episodes
  # as gestation-based episodes if the outcome containing gestational info does not fall within
  # the term durations defined by Matcho et al.
  cdm$final_df <- cdm$add_gestation_df %>%
    dplyr::compute()

  # remove any outcomes where the gestational age based on max_gest_date is over the max term duration defined by Matcho et al.
  cdm$over_max_df <- cdm$final_df %>%
    # it has both an outcome and a gestation but is over the max
    dplyr::filter(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$is_under_max == 0) %>%
    dplyr::mutate(
      removed_category = .data$category,
      category = "PREG",
      visit_date = .data$max_gest_date,
      removed_outcome = 1
    ) %>%
    dplyr::compute()

  # filter out these episodes from main table
  cdm$final_df <- cdm$final_df %>%
    dplyr::filter(!(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$is_under_max == 0)) %>%
    dplyr::mutate(
      removed_outcome = 0
    ) %>%
    dplyr::compute()

  log4r::info(logger, sprintf(
    "Total number of episodes over maximum term duration: %s",
    cdm$over_max_df %>%
      dplyr::tally() %>%
      dplyr::pull(.data$n) %>%
      as.integer()
  ))

  # join episodes with new values back to main table
  cdm$final_df <- cdm$final_df %>%
    dplyr::union_all(cdm$over_max_df) %>%
    dplyr::compute()

  ###### remove any outcomes where the gestational age based on max_gest_date is under the min term duration defined by Matcho et al. ######

  # filter to episodes with max_gest_date is under the min term duration and where the number of days between the visit_date and
  # max_gest_date is negative with buffer
  cdm$under_min_df <- cdm$final_df %>%
    dplyr::filter(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$is_over_min == 0 & .data$days_diff < -buffer_days) %>%
    dplyr::mutate(
      removed_category = .data$category,
      category = "PREG",
      visit_date = .data$max_gest_date,
      removed_outcome = 1
    ) %>%
    dplyr::compute()

  # filter out these episodes from main table
  cdm$final_df <- cdm$final_df %>%
    dplyr::filter(!(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$is_over_min == 0 & .data$days_diff < -buffer_days)) %>%
    dplyr::compute()

  log4r::info(logger, sprintf(
      "Total number of episodes under minimum term duration: %s",
      cdm$under_min_df %>%
        dplyr::count() %>%
        dplyr::pull(.data$n) %>%
        as.integer()
  ))

  # join episodes with new values to main table
  cdm$final_df <- cdm$final_df %>%
    dplyr::union_all(cdm$under_min_df) %>%
    dplyr::compute()

  ###### remove any outcomes where the difference between max_gest_date in days is negative ######
  # filter to episodes with max_gest_date is after the outcome visit_date with buffer
  cdm$neg_days_df <- cdm$final_df %>%
    dplyr::filter(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$days_diff < -buffer_days) %>%
    dplyr::mutate(
      removed_category = .data$category,
      category = "PREG",
      visit_date = .data$max_gest_date,
      removed_outcome = 1
    ) %>%
    dplyr::compute()

  log4r::info(logger, sprintf(
    "Total number of episodes with negative number of days between outcome and max_gest_date: %s",
    cdm$neg_days_df %>%
      dplyr::tally() %>%
      dplyr::pull(.data$n) %>%
      as.integer()
  ))

  # filter out these episodes from main table
  cdm$clean_episodes_df <- cdm$final_df %>%
    dplyr::filter(!(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$days_diff < -buffer_days)) %>%
    # join episodes with new values to main table
    dplyr::union_all(cdm$neg_days_df) %>%
    ###### add columns for quality check ######
    # get new gestational age at visit date
    dplyr::mutate(
      gest_at_outcome = !!CDMConnector::datediff("max_gest_start_date", "visit_date", "day"),
      min_gest_date_diff = !!CDMConnector::datediff("min_gest_date", "min_gest_date_2", "day"),
      date_diff_max_end = !!CDMConnector::datediff("end_gest_date", "max_gest_date", "day")
    ) %>%
    # redo column for episode
    dplyr::group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    dplyr::mutate(episode = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  return(cdm)
}

remove_overlaps <- function(cdm, logger) {
  # Identify episodes that overlap and keep only the latter episode if the previous episode is PREG.
  # If the latter episode doesn't have gestational info, redefine the start date to be the
  # previous episode end date plus the retry period.
  cdm$df <- cdm$clean_episodes_df

  cdm$df <- cdm$df %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(visit_date) %>%
    # get previous date
    dplyr::mutate(
      prev_date = dplyr::lag(.data$visit_date),
      # get previous category
      prev_category = dplyr::lag(.data$category),
      # get previous retry period
      prev_retry = dplyr::lag(.data$retry),
      # get previous gest_id
      prev_gest_id = dplyr::lag(.data$gest_id),
      # get difference in days between start date and previous visit date
      # us gestation-based date if available
    ) %>%
    dplyr::mutate(
      prev_date_diff = ifelse(
        !is.na(.data$max_gest_start_date),
        !!CDMConnector::datediff("prev_date", "max_gest_start_date", "day"),
        !!CDMConnector::datediff("prev_date", "max_start_date", "day")
      )
    ) %>%
    dplyr::mutate(
      # if the difference in days is negative, indicate overlap of episodes
      has_overlap = ifelse(.data$prev_date_diff < 0, 1, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # overlapped episodes
  cdm$overlap_df <- cdm$df %>%
    dplyr::filter(.data$has_overlap == 1 & .data$prev_category == "PREG") %>%
    dplyr::compute(name = "overlap_df")

  # get list of gest_ids to remove
  gest_id_list <- cdm$overlap_df %>%
    dplyr::distinct(.data$prev_gest_id) %>%
    dplyr::pull()

  gest_id_list <- if (length(gest_id_list) == 0) {
    ""
  }

  # remove episodes that overlap
  cdm$final_df <- cdm$df %>%
    dplyr::filter(!(.data$gest_id %in% gest_id_list & .data$category == "PREG")) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$visit_date) %>%
    # recalculate
    # get previous date
    dplyr::mutate(
      prev_date = dplyr::lag(.data$visit_date),
      # get previous category
      prev_category = dplyr::lag(.data$category),
      # get previous retry period
      prev_retry = dplyr::lag(.data$retry),
      # get previous gest_id
      prev_gest_id = dplyr::lag(.data$gest_id),
      # get difference in days between start date and previous visit date
      # us gestation-based date if available
    ) %>%
    # dplyr::compute(name = "final_df") %>%
    dplyr::mutate(
      prev_date_diff_gest_tmp = !!CDMConnector::datediff("prev_date", "max_gest_start_date", "day"),
      prev_date_diff_start_tmp = !!CDMConnector::datediff("prev_date", "max_start_date", "day")
    ) %>%
    dplyr::mutate(
      prev_date_diff = dplyr::case_when(
        !is.na(.data$max_gest_start_date) ~ .data$prev_date_diff_gest_tmp,
        .default = .data$prev_date_diff_start_tmp
      )
    ) %>%
    # dplyr::compute(name = "final_df") %>%
    dplyr::select(
      -"prev_date_diff_gest_tmp",
      -"prev_date_diff_start_tmp"
    ) %>%
    dplyr::mutate(
      # if the difference in days is negative, indicate overlap of episodes
      has_overlap = dplyr::case_when(
        .data$prev_date_diff < 0 ~ 1,
        .default = 0
      )
    ) %>%
    # dplyr::compute(name = "final_df") %>%
    dplyr::mutate(
      # get estimated start date
      estimated_start_date = dplyr::case_when(
        # if there's an overlap and a retry period from the earlier episodes
        # and the last episode was not preg (or else would be in gest_id_list)
        # start date = last visit date + retry period
        .data$has_overlap == 1 & !is.na(.data$prev_retry) ~ !!CDMConnector::dateadd(date = "prev_date", "prev_retry"),
        is.na(.data$max_gest_start_date) ~ .data$max_start_date,
        TRUE ~ .data$max_gest_start_date
      )
    ) %>%
    # dplyr::compute(name = "final_df") %>%
    dplyr::mutate(
      # get estimated gestational age in days at outcome_visit_date using estimated_start_date
      gest_at_outcome = !!CDMConnector::datediff("estimated_start_date", "visit_date", "day")
    ) %>%
    # dplyr::compute(name = "final_df") %>%
    dplyr::mutate(
      # add column to check if gest_at_outcome is less than or equal to max_term, 1 indicates yes
      is_under_max = ifelse(.data$gest_at_outcome <= .data$max_term, 1, 0),
      # add column to check if gest_at_outcome is greater than or equal to min_term, 1 indicates yes
      is_over_min = ifelse(.data$gest_at_outcome >= .data$min_term, 1, 0)
    ) %>%
    # dplyr::compute(name = "final_df") %>%
    # redo column for episode
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$visit_date) %>%
    dplyr::mutate(
      episode = dplyr::row_number(),
      # check that there are no more overlapping episodes
      prev_date = dplyr::lag(.data$visit_date),
      preg_gest_id = dplyr::lag(.data$gest_id)
    ) %>%
    # dplyr::compute(name = "final_df") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      prev_date_diff = dplyr::case_when(
        !is.na(.data$estimated_start_date) ~ !!CDMConnector::datediff("prev_date", "estimated_start_date", "day")
      ),
      # checked, there are no remaining
      has_overlap = ifelse(.data$prev_date_diff < 0, 1, 0)
    ) %>%
    dplyr::compute(name = "final_df")

  # still_overlaps <- final_df %>%
  #     filter(has_overlap == 1)

  # if there are any remaining episodes with gestational age in weeks at
  # outcome date not within the term durations, reclassify as PREG
  cdm$temp_df <- cdm$final_df %>%
    dplyr::filter(!is.na(.data$max_gest_week) & !is.na(.data$concept_name) & .data$is_over_min == 0) %>%
    dplyr::mutate(
      removed_category = .data$category,
      category = "PREG",
      visit_date = .data$max_gest_date,
      removed_outcome = 1
    ) %>%
    dplyr::compute()

  cdm$remove_overlaps_df <- cdm$final_df %>%
    dplyr::filter(!(!is.na(.data$max_gest_week) & !is.na(.data$concept_name) & .data$is_over_min == 0)) %>%
    dplyr::union_all(cdm$temp_df) %>%
    dplyr::compute()

  nRemovedOutcomes <- cdm$remove_overlaps_df %>%
    dplyr::filter(removed_outcome == 1) %>%
    dplyr::tally() %>%
    dplyr::pull(.data$n)

  log4r::info(logger, sprintf("Total number of episodes with removed outcome: %s", nRemovedOutcomes))

  return(cdm)
}

final_episodes <- function(cdm) {
  # Keep subset of columns with episode start and end as well as category.
  # select columns and drop duplicates
  cdm$final_episodes_df <- cdm$remove_overlaps_df %>%
    dplyr::distinct(
      .data$person_id,
      .data$category,
      .data$visit_date,
      .data$estimated_start_date,
      .data$episode
    ) %>%
    dplyr::compute()

  return(cdm)
}

final_episodes_with_length <- function(cdm) {
  # Find the first gestation record within an episode and calculate the episode
  # length based on the date of the first gestation record and the visit date.
  cdm$df <- cdm$final_episodes_df %>%
    dplyr::compute()

  # select columns and rename column
  cdm$gest_df <- cdm$gestation_visits_df %>%
    dplyr::select("person_id", "gest_value", "visit_date") %>%
    dplyr::rename(gest_date = .data$visit_date) %>%
    dplyr::compute()

  cdm$merged <- cdm$gest_df %>%
    dplyr::right_join(
      cdm$df,
      by = dplyr::join_by(
        person_id,
        between(gest_date, estimated_start_date, visit_date)
      )
    ) %>%
    dplyr::group_by(.data$person_id, .data$episode) %>%
    dplyr::slice_min(.data$gest_date, n = 1) %>%
    # keep max gest_value if two or more gestation records share same date
    dplyr::ungroup() %>%
    dplyr::compute() %>%
    dplyr::group_by(.data$person_id, .data$episode) %>%
    dplyr::slice_max(.data$gest_value, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    # flag episodes with gestational info
    dplyr::mutate(gest_flag = ifelse(is.na(.data$gest_date), NA, "yes")) %>%
    dplyr::compute()

  # get episode length if there is a gestation record date, otherwise impute 1
  cdm$final_df <- cdm$merged %>%
    dplyr::mutate(
      episode_length = dplyr::if_else(
        !is.na(gest_date),
        !!CDMConnector::datediff("gest_date", "visit_date", "day"),
        1
    )) %>%
    dplyr::compute()

  # if an episode length is 0, change to 1
  cdm$hip_episodes <- cdm$final_df %>%
    dplyr::mutate(
      episode_length = dplyr::if_else(.data$episode_length == 0, 1, .data$episode_length)) %>%
    dplyr::select(-"gest_value") %>%
    dplyr::distinct() %>%
    dplyr::compute(name = "hip_episodes", temporary = FALSE, overwrite = TRUE)
  return(cdm)
}
