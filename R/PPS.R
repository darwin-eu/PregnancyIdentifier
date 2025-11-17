#' runPps
#'
#' Runs PPS algorithm
#'
#' @param cdm (`cdm_reference`)
#' @param outputDir output directory
#' @param uploadConceptSets if concept sets should be uploaded
#' @param ... optional parameters
#'
#' @return cdm object
#' @export
runPps <- function(cdm, outputDir, uploadConceptSets = FALSE, logger, ...) {
  dir.create(path = outputDir, recursive = TRUE, showWarnings = FALSE)

  log4r::info(logger, "START Running PPS")
  if (uploadConceptSets) {
    log4r::info(logger, "Uploading Concepts")
    cdm <- uploadConceptSets(cdm, logger)
  }
  log4r::info(logger, "Pull PPS Concepts from Tables")
  # pull PPS concepts from each table
  cdm <- input_GT_concepts(cdm, logger)

  # get the gestational timing information for each concept
  log4r::info(logger, "Get gestational timing information")
  get_PPS_episodes_df <- get_PPS_episodes(cdm, outputDir)
  saveRDS(get_PPS_episodes_df, file.path(outputDir, "PPS_gest_timing_episodes.rds"))

  # get the min and max dates for each episode
  log4r::info(logger, "Get min and max dates for episodes")
  PPS_episodes_df <- get_episode_max_min_dates(get_PPS_episodes_df)
  saveRDS(PPS_episodes_df, file.path(outputDir, "PPS_min_max_episodes.rds"))
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

# From: https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/algorithm/PPS_algorithm_functions.R

rename_cols <- function(cdm, tblName, outcomeTblName, start_date_col, id_col, logger) {
  log4r::info(logger, sprintf("Pulling data from %s", tblName))
  cdm[[outcomeTblName]] <- cdm[[tblName]] %>%
    dplyr::rename(
      domain_concept_start_date = start_date_col,
      domain_concept_id = id_col
    ) %>%
    dplyr::inner_join(cdm$pps_concepts, by = "domain_concept_id") %>%
    dplyr::select(person_id, domain_concept_start_date, domain_concept_id) %>%
    dplyr::distinct() %>%
    dplyr::compute()
  return(cdm)
}

input_GT_concepts <- function(cdm, logger) {
  cdm <- rename_cols(
    cdm = cdm,
    tblName  = "condition_occurrence",
    outcomeTblName = "c_o",
    start_date_col = "condition_start_date",
    id_col = "condition_concept_id",
    logger = logger
  )
  cdm <- rename_cols(
    cdm = cdm,
    tblName  = "procedure_occurrence",
    outcomeTblName = "p_o",
    start_date_col = "procedure_date",
    id_col = "procedure_concept_id",
    logger = logger
  )
  cdm <- rename_cols(
    cdm = cdm,
    tblName = "observation",
    outcomeTblName = "o_df",
    start_date_col = "observation_date",
    id_col = "observation_concept_id",
    logger = logger
  )
  cdm <- rename_cols(
    cdm = cdm,
    tblName = "measurement",
    outcomeTblName = "m_df",
    start_date_col = "measurement_date",
    id_col = "measurement_concept_id",
    logger = logger
  )
  cdm <- rename_cols(
    cdm = cdm,
    tblName  = "visit_occurrence",
    outcomeTblName = "v_o",
    start_date_col = "visit_start_date",
    id_col = "visit_concept_id",
    logger = logger
  )

  cdm$input_gt_concepts_df <- list(cdm$c_o, cdm$p_o, cdm$o_df, cdm$m_df, cdm$v_o) %>%
    purrr::reduce(dplyr::union_all) %>%
    dplyr::compute()

  return(cdm)
}

records_comparison <- function(personlist, i) {
  # for the below: t = time (actual), c = concept (expected)
  # first do the comparisons to the records PREVIOUS to record i

  # Iterate through the previous records
  for (j in 1:(i - 1)) {
    # Obtain the difference in actual dates of the consecutive patient records
    delta_t <- as.numeric(difftime(personlist$domain_concept_start_date[i], personlist$domain_concept_start_date[i - j], units = "days") / 30)
    # Obtain the max expected month difference based on clinician knowledge of the two concepts (allow two extra months for leniency)
    adjConceptMonths_MaxExpectedDelta <- personlist$max_month[i] - personlist$min_month[i - j] + 2
    # Obtain the min expected month difference based on clinician knowledge of the two concepts (allow two extra months for leniency)
    adjConceptMonths_MinExpectedDelta <- personlist$min_month[i] - personlist$max_month[i - j] - 2
    # Save a boolean indicating whether the actual date difference falls within the max and min expected date differences for the consecutive concepts
    agreement_t_c <- (adjConceptMonths_MaxExpectedDelta >= delta_t) & (delta_t >= adjConceptMonths_MinExpectedDelta)

    # If there is agreement between the concepts, update the return_agreement_t_c variable
    if (agreement_t_c == TRUE) {
      return(TRUE) # return early -- only needs to be true once
    }
  }
  # Next, do the comparisons to the records SURROUNDING record i
  len_to_start <- i - 1
  len_to_end <- nrow(personlist) - i
  bridge_len <- min(len_to_start, len_to_end)
  if (bridge_len == 0) {
    return(FALSE)
  } # no records surrounding

  # Iterate through the bridge records around record i, in case record i was an outlier
  for (s in seq_len(len_to_start)) {
    for (e in seq_len(len_to_end)) {
      # Obtain the time difference in months between the bridge records
      bridge_delta_t <- as.numeric(difftime(personlist$domain_concept_start_date[i + e],
        personlist$domain_concept_start_date[i - s],
        units = "days"
      ) / 30)
      # Obtain the max and min expected month differences based on clinician knowledge of the bridge concepts (allow two extra months for leniency)
      bridge_adjConceptMonths_MaxExpectedDelta <- personlist$max_month[i + e] - personlist$min_month[i - s] + 2
      bridge_adjConceptMonths_MinExpectedDelta <- personlist$min_month[i + e] - personlist$max_month[i - s] - 2
      # Check if there is agreement between the bridge concepts
      bridge_agreement_t_c <- (bridge_adjConceptMonths_MaxExpectedDelta >= bridge_delta_t) &
        (bridge_delta_t >= bridge_adjConceptMonths_MinExpectedDelta)
      # If there is agreement between the bridge concepts, update the return_agreement_t_c variable
      if (bridge_agreement_t_c == TRUE) {
        return(TRUE) # return early -- only needs to be true once
      }
    }
  }

  # Return the final agreement status between the concepts
  return(FALSE)
}

assign_episodes <- function(personlist, ...) {
  if (nrow(personlist) == 1) {
    personlist$person_episode_number <- 1
    return(personlist)
  }

  # Filter to plausible pregnancy timelines and concept month sequences, and number by episode to get person_episode_number

  # Initialize variables for episode numbering and storing episode information
  # # Treat the first record as belonging to the first episode
  person_episode_number <- 1
  person_episodes <- 1
  person_episode_chr <- "1"
  person_episode_dates <- list()

  # Add the date of the current record to the corresponding episode in person_episode_dates
  person_episode_dates[[person_episode_chr]] <- c(personlist$domain_concept_start_date[1])

  for (i in 2:nrow(personlist)) {
    # Calculate the time difference in months between the current record and the previous record
    delta_t <- as.numeric(difftime(personlist$domain_concept_start_date[i],
      personlist$domain_concept_start_date[i - 1],
      units = "days"
    ) / 30)

    # Perform the checks to determine whether this is a continuation of an episode or the start of a new episode
    agreement_t_c <- records_comparison(personlist, i)

    # If there is no agreement between the concepts and the time difference is greater than 2 months,
    # change to 1 month, ie retry period
    # increment the person_episode_number to indicate a new episode
    if ((!agreement_t_c) && (delta_t > 1)) {
      person_episode_number <- person_episode_number + 1
    } else if (delta_t > 10) {
      # If the time difference is greater than 10 months, increment the person_episode_number to indicate a new episode
      person_episode_number <- person_episode_number + 1
    }

    # Append the person_episode_number to the person_episodes list
    person_episodes <- c(person_episodes, person_episode_number)

    person_episode_chr <- as.character(person_episode_number)

    # Check if the person_episode_number is already in the person_episode_dates list
    if (!(person_episode_chr %in% names(person_episode_dates))) {
      person_episode_dates[[person_episode_chr]] <- personlist$domain_concept_start_date[i]
    } else {
      # Add the date of the current record to the corresponding episode in person_episode_dates
      person_episode_dates[[person_episode_chr]] <- c(
        person_episode_dates[[person_episode_chr]],
        personlist$domain_concept_start_date[i]
      )
    }
  }

  # - Check that all the episodes are < 12 mo in length (the 9-10 mo of pregnancy plus the few months of delivery concept ramblings).
  # In the case that any have to be removed, loop through the episodes of the patient again and renumber the remaining episodes
  episodes_to_remove <- c()
  for (episode in names(person_episode_dates)) {
    len_of_episode <- as.numeric(difftime(person_episode_dates[[episode]][length(person_episode_dates[[episode]])],
      person_episode_dates[[episode]][1],
      units = "days"
    ) / 30)
    if (len_of_episode > 12) {
      episodes_to_remove <- c(episodes_to_remove, episode)
    }
  }
  new_person_episodes <- ifelse(as.character(person_episodes) %in% episodes_to_remove, 0, person_episodes)
  numUniqueNonZero <- sum(unique(new_person_episodes) != 0)
  nonZeroNewList <- 1:numUniqueNonZero
  nonZeroOrigList <- unique(new_person_episodes)[unique(new_person_episodes) != 0]
  new_person_episodes <- nonZeroNewList[match(new_person_episodes, nonZeroOrigList)]
  personlist$person_episode_number <- new_person_episodes

  return(personlist)
}

get_PPS_episodes <- function(cdm, outputDir) {
  cdm$patients_with_preg_concepts <- cdm$input_gt_concepts_df %>%
    dplyr::filter(!is.na(.data$domain_concept_start_date)) %>%
    dplyr::left_join(cdm$pps_concepts, by = "domain_concept_id") %>%
    dplyr::compute(name = "patients_with_preg_concepts")

  cdm$patients_with_preg_concepts %>%
    dplyr::group_by(.data$domain_concept_id, .data$domain_concept_name) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    write.csv(file.path(outputDir, "PPS-concept_counts.csv"), row.names = FALSE)

  cdm$patients_with_preg_concepts <- cdm$patients_with_preg_concepts %>%
    dplyr::inner_join(
      cdm$person %>%
        dplyr::select("person_id", "gender_concept_id", "year_of_birth", "day_of_birth", "month_of_birth"),
      by = "person_id"
    ) %>%
    dplyr::mutate(
      day_of_birth = dplyr::if_else(is.na(.data$day_of_birth), 1, .data$day_of_birth),
      month_of_birth = dplyr::if_else(is.na(.data$month_of_birth), 1, .data$month_of_birth),
      date_of_birth = as.Date(paste0(as.character(as.integer(.data$year_of_birth)), "-", as.character(as.integer(.data$month_of_birth)), "-", as.character(as.integer(.data$day_of_birth))))
    ) %>%
    dplyr::mutate(
      date_diff = !!CDMConnector::datediff("date_of_birth", "domain_concept_start_date", "day"),
      age = date_diff / 365
    ) %>%
    # women of reproductive age
    dplyr::filter(
      .data$gender_concept_id == 8532,
      #.data$sex_at_birth_concept_id != 45880669,
      .data$age >= 15,
      .data$age < 56
    ) %>%
    dplyr::select(
      -dplyr::ends_with("_of_birth"),
      -"date_diff",
      -"gender_concept_id"
    )

  # OBTAIN ALL RELEVANT INPUT PATIENTS AND SAVE GT INFORMATION PER CONCEPT TO A LOOKUP DICTIONARY
  # First we save the women that have gestational timing concepts, and save the gestational timing information for each concept.
  # We add the concepts and their gestational timing months ([min,max]) during pregnancy to a dictionary (hash) in
  # concept key: month value list format e.g. {2211756: [4,8], 2101830: [2,2]...}


  # SAVE EXPECTED GESTATIONAL TIMING MONTH INFORMATION FOR EACH OF THE PATIENT RECORDS
  # Looping over each person with pregnancy concepts, order their concepts by date of each record, and for each concept ID in order, loop through the
  # keys of the dictionary and compare to the concept ID, if there’s a match, save the month value(s) to a list for the record date. You’ll end up with
  # record date: list of matching months, save this to a new dictionary with record dates as the keys. Where no match occurs, put NA
  #   person_dates_dict <- split(person_dates_df$list_col, person_dates_df$person_id)

  person_dates_df <- cdm$patients_with_preg_concepts %>%
    dplyr::collect(page_size = 50000) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$domain_concept_start_date)

  res <- person_dates_df
  if (nrow(res) > 0) {
    res <- res %>%
      dplyr::group_modify(assign_episodes)
  }

  return(res)
}

get_episode_max_min_dates <- function(get_PPS_episodes_df) {
  if (!"person_episode_number" %in% names(get_PPS_episodes_df)) {
    if (nrow(get_PPS_episodes_df) > 0) {
      get_PPS_episodes_df <- get_PPS_episodes_df %>%
        dplyr::mutate(
          person_episode_number = .data$person_id
        )
    } else {
      get_PPS_episodes_df <- get_PPS_episodes_df %>%
        dplyr::mutate(
          person_episode_number = integer(0)
        )
    }
  }

  df <- get_PPS_episodes_df %>%
    dplyr::filter(!is.na(.data$person_episode_number)) %>%
    dplyr::group_by(.data$person_id, .data$person_episode_number) %>%
    dplyr::summarise(
      # first time pregnancy concept appears
      episode_min_date = min(.data$domain_concept_start_date, na.rm = TRUE),
      # last time a pregnancy concept appears
      episode_max_date = max(.data$domain_concept_start_date, na.rm = TRUE),
      episode_max_date_plus_two_months = lubridate::`%m+%`(.data$episode_max_date, months(2)),
      # add the number of unique gestational timing concepts per episode
      n_gt_concepts = dplyr::n_distinct(domain_concept_id)
    ) %>%
    dplyr::ungroup()

  return(df)
}
