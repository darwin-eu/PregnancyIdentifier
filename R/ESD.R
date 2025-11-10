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

# From: https://github.com/louisahsmith/allofus-pregnancy/blob/main/code/algorithm/ESD_algorithm_functions.R

#' runEsd
#'
#' Runs ESD
#'
#' @param HIPPS (`data.frame`)
#' @param cdm (`cdm_reference`)
#' @param outputDir (`character(1)`)
#' @param uploadConceptSets if concept sets should be uploaded
#' @param ... Extra (development) parameters
#'
#' @return `NULL`
#' @export
runEsd <- function(HIPPS, cdm, outputDir, uploadConceptSets = FALSE, ...) {
  dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
  message("  * Running ESD")

  if (uploadConceptSets) {
    cdm <- uploadConceptSets(cdm)
  }

  # get timing concepts
  get_timing_concepts_df <- get_timing_concepts(
    cdm = cdm,
    HIPPS
  )

  # get gestational timing info
  episodes_with_gestational_timing_info_df <- episodes_with_gestational_timing_info(get_timing_concepts_df)
  saveRDS(episodes_with_gestational_timing_info_df, file.path(outputDir, "ESD.rds"))
}

get_preg_related_concepts <- function(df, person_id_list, df_date_col) {
  df %>%
    dplyr::select("person_id", all_of(df_date_col), "concept_id", "concept_name", "value_col") %>%
    dplyr::rename(all_of(c(domain_concept_start_date = df_date_col))) %>%
    dplyr::inner_join(person_id_list, by = dplyr::join_by(
      person_id, domain_concept_start_date >= start_date,
      domain_concept_start_date <= recorded_episode_end
    ), copy = TRUE) %>%
    dplyr::select(
      "person_id",
      "domain_concept_start_date",
      domain_concept_id = "concept_id",
      domain_concept_name = "concept_name",
      "start_date",
      "recorded_episode_end",
      "value_col",
      "episode_number"
    )
}

get_timing_concepts <- function(cdm, final_merged_episode_detailed_df) {
  # obtain the gestational timing <= 3 month concept information to use as additional information for precision category designation

  pregnant_dates <- final_merged_episode_detailed_df

  algo2_timing_concepts_id_list <- cdm$pps_concepts %>%
    dplyr::select("domain_concept_id") %>%
    dplyr::pull(.data$domain_concept_id) %>%
    as.integer()

  observation_concept_list <- c(3011536, 3026070, 3024261, 4260747, 40758410, 3002549, 43054890, 46234792, 4266763, 40485048, 3048230, 3002209, 3012266)
  measurement_concept_list <- c(3036844, 3048230, 3001105, 3002209, 3050433, 3012266)

  est_date_of_delivery_concepts <- c(1175623, 1175623, 3001105, 3011536, 3024261, 3024973, 3026070, 3036322, 3038318, 3038608, 4059478, 4128833, 40490322, 40760182, 40760183, 42537958)
  est_date_of_conception_concepts <- c(3002314, 3043737, 4058439, 4072438, 4089559, 44817092)
  len_of_gestation_at_birth_concepts <- c(4260747, 43054890, 46234792, 4266763, 40485048)

  # need to find concept names that contain 'gestation period' as well as the specific concepts
  concepts_to_search <- cdm$concept %>%
    dplyr::filter(
      .data$concept_name %like% "gestation period"
      | concept_id %in% c(
        observation_concept_list, measurement_concept_list, algo2_timing_concepts_id_list,
        est_date_of_delivery_concepts, est_date_of_conception_concepts,
        len_of_gestation_at_birth_concepts
      )
    ) %>%
    dplyr::select("concept_id", "concept_name")

  # add: change to pregnancy start rather than recorded episode start
  person_id_list <- pregnant_dates %>%
    dplyr::mutate(
      start_date = pmin(.data$pregnancy_start, .data$recorded_episode_start, na.rm = TRUE)
    ) %>%
    dplyr::select("person_id", "start_date", "recorded_episode_end", "episode_number")

  c_o <- concepts_to_search %>%
    dplyr::inner_join(cdm$condition_occurrence, by = c("concept_id" = "condition_concept_id")) %>%
    dplyr::mutate(
      value_col = .data$concept_name
    ) %>%
    get_preg_related_concepts(person_id_list, "condition_start_date")

  o_df <- concepts_to_search %>%
    dplyr::inner_join(cdm$observation, by = c("concept_id" = "observation_concept_id")) %>%
    dplyr::mutate(
      value_col = .data$value_as_string
    ) %>%
    get_preg_related_concepts(person_id_list, "observation_date")

  m_df <- concepts_to_search %>%
    dplyr::inner_join(cdm$measurement, by = c("concept_id" = "measurement_concept_id")) %>%
    dplyr::mutate(
      value_col = .data$value_as_number
    ) %>%
    get_preg_related_concepts(person_id_list, "measurement_date")

  p_df <- concepts_to_search %>%
    dplyr::inner_join(cdm$procedure_occurrence, by = c("concept_id" = "procedure_concept_id")) %>%
    dplyr::mutate(
      value_col = .data$concept_name
    ) %>%
    get_preg_related_concepts(person_id_list, "procedure_date")

  preg_related_concepts <- list(
    c_o,
    o_df,
    m_df %>%
      dplyr::mutate(value_col = as.character(.data$value_col)),
    p_df
  ) %>%
    purrr::reduce(dplyr::union_all)

  algo2_timing_concepts_df <- cdm$pps_concepts %>%
    dplyr::select("domain_concept_id", "min_month", "max_month")

  preg_related_concepts_local <- preg_related_concepts %>%
    dplyr::left_join(algo2_timing_concepts_df, by = "domain_concept_id") %>%
    dplyr::collect(page_size = 20000) %>%
    dplyr::mutate(domain_value = stringr::str_replace(.data$value_col, "\\|text_result_val:", "")) %>%
    dplyr::mutate(domain_value = stringr::str_replace(.data$domain_value, "\\|mapped_text_result_val:", "")) %>%
    dplyr::mutate(domain_value = stringr::str_replace(.data$domain_value, "Gestation period, ", "")) %>%
    dplyr::mutate(domain_value = stringr::str_replace(.data$domain_value, "gestation period, ", "")) %>%
    dplyr::mutate(domain_value = stringr::str_replace(.data$domain_value, " weeks", "")) %>%
    dplyr::mutate(domain_value = as.integer(as.numeric(.data$domain_value))) %>%
    dplyr::mutate(
      keep_value = dplyr::if_else(
        (stringr::str_detect(tolower(domain_concept_name), "gestation period,"))
        | (stringr::str_detect(tolower(domain_concept_name), "gestational age"))
        | (domain_concept_id %in% c(3048230, 3002209, 3012266) & domain_value < 44 & domain_value > 0),
        1,
        0
      ),
      extrapolated_preg_start = dplyr::if_else(
        keep_value == 1,
        domain_concept_start_date - (domain_value * 7),
        lubridate::NA_Date_
      )
    )

  return(preg_related_concepts_local)
}


validate <- function(date_text) {
  tryCatch(
    {
      as.Date(date_text, "%Y-%m-%d")
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}

findIntersection <- function(intervals) {
  if (length(intervals) == 1) {
    intervals <- matrix(intervals[[1]], ncol = 2)
  } else {
    # Sort intervals
    intervals <- purrr::reduce(intervals, rbind)
  }
  intervals <- as.data.frame(intervals) %>%
    dplyr::mutate(across(everything(), as.Date)) %>%
    dplyr::arrange(V1)

  # First remove outlier ranges via the IQR*1.5 approach. Outlier ranges are determined by the number of overlaps each range has with other ranges.
  overlapCountDict <- rep(0, nrow(intervals))
  for (j in 1:nrow(intervals)) {
    for (m in 1:nrow(intervals)) {
      if (j != m) {
        # First interval
        last <- intervals[j, 2]
        first <- intervals[j, 1]
        # 1st condition - does first date equal either the first or last date
        if ((intervals[m, 1] == last) || (intervals[m, 1] == first)) {
          overlapCountDict[j] <- overlapCountDict[j] + 1
          next
          # 2nd condition - does second date equal either the first or last date
        } else if ((intervals[m, 2] == last) || (intervals[m, 2] == first)) {
          overlapCountDict[j] <- overlapCountDict[j] + 1
          next
          # 3rd condition - does second date fall between the first and last date
        } else if ((intervals[m, 2] < last) && (intervals[m, 2] > first)) {
          overlapCountDict[j] <- overlapCountDict[j] + 1
          next
          # 4th condition - does first date fall between the first and last date
        } else if ((intervals[m, 1] < last) && (intervals[m, 1] > first)) {
          overlapCountDict[j] <- overlapCountDict[j] + 1
          next
        }
      }
    }
  }

  intervals$overlapCountDict <- overlapCountDict
  # if there were no overlaps with a given date, overlapCountDict is 0
  # but the other ones will be higher
  allCounts <- overlapCountDict
  # these are both going to be 1 if all overlap
  countsQ1 <- quantile(allCounts, 0.25)
  countsQ3 <- quantile(allCounts, 0.75)
  # and outlier metric will be 0
  outlierMetric <- (countsQ3 - countsQ1) * 1.5
  outlierThreshold <- abs(countsQ1 - outlierMetric)
  if (outlierThreshold == 0) {
    filteredIntervals <- dplyr::filter(intervals, overlapCountDict > outlierThreshold)
  } else {
    filteredIntervals <- dplyr::filter(intervals, overlapCountDict >= outlierThreshold)
  }
  filteredIntervals <- arrange(filteredIntervals, desc(overlapCountDict))

  # Now the outliers are removed, proceed with obtaining the overlaps
  N <- nrow(filteredIntervals)

  # If only one interval
  if (N == 1) {
    # First interval
    last <- filteredIntervals[1, 2] # last day of overlapping ranges
    min_start <- filteredIntervals[1, 2] # minimum day of intersection of ranges
    first <- filteredIntervals[1, 1] # first day of overlapping ranges
    max_start <- filteredIntervals[1, 1] # maximum day of intersection of ranges
    intersection_list <- c(last, first, min_start, max_start)
    # If no intervals, take the first only if more than one
  } else if (N == 0) {
    # First interval
    intervalsToSort <- intervals[order(overlapCountDict, decreasing = TRUE), , drop = FALSE]
    last <- intervalsToSort[1, 2]
    min_start <- intervalsToSort[1, 2]
    first <- intervalsToSort[1, 1]
    max_start <- intervalsToSort[1, 1]
    intersection_list <- c(last, first, min_start, max_start)
  } else {
    # First interval
    last <- filteredIntervals[1, 2] # last day of overlapping ranges
    min_start <- filteredIntervals[1, 2] # minimum (last) day of intersection of ranges
    first <- filteredIntervals[1, 1] # first day of overlapping ranges
    max_start <- filteredIntervals[1, 1] # maximum (first) day of intersection of ranges

    # Check rest of the intervals and find the intersection
    for (i in 2:N) {
      if (filteredIntervals[i, 1] < first) {
        first <- filteredIntervals[i, 1]
      }
      if (filteredIntervals[i, 2] > last) {
        last <- filteredIntervals[i, 2]
      }
      if ((filteredIntervals[i, 2] < min_start) && (filteredIntervals[i, 2] > max_start)) {
        min_start <- filteredIntervals[i, 2]
      }
      if ((filteredIntervals[i, 1] > max_start) && (filteredIntervals[i, 1] < min_start)) {
        max_start <- filteredIntervals[i, 1]
      }
    }

    intersection_list <- c(last, first, min_start, max_start)
  }

  return(intersection_list)
}

# check for GW concept overlap to the intervals
remove_GW_outliers <- function(lol_of_GW_concepts) {
  list_of_GW_concepts <- as.Date(unlist(lol_of_GW_concepts))
  median_startdate <- sort(list_of_GW_concepts)[ceiling(length(list_of_GW_concepts) / 2)]
  allDistances <- rep(0, length(list_of_GW_concepts))
  for (j in seq_along(list_of_GW_concepts)) {
    allDistances[j] <- as.numeric(max(list_of_GW_concepts[j], median_startdate) - min(list_of_GW_concepts[j], median_startdate))
  }
  distQ1 <- quantile(allDistances, 0.25)
  distQ3 <- quantile(allDistances, 0.75)
  outlierMetric <- (distQ3 - distQ1) * 1.5
  outlierLowerThreshold <- distQ1 - outlierMetric
  outlierUpperThreshold <- distQ3 + outlierMetric
  filteredDates <- list_of_GW_concepts[allDistances >= outlierLowerThreshold & allDistances <= outlierUpperThreshold]
  return(filteredDates)
}

# definition to get accuracy category
assign_precision_category <- function(precision_days) {
  dplyr::case_when(
    precision_days == -1 ~ "week_poor-support",
    precision_days >= 0 & precision_days <= 7 ~ "week",
    precision_days > 7 & precision_days <= 14 ~ "two-week",
    precision_days > 14 & precision_days <= 21 ~ "three-week",
    precision_days > 21 & precision_days <= 28 ~ "month",
    precision_days > 28 & precision_days <= 56 ~ "two-month",
    precision_days > 56 & precision_days <= 84 ~ "three-month",
    TRUE ~ "non-specific"
  )
}

# applying udf to the date array column to obtain the new column 'final_timing_info' (a list of [inferred_episode_start, precision_days, precision_category]) for each row
get_gt_timing <- function(dateslist) {
  # Iterate over the dateslist
  timing_arr <- purrr::map(dateslist[purrr::map_lgl(dateslist, validate)], sort)
  GW_list <- dateslist[sapply(dateslist, function(x) length(x) == 1)]
  GR3m_list <- dateslist[sapply(dateslist, function(x) length(x) == 2)]

  inferred_start_date <- as.Date("2000-01-01")
  precision_days <- 999
  precision_category <- "-999"
  intervalsCount <- 0
  majorityOverlapCount <- 0

  # get length of list with GR3m ranges
  N <- length(GR3m_list)

  if (N > 0) {
    common_GR3m_interval <- findIntersection(GR3m_list)
    # [datetime.date(2014, 11, 2), datetime.date(2014, 7, 3), datetime.date(2014, 10, 2), datetime.date(2014, 8, 17)]
  } else {
    common_GR3m_interval <- NULL
  }

  plausibleDays <- 0
  maxRangeDays <- 0
  range_s <- NULL
  range_e <- NULL
  interval_s <- NULL
  interval_e <- NULL
  daterangeMidpoint <- NULL

  if (!is.null(common_GR3m_interval)) { # if it's not an empty array
    range_e <- as.Date(common_GR3m_interval[1]) # end date of range
    range_s <- as.Date(common_GR3m_interval[2]) # start date of range
    interval_e <- as.Date(common_GR3m_interval[3]) # end date of intersection
    interval_s <- as.Date(common_GR3m_interval[4]) # start date of intersection

    plausibleDays <- as.numeric(difftime(interval_e, interval_s, units = "days"))
    maxRangeDays <- as.numeric(difftime(range_e, range_s, units = "days"))
    daterangeMidpoint <- interval_s + lubridate::days(as.integer(plausibleDays / 2)) # get midpoint of intersection
    # utilize the overlap more when it gets narrowed down to less than a week by taking the midpoint and adding 3 days either side (otherwise unlikely to overlap much with GW concepts and thus will be ignored)
    if (plausibleDays < 7) {
      interval_s <- daterangeMidpoint - lubridate::days(3)
      interval_e <- daterangeMidpoint + lubridate::days(3)
      plausibleDays <- 6
    }
  }

  # there are week-level estimates
  if (length(GW_list) > 0) {
    if (!is.null(interval_s)) { # GR3m interval is not null
      intervalsCount <- intervalsCount + 1
      # check for overlaps with GR3m concept ranges
      gwConceptCount <- length(GW_list)
      overlapping_gwConcepts <- list()

      for (gwlist in GW_list) {
        gw_concept_start_date <- gwlist[[1]]

        if (gw_concept_start_date >= interval_s && gw_concept_start_date <= interval_e) {
          overlapping_gwConcepts <- c(overlapping_gwConcepts, gw_concept_start_date)
        }
      }

      overlapping_gwConcepts_count <- length(overlapping_gwConcepts)
      perc_overlapping <- (overlapping_gwConcepts_count / gwConceptCount) * 100

      if (perc_overlapping > 50) {
        majorityOverlapCount <- majorityOverlapCount + 1
        filtDates <- remove_GW_outliers(overlapping_gwConcepts)
        inferred_start_date <- filtDates[[1]] # latest date
        precision_days <- as.numeric(max(filtDates) - min(filtDates))
      } else {
        filtDates <- remove_GW_outliers(GW_list)
        inferred_start_date <- filtDates[[1]]
        precision_days <- as.numeric(max(filtDates) - min(filtDates))

        if (length(filtDates) == 1) {
          # there's only one gw concept and it doesn't overlap
          precision_days <- -1
        }
      }
    } else {
      filtDates <- remove_GW_outliers(GW_list)
      inferred_start_date <- filtDates[[1]]
      precision_days <- as.numeric(max(filtDates) - min(filtDates))

      if (length(filtDates) == 1) {
        precision_days <- -1
      }
    }
  } else {
    inferred_start_date <- daterangeMidpoint
    precision_days <- maxRangeDays
  }

  precision_category <- assign_precision_category(precision_days)

  single_episode_timingres <- list(
    inferred_start_date = inferred_start_date,
    precision_days = precision_days,
    precision_category = precision_category,
    intervalsCount = intervalsCount,
    majorityOverlapCount = majorityOverlapCount
  )

  return(single_episode_timingres)
}

episodes_with_gestational_timing_info <- function(get_timing_concepts_df) {
  # add on either GW or GR3m designation depending on whether the concept is present
  if (nrow(get_timing_concepts_df) > 0) {
    get_timing_concepts_df <- get_timing_concepts_df %>%
      dplyr::mutate(
        domain_concept_id = as.integer(.data$domain_concept_id),
        GT_type = dplyr::case_when(
          stringr::str_detect(stringr::str_to_lower(.data$domain_concept_name), "gestation period")
          | domain_concept_id %in% c(3048230, 3002209, 3012266, 3050433)
          ~ "GW",
          !is.na(min_month) ~ "GR3m",
          TRUE ~ NA
        )
      )

    # add on the max and min pregnancy start dates predicted by each concept
    timing_designation_df <- get_timing_concepts_df %>%
      dplyr::mutate(
        min_days_to_pregnancy_start = dplyr::if_else(.data$GT_type == "GR3m", round(.data$min_month * 30.4), NA),
        max_days_to_pregnancy_start = dplyr::if_else(.data$GT_type == "GR3m", round(.data$max_month * 30.4), NA)
      )

    # add the max and min possible pregnancy start dates according to the GR3m concepts
    timing_designation_df <- timing_designation_df %>%
      dplyr::mutate(
        min_pregnancy_start = dplyr::if_else(
          .data$GT_type == "GR3m",
          .data$domain_concept_start_date - as.integer(.data$min_days_to_pregnancy_start),
          lubridate::NA_Date_
        ),
        max_pregnancy_start = dplyr::if_else(
          .data$GT_type == "GR3m",
          .data$domain_concept_start_date - as.integer(.data$max_days_to_pregnancy_start),
          lubridate::NA_Date_
        )
      ) %>%
      dplyr::select(-"min_days_to_pregnancy_start", -"max_days_to_pregnancy_start")

    # remove type if null values
    timing_designation_df <- timing_designation_df %>%
      dplyr::mutate(GT_type = dplyr::case_when(
        .data$GT_type == "GW" & is.na(.data$domain_value) ~ NA,
        .data$GT_type == "GW" & is.na(.data$extrapolated_preg_start) ~ NA,
        TRUE ~ .data$GT_type
      ))

    # filter to rows with GW or GR3m type
    timing_designation_df <- timing_designation_df %>%
      dplyr::filter(.data$GT_type == "GW" | .data$GT_type == "GR3m")

    # get start date range for GR3m
    timing_designation_df <- timing_designation_df %>%
      dplyr::mutate(dplyr::across(c(.data$extrapolated_preg_start, .data$min_pregnancy_start, .data$max_pregnancy_start), as.character)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        preg_start_range = ifelse(is.na(.data$extrapolated_preg_start), paste(
          as.character(.data$max_pregnancy_start),
          as.character(.data$min_pregnancy_start)
        ), .data$extrapolated_preg_start),
        # get start dates for GW
        extr = .data$extrapolated_preg_start
      ) %>%
      dplyr::ungroup()

    # create list of all dates
    timing_designation_df <- timing_designation_df %>%
      dplyr::mutate(all_GT_info = ifelse(is.na(.data$extr), .data$preg_start_range, .data$extr))

    # add on a categorization column to ensure gestation week concepts are distinguished as a single entity and others are unique per concept (important for the next steps of removing duplicates)
    timing_designation_df <- timing_designation_df %>%
      # add gt-type here so othe rmeasurements not rolled up
      dplyr::mutate(
        domain_concept_name_rollup = ifelse(
          !is.na(.data$domain_value) & .data$GT_type == "GW",
          "Gestation Week",
          .data$domain_concept_name
        )
      )

    # IMPORTANT: sort by domain value (gest week concepts) from highest (latest in pregancy) to lowest (earliest in pregnancy)
    # so that later on the first element of the GW list can be taken for 'latest in pregnancy' concept
    timing_designation_df <- timing_designation_df %>%
      dplyr::arrange(.data$person_id, .data$episode_number, dplyr::desc(.data$domain_value)) %>%
      # remove all rows but the first for each concept date ~ GT_type combination (as these are likely to be historical references)
      # so on each date, the highest domain value will be chosen
      dplyr::group_by(.data$person_id, .data$episode_number, .data$domain_concept_name_rollup, .data$domain_concept_start_date, .data$GT_type) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    # group the dataset by patient and episode number and pass relevant columns to a function that adds inferred_start_date and precision
    new_timing_designation_df <- timing_designation_df %>%
      dplyr::group_by(.data$person_id, .data$episode_number) %>%
      # first is the date with the highest domain value
      dplyr::summarise(
        GT_info_list = purrr::map(list(.data$all_GT_info), ~ stringr::str_split(.x, " ")),
        GW_flag = as.numeric(any(.data$GT_type == "GW")),
        GR3m_flag = as.numeric(any(.data$GT_type == "GR3m")),
        .groups = "drop"
      )

    new_timing_designation_df <- new_timing_designation_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      final_timing_info = list(get_gt_timing(.data$GT_info_list)),
      inferred_episode_start = .data$final_timing_info[[1]],
      precision_days = .data$final_timing_info[[2]],
      precision_category = .data$final_timing_info[[3]],
      intervalsCount = .data$final_timing_info[[4]],
      majorityOverlapCount = .data$final_timing_info[[5]]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "person_id", "episode_number", "GT_info_list", "GW_flag", "GR3m_flag", "inferred_episode_start", "precision_days",
      "precision_category", "intervalsCount", "majorityOverlapCount"
    )

    # print the GW and GR3m concept overlap information to log
    majorityOverlapCountTotal <- sum(new_timing_designation_df$majorityOverlapCount, na.rm = TRUE)
    intervalsCountTotal <- sum(new_timing_designation_df$intervalsCount, na.rm = TRUE)
    percMajority <- (majorityOverlapCountTotal / intervalsCountTotal) * 100


    message(sprintf("-- Number of episodes with GR3m intervals: %s", intervalsCountTotal))
    message(sprintf("-- Percent of cases that contain a GR3m intersection that ALSO have majority GW overlap:: %s", percMajority))
  } else {
    message(sprintf("-- Number of episodes with GR3m intervals: %s", 0))
    message(sprintf("-- Percent of cases that contain a GR3m intersection that ALSO have majority GW overlap:: %s", 0))

    new_timing_designation_df <- dplyr::tibble(
      person_id = integer(0),
      episode_number = integer(0),
      GT_info_list = character(0),
      GW_flag = numeric(0),
      GR3m_flag = numeric(0),
      inferred_episode_start = numeric(0),
      precision_days = numeric(0),
      precision_category = character(0),
      intervalsCount = numeric(0),
      majorityOverlapCount = numeric(0)
    )
  }

  return(new_timing_designation_df)
}

merged_episodes_with_metadata <- function(episodes_with_gestational_timing_info_df, final_merged_episode_detailed_df, cdm) {
  # Add other pregnancy and demographic related info for each episode.

  # Assign input data frames to variables
  demographics_df <- final_merged_episode_detailed_df
  timing_df <- episodes_with_gestational_timing_info_df %>%
    dplyr::select(-"GT_info_list")
  term_max_min <- cdm$matcho_term_durations %>%
    dplyr::collect()

  final_df <- demographics_df %>%
    dplyr::left_join(timing_df, by = c("person_id", "episode_number")) %>%
    dplyr::distinct()

  # Add missing GW_flag and GR3m_flag
  final_df <- final_df %>%
    dplyr::mutate(
      GW_flag = dplyr::if_else(is.na(.data$GW_flag), 0, .data$GW_flag),
      GR3m_flag = dplyr::if_else(is.na(.data$GR3m_flag), 0, .data$GR3m_flag)
    )

  # Check if categories match between algorithms and dates are within 14 days of each other for outcomes only
  final_df <- final_df %>%
    dplyr::mutate(
      outcome_match = dplyr::case_when(
        .data$HIP_outcome_category == .data$PPS_outcome_category
        & .data$HIP_outcome_category != "PREG"
        & abs(as.numeric(difftime(.data$HIP_end_date, .data$PPS_end_date, units = "days"))) <= 14
        ~ 1,

        .data$HIP_outcome_category == "PREG"
        & .data$PPS_outcome_category == "PREG"
        ~ 1,
        TRUE ~ 0
    ))

  # If categories don't match, take the category that occurs second (outcome category from HIP algorithm is prioritized)
  final_df <- final_df %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$episode_number) %>%
    dplyr::mutate(
      next_HIP_outcome = dplyr::lead(.data$HIP_outcome_category),
      final_outcome_category = dplyr::case_when(
        .data$outcome_match == 1 ~ .data$HIP_outcome_category,

        .data$outcome_match == 0
        & is.na(.data$PPS_outcome_category)
        ~ .data$HIP_outcome_category,

        .data$outcome_match == 0
        & is.na(.data$HIP_outcome_category)
        ~ .data$PPS_outcome_category,

        # if they don't match, but the hip end date is not within 7 days before the PPS outcome
        # add: go with HIP if the PPS is the next one and there's sufficient separation
        .data$outcome_match == 0
        & .data$HIP_outcome_category != "PREG"
        & .data$PPS_outcome_category != "PREG"
        & !is.na(.data$next_HIP_outcome)
        & .data$PPS_outcome_category == .data$next_HIP_outcome
        & .data$HIP_end_date <= .data$PPS_end_date - lubridate::days(14)
        ~ .data$HIP_outcome_category,

        # but otherwise go with PPS
        .data$outcome_match == 0
        & .data$HIP_outcome_category != "PREG"
        & .data$PPS_outcome_category != "PREG"
        & .data$HIP_end_date <= .data$PPS_end_date - lubridate::days(7)
        ~ .data$PPS_outcome_category,

        # or if they're similar timing go with HIP
        TRUE ~ .data$HIP_outcome_category
      )
    ) %>%
    dplyr::ungroup()

  # If categories don't match, take the end date that occurs second (outcome date from HIP is prioritized)
  final_df <- final_df %>%
    dplyr::mutate(
      inferred_episode_end = dplyr::case_when(
        .data$outcome_match == 1 ~ .data$HIP_end_date,
        .data$outcome_match == 0 & is.na(.data$PPS_outcome_category) ~ .data$HIP_end_date,
        .data$outcome_match == 0 & is.na(.data$HIP_outcome_category) ~ .data$PPS_end_date,

        .data$outcome_match == 0
        & .data$HIP_outcome_category != "PREG"
        & .data$PPS_outcome_category != "PREG"
        & !is.na(.data$next_HIP_outcome)
        & .data$PPS_outcome_category == .data$next_HIP_outcome
        & .data$HIP_end_date <= .data$PPS_end_date - lubridate::days(14)
        ~ .data$HIP_end_date,

        .data$outcome_match == 0
        & .data$HIP_outcome_category != "PREG"
        & .data$PPS_outcome_category != "PREG"
        & .data$HIP_end_date <= .data$PPS_end_date - lubridate::days(7)
        ~ .data$PPS_end_date,

        !is.na(.data$HIP_end_date) ~ .data$HIP_end_date,
        !is.na(.data$PPS_end_date) ~ .data$PPS_end_date
    ))

  # Join with term_max_min data frame and drop 'retry' column
  final_df <- final_df %>%
    dplyr::left_join(
      term_max_min,
      by = c("final_outcome_category" = "category")
    ) %>%
    dplyr::select(-"retry")

  # If no start date, subtract the max term in days from inferred episode end
  final_df <- final_df %>%
    dplyr::mutate(
      inferred_episode_start = dplyr::if_else(
        is.na(.data$inferred_episode_start),
        as.Date(as.Date(.data$inferred_episode_end) - as.numeric(.data$max_term)),
        as.Date(.data$inferred_episode_start))
    )

  # Convert precision_days to integer type
  final_df <- final_df %>%
    dplyr::mutate(precision_days = as.integer(.data$precision_days))

  # Add missing accuracy info for remaining episodes
  final_df <- final_df %>%
    dplyr::mutate(
      precision_days = dplyr::if_else(
        is.na(.data$precision_days),
        .data$max_term - .data$min_term,
        .data$precision_days)
    )

  # Add missing accuracy category for remaining episodes
  final_df <- final_df %>%
    dplyr::mutate(
      precision_category = dplyr::if_else(
        is.na(.data$precision_category),
        assign_precision_category(.data$precision_days),
        .data$precision_category)
    )

  # Calculate gestational age at inferred episode end
  final_df <- final_df %>%
    dplyr::mutate(
      gestational_age_days_calculated = as.integer(difftime(
        time1 = .data$inferred_episode_end,
        time2 = .data$inferred_episode_start,
        units = "days"
      )))

  # Check if outcome aligns with term duration expected of that outcome
  final_df <- final_df %>%
    dplyr::mutate(
      term_duration_flag = dplyr::case_when(
        .data$gestational_age_days_calculated >= .data$min_term
        & .data$gestational_age_days_calculated <= .data$max_term
        ~ 1,

        .data$final_outcome_category == "PREG"
        & .data$gestational_age_days_calculated <= 301
        ~ 1,

        TRUE ~ 0
    )) %>%
    dplyr::select(-"min_term", -"max_term")

  # Add outcome concordance score - 2 highly concordant, 1 somewhat concordant, 0 not accurate/not enough info
  final_df <- final_df %>%
    dplyr::mutate(
      outcome_concordance_score = dplyr::case_when(
        .data$outcome_match == 1
        & .data$term_duration_flag == 1
        & .data$GW_flag == 1
        ~ 2,

        .data$outcome_match == 0
        & .data$term_duration_flag == 1
        & .data$GW_flag == 1
        ~ 1,

        TRUE ~ 0
    ))

  # Calculate preterm status from calculation
  final_df <- final_df %>%
    dplyr::mutate(
      preterm_status_from_calculation = dplyr::if_else(
        .data$gestational_age_days_calculated < 259,
        1,
        0
      )
    )

  # Print the min episode/pregnancy start and max episode/pregnancy end to check on time period of dataset
  min_episode_date <- min(final_df$recorded_episode_start, na.rm = TRUE)
  max_episode_date <- max(final_df$recorded_episode_end, na.rm = TRUE)
  min_pregnancy_date <- min(final_df$inferred_episode_start, na.rm = TRUE)
  max_pregnancy_date <- max(final_df$inferred_episode_end, na.rm = TRUE)

  message(sprintf("  - Min episode start date: %s", min_episode_date))
  message(sprintf("  - Max episode end date: %s", max_episode_date))
  message(sprintf("  - Min pregnancy start date: %s", min_pregnancy_date))
  message(sprintf("  - Max pregnancy end date: %s", max_pregnancy_date))

  return(final_df)
}
