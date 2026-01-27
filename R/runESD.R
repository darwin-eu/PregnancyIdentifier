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
#' Runs the Episode Start Date (ESD) algorithm to infer pregnancy episode start dates for merged HIP/PPS episodes.
#'
#' This function performs the following major steps:
#' \enumerate{
#'   \item Loads previously merged HIPPS episode table from \code{outputDir}.
#'   \item Extracts gestational timing concept evidence (e.g. gestational week, trimester concepts) for each episode within the specified date range.
#'   \item Infers episode start and start precision using all available gestational timing evidence, with logging.
#'   \item Merges inferred start dates, timing evidence, and metadata back onto the merged episodes table and computes final start/end/outcome fields.
#'   \item Filters episodes to retain only those overlapping the requested \code{startDate}--\code{endDate} study period, including episodes with missing inferred dates.
#'   \item Writes the resulting cohort of identified pregnancy episodes to an RDS file (\code{final_pregnancy_episodes.rds}) in \code{outputDir}.
#' }
#'
#' @param cdm A CDM reference, must include all necessary OMOP tables and concept sets for pregnancy inference algorithms.
#' @param outputDir Character. Path to directory where input and output RDS files reside.
#' @param startDate Earliest episode date to include (as.Date). Default: \code{as.Date("1900-01-01")}.
#' @param endDate Latest episode date to include (as.Date). Default: \code{Sys.Date()}.
#' @param logger A \code{log4r} logger object for info/debug messages.
#' @param debugMode (`logical(1)`) Should the ESD algorithm write intermidation datasets to the outputDir? `TRUE` or `FALSE` (default)
#'
#' @return Invisibly returns \code{NULL}. Main result is written as an RDS file (\code{final_pregnancy_episodes.rds}) to \code{outputDir}.
#'         The output contains one row per inferred pregnancy episode, with all metadata and gestational timing fields required for downstream analysis.
#' @export
runEsd <- function(cdm,
                   outputDir,
                   startDate = as.Date("1900-01-01"),
                   endDate = Sys.Date(),
                   logger,
                   debugMode = FALSE) {

  log4r::info(logger, "Running ESD")

  hippsEpisodes <- readRDS(file.path(outputDir, "HIPPS_episodes.rds"))

  # 1) Pull gestational timing concepts (GW / GR3m candidates) for HIP episodes
  timingConceptsDf <- getTimingConcepts(
    cdm = cdm,
    startDate = startDate,
    endDate = endDate,
    hippsEpisodes = hippsEpisodes
  )

  # 2) Convert concept evidence into inferred episode start + precision
  esdDf <- episodesWithGestationalTimingInfo(timingConceptsDf, logger = logger)

  if (debugMode) {
    saveRDS(esdDf, file.path(outputDir, "ESD.rds"))
  }

  # 3) Merge timing output back onto HIP/PPS metadata + derive final dates/outcomes
  mergedDf <- mergedEpisodesWithMetadata(
    episodes_with_gestational_timing_info_df = esdDf,
    hippsEpisodes = hippsEpisodes,
    cdm = cdm,
    logger = logger
  )

  # 4) Apply study period (keeps rows with missing inferred dates)
  mergedDf <- mergedDf %>%
    dplyr::filter(
      (.data$inferred_episode_start >= startDate | is.na(.data$inferred_episode_start)) &
        (.data$inferred_episode_end <= endDate | is.na(.data$inferred_episode_end))
    )

  outputPath <- file.path(outputDir, "final_pregnancy_episodes.rds")
  saveRDS(mergedDf, outputPath)
  log4r::info(logger, sprintf("Wrote output to %s", outputPath))

  invisible(NULL)
}

# ============================================================
# Pull gestational timing concepts around each episode
# ============================================================

getPregRelatedConcepts <- function(df, personIdList, dateCol) {
  df %>%
    dplyr::select("person_id", dplyr::all_of(dateCol), "concept_id", "concept_name", "value_col") %>%
    dplyr::rename(domain_concept_start_date = dplyr::all_of(dateCol)) %>%
    dplyr::inner_join(
      personIdList,
      by = dplyr::join_by(
        .data$person_id,
        .data$domain_concept_start_date >= .data$start_date,
        .data$domain_concept_start_date <= .data$recorded_episode_end
      )
    ) %>%
    dplyr::transmute(
      .data$person_id,
      .data$domain_concept_start_date,
      domain_concept_id   = .data$concept_id,
      domain_concept_name = .data$concept_name,
      .data$start_date,
      .data$recorded_episode_end,
      value_col = .data$value_col,
      episode_number = .data$episode_number
    )
}

getTimingConcepts <- function(cdm,
                              startDate = as.Date("1900-01-01"),
                              endDate = Sys.Date(),
                              hippsEpisodes) {
  # obtain the gestational timing <= 3 month concept information to use as additional information for precision category designation

  ppsConcepts <-
    system.file("concepts", "PPS_concepts.xlsx", package = "PregnancyIdentifier", mustWork = TRUE) %>%
    readxl::read_xlsx()

  esdConcepts <-
    system.file("concepts", "ESD_concepts.xlsx", package = "PregnancyIdentifier", mustWork = TRUE) %>%
    readxl::read_xlsx()

  conceptIdsToSearch <- as.integer(c(ppsConcepts$pps_concept_id, esdConcepts$esd_concept_id))

  # need to find concept names that contain 'gestation period' as well as the specific concepts
  conceptsToSearch <- cdm$concept %>%
    dplyr::filter(
      .data$concept_name %like% "gestation period" | # TODO why not precompute this list of concepts?
        .data$concept_id %in% .env$conceptIdsToSearch
    ) %>%
    dplyr::select("concept_id", "concept_name")

  # add: change to pregnancy start rather than recorded episode start
  personIdList <- hippsEpisodes %>%
    dplyr::mutate(start_date = pmin(.data$pregnancy_start, .data$recorded_episode_start, na.rm = TRUE)) %>%
    dplyr::select("person_id", "start_date", "recorded_episode_end", "episode_number")

  # Persist to DB so the non-equi join runs on the database backend
  cdm <- CDMConnector::insertTable(cdm, name = "person_id_list", table = personIdList, overwrite = TRUE)
  personIdList <- cdm$person_id_list

  # Generic domain extraction: join concept IDs -> domain table, set value_col, then restrict to episode window
  pullDomain <- function(domainDf, domainIdCol, domainDateCol, valueExpr) {
    conceptsToSearch %>%
      dplyr::inner_join(
        domainDf %>%
          dplyr::filter(.data[[domainDateCol]] >= startDate, .data[[domainDateCol]] <= endDate),
        by = stats::setNames(domainIdCol, "concept_id") # concept_id -> domain concept column
      ) %>%
      dplyr::mutate(value_col = {{ valueExpr }}) %>%
      getPregRelatedConcepts(personIdList, domainDateCol)
  }

  pregRelatedConcepts <- list(
    pullDomain(cdm$condition_occurrence, "condition_concept_id", "condition_start_date", .data$concept_name),
    pullDomain(cdm$observation, "observation_concept_id", "observation_date", .data$value_as_string),
    pullDomain(cdm$measurement, "measurement_concept_id", "measurement_date", .data$value_as_number) %>%
      dplyr::mutate(value_col = as.character(.data$value_col)),
    pullDomain(cdm$procedure_occurrence, "procedure_concept_id", "procedure_date", .data$concept_name)
  ) %>% purrr::reduce(dplyr::union_all)

  # Clean/parse gestational-week style values and compute extrapolated pregnancy starts
  suppressWarnings({

  pregRelatedConcepts %>%
    dplyr::left_join(
      dplyr::select(cdm$preg_pps_concepts, "pps_concept_id", "min_month" = "pps_min_month", "max_month" = "pps_max_month"),
      by = c("domain_concept_id" = "pps_concept_id")) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      domain_value = stringr::str_replace(.data$value_col, "\\|text_result_val:", ""),
      domain_value = stringr::str_replace(.data$domain_value, "\\|mapped_text_result_val:", ""),
      domain_value = stringr::str_replace(.data$domain_value, "Gestation period, ", ""),
      domain_value = stringr::str_replace(.data$domain_value, "gestation period, ", ""),
      domain_value = stringr::str_replace(.data$domain_value, " weeks", ""),
      # TODO NA introduced by coercion here
      domain_value = as.integer(as.numeric(.data$domain_value))
    ) %>%
    dplyr::mutate(
      keep_value = dplyr::if_else(
        (
          stringr::str_detect(tolower(.data$domain_concept_name), "gestation period,") |
            stringr::str_detect(tolower(.data$domain_concept_name), "gestational age") |
            .data$domain_concept_id %in% c(3048230, 3002209, 3012266)
        ) &
          (.data$domain_value <= 44 & .data$domain_value > 0),
        1, 0
      ),
      extrapolated_preg_start = dplyr::if_else(
        .data$keep_value == 1,
        .data$domain_concept_start_date - (.data$domain_value * 7),
        lubridate::NA_Date_
      )
    )
  })
}

# ============================================================
# Interval + outlier utilities (used by get_gt_timing)
# ============================================================

validate <- function(dateText) {
  tryCatch(
    { as.Date(dateText, "%Y-%m-%d"); TRUE },
    error = function(e) FALSE
  )
}

findIntersection <- function(intervals) {
  # intervals: list of length-2 vectors (start,end) or a single such vector
  intervalsDf <- if (length(intervals) == 1) {
    as.data.frame(matrix(intervals[[1]], ncol = 2))
  } else {
    as.data.frame(purrr::reduce(intervals, rbind))
  }

  intervalsDf <- intervalsDf %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~as.Date(., format = "%Y-%m-%d"))) %>%
    dplyr::arrange(.data$V1)

  # First remove outlier ranges via the IQR*1.5 approach.
  # Outlier ranges are determined by the number of overlaps each range has with other ranges.
  n <- nrow(intervalsDf)
  overlapCount <- integer(n)

  for (j in seq_len(n)) {
    last  <- intervalsDf[j, 2]
    first <- intervalsDf[j, 1]
    for (m in seq_len(n)) {
      if (j == m) next
      if ((intervalsDf[m, 1] == last) || (intervalsDf[m, 1] == first) ||
          (intervalsDf[m, 2] == last) || (intervalsDf[m, 2] == first) ||
          ((intervalsDf[m, 2] < last) && (intervalsDf[m, 2] > first)) ||
          ((intervalsDf[m, 1] < last) && (intervalsDf[m, 1] > first))) {
        overlapCount[j] <- overlapCount[j] + 1
      }
    }
  }

  intervalsDf <- intervalsDf %>%
    dplyr::mutate(overlapCountDict = overlapCount)

  countsQ1 <- stats::quantile(overlapCount, 0.25)
  countsQ3 <- stats::quantile(overlapCount, 0.75)
  outlierThreshold <- abs(countsQ1 - (countsQ3 - countsQ1) * 1.5)

  filteredIntervals <- if (outlierThreshold == 0) {
    dplyr::filter(intervalsDf, .data$overlapCountDict > outlierThreshold)
  } else {
    dplyr::filter(intervalsDf, .data$overlapCountDict >= outlierThreshold)
  } %>%
    dplyr::arrange(dplyr::desc(.data$overlapCountDict))

  # Now the outliers are removed, proceed with obtaining the overlaps
  N <- nrow(filteredIntervals)

  # Helper to return the expected 4-element output
  makeResult <- function(rangeEnd, rangeStart, interEnd, interStart) {
    c(rangeEnd, rangeStart, interEnd, interStart)
  }

  if (N == 1) {
    # Single interval: overlap is itself
    return(makeResult(filteredIntervals[1, 2], filteredIntervals[1, 1],
                      filteredIntervals[1, 2], filteredIntervals[1, 1]))
  }

  if (N == 0) {
    # If everything is filtered out, take the interval with the most overlaps
    top <- intervalsDf[order(intervalsDf$overlapCountDict, decreasing = TRUE), , drop = FALSE][1, ]
    return(makeResult(top[1, 2], top[1, 1], top[1, 2], top[1, 1]))
  }

  # First interval anchors the working intersection
  rangeEnd   <- filteredIntervals[1, 2] # last day of overlapping ranges
  rangeStart <- filteredIntervals[1, 1] # first day of overlapping ranges
  interEnd   <- filteredIntervals[1, 2] # minimum (last) day of intersection of ranges
  interStart <- filteredIntervals[1, 1] # maximum (first) day of intersection of ranges

  for (i in 2:N) {
    if (filteredIntervals[i, 1] < rangeStart) rangeStart <- filteredIntervals[i, 1]
    if (filteredIntervals[i, 2] > rangeEnd)   rangeEnd   <- filteredIntervals[i, 2]
    if ((filteredIntervals[i, 2] < interEnd) && (filteredIntervals[i, 2] > interStart)) interEnd <- filteredIntervals[i, 2]
    if ((filteredIntervals[i, 1] > interStart) && (filteredIntervals[i, 1] < interEnd)) interStart <- filteredIntervals[i, 1]
  }

  makeResult(rangeEnd, rangeStart, interEnd, interStart)
}

# check for GW concept overlap to the intervals
removeGWOutliers <- function(lol_of_GW_concepts) {
  dates <- as.Date(unlist(lol_of_GW_concepts))
  medianStart <- sort(dates)[ceiling(length(dates) / 2)]

  # distance from median date (days)
  allDistances <- purrr::map_dbl(dates, ~ as.numeric(max(.x, medianStart) - min(.x, medianStart)))

  distQ1 <- stats::quantile(allDistances, 0.25)
  distQ3 <- stats::quantile(allDistances, 0.75)
  outlierMetric <- (distQ3 - distQ1) * 1.5
  lower <- distQ1 - outlierMetric
  upper <- distQ3 + outlierMetric

  dates[allDistances >= lower & allDistances <= upper]
}

# definition to get accuracy category
assignPrecisionCategory <- function(precision_days) {
  dplyr::case_when(
    precision_days == -1 ~ "week_poor-support",
    precision_days >= 0  & precision_days <= 7   ~ "week",
    precision_days > 7   & precision_days <= 14  ~ "two-week",
    precision_days > 14  & precision_days <= 21  ~ "three-week",
    precision_days > 21  & precision_days <= 28  ~ "month",
    precision_days > 28  & precision_days <= 56  ~ "two-month",
    precision_days > 56  & precision_days <= 84  ~ "three-month",
    TRUE ~ "non-specific"
  )
}

# applying udf to the date array column to obtain the new column 'final_timing_info'
# (a list of [inferred_episode_start, precision_days, precision_category]) for each row
getGtTiming <- function(dateslist) {

  # Iterate over the dateslist
  timingArr <- purrr::map(dateslist[purrr::map_lgl(dateslist, validate)], sort)
  GW_list   <- dateslist[sapply(dateslist, function(x) length(x) == 1)]
  GR3m_list <- dateslist[sapply(dateslist, function(x) length(x) == 2)]

  inferred_start_date  <- as.Date("2000-01-01", format = "%Y-%m-%d")
  precision_days       <- 999
  precision_category   <- "-999"
  intervalsCount       <- 0
  majorityOverlapCount <- 0

  # get length of list with GR3m ranges
  N <- length(GR3m_list)
  common_GR3m_interval <- if (N > 0) findIntersection(GR3m_list) else NULL

  plausibleDays <- 0
  maxRangeDays  <- 0
  range_s <- range_e <- interval_s <- interval_e <- daterangeMidpoint <- NULL

  if (!is.null(common_GR3m_interval)) {
    range_e    <- as.Date(common_GR3m_interval[1], format = "%Y-%m-%d") # end date of range
    range_s    <- as.Date(common_GR3m_interval[2], format = "%Y-%m-%d") # start date of range
    interval_e <- as.Date(common_GR3m_interval[3], format = "%Y-%m-%d") # end date of intersection
    interval_s <- as.Date(common_GR3m_interval[4], format = "%Y-%m-%d") # start date of intersection

    plausibleDays <- as.numeric(difftime(interval_e, interval_s, units = "days"))
    maxRangeDays  <- as.numeric(difftime(range_e, range_s, units = "days"))
    daterangeMidpoint <- interval_s + lubridate::days(as.integer(plausibleDays / 2))

    # Utilize the overlap more when it gets narrowed down to < 1 week by taking the midpoint
    # and adding 3 days either side (otherwise unlikely to overlap much with GW concepts and thus will be ignored)
    if (plausibleDays < 7) {
      interval_s <- daterangeMidpoint - lubridate::days(3)
      interval_e <- daterangeMidpoint + lubridate::days(3)
      plausibleDays <- 6
    }
  }

  # there are week-level estimates
  if (length(GW_list) > 0) {
    # If GR3m interval exists, prefer GW concepts that overlap the GR3m intersection.
    if (!is.null(interval_s)) {
      intervalsCount <- intervalsCount + 1

      gwConceptCount <- length(GW_list)
      overlappingGw <- list()

      for (gwlist in GW_list) {
        gwDate <- gwlist[[1]]
        if (gwDate >= interval_s && gwDate <= interval_e) overlappingGw <- c(overlappingGw, gwDate)
      }

      percOverlapping <- (length(overlappingGw) / gwConceptCount) * 100

      if (percOverlapping > 50) {
        majorityOverlapCount <- majorityOverlapCount + 1
        filtDates <- removeGWOutliers(overlappingGw)
      } else {
        filtDates <- removeGWOutliers(GW_list)
        if (length(filtDates) == 1) precision_days <- -1 # only one GW concept and it doesn't overlap
      }

      inferred_start_date <- filtDates[[1]] # latest date
      precision_days <- if (precision_days == -1) -1 else as.numeric(max(filtDates) - min(filtDates))

    } else {
      # GW only (no GR3m information)
      filtDates <- removeGWOutliers(GW_list)
      inferred_start_date <- filtDates[[1]]
      precision_days <- as.numeric(max(filtDates) - min(filtDates))
      if (length(filtDates) == 1) precision_days <- -1
    }
  } else {
    # GR3m only (no GW concepts)
    inferred_start_date <- daterangeMidpoint
    precision_days <- maxRangeDays
  }

  precision_category <- assignPrecisionCategory(precision_days)

  list(
    inferred_start_date = inferred_start_date,
    precision_days = precision_days,
    precision_category = precision_category,
    intervalsCount = intervalsCount,
    majorityOverlapCount = majorityOverlapCount
  )
}

episodesWithGestationalTimingInfo <- function(get_timing_concepts_df, logger) {
  # add on either GW or GR3m designation depending on whether the concept is present

  esdConcepts2 <-
    system.file("concepts", "ESD_concepts2.xlsx", package = "PregnancyIdentifier", mustWork = TRUE) %>%
    readxl::read_xlsx()

  if (nrow(get_timing_concepts_df) == 0) {
    log4r::info(logger, sprintf("Number of episodes with GR3m intervals: %s", 0))
    log4r::info(logger, sprintf("Percent of cases that contain a GR3m intersection that ALSO have majority GW overlap:: %s", 0))

    return(dplyr::tibble(
      person_id = integer(0),
      episode_number = integer(0),
      GT_info_list = character(0),
      GW_flag = numeric(0),
      GR3m_flag = numeric(0),
      inferred_episode_start = as.Date(character(0)),
      precision_days = numeric(0),
      precision_category = character(0),
      intervalsCount = numeric(0),
      majorityOverlapCount = numeric(0)
    ))
  }

  timingDf <- get_timing_concepts_df %>%
    dplyr::mutate(
      domain_concept_id = as.integer(.data$domain_concept_id),
      GT_type = dplyr::case_when(
        stringr::str_detect(stringr::str_to_lower(.data$domain_concept_name), "gestation period") |
          .data$domain_concept_id %in% local(esdConcepts2$concept_id) ~ "GW",
        !is.na(.data$min_month) ~ "GR3m",
        TRUE ~ NA_character_
      )
    )

  # Add on the max and min pregnancy start dates predicted by each concept (GR3m only)
  timingDf <- timingDf %>%
    dplyr::mutate(
      min_days_to_pregnancy_start = dplyr::if_else(.data$GT_type == "GR3m", round(.data$min_month * 30.4), NA_real_),
      max_days_to_pregnancy_start = dplyr::if_else(.data$GT_type == "GR3m", round(.data$max_month * 30.4), NA_real_),
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

  # Remove type if GW values are null (preserves original logic)
  timingDf <- timingDf %>%
    dplyr::mutate(
      GT_type = dplyr::case_when(
        .data$GT_type == "GW" & (is.na(.data$domain_value) | is.na(.data$extrapolated_preg_start)) ~ NA_character_,
        TRUE ~ .data$GT_type
      )
    ) %>%
    dplyr::filter(.data$GT_type %in% c("GW", "GR3m"))

  # Build the date ranges used downstream:
  # - GR3m uses "max_pregnancy_start min_pregnancy_start"
  # - GW uses extrapolated pregnancy start (single date)
  timingDf <- timingDf %>%
    dplyr::mutate(
      dplyr::across(c("extrapolated_preg_start", "min_pregnancy_start", "max_pregnancy_start"), as.character),
      preg_start_range = dplyr::if_else(
        is.na(.data$extrapolated_preg_start),
        paste(.data$max_pregnancy_start, .data$min_pregnancy_start),
        .data$extrapolated_preg_start
      ),
      extr = .data$extrapolated_preg_start,
      all_GT_info = dplyr::if_else(is.na(.data$extr), .data$preg_start_range, .data$extr),
      # ensure GW concepts are treated as a single entity (important for deduping)
      domain_concept_name_rollup = dplyr::if_else(
        !is.na(.data$domain_value) & .data$GT_type == "GW",
        "Gestation Week",
        .data$domain_concept_name
      )
    )

  # IMPORTANT: sort gest week concepts from highest (latest in pregnancy) to lowest (earliest)
  # so that later on the first element of the GW list can be taken for 'latest in pregnancy' concept
  timingDf <- timingDf %>%
    dplyr::arrange(.data$person_id, .data$episode_number, dplyr::desc(.data$domain_value)) %>%
    dplyr::group_by(
      .data$person_id, .data$episode_number,
      .data$domain_concept_name_rollup, .data$domain_concept_start_date, .data$GT_type
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Group by patient + episode, then compute inferred start date and precision info
  summaryDf <- timingDf %>%
    dplyr::group_by(.data$person_id, .data$episode_number) %>%
    dplyr::summarise(
      GT_info_list = purrr::map(list(.data$all_GT_info), ~ stringr::str_split(.x, " ")),
      GW_flag  = as.numeric(any(.data$GT_type == "GW")),
      GR3m_flag = as.numeric(any(.data$GT_type == "GR3m")),
      .groups = "drop"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      final_timing_info = list(getGtTiming(.data$GT_info_list)),
      inferred_episode_start = .data$final_timing_info$inferred_start_date,
      precision_days = .data$final_timing_info$precision_days,
      precision_category = .data$final_timing_info$precision_category,
      intervalsCount = .data$final_timing_info$intervalsCount,
      majorityOverlapCount = .data$final_timing_info$majorityOverlapCount
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "person_id", "episode_number", "GT_info_list", "GW_flag", "GR3m_flag",
      "inferred_episode_start", "precision_days", "precision_category",
      "intervalsCount", "majorityOverlapCount"
    )

  # print the GW and GR3m concept overlap information to log
  majorityOverlapCountTotal <- sum(summaryDf$majorityOverlapCount, na.rm = TRUE)
  intervalsCountTotal <- sum(summaryDf$intervalsCount, na.rm = TRUE)
  percMajority <- (majorityOverlapCountTotal / intervalsCountTotal) * 100

  log4r::info(logger, sprintf("Number of episodes with GR3m intervals: %s", intervalsCountTotal))
  log4r::info(logger, sprintf(
    "Percent of cases that contain a GR3m intersection that ALSO have majority GW overlap:: %s",
    percMajority
  ))

  summaryDf
}

mergedEpisodesWithMetadata <- function(episodes_with_gestational_timing_info_df,
                                       hippsEpisodes,
                                       cdm,
                                       logger) {
  # Add other pregnancy and demographic related info for each episode.

  timingDf <- episodes_with_gestational_timing_info_df %>%
    dplyr::select(-"GT_info_list")
  termMaxMin <- cdm$preg_matcho_term_durations %>% dplyr::collect()

  finalDf <- hippsEpisodes %>%
    dplyr::left_join(timingDf, by = c("person_id", "episode_number")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      # Add missing GW_flag and GR3m_flag
      GW_flag  = dplyr::coalesce(.data$GW_flag,  0),
      GR3m_flag = dplyr::coalesce(.data$GR3m_flag, 0)
    )

  # Check if categories match between algorithms and dates are within 14 days of each other for outcomes only
  finalDf <- finalDf %>%
    dplyr::mutate(
      outcome_match = dplyr::case_when(
        .data$HIP_outcome_category == .data$PPS_outcome_category &
        .data$HIP_outcome_category != "PREG" &
        abs(as.numeric(difftime(.data$HIP_end_date, .data$PPS_end_date, units = "days"))) <= 14 ~ 1,
        .data$HIP_outcome_category == "PREG" & .data$PPS_outcome_category == "PREG" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$episode_number, .by_group = TRUE) %>%
    dplyr::mutate(next_HIP_outcome = dplyr::lead(.data$HIP_outcome_category)) %>%
    dplyr::ungroup()

  # If categories don't match, take the category that occurs second (outcome category from HIP algorithm is prioritized)
  finalDf <- finalDf %>%
    dplyr::mutate(
      final_outcome_category = dplyr::case_when(
        .data$outcome_match == 1 ~ .data$HIP_outcome_category,

        .data$outcome_match == 0 & is.na(.data$PPS_outcome_category) ~ .data$HIP_outcome_category,
        .data$outcome_match == 0 & is.na(.data$HIP_outcome_category) ~ .data$PPS_outcome_category,

        # if they don't match, but the hip end date is not within 7 days before the PPS outcome
        # add: go with HIP if the PPS is the next one and there's sufficient separation
        .data$outcome_match == 0 &
          .data$HIP_outcome_category != "PREG" &
          .data$PPS_outcome_category != "PREG" &
          !is.na(.data$next_HIP_outcome) &
          .data$PPS_outcome_category == .data$next_HIP_outcome &
          .data$HIP_end_date <= .data$PPS_end_date - lubridate::days(14) ~ .data$HIP_outcome_category,

        # but otherwise go with PPS
        .data$outcome_match == 0 &
          .data$HIP_outcome_category != "PREG" &
          .data$PPS_outcome_category != "PREG" &
          .data$HIP_end_date <= .data$PPS_end_date - lubridate::days(7) ~ .data$PPS_outcome_category,

        # or if they're similar timing go with HIP
        TRUE ~ .data$HIP_outcome_category
      ),
      # If categories don't match, take the end date that occurs second (outcome date from HIP is prioritized)
      inferred_episode_end = dplyr::case_when(
        .data$outcome_match == 1 ~ .data$HIP_end_date,
        .data$outcome_match == 0 & is.na(.data$PPS_outcome_category) ~ .data$HIP_end_date,
        .data$outcome_match == 0 & is.na(.data$HIP_outcome_category) ~ .data$PPS_end_date,

        .data$outcome_match == 0 &
          .data$HIP_outcome_category != "PREG" &
          .data$PPS_outcome_category != "PREG" &
          !is.na(.data$next_HIP_outcome) &
          .data$PPS_outcome_category == .data$next_HIP_outcome &
          .data$HIP_end_date <= .data$PPS_end_date - lubridate::days(14) ~ .data$HIP_end_date,

        .data$outcome_match == 0 &
          .data$HIP_outcome_category != "PREG" &
          .data$PPS_outcome_category != "PREG" &
          .data$HIP_end_date <= .data$PPS_end_date - lubridate::days(7) ~ .data$PPS_end_date,

        !is.na(.data$HIP_end_date) ~ .data$HIP_end_date,
        !is.na(.data$PPS_end_date) ~ .data$PPS_end_date
      )
    )

  # Join with term_max_min data frame and drop 'retry' column
  finalDf <- finalDf %>%
    dplyr::left_join(termMaxMin, by = c("final_outcome_category" = "category")) %>%
    dplyr::select(-"retry") %>%
    dplyr::mutate(
      # If no start date, subtract max term from inferred end date
      inferred_episode_start = dplyr::if_else(
        is.na(.data$inferred_episode_start),
        as.Date(as.Date(.data$inferred_episode_end) - as.numeric(.data$max_term)),
        as.Date(.data$inferred_episode_start)
      ),
      # Convert precision_days to integer type (then fill missing)
      precision_days = as.integer(.data$precision_days),
      precision_days = dplyr::if_else(
        is.na(.data$precision_days),
        .data$max_term - .data$min_term,
        .data$precision_days
      ),
      precision_category = dplyr::if_else(
        is.na(.data$precision_category),
        assignPrecisionCategory(.data$precision_days),
        .data$precision_category
      ),
      # Calculate gestational age at inferred episode end
      gestational_age_days_calculated = as.integer(difftime(
        time1 = .data$inferred_episode_end,
        time2 = .data$inferred_episode_start,
        units = "days"
      )),
      # Check if outcome aligns with term duration expected of that outcome
      term_duration_flag = dplyr::case_when(
        .data$gestational_age_days_calculated >= .data$min_term &
          .data$gestational_age_days_calculated <= .data$max_term ~ 1,
        .data$final_outcome_category == "PREG" &
          .data$gestational_age_days_calculated <= 301 ~ 1,
        TRUE ~ 0
      ),
      # Add outcome concordance score - 2 highly concordant, 1 somewhat concordant, 0 not accurate/not enough info
      outcome_concordance_score = dplyr::case_when(
        .data$outcome_match == 1 & .data$term_duration_flag == 1 & .data$GW_flag == 1 ~ 2,
        .data$outcome_match == 0 & .data$term_duration_flag == 1 & .data$GW_flag == 1 ~ 1,
        TRUE ~ 0
      ),
      # Calculate preterm status from calculation
      preterm_status_from_calculation = dplyr::if_else(.data$gestational_age_days_calculated < 259, 1, 0)
    ) %>%
    dplyr::select(-"min_term", -"max_term")

  # Print time period checks
  min_episode_date  <- min(finalDf$recorded_episode_start, na.rm = TRUE)
  max_episode_date  <- max(finalDf$recorded_episode_end, na.rm = TRUE)
  min_preg_date     <- min(finalDf$inferred_episode_start, na.rm = TRUE)
  max_preg_date     <- max(finalDf$inferred_episode_end, na.rm = TRUE)

  log4r::info(logger, sprintf("Min episode start date: %s", min_episode_date))
  log4r::info(logger, sprintf("Max episode end date: %s", max_episode_date))
  log4r::info(logger, sprintf("Min pregnancy start date: %s", min_preg_date))
  log4r::info(logger, sprintf("Max pregnancy end date: %s", max_preg_date))

  finalDf
}
