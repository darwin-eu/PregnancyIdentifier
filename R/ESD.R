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

# Time a block, log "<label>: X.X s" at info level, and return the block's value.
withTiming <- function(label, logger, expr) {
  t0 <- Sys.time()
  on.exit({
    log4r::info(logger, sprintf("[ESD timing] %s: %.1f s", label, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  })
  expr
}

#' runEsd
#'
#' Runs the Episode Start Date (ESD) algorithm to infer pregnancy episode start dates for merged HIP/PPS episodes.
#'
#' This function performs the following major steps:
#' \enumerate{
#'   \item Loads previously merged HIPPS episode table from \code{outputFolder}.
#'   \item Extracts gestational timing concept evidence (e.g. gestational week, trimester concepts) for each episode within the specified date range.
#'   \item Infers episode start and start precision using all available gestational timing evidence, with logging.
#'   \item Merges inferred start dates, timing evidence, and metadata back onto the merged episodes table and computes final start/end/outcome fields.
#'   \item Filters episodes to retain only those with start on/after \code{startDate} and end on/before \code{endDate}; episodes with missing inferred dates are retained.
#'   \item Writes the resulting cohort of identified pregnancy episodes to an RDS file (\code{final_pregnancy_episodes.rds}) in \code{outputFolder}.
#' }
#'
#' @param cdm A CDM reference, must include all necessary OMOP tables and concept sets for pregnancy inference algorithms.
#' @param outputFolder Character. Path to directory where input and output RDS files reside.
#' @param startDate Earliest episode date to include (as.Date). Default: \code{as.Date("1900-01-01")}.
#' @param endDate Latest episode date to include (as.Date). Default: \code{Sys.Date()}.
#' @param logger A \code{log4r} logger object for info/debug messages.
#' @param debugMode (`logical(1)`) Should the ESD algorithm write intermediate datasets to the outputFolder? `TRUE` or `FALSE` (default)
#' @param conformToValidation (`logical(1)`: `FALSE`) If `TRUE`, modify episodes to conform (remove overlaps and length > 308 days). Validation and logging always run.
#'
#' @return Invisibly returns \code{NULL}. Main result is written as an RDS file (\code{final_pregnancy_episodes.rds}) to \code{outputFolder}.
#'         The output contains one row per inferred pregnancy episode. Columns include: \code{final_episode_start_date}, \code{final_episode_end_date}, \code{final_outcome_category} (no prefix), and ESD-derived columns with \code{esd_} prefix: \code{esd_precision_days}, \code{esd_precision_category}, \code{esd_gestational_age_days_calculated}, \code{esd_gw_flag}, \code{esd_gr3m_flag}, \code{esd_outcome_match}, \code{esd_term_duration_flag}, \code{esd_outcome_concordance_score}, \code{esd_preterm_status_from_calculation}, plus merge/HIPPS metadata (e.g. \code{merge_episode_start}, \code{hip_end_date}, \code{pps_end_date}).
#' @export
runEsd <- function(cdm,
                   outputFolder,
                   startDate = as.Date("1900-01-01"),
                   endDate = Sys.Date(),
                   logger,
                   debugMode = FALSE,
                   conformToValidation = FALSE) {
  checkmate::assertClass(logger, "logger", null.ok = FALSE)
  checkmate::assertLogical(conformToValidation, len = 1, any.missing = FALSE)

  log4r::info(logger, "Running ESD")

  hippsEpisodes <- readRDS(file.path(outputFolder, "hipps_episodes.rds"))
  requiredHippsCols <- c("person_id", "merge_episode_number", "merge_pregnancy_start", "merge_episode_start", "merge_episode_end")
  checkmate::assertNames(
    names(hippsEpisodes),
    must.include = requiredHippsCols,
    .var.name = "hipps_episodes.rds column names"
  )

  # Term durations: read from package Excel (no DB needed; HIP uses cdm$preg_matcho_term_durations from init)
  termMaxMin <- readxl::read_excel(
    system.file("concepts", "Matcho_term_durations.xlsx", package = "PregnancyIdentifier", mustWork = TRUE)
  )

  # ESD concept counts (record_count, person_count) for outputFolder / export
  withTiming("writeEsdConceptCounts", logger, {
    writeEsdConceptCounts(cdm, startDate, endDate, outputFolder)
  })

  # 1) Pull gestational timing concepts (GW / GR3m candidates) for HIP episodes
  timingConceptsDf <- withTiming("getTimingConcepts (total)", logger, {
    getTimingConcepts(
      cdm = cdm,
      startDate = startDate,
      endDate = endDate,
      hippsEpisodes = hippsEpisodes,
      logger = logger
    )
  })
  log4r::info(logger, sprintf("[ESD timing] getTimingConcepts result rows: %d", nrow(timingConceptsDf)))

  # 2) Convert concept evidence into inferred episode start + precision
  esdDf <- withTiming("episodesWithGestationalTimingInfo (total)", logger, {
    episodesWithGestationalTimingInfo(timingConceptsDf, logger = logger)
  })

  if (debugMode) {
    esdOut <- esdDf %>%
      dplyr::rename(
        esd_precision_days = "precision_days",
        esd_precision_category = "precision_category",
        esd_gw_flag = "gw_flag",
        esd_gr3m_flag = "gr3m_flag"
      ) %>%
      dplyr::select(
        "person_id",
        "merge_episode_number",
        "inferred_episode_start",
        "esd_precision_days",
        "esd_precision_category",
        "esd_gw_flag",
        "esd_gr3m_flag",
        "intervals_count",
        "majority_overlap_count",
        "gt_info_list"
      )
    saveRDS(esdOut, file.path(outputFolder, "esd.rds"))
  }

  # 3) Merge timing output back onto HIP/PPS metadata + derive final dates/outcomes (no DB: uses termMaxMin)
  mergedDf <- withTiming("mergedEpisodesWithMetadata", logger, {
    mergedEpisodesWithMetadata(
      episodesWithGestationalTimingInfoDf = esdDf,
      hippsEpisodes = hippsEpisodes,
      termMaxMin = termMaxMin,
      logger = logger
    )
  })

  # 4) Add delivery mode
  mergedDf <- withTiming("addDeliveryMode", logger, {
    addDeliveryMode(cdm, mergedDf, logger = logger)
  })

  # 5) Apply study period: retain episodes that end on or before endDate and start on or after startDate;
  #    episodes with NA inferred_episode_start or inferred_episode_end are retained.
  # Coerce to Date so comparison is robust across backends (e.g. when values are POSIXct or character).
  mergedDf <- mergedDf %>%
    dplyr::mutate(
      esd_study_start = as.Date(.data$inferred_episode_start),
      esd_study_end = as.Date(.data$inferred_episode_end),
      esd_keep = (is.na(.data$esd_study_start) | .data$esd_study_start >= startDate) &
        (is.na(.data$esd_study_end) | .data$esd_study_end <= endDate)
    )
  mergedDf <- mergedDf %>%
    dplyr::filter(.data$esd_keep) %>%
    dplyr::select(-"esd_study_start", -"esd_study_end", -"esd_keep")

  outputPath <- file.path(outputFolder, "final_pregnancy_episodes.rds")
  mergedDf <- mergedDf %>%
    dplyr::rename(
      final_episode_start_date = "inferred_episode_start",
      final_episode_end_date = "inferred_episode_end",
      esd_precision_days = "precision_days",
      esd_precision_category = "precision_category",
      esd_gestational_age_days_calculated = "gestational_age_days_calculated",
      esd_gw_flag = "gw_flag",
      esd_gr3m_flag = "gr3m_flag",
      esd_outcome_match = "outcome_match",
      esd_term_duration_flag = "term_duration_flag",
      esd_outcome_concordance_score = "outcome_concordance_score",
      esd_preterm_status_from_calculation = "preterm_status_from_calculation"
    ) %>%
    dplyr::select(
      "person_id",
      "merge_episode_number",
      "final_episode_start_date",
      "final_episode_end_date",
      "final_outcome_category",
      "merge_episode_start",
      "merge_episode_end",
      "hip_end_date",
      "pps_end_date",
      "hip_outcome_category",
      "pps_outcome_category",
      "esd_precision_days",
      "esd_precision_category",
      "esd_gestational_age_days_calculated",
      "esd_gw_flag",
      "esd_gr3m_flag",
      "esd_outcome_match",
      "esd_term_duration_flag",
      "esd_outcome_concordance_score",
      "esd_preterm_status_from_calculation",
      dplyr::everything()
    )
  # Validation and logging always run (validateEpisodePeriods).
  validateEpisodePeriods(
    mergedDf,
    personIdCol = "person_id",
    startDateCol = "final_episode_start_date",
    endDateCol = "final_episode_end_date",
    logger = logger
  )
  priorForAttrition <- getAttritionPrior(outputFolder, "hipps_episodes")
  recordCohortAttrition <- file.exists(attritionFileName(outputFolder)) && !is.null(priorForAttrition)

  # Cohort attrition: apply the 7 criteria in order and record each step when attrition file exists.
  cohortReasons <- c(
    "Initial qualifying events",
    "In observation at pregnancy start date",
    "In observation at pregnancy end date",
    "Pregnancy end date > pregnancy start date",
    "Gestational length < 308 days",
    "Gestational length days != 0",
    "No overlapping pregnancy records"
  )

  .nRec <- function(df) as.integer(nrow(df))
  .nPer <- function(df) as.integer(dplyr::n_distinct(df$person_id))
  .appendCohortStep <- function(priorR, priorP, postR, postP, reason) {
    if (!recordCohortAttrition) return(invisible(NULL))
    appendAttrition(
      outputFolder,
      step = "cohort_attrition",
      table = "final_episodes",
      outcome = NA_character_,
      prior_records = priorR,
      prior_persons = priorP,
      dropped_records = priorR - postR,
      dropped_persons = priorP - postP,
      post_records = postR,
      post_persons = postP,
      reason = reason
    )
  }

  # 1) Initial qualifying events (count after study period filter)
  pr <- priorForAttrition$post_records
  pp <- priorForAttrition$post_persons
  postR <- .nRec(mergedDf)
  postP <- .nPer(mergedDf)
  .appendCohortStep(pr, pp, postR, postP, cohortReasons[1L])
  pr <- postR
  pp <- postP

  # 2) In observation at pregnancy start date (only when conformToValidation)
  if (conformToValidation && "observation_period" %in% names(cdm)) {
    op <- cdm$observation_period %>%
      dplyr::select("person_id", "observation_period_start_date", "observation_period_end_date") %>%
      dplyr::collect()
    keepStart <- mergedDf %>%
      dplyr::filter(!is.na(.data$final_episode_start_date)) %>%
      dplyr::inner_join(op, by = "person_id") %>%
      dplyr::filter(
        .data$final_episode_start_date >= .data$observation_period_start_date,
        .data$final_episode_start_date <= .data$observation_period_end_date
      ) %>%
      dplyr::select("person_id", "merge_episode_number") %>%
      dplyr::distinct()
    mergedDf <- mergedDf %>%
      dplyr::inner_join(keepStart, by = c("person_id", "merge_episode_number"))
  }
  postR <- .nRec(mergedDf)
  postP <- .nPer(mergedDf)
  .appendCohortStep(pr, pp, postR, postP, cohortReasons[2L])
  pr <- postR
  pp <- postP

  # 3) In observation at pregnancy end date (only when conformToValidation)
  if (conformToValidation && "observation_period" %in% names(cdm)) {
    if (!exists("op", inherits = FALSE)) {
      op <- cdm$observation_period %>%
        dplyr::select("person_id", "observation_period_start_date", "observation_period_end_date") %>%
        dplyr::collect()
    }
    keepEnd <- mergedDf %>%
      dplyr::filter(!is.na(.data$final_episode_end_date)) %>%
      dplyr::inner_join(op, by = "person_id") %>%
      dplyr::filter(
        .data$final_episode_end_date >= .data$observation_period_start_date,
        .data$final_episode_end_date <= .data$observation_period_end_date
      ) %>%
      dplyr::select("person_id", "merge_episode_number") %>%
      dplyr::distinct()
    mergedDf <- mergedDf %>%
      dplyr::inner_join(keepEnd, by = c("person_id", "merge_episode_number"))
  }
  postR <- .nRec(mergedDf)
  postP <- .nPer(mergedDf)
  .appendCohortStep(pr, pp, postR, postP, cohortReasons[3L])
  pr <- postR
  pp <- postP

  # 4) Pregnancy end date > pregnancy start date
  # Only fix inverted dates and drop remaining bad episodes when conformToValidation is enabled.
  # When disabled, episodes with start >= end are left as-is and characterized via quality flags.
  if (conformToValidation) {
    fixResult <- fixStartBeforeEnd(
      mergedDf,
      termMaxMin,
      startDateCol = "final_episode_start_date",
      endDateCol = "final_episode_end_date",
      outcomeCol = "final_outcome_category"
    )
    mergedDf <- fixResult$df
    log4r::info(logger, sprintf("fix_start_before_end: episodes with start >= end before fix: %d", fixResult$n_corrected))
    mergedDf <- mergedDf %>%
      dplyr::filter(
        is.na(.data$final_episode_start_date) | is.na(.data$final_episode_end_date) |
          as.Date(.data$final_episode_end_date) > as.Date(.data$final_episode_start_date)
      )
  }
  postR <- .nRec(mergedDf)
  postP <- .nPer(mergedDf)
  .appendCohortStep(pr, pp, postR, postP, cohortReasons[4L])
  pr <- postR
  pp <- postP

  # 5) Gestational length < 308 days (only when conformToValidation: drop episodes longer than 308 days)
  if (conformToValidation) {
    mergedDf <- mergedDf %>%
      dplyr::mutate(
        .gest_days = dplyr::coalesce(
          as.numeric(.data$esd_gestational_age_days_calculated),
          as.numeric(as.Date(.data$final_episode_end_date) - as.Date(.data$final_episode_start_date))
        )
      ) %>%
      dplyr::filter(is.na(.data$.gest_days) | .data$.gest_days < 308) %>%
      dplyr::select(-".gest_days")
  }
  postR <- .nRec(mergedDf)
  postP <- .nPer(mergedDf)
  .appendCohortStep(pr, pp, postR, postP, cohortReasons[5L])
  pr <- postR
  pp <- postP

  # 6) Gestational length days != 0 (only when conformToValidation)
  if (conformToValidation) {
    mergedDf <- mergedDf %>%
      dplyr::mutate(
        .gest_days = dplyr::coalesce(
          as.numeric(.data$esd_gestational_age_days_calculated),
          as.numeric(as.Date(.data$final_episode_end_date) - as.Date(.data$final_episode_start_date))
        )
      ) %>%
      dplyr::filter(is.na(.data$.gest_days) | .data$.gest_days != 0) %>%
      dplyr::select(-".gest_days")
  }
  postR <- .nRec(mergedDf)
  postP <- .nPer(mergedDf)
  .appendCohortStep(pr, pp, postR, postP, cohortReasons[6L])
  pr <- postR
  pp <- postP

  # 7) No overlapping pregnancy records (only when conformToValidation)
  if (conformToValidation) {
    mergedDf <- removeOverlaps(
      mergedDf,
      personIdCol = "person_id",
      startDateCol = "final_episode_start_date",
      endDateCol = "final_episode_end_date"
    )
  }
  postR <- .nRec(mergedDf)
  postP <- .nPer(mergedDf)
  .appendCohortStep(pr, pp, postR, postP, cohortReasons[7L])

  # ---- Per-record quality flags ----
  # Characterize each episode without removing any records. These flags indicate

  # what cleanup (conformToValidation=TRUE) would remove.

  # Observation period flags
  if ("observation_period" %in% names(cdm)) {
    if (!exists("op", inherits = FALSE)) {
      op <- cdm$observation_period %>%
        dplyr::select("person_id", "observation_period_start_date", "observation_period_end_date") %>%
        dplyr::collect()
    }
    startInObs <- mergedDf %>%
      dplyr::filter(!is.na(.data$final_episode_start_date)) %>%
      dplyr::inner_join(op, by = "person_id") %>%
      dplyr::filter(
        .data$final_episode_start_date >= .data$observation_period_start_date,
        .data$final_episode_start_date <= .data$observation_period_end_date
      ) %>%
      dplyr::select("person_id", "merge_episode_number") %>%
      dplyr::distinct() %>%
      dplyr::mutate(.start_in_obs = TRUE)
    endInObs <- mergedDf %>%
      dplyr::filter(!is.na(.data$final_episode_end_date)) %>%
      dplyr::inner_join(op, by = "person_id") %>%
      dplyr::filter(
        .data$final_episode_end_date >= .data$observation_period_start_date,
        .data$final_episode_end_date <= .data$observation_period_end_date
      ) %>%
      dplyr::select("person_id", "merge_episode_number") %>%
      dplyr::distinct() %>%
      dplyr::mutate(.end_in_obs = TRUE)
    mergedDf <- mergedDf %>%
      dplyr::left_join(startInObs, by = c("person_id", "merge_episode_number")) %>%
      dplyr::mutate(
        is_start_outside_observation_period = dplyr::if_else(
          is.na(.data$final_episode_start_date), FALSE,
          dplyr::coalesce(!.data$.start_in_obs, TRUE)
        )
      ) %>%
      dplyr::select(-".start_in_obs") %>%
      dplyr::left_join(endInObs, by = c("person_id", "merge_episode_number")) %>%
      dplyr::mutate(
        is_end_outside_observation_period = dplyr::if_else(
          is.na(.data$final_episode_end_date), FALSE,
          dplyr::coalesce(!.data$.end_in_obs, TRUE)
        )
      ) %>%
      dplyr::select(-".end_in_obs")
  } else {
    mergedDf$is_start_outside_observation_period <- NA
    mergedDf$is_end_outside_observation_period <- NA
  }

  # Date / length flags
  mergedDf <- mergedDf %>%
    dplyr::mutate(
      .gest_days_flag = dplyr::coalesce(
        as.numeric(.data$esd_gestational_age_days_calculated),
        as.numeric(as.Date(.data$final_episode_end_date) - as.Date(.data$final_episode_start_date))
      ),
      is_start_gte_end = dplyr::if_else(
        !is.na(.data$final_episode_start_date) & !is.na(.data$final_episode_end_date),
        as.Date(.data$final_episode_start_date) >= as.Date(.data$final_episode_end_date),
        FALSE
      ),
      is_zero_length = dplyr::if_else(
        !is.na(.data$.gest_days_flag), .data$.gest_days_flag == 0, FALSE
      ),
      is_too_long = dplyr::if_else(
        !is.na(.data$.gest_days_flag), .data$.gest_days_flag >= 308, FALSE
      )
    ) %>%
    dplyr::select(-".gest_days_flag")

  # Overlap flag: mark episodes that overlap with another episode for the same person
  mergedDf <- mergedDf %>%
    dplyr::arrange(.data$person_id, .data$final_episode_start_date, .data$final_episode_end_date) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::mutate(
      .end_num = as.numeric(as.Date(.data$final_episode_end_date)),
      .start_num = as.numeric(as.Date(.data$final_episode_start_date)),
      .prev_max_end = dplyr::lag(cummax(dplyr::coalesce(.data$.end_num, -Inf)), default = NA_real_),
      is_overlapping = !is.na(.data$.prev_max_end) & !is.na(.data$.start_num) &
        .data$.start_num < .data$.prev_max_end
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-".end_num", -".start_num", -".prev_max_end")

  saveRDS(mergedDf, outputPath)
  log4r::info(logger, sprintf("Wrote output to %s", outputPath))

  # Attrition: final_episodes overall (summary row)
  if (recordCohortAttrition) {
    appendAttrition(
      outputFolder,
      step = "final_episodes",
      table = "final_episodes",
      outcome = NA_character_,
      prior_records = postR,
      prior_persons = postP,
      dropped_records = 0L,
      dropped_persons = 0L,
      post_records = postR,
      post_persons = postP
    )
  } else if (!is.null(priorForAttrition)) {
    postR <- .nRec(mergedDf)
    postP <- .nPer(mergedDf)
    appendAttrition(
      outputFolder,
      step = "final_episodes",
      table = "final_episodes",
      outcome = NA_character_,
      prior_records = priorForAttrition$post_records,
      prior_persons = priorForAttrition$post_persons,
      dropped_records = priorForAttrition$post_records - postR,
      dropped_persons = priorForAttrition$post_persons - postP,
      post_records = postR,
      post_persons = postP
    )
  }
  # By outcome (only when attrition file exists, e.g. pipeline run via runPregnancyIdentifier)
  if (file.exists(attritionFileName(outputFolder)) && "final_outcome_category" %in% names(mergedDf)) {
    outcomeCol <- "final_outcome_category"
    outcomes <- sort(unique(mergedDf[[outcomeCol]][!is.na(mergedDf[[outcomeCol]])]))
    for (oc in outcomes) {
      sub <- mergedDf %>% dplyr::filter(.data[[outcomeCol]] == .env$oc)
      appendAttrition(
        outputFolder,
        step = "final_episodes_by_outcome",
        table = "final_episodes",
        outcome = oc,
        prior_records = NA_integer_,
        prior_persons = NA_integer_,
        dropped_records = NA_integer_,
        dropped_persons = NA_integer_,
        post_records = nrow(sub),
        post_persons = dplyr::n_distinct(sub$person_id)
      )
    }
  }

  # Clean up intermediate database tables created during ESD.
  # These tables were inserted inside helper functions (getTimingConcepts, addDeliveryMode)
  # that don't return the updated cdm, so the tables may not be registered on our cdm object.
  tryCatch(
    CDMConnector::dropSourceTable(cdm, c("person_id_list", "esd_delivery_mode_cohort")),
    error = function(e) NULL # table may not exist (e.g. empty input or already dropped)
  )

  invisible(NULL)
}

# ============================================================
# Remove long pregnancies and overlapping episodes
# ============================================================

#' Remove pregnancies longer than a maximum duration
#'
#' Filters out episodes whose period (end date minus start date in days) exceeds
#' \code{maxDays}. Episodes with missing start or end date are retained.
#' Logs how many episodes were removed.
#'
#' @param df Data frame with at least start and end date columns.
#' @param logger A \code{log4r} logger object.
#' @param startDateCol Character. Name of the episode start date column (default \code{"final_episode_start_date"}).
#' @param endDateCol Character. Name of the episode end date column (default \code{"final_episode_end_date"}).
#' @param maxDays Numeric. Maximum allowed duration in days (default 308).
#' @return The data frame with long episodes removed.
#' @noRd
removeLongPregnancies <- function(df,
                                  logger,
                                  startDateCol = "final_episode_start_date",
                                  endDateCol = "final_episode_end_date",
                                  maxDays = 308) {
  checkmate::assertDataFrame(df, min.rows = 0)
  checkmate::assertClass(logger, "logger", null.ok = FALSE)
  checkmate::assertString(startDateCol)
  checkmate::assertString(endDateCol)
  checkmate::assertNumber(maxDays, lower = 0, finite = TRUE)
  stopifnot(
    startDateCol %in% names(df),
    endDateCol %in% names(df)
  )

  nBefore <- nrow(df)
  out <- df %>%
    dplyr::mutate(
      .days = as.numeric(as.Date(.data[[endDateCol]]) - as.Date(.data[[startDateCol]]))
    ) %>%
    dplyr::filter(is.na(.data$.days) | .data$.days <= .env$maxDays) %>%
    dplyr::select(-".days")
  nRemoved <- nBefore - nrow(out)

  if (nRemoved > 0) {
    log4r::info(
      logger,
      sprintf(
        "removeLongPregnancies: removed %d episode(s) with period length greater than %d days.",
        nRemoved,
        maxDays
      )
    )
  }

  out
}

# ============================================================
# Remove overlapping episodes from final ESD output
# ============================================================

#' Outcome category rank for prioritization (HIP hierarchy)
#' Lower value = higher priority. LB (live birth) first, then SB, DELIV, ECT, AB, SA, PREG.
#' @noRd
outcomeRank <- function(cat) {
  dplyr::case_when(
    cat == "LB" ~ 1L,
    cat == "SB" ~ 2L,
    cat == "DELIV" ~ 3L,
    cat == "ECT" ~ 4L,
    cat == "AB" ~ 5L,
    cat == "SA" ~ 6L,
    cat == "PREG" ~ 7L,
    TRUE ~ 99L
  )
}

#' Find connected components of overlapping intervals within a set of (start, end) pairs.
#' Returns integer vector of group id (1, 2, ...) per row.
#' Uses O(n log n) sweep by start date instead of O(n^2) pairwise union-find.
#' @noRd
overlapGroups <- function(start, end) {
  n <- length(start)
  if (n == 0) return(integer(0))
  groups <- integer(n)
  valid <- !is.na(start) & !is.na(end)
  if (any(valid)) {
    idx <- seq_len(n)
    valid_idx <- idx[valid]
    o <- order(start[valid_idx], end[valid_idx])
    ordered_idx <- valid_idx[o]
    current_group_id <- 1L
    first_i <- ordered_idx[1L]
    groups[first_i] <- current_group_id
    current_group_max_end <- end[first_i]
    if (length(ordered_idx) > 1L) {
      for (k in 2:length(ordered_idx)) {
        i <- ordered_idx[k]
        s_i <- start[i]
        e_i <- end[i]
        if (s_i < current_group_max_end) {
          groups[i] <- current_group_id
          if (e_i > current_group_max_end) {
            current_group_max_end <- e_i
          }
        } else {
          current_group_id <- current_group_id + 1L
          groups[i] <- current_group_id
          current_group_max_end <- e_i
        }
      }
    }
    max_valid_group <- current_group_id
  } else {
    max_valid_group <- 0L
  }
  if (any(!valid)) {
    na_idx <- which(!valid)
    groups[na_idx] <- max_valid_group + seq_along(na_idx)
  }
  groups
}

#' Remove overlapping episodes from final ESD output
#'
#' Within each person, groups episodes whose date ranges overlap and keeps a single
#' episode per group. Prioritization: (1) \code{hip_flag == 1} over 0; (2) among HIP
#' episodes, HIP outcome hierarchy (live birth > stillbirth > delivery > ectopic >
#' abortion/miscarriage > ongoing); (3) \code{esd_outcome_match == 1} (concordant);
#' (4) better ESD precision (lower \code{esd_precision_days}); (5) gestational week
#' evidence (\code{esd_gw_flag}); (6) earliest \code{final_episode_end_date}; (7) first row.
#'
#' @param esdDf Data frame of final ESD output (e.g. from \code{runEsd} or
#'   \code{final_pregnancy_episodes.rds}) with at least \code{person_id},
#'   \code{final_episode_start_date}, \code{final_episode_end_date}. Optional
#'   columns used for prioritization: \code{hip_flag}, \code{final_outcome_category},
#'   \code{hip_outcome_category}, \code{esd_outcome_match}, \code{esd_precision_days},
#'   \code{esd_gw_flag}. If \code{hip_flag} is missing it is treated as 0.
#' @param personIdCol Character. Name of the person identifier column (default \code{"person_id"}).
#' @param startDateCol Character. Name of the episode start date column (default \code{"final_episode_start_date"}).
#' @param endDateCol Character. Name of the episode end date column (default \code{"final_episode_end_date"}).
#' @return Data frame with the same columns as \code{esdDf}, with overlapping
#'   episodes removed (one episode retained per overlap group per person).
#' @noRd
removeOverlaps <- function(esdDf,
                           personIdCol = "person_id",
                           startDateCol = "final_episode_start_date",
                           endDateCol = "final_episode_end_date") {
  checkmate::assertDataFrame(esdDf, min.rows = 0)
  checkmate::assertString(personIdCol)
  checkmate::assertString(startDateCol)
  checkmate::assertString(endDateCol)
  stopifnot(
    personIdCol %in% names(esdDf),
    startDateCol %in% names(esdDf),
    endDateCol %in% names(esdDf)
  )

  if (nrow(esdDf) == 0) return(esdDf)

  originalCols <- names(esdDf)
  work <- esdDf
  if (!"hip_flag" %in% originalCols) work$hip_flag <- 0L
  if (!"final_outcome_category" %in% originalCols) work$final_outcome_category <- NA_character_
  if (!"hip_outcome_category" %in% originalCols) work$hip_outcome_category <- NA_character_
  if (!"esd_outcome_match" %in% originalCols) work$esd_outcome_match <- NA_integer_
  if (!"esd_precision_days" %in% originalCols) work$esd_precision_days <- NA_integer_
  if (!"esd_gw_flag" %in% originalCols) work$esd_gw_flag <- NA_real_

  df <- work %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .row = dplyr::row_number(),
      .hip_flag = dplyr::coalesce(.data$hip_flag, 0L),
      .outcome_rank = outcomeRank(.data$final_outcome_category),
      .hip_outcome_rank = outcomeRank(.data$hip_outcome_category),
      .esd_match = dplyr::coalesce(.data$esd_outcome_match, 0L),
      .precision = dplyr::if_else(
        is.na(.data$esd_precision_days),
        Inf,
        as.numeric(.data$esd_precision_days)
      ),
      .gw_flag = dplyr::coalesce(as.numeric(.data$esd_gw_flag), 0),
      .end_date = as.Date(.data[[endDateCol]])
    )

  # Compute overlap group per person (use .row to assign back to correct rows)
  by_person <- df %>%
    dplyr::group_by(.data[[personIdCol]]) %>%
    dplyr::group_split()

  group_ids <- integer(nrow(df))
  for (i in seq_along(by_person)) {
    block <- by_person[[i]]
    start <- as.Date(block[[startDateCol]])
    end <- as.Date(block[[endDateCol]])
    grp <- overlapGroups(start, end)
    group_ids[block$.row] <- grp
  }

  df <- df %>%
    dplyr::mutate(.overlap_group = .env$group_ids)

  # Within each person and overlap group, keep best row (arrange by priority, then slice first)
  kept <- df %>%
    dplyr::arrange(
      .data[[personIdCol]],
      .data$.overlap_group,
      dplyr::desc(.data$.hip_flag),
      .data$.outcome_rank,
      .data$.hip_outcome_rank,
      dplyr::desc(.data$.esd_match),
      .data$.precision,
      dplyr::desc(.data$.gw_flag),
      .data$.end_date,
      .data$.row
    ) %>%
    dplyr::group_by(.data[[personIdCol]], .data$.overlap_group) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::starts_with(".")) %>%
    dplyr::select(dplyr::all_of(originalCols))

  dplyr::as_tibble(kept)
}

# ============================================================
# ESD concept counts (record_count, person_count) for export
# ============================================================

#' Write ESD concept counts (records and persons per concept) to CSV.
#' Pulls records from condition, observation, measurement, procedure in the
#' study window and aggregates by ESD concept ID.
#' @noRd
writeEsdConceptCounts <- function(cdm, startDate, endDate, outputFolder) {
  esdConcepts <-
    system.file("concepts", "ESD_concepts.xlsx", package = "PregnancyIdentifier", mustWork = TRUE) %>%
    readxl::read_xlsx() %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(esd_concept_id = as.integer(.data$esd_concept_id))
  conceptIds <- as.integer(esdConcepts$esd_concept_id)
  conceptsToSearch <- dplyr::tibble(concept_id = conceptIds)

  pullDomainCounts <- function(domainDf, domainIdCol, domainDateCol) {
    domainDf %>%
      dplyr::filter(
        .data[[domainIdCol]] %in% .env$conceptIds,
        .data[[domainDateCol]] >= .env$startDate,
        .data[[domainDateCol]] <= .env$endDate
      ) %>%
      dplyr::transmute(
        person_id = .data$person_id,
        concept_id = .data[[domainIdCol]]
      )
  }

  esdRecords <- list(
    pullDomainCounts(cdm$condition_occurrence, "condition_concept_id", "condition_start_date"),
    pullDomainCounts(cdm$procedure_occurrence, "procedure_concept_id", "procedure_date"),
    pullDomainCounts(cdm$observation, "observation_concept_id", "observation_date"),
    pullDomainCounts(cdm$measurement, "measurement_concept_id", "measurement_date")
  ) %>%
    purrr::reduce(dplyr::union_all)

  esdCounts <- esdRecords %>%
    dplyr::group_by(.data$concept_id) %>%
    dplyr::summarise(
      record_count = dplyr::n(),
      person_count = dplyr::n_distinct(.data$person_id),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::rename(esd_concept_id = "concept_id") %>%
    dplyr::left_join(
      dplyr::select(esdConcepts, "esd_concept_id", dplyr::any_of("esd_concept_name")),
      by = "esd_concept_id"
    )
  if (!"esd_concept_name" %in% names(esdCounts)) {
    esdCounts$esd_concept_name <- NA_character_
  }
  esdCounts <- esdCounts %>%
    dplyr::select("esd_concept_id", "esd_concept_name", "record_count", "person_count")
  utils::write.csv(esdCounts, file.path(outputFolder, "esd_concept_counts.csv"), row.names = FALSE)
  invisible(NULL)
}

# ============================================================
# Pull gestational timing concepts around each episode
# ============================================================

# Pull domain rows that match the gestational concept set, restricted to the
# study date window and to persons in the HIPPS cohort. Returns a *local*
# data.frame with columns: person_id, domain_concept_start_date,
# domain_concept_id, value_col (or NA when valueCol is NULL).
#
# Replaces the previous DB-side non-equi date-range join. The interval join
# (concept date within episode window) is performed locally via
# data.table::foverlaps which is O(N log N) versus the DB's nested-loop or
# range-broadcast plans on tables with >10^7 rows.
collectDomainForJoin <- function(domainDf, domainIdCol, domainDateCol, valueCol,
                                 conceptIds, personIdsTbl, startDate, endDate) {
  q <- domainDf %>%
    dplyr::filter(
      .data[[domainIdCol]] %in% .env$conceptIds,
      .data[[domainDateCol]] >= startDate,
      .data[[domainDateCol]] <= endDate
    ) %>%
    dplyr::semi_join(personIdsTbl, by = "person_id")
  if (is.null(valueCol)) {
    out <- q %>% dplyr::transmute(
      .data$person_id,
      domain_concept_start_date = .data[[domainDateCol]],
      domain_concept_id = .data[[domainIdCol]]
    ) %>% dplyr::collect()
    out$value_col <- NA_character_
    return(out)
  }
  q %>% dplyr::transmute(
    .data$person_id,
    domain_concept_start_date = .data[[domainDateCol]],
    domain_concept_id = .data[[domainIdCol]],
    value_col = .data[[valueCol]]
  ) %>% dplyr::collect()
}

# Local interval join: for each domain event, find episodes (per person) whose
# (start_date, merge_episode_end) window contains the event date. Returns one
# row per (event, episode) match (matching the previous SQL non-equi join's
# many-to-many semantics).
intervalJoinLocal <- function(domainDf, personIdListLocal) {
  emptyOut <- data.table::data.table(
    person_id = integer(),
    domain_concept_start_date = as.Date(character()),
    domain_concept_id = integer(),
    domain_concept_name = character(),
    value_col = character(),
    start_date = as.Date(character()),
    merge_episode_end = as.Date(character()),
    merge_episode_number = integer()
  )
  if (nrow(domainDf) == 0L || nrow(personIdListLocal) == 0L) return(emptyOut)

  dt_dom <- data.table::as.data.table(domainDf)
  dt_dom[, domain_concept_start_date := as.Date(domain_concept_start_date)]
  dt_dom[, .ev_end := domain_concept_start_date]

  dt_epi <- data.table::as.data.table(personIdListLocal)
  dt_epi[, `:=`(
    start_date = as.Date(start_date),
    merge_episode_end = as.Date(merge_episode_end)
  )]
  data.table::setkey(dt_epi, person_id, start_date, merge_episode_end)

  joined <- data.table::foverlaps(
    dt_dom,
    dt_epi,
    by.x = c("person_id", "domain_concept_start_date", ".ev_end"),
    by.y = c("person_id", "start_date", "merge_episode_end"),
    nomatch = NULL
  )
  joined[, .ev_end := NULL]
  joined[]
}

getTimingConcepts <- function(cdm,
                              startDate = as.Date("1900-01-01"),
                              endDate = Sys.Date(),
                              hippsEpisodes,
                              logger) {
  # obtain the gestational timing <= 3 month concept information to use as additional information for precision category designation

  ppsConcepts <-
    system.file("concepts", "PPS_concepts_reviewed1702026.xlsx", package = "PregnancyIdentifier", mustWork = TRUE) %>%
    readxl::read_xlsx()

  esdConcepts <-
    system.file("concepts", "ESD_concepts.xlsx", package = "PregnancyIdentifier", mustWork = TRUE) %>%
    readxl::read_xlsx()

  gestationalAgeConcepts <- utils::read.csv(
    system.file("concepts", "gestational_age_concepts.csv", package = "PregnancyIdentifier", mustWork = TRUE),
    colClasses = c(concept_id = "integer")
  )
  gestationalAgeConceptIds <- as.integer(gestationalAgeConcepts$concept_id)
  # Gestation-at-birth concepts (length of gestation at birth): use value 1-44 as GA weeks like GW concepts
  gestationAtBirthConceptIds <- c(4260747L, 46234792L)
  # LMP/EDD concepts: value_col is a date; use as direct pregnancy start (LMP) or EDD - 280 days (EDD)
  lmpConceptIds <- as.integer(esdConcepts$esd_concept_id[esdConcepts$esd_category == "estConceptionConceptIds"])
  eddConceptIds <- as.integer(esdConcepts$esd_concept_id[esdConcepts$esd_category == "estDeliveryConceptIds"])

  conceptIdsToSearch <- as.integer(c(ppsConcepts$pps_concept_id, esdConcepts$esd_concept_id))
  # Include gestational_age_concepts.csv so GA from measurement (e.g. 3012266 "Gestational age") is pulled
  conceptIdsToSearch <- c(conceptIdsToSearch, gestationalAgeConceptIds)

  # Resolve "gestation period"-named concepts from the OMOP vocabulary plus the
  # explicit concept-id list, and pull both id and name to R. The full concept
  # set (ids + names) is small (typically <1k rows); keeping it local lets us
  # filter each domain by a plain `concept_id %in% ids` and avoids re-running
  # the LIKE scan once per domain join.
  conceptsToSearchLocal <- withTiming("getTimingConcepts: resolve concept set", logger, {
    cdm$concept %>%
      dplyr::filter(
        .data$concept_name %like% "gestation period" |
          .data$concept_id %in% .env$conceptIdsToSearch
      ) %>%
      dplyr::select("concept_id", "concept_name") %>%
      dplyr::collect() %>%
      dplyr::mutate(concept_id = as.integer(.data$concept_id))
  })
  log4r::info(logger, sprintf("[ESD timing] resolved gestational concept set rows: %d", nrow(conceptsToSearchLocal)))
  conceptIdsForFilter <- as.integer(unique(conceptsToSearchLocal$concept_id))

  # Episode windows for the local interval join
  personIdListLocal <- hippsEpisodes %>%
    dplyr::mutate(start_date = pmin(.data$merge_pregnancy_start, .data$merge_episode_start, na.rm = TRUE)) %>%
    dplyr::select("person_id", "start_date", "merge_episode_end", "merge_episode_number") %>%
    dplyr::mutate(
      start_date = as.Date(.data$start_date),
      merge_episode_end = as.Date(.data$merge_episode_end)
    )

  # Push a small per-person filter list to the DB so each domain query can
  # `semi_join` person_id without scanning everyone.
  personIdsForDb <- personIdListLocal %>%
    dplyr::distinct(.data$person_id)
  cdm <- omopgenerics::insertTable(
    cdm, name = "person_id_list", table = personIdsForDb,
    overwrite = TRUE, temporary = FALSE
  )
  personIdsTbl <- cdm$person_id_list

  # Pull each domain pre-filtered to the gestational concepts and HIPPS persons.
  # We collect to R, then run the date-range join locally with foverlaps.
  # condition/procedure: original code used the matched concept name as value_col
  #   (set after binding from the local concepts table)
  # observation: value_as_string ; measurement: value_as_number (cast to char locally)
  condDf <- withTiming("getTimingConcepts: pull condition_occurrence", logger, {
    collectDomainForJoin(
      cdm$condition_occurrence, "condition_concept_id", "condition_start_date",
      valueCol = NULL, conceptIdsForFilter, personIdsTbl, startDate, endDate
    )
  })
  log4r::info(logger, sprintf("[ESD timing] condition_occurrence rows pulled: %d", nrow(condDf)))

  obsDf <- withTiming("getTimingConcepts: pull observation", logger, {
    collectDomainForJoin(
      cdm$observation, "observation_concept_id", "observation_date",
      valueCol = "value_as_string", conceptIdsForFilter, personIdsTbl, startDate, endDate
    )
  })
  log4r::info(logger, sprintf("[ESD timing] observation rows pulled: %d", nrow(obsDf)))

  measDf <- withTiming("getTimingConcepts: pull measurement", logger, {
    df <- collectDomainForJoin(
      cdm$measurement, "measurement_concept_id", "measurement_date",
      valueCol = "value_as_number", conceptIdsForFilter, personIdsTbl, startDate, endDate
    )
    df$value_col <- as.character(df$value_col)
    df
  })
  log4r::info(logger, sprintf("[ESD timing] measurement rows pulled: %d", nrow(measDf)))

  procDf <- withTiming("getTimingConcepts: pull procedure_occurrence", logger, {
    collectDomainForJoin(
      cdm$procedure_occurrence, "procedure_concept_id", "procedure_date",
      valueCol = NULL, conceptIdsForFilter, personIdsTbl, startDate, endDate
    )
  })
  log4r::info(logger, sprintf("[ESD timing] procedure_occurrence rows pulled: %d", nrow(procDf)))

  # Bind domains + attach concept_name from the local concepts table. For
  # condition/procedure rows (where value_col is still NA) populate value_col
  # from the matched concept name, matching the previous behaviour.
  domainDf <- withTiming("getTimingConcepts: bind domains + attach concept_name", logger, {
    bound <- data.table::rbindlist(
      list(condDf, obsDf, measDf, procDf),
      use.names = TRUE, fill = TRUE
    )
    bound[, domain_concept_id := as.integer(domain_concept_id)]
    bound[, value_col := as.character(value_col)]
    cs <- data.table::as.data.table(conceptsToSearchLocal)
    data.table::setnames(cs, c("concept_id", "concept_name"),
                         c("domain_concept_id", "domain_concept_name"))
    bound <- cs[bound, on = "domain_concept_id"]
    bound[is.na(value_col), value_col := domain_concept_name]
    bound
  })
  log4r::info(logger, sprintf("[ESD timing] total domain rows after bind: %d", nrow(domainDf)))

  # Local interval join (replaces the previous DB-side non-equi range join)
  joined <- withTiming("getTimingConcepts: foverlaps interval join", logger, {
    intervalJoinLocal(domainDf, personIdListLocal)
  })
  log4r::info(logger, sprintf("[ESD timing] interval-join output rows: %d", nrow(joined)))

  # Attach min_month / max_month from preg_pps_concepts (small DB pull)
  ppsRanges <- withTiming("getTimingConcepts: pull preg_pps_concepts", logger, {
    cdm$preg_pps_concepts %>%
      dplyr::select("pps_concept_id", min_month = "pps_min_month", max_month = "pps_max_month") %>%
      dplyr::collect() %>%
      dplyr::mutate(pps_concept_id = as.integer(.data$pps_concept_id))
  })

  pregRelatedConcepts <- withTiming("getTimingConcepts: attach pps ranges", logger, {
    out <- merge(
      joined,
      data.table::as.data.table(ppsRanges),
      by.x = "domain_concept_id", by.y = "pps_concept_id",
      all.x = TRUE
    )
    dplyr::as_tibble(out)
  })

  # Clean/parse gestational-week style values and compute extrapolated pregnancy starts.
  # Non-numeric or invalid values coerce to NA; suppressWarnings hides "NAs introduced by coercion".
  withTiming("getTimingConcepts: parse values + extrapolate", logger, {
  suppressWarnings({

  pregRelatedConcepts %>%
    dplyr::mutate(
      domain_value = stringr::str_replace(.data$value_col, "\\|text_result_val:", ""),
      domain_value = stringr::str_replace(.data$domain_value, "\\|mapped_text_result_val:", ""),
      domain_value = stringr::str_replace(.data$domain_value, "Gestation period, ", ""),
      domain_value = stringr::str_replace(.data$domain_value, "gestation period, ", ""),
      domain_value = stringr::str_replace(.data$domain_value, " weeks", ""),
      domain_value = as.integer(as.numeric(.data$domain_value))
    ) %>%
    dplyr::mutate(
      keep_value = dplyr::if_else(
        (
          stringr::str_detect(tolower(.data$domain_concept_name), "gestation period,") |
            stringr::str_detect(tolower(.data$domain_concept_name), "gestational age") |
            .data$domain_concept_id %in% .env$gestationalAgeConceptIds |
            .data$domain_concept_id %in% .env$gestationAtBirthConceptIds
        ) &
          (.data$domain_value <= 44 & .data$domain_value > 0),
        1, 0
      ),
      extrapolated_preg_start = dplyr::if_else(
        .data$keep_value == 1,
        .data$domain_concept_start_date - (.data$domain_value * 7),
        lubridate::NA_Date_
      )
    ) %>%
    # Parse LMP/EDD date values: use as pregnancy start (LMP) or EDD - 280 days (EDD)
    dplyr::mutate(
      value_col_clean = stringr::str_replace(.data$value_col, "\\|text_result_val:", ""),
      value_col_clean = stringr::str_replace(.data$value_col_clean, "\\|mapped_text_result_val:", ""),
      parsed_date = suppressWarnings(as.Date(.data$value_col_clean, "%Y-%m-%d")),
      parsed_date = dplyr::if_else(
        is.na(.data$parsed_date),
        suppressWarnings(as.Date(.data$value_col_clean, "%Y/%m/%d")),
        .data$parsed_date
      ),
      extrapolated_preg_start = dplyr::if_else(
        .data$domain_concept_id %in% .env$lmpConceptIds & !is.na(.data$parsed_date),
        .data$parsed_date,
        .data$extrapolated_preg_start
      ),
      extrapolated_preg_start = dplyr::if_else(
        .data$domain_concept_id %in% .env$eddConceptIds & !is.na(.data$parsed_date),
        .data$parsed_date - 280L,
        .data$extrapolated_preg_start
      ),
      keep_value = dplyr::if_else(
        (.data$domain_concept_id %in% .env$lmpConceptIds | .data$domain_concept_id %in% .env$eddConceptIds) &
          !is.na(.data$parsed_date),
        1L,
        .data$keep_value
      ),
      date_type = dplyr::case_when(
        .data$domain_concept_id %in% .env$lmpConceptIds & !is.na(.data$parsed_date) ~ "LMP",
        .data$domain_concept_id %in% .env$eddConceptIds & !is.na(.data$parsed_date) ~ "EDD",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-"value_col_clean", -"parsed_date")
  })
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
  # intervals: list of length-2 vectors (start,end) or a single such vector.
  # Hot path — called once per episode that has any GR3m intervals. Use base R
  # vectors throughout; avoid data.frame / dplyr overhead per call.
  if (length(intervals) == 1L) {
    mat <- matrix(intervals[[1]], ncol = 2)
  } else {
    mat <- do.call(rbind, intervals)
  }
  v1 <- as.Date(mat[, 1], format = "%Y-%m-%d")
  v2 <- as.Date(mat[, 2], format = "%Y-%m-%d")
  ord <- order(v1)
  v1 <- v1[ord]
  v2 <- v2[ord]
  n <- length(v1)
  if (n == 0L) return(c(NA, NA, NA, NA))
  if (n == 1L) return(c(v2[1], v1[1], v2[1], v1[1]))

  # Vectorized overlap-count: original semantics — m's interval [m1, m2] is
  # counted against j's [j1, j2] when m's start or end equals either of j's
  # endpoints, OR when m's start or end falls strictly inside j's open interval.
  m1 <- matrix(v1, nrow = n, ncol = n)
  m2 <- matrix(v2, nrow = n, ncol = n)
  j1 <- matrix(v1, nrow = n, ncol = n, byrow = TRUE)
  j2 <- matrix(v2, nrow = n, ncol = n, byrow = TRUE)
  overlap <- (m1 == j2) | (m1 == j1) | (m2 == j2) | (m2 == j1) |
    (m2 < j2 & m2 > j1) | (m1 < j2 & m1 > j1)
  diag(overlap) <- FALSE
  overlapCount <- colSums(overlap)

  countsQ1 <- stats::quantile(overlapCount, 0.25, names = FALSE)
  countsQ3 <- stats::quantile(overlapCount, 0.75, names = FALSE)
  outlierThreshold <- abs(countsQ1 - (countsQ3 - countsQ1) * 1.5)

  keep <- if (outlierThreshold == 0) overlapCount > outlierThreshold else overlapCount >= outlierThreshold
  if (any(keep)) {
    fOrd <- order(-overlapCount[keep])
    fv1 <- v1[keep][fOrd]
    fv2 <- v2[keep][fOrd]
  } else {
    fv1 <- fv2 <- as.Date(character(0))
  }
  N <- length(fv1)

  if (N == 1L) return(c(fv2[1], fv1[1], fv2[1], fv1[1]))
  if (N == 0L) {
    top <- which.max(overlapCount)
    return(c(v2[top], v1[top], v2[top], v1[top]))
  }

  rangeStart <- fv1[1]; rangeEnd <- fv2[1]
  interStart <- fv1[1]; interEnd <- fv2[1]
  for (i in 2:N) {
    if (fv1[i] < rangeStart) rangeStart <- fv1[i]
    if (fv2[i] > rangeEnd)   rangeEnd   <- fv2[i]
    if ((fv2[i] < interEnd) && (fv2[i] > interStart)) interEnd <- fv2[i]
    if ((fv1[i] > interStart) && (fv1[i] < interEnd)) interStart <- fv1[i]
  }
  c(rangeEnd, rangeStart, interEnd, interStart)
}

# check for GW concept overlap to the intervals
removeGWOutliers <- function(lolOfGwConcepts) {
  dates <- as.Date(unlist(lolOfGwConcepts))
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
assignPrecisionCategory <- function(precisionDays) {
  dplyr::case_when(
    precisionDays == -1 ~ "week_poor-support",
    precisionDays >= 0  & precisionDays <= 7   ~ "week",
    precisionDays > 7   & precisionDays <= 14  ~ "two-week",
    precisionDays > 14  & precisionDays <= 21  ~ "three-week",
    precisionDays > 21  & precisionDays <= 28  ~ "month",
    precisionDays > 28  & precisionDays <= 56  ~ "two-month",
    precisionDays > 56  & precisionDays <= 84  ~ "three-month",
    TRUE ~ "non-specific"
  )
}

# applying udf to the date array column to obtain the new column 'final_timing_info'
# (a list of [inferred_episode_start, precision_days, precision_category]) for each row
getGtTiming <- function(datesList) {

  # Iterate over the datesList
  timingArr <- purrr::map(datesList[purrr::map_lgl(datesList, validate)], sort)
  gwList   <- datesList[purrr::map_lgl(datesList, function(x) length(x) == 1)]
  gr3mList <- datesList[purrr::map_lgl(datesList, function(x) length(x) == 2)]

  inferredStartDate  <- as.Date("2000-01-01", format = "%Y-%m-%d")
  precisionDays       <- 999
  precisionCategory   <- "-999"
  intervalsCount       <- 0
  majorityOverlapCount <- 0

  # get length of list with GR3m ranges
  N <- length(gr3mList)
  commonGr3mInterval <- if (N > 0) findIntersection(gr3mList) else NULL

  plausibleDays <- 0
  maxRangeDays  <- 0
  rangeS <- rangeE <- intervalS <- intervalE <- daterangeMidpoint <- NULL

  if (!is.null(commonGr3mInterval)) {
    rangeE    <- as.Date(commonGr3mInterval[1], format = "%Y-%m-%d") # end date of range
    rangeS    <- as.Date(commonGr3mInterval[2], format = "%Y-%m-%d") # start date of range
    intervalE <- as.Date(commonGr3mInterval[3], format = "%Y-%m-%d") # end date of intersection
    intervalS <- as.Date(commonGr3mInterval[4], format = "%Y-%m-%d") # start date of intersection

    plausibleDays <- as.numeric(difftime(intervalE, intervalS, units = "days"))
    maxRangeDays  <- as.numeric(difftime(rangeE, rangeS, units = "days"))
    daterangeMidpoint <- intervalS + lubridate::days(as.integer(plausibleDays / 2))

    # Utilize the overlap more when it gets narrowed down to < 1 week by taking the midpoint
    # and adding 3 days either side (otherwise unlikely to overlap much with GW concepts and thus will be ignored)
    if (plausibleDays < 7) {
      intervalS <- daterangeMidpoint - lubridate::days(3)
      intervalE <- daterangeMidpoint + lubridate::days(3)
      plausibleDays <- 6
    }
  }

  # there are week-level estimates
  if (length(gwList) > 0) {
    # If GR3m interval exists, prefer GW concepts that overlap the GR3m intersection.
    if (!is.null(intervalS)) {
      intervalsCount <- intervalsCount + 1

      gwConceptCount <- length(gwList)
      overlappingGw <- list()

      for (gwItem in gwList) {
        gwDate <- gwItem[[1]]
        if (gwDate >= intervalS && gwDate <= intervalE) overlappingGw <- c(overlappingGw, gwDate)
      }

      percOverlapping <- (length(overlappingGw) / gwConceptCount) * 100

      if (percOverlapping > 50) {
        majorityOverlapCount <- majorityOverlapCount + 1
        filtDates <- removeGWOutliers(overlappingGw)
      } else {
        filtDates <- removeGWOutliers(gwList)
        if (length(filtDates) == 1) precisionDays <- -1 # only one GW concept and it doesn't overlap
      }

      inferredStartDate <- filtDates[[1]] # earliest inferred start (from highest gest week)
      precisionDays <- if (precisionDays == -1) -1 else as.numeric(max(filtDates) - min(filtDates))

    } else {
      # GW only (no GR3m information)
      filtDates <- removeGWOutliers(gwList)
      inferredStartDate <- filtDates[[1]]
      precisionDays <- as.numeric(max(filtDates) - min(filtDates))
      if (length(filtDates) == 1) precisionDays <- -1
    }
  } else {
    # GR3m only (no GW concepts)
    inferredStartDate <- daterangeMidpoint
    precisionDays <- maxRangeDays
  }

  precisionCategory <- assignPrecisionCategory(precisionDays)

  list(
    inferred_start_date = inferredStartDate,
    precision_days = precisionDays,
    precision_category = precisionCategory,
    intervals_count = intervalsCount,
    majority_overlap_count = majorityOverlapCount
  )
}

episodesWithGestationalTimingInfo <- function(getTimingConceptsDf, logger) {
  # add on either GW or GR3m designation depending on whether the concept is present
  # GW concept IDs: from ESD_concepts.xlsx column is_gw_concept
  esdConcepts <-
    system.file("concepts", "ESD_concepts.xlsx", package = "PregnancyIdentifier", mustWork = TRUE) %>%
    readxl::read_xlsx()

  gwConceptIdsFromEsd <- as.integer(esdConcepts$esd_concept_id[as.logical(esdConcepts$is_gw_concept)])

  if (nrow(getTimingConceptsDf) == 0) {
    log4r::info(logger, sprintf("Number of episodes with GR3m intervals: %s", 0))
    log4r::info(logger, sprintf("Percent of cases that contain a GR3m intersection that ALSO have majority GW overlap:: %s", 0))

    return(dplyr::tibble(
      person_id = integer(0),
      merge_episode_number = integer(0),
      gt_info_list = character(0),
      gw_flag = numeric(0),
      gr3m_flag = numeric(0),
      inferred_episode_start = as.Date(character(0)),
      precision_days = numeric(0),
      precision_category = character(0),
      intervals_count = numeric(0),
      majority_overlap_count = numeric(0)
    ))
  }

  # Backward compatibility: date_type added when LMP/EDD parsing was implemented
  if (!"date_type" %in% names(getTimingConceptsDf)) {
    getTimingConceptsDf <- getTimingConceptsDf %>%
      dplyr::mutate(date_type = NA_character_)
  }
  # Gestation-at-birth concepts (4260747, 46234792): treat as GW when they have extrapolated_preg_start
  gestationAtBirthConceptIds <- c(4260747L, 46234792L)
  timingDf <- getTimingConceptsDf %>%
    dplyr::mutate(
      domain_concept_id = as.integer(.data$domain_concept_id),
      gt_type = dplyr::case_when(
        .data$date_type %in% c("LMP", "EDD") ~ .data$date_type,
        stringr::str_detect(stringr::str_to_lower(.data$domain_concept_name), "gestation period") |
          .data$domain_concept_id %in% .env$gwConceptIdsFromEsd |
          .data$domain_concept_id %in% .env$gestationAtBirthConceptIds ~ "GW",
        !is.na(.data$min_month) ~ "GR3m",
        TRUE ~ NA_character_
      )
    )

  # Add on the max and min pregnancy start dates predicted by each concept (GR3m only)
  timingDf <- timingDf %>%
    dplyr::mutate(
      min_days_to_pregnancy_start = dplyr::if_else(.data$gt_type == "GR3m", round(.data$min_month * 30.4), NA_real_),
      max_days_to_pregnancy_start = dplyr::if_else(.data$gt_type == "GR3m", round(.data$max_month * 30.4), NA_real_),
      min_pregnancy_start = dplyr::if_else(
        .data$gt_type == "GR3m",
        .data$domain_concept_start_date - as.integer(.data$min_days_to_pregnancy_start),
        lubridate::NA_Date_
      ),
      max_pregnancy_start = dplyr::if_else(
        .data$gt_type == "GR3m",
        .data$domain_concept_start_date - as.integer(.data$max_days_to_pregnancy_start),
        lubridate::NA_Date_
      )
    ) %>%
    dplyr::select(-"min_days_to_pregnancy_start", -"max_days_to_pregnancy_start")

  # Remove type if GW values are null (preserves original logic). LMP/EDD are kept (they use extrapolated_preg_start only).
  timingDf <- timingDf %>%
    dplyr::mutate(
      gt_type = dplyr::case_when(
        .data$gt_type == "GW" & (is.na(.data$domain_value) | is.na(.data$extrapolated_preg_start)) ~ NA_character_,
        TRUE ~ .data$gt_type
      )
    ) %>%
    dplyr::filter(.data$gt_type %in% c("GW", "GR3m", "LMP", "EDD"))

  # Build a typed list-column `gt_event` per row directly (no string round-trip):
  #   - GR3m rows  -> length-2 character vector c(max_pregnancy_start, min_pregnancy_start)
  #   - other rows -> length-1 character vector format(extrapolated_preg_start)
  # Downstream getGtTiming inspects length(x) to discriminate the two cases.
  timingDf <- withTiming("episodesWithGestationalTimingInfo: build gt_event", logger, {
    n <- nrow(timingDf)
    extr_chr <- format(as.Date(timingDf$extrapolated_preg_start))
    mn_chr <- format(as.Date(timingDf$min_pregnancy_start))
    mx_chr <- format(as.Date(timingDf$max_pregnancy_start))
    gt_event <- vector("list", n)
    has_extr <- !is.na(timingDf$extrapolated_preg_start)
    if (any(has_extr)) {
      gt_event[has_extr] <- as.list(extr_chr[has_extr])
    }
    if (any(!has_extr)) {
      gt_event[!has_extr] <- Map(c, mx_chr[!has_extr], mn_chr[!has_extr])
    }
    timingDf$gt_event <- gt_event
    timingDf %>%
      dplyr::mutate(
        # GW concepts roll up to a single name so dedup collapses any overlapping
        # GW codes recorded on the same day.
        domain_concept_name_rollup = dplyr::if_else(
          !is.na(.data$domain_value) & .data$gt_type == "GW",
          "Gestation Week",
          .data$domain_concept_name
        )
      )
  })

  # Dedup so each (person, episode, concept_rollup, concept_start_date, gt_type)
  # contributes one row.
  #
  # Step 1: sort desc by domain_value first so `unique(..., by = ...)` keeps the
  #         highest-week record when multiple GW concepts share a key.
  # Step 2: re-sort by the dedup keys themselves. This matches dplyr's
  #         group_by + slice(1) + ungroup post-condition (rows sorted by group
  #         keys), which downstream code relies on: getGtTiming picks
  #         filtDates[[1]] as the inferred start, so the row order determines
  #         which gestational-week record anchors the episode start.
  timingDf <- withTiming("episodesWithGestationalTimingInfo: dedup by (person, episode, concept, date, type)", logger, {
    dt <- data.table::as.data.table(timingDf)
    data.table::setorderv(dt, c("person_id", "merge_episode_number", "domain_value"), order = c(1L, 1L, -1L), na.last = TRUE)
    dt <- unique(dt, by = c("person_id", "merge_episode_number", "domain_concept_name_rollup", "domain_concept_start_date", "gt_type"))
    data.table::setorderv(dt, c("person_id", "merge_episode_number", "domain_concept_name_rollup", "domain_concept_start_date", "gt_type"))
    dt
  })
  log4r::info(logger, sprintf("[ESD timing] timingDf rows after dedup: %d", nrow(timingDf)))

  # Group by (person, episode), collect gt_events into a list, and call
  # getGtTiming via purrr::map (much cheaper than dplyr::rowwise).
  summaryDf <- withTiming("episodesWithGestationalTimingInfo: per-episode summarise + getGtTiming map", logger, {
    grouped <- timingDf[, list(
      gt_info_list = list(gt_event),
      gw_flag  = as.numeric(any(gt_type == "GW")),
      gr3m_flag = as.numeric(any(gt_type == "GR3m"))
    ), by = .(person_id, merge_episode_number)]
    log4r::info(logger, sprintf("[ESD timing] episodes to process via getGtTiming: %d", nrow(grouped)))

    final_timing_info <- purrr::map(grouped$gt_info_list, getGtTiming)
    inferredNum <- vapply(final_timing_info, function(x) {
      v <- x$inferred_start_date
      if (is.null(v) || length(v) == 0L) NA_real_ else as.numeric(v)
    }, numeric(1))
    grouped[, `:=`(
      inferred_episode_start = as.Date(inferredNum, origin = "1970-01-01"),
      precision_days = vapply(final_timing_info, function(x) as.numeric(x$precision_days), numeric(1)),
      precision_category = vapply(final_timing_info, function(x) as.character(x$precision_category), character(1)),
      intervals_count = vapply(final_timing_info, function(x) as.numeric(x$intervals_count), numeric(1)),
      majority_overlap_count = vapply(final_timing_info, function(x) as.numeric(x$majority_overlap_count), numeric(1))
    )]
    dplyr::as_tibble(grouped[, .(
      person_id, merge_episode_number, gt_info_list, gw_flag, gr3m_flag,
      inferred_episode_start, precision_days, precision_category,
      intervals_count, majority_overlap_count
    )])
  })

  # print the GW and GR3m concept overlap information to log
  majorityOverlapCountTotal <- sum(summaryDf$majority_overlap_count, na.rm = TRUE)
  intervalsCountTotal <- sum(summaryDf$intervals_count, na.rm = TRUE)
  percMajority <- if (intervalsCountTotal > 0) (majorityOverlapCountTotal / intervalsCountTotal) * 100 else 0

  log4r::info(logger, sprintf("Number of episodes with GR3m intervals: %s", intervalsCountTotal))
  log4r::info(logger, sprintf(
    "Percent of cases that contain a GR3m intersection that ALSO have majority GW overlap:: %s",
    percMajority
  ))

  summaryDf
}

mergedEpisodesWithMetadata <- function(episodesWithGestationalTimingInfoDf,
                                       hippsEpisodes,
                                       termMaxMin,
                                       logger) {
  # Add other pregnancy and demographic related info for each episode.
  # termMaxMin: collected preg_matcho_term_durations (no DB access).
  if (nrow(hippsEpisodes) == 0) {
    log4r::info(logger, "No HIPPS episodes; returning 0-row final schema.")
    return(emptyFinalPregnancyEpisodes())
  }

  timingDf <- episodesWithGestationalTimingInfoDf %>%
    dplyr::select(-"gt_info_list")

  finalDf <- hippsEpisodes %>%
    dplyr::left_join(timingDf, by = c("person_id", "merge_episode_number")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      # Add missing gw_flag and gr3m_flag
      gw_flag  = dplyr::coalesce(.data$gw_flag,  0),
      gr3m_flag = dplyr::coalesce(.data$gr3m_flag, 0)
    )

  # Check if categories match between algorithms and dates are within 14 days of each other for outcomes only
  finalDf <- finalDf %>%
    dplyr::mutate(
      outcome_match = dplyr::case_when(
        .data$hip_outcome_category == .data$pps_outcome_category &
        .data$hip_outcome_category != "PREG" &
        abs(as.numeric(difftime(.data$hip_end_date, .data$pps_end_date, units = "days"))) <= 14 ~ 1,
        .data$hip_outcome_category == "PREG" & .data$pps_outcome_category == "PREG" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$merge_episode_number, .by_group = TRUE) %>%
    dplyr::mutate(next_hip_outcome = dplyr::lead(.data$hip_outcome_category)) %>%
    dplyr::ungroup()

  # If categories don't match, take the category that occurs second (outcome category from HIP algorithm is prioritized)
  finalDf <- finalDf %>%
    dplyr::mutate(
      final_outcome_category = dplyr::case_when(
        .data$outcome_match == 1 ~ .data$hip_outcome_category,

        .data$outcome_match == 0 & is.na(.data$pps_outcome_category) ~ .data$hip_outcome_category,
        .data$outcome_match == 0 & is.na(.data$hip_outcome_category) ~ .data$pps_outcome_category,

        # if they don't match, but the hip end date is not within 7 days before the PPS outcome
        # add: go with HIP if the PPS is the next one and there's sufficient separation
        .data$outcome_match == 0 &
          .data$hip_outcome_category != "PREG" &
          .data$pps_outcome_category != "PREG" &
          !is.na(.data$next_hip_outcome) &
          .data$pps_outcome_category == .data$next_hip_outcome &
          .data$hip_end_date <= .data$pps_end_date - lubridate::days(14) ~ .data$hip_outcome_category,

        # but otherwise go with PPS
        .data$outcome_match == 0 &
          .data$hip_outcome_category != "PREG" &
          .data$pps_outcome_category != "PREG" &
          .data$hip_end_date <= .data$pps_end_date - lubridate::days(7) ~ .data$pps_outcome_category,

        # or if they're similar timing go with HIP
        TRUE ~ .data$hip_outcome_category
      ),
      # If categories don't match, take the end date that occurs second (outcome date from HIP is prioritized)
      inferred_episode_end = dplyr::case_when(
        .data$outcome_match == 1 ~ .data$hip_end_date,
        .data$outcome_match == 0 & is.na(.data$pps_outcome_category) ~ .data$hip_end_date,
        .data$outcome_match == 0 & is.na(.data$hip_outcome_category) ~ .data$pps_end_date,

        .data$outcome_match == 0 &
          .data$hip_outcome_category != "PREG" &
          .data$pps_outcome_category != "PREG" &
          !is.na(.data$next_hip_outcome) &
          .data$pps_outcome_category == .data$next_hip_outcome &
          .data$hip_end_date <= .data$pps_end_date - lubridate::days(14) ~ .data$hip_end_date,

        .data$outcome_match == 0 &
          .data$hip_outcome_category != "PREG" &
          .data$pps_outcome_category != "PREG" &
          .data$hip_end_date <= .data$pps_end_date - lubridate::days(7) ~ .data$pps_end_date,

        !is.na(.data$hip_end_date) ~ .data$hip_end_date,
        !is.na(.data$pps_end_date) ~ .data$pps_end_date
      )
    )

  # Join with term_max_min data frame and drop 'retry' column.
  # When final_outcome_category is missing from Matcho (e.g. PREG), coalesce min_term/max_term
  # so the start-date fallback and precision_days still work.
  finalDf <- finalDf %>%
    dplyr::left_join(termMaxMin, by = c("final_outcome_category" = "category")) %>%
    dplyr::select(-"retry") %>%
    dplyr::mutate(
      min_term = dplyr::coalesce(.data$min_term, 140),
      max_term = dplyr::coalesce(.data$max_term, 308),
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
          .data$gestational_age_days_calculated <= 308 ~ 1,
        TRUE ~ 0
      ),
      # Add outcome concordance score - 2 highly concordant, 1 somewhat concordant, 0 not accurate/not enough info
      outcome_concordance_score = dplyr::case_when(
        .data$outcome_match == 1 & .data$term_duration_flag == 1 & .data$gw_flag == 1 ~ 2,
        .data$outcome_match == 0 & .data$term_duration_flag == 1 & .data$gw_flag == 1 ~ 1,
        TRUE ~ 0
      ),
      # Calculate preterm status from calculation
      preterm_status_from_calculation = dplyr::if_else(.data$gestational_age_days_calculated < 259, 1, 0)
    ) %>%
    dplyr::select(-"min_term", -"max_term")

  # Print time period checks
  min_episode_date  <- min(finalDf$merge_episode_start, na.rm = TRUE)
  max_episode_date  <- max(finalDf$merge_episode_end, na.rm = TRUE)
  min_preg_date     <- min(finalDf$inferred_episode_start, na.rm = TRUE)
  max_preg_date     <- max(finalDf$inferred_episode_end, na.rm = TRUE)

  log4r::info(logger, sprintf("Min episode start date: %s", min_episode_date))
  log4r::info(logger, sprintf("Max episode end date: %s", max_episode_date))
  log4r::info(logger, sprintf("Min pregnancy start date: %s", min_preg_date))
  log4r::info(logger, sprintf("Max pregnancy end date: %s", max_preg_date))

  # One row per distinct episode (person + end date + outcome); collapse duplicates from merge/join.
  # Use minimal key + .keep_all = TRUE so export columns (hip_end_date, pps_end_date, etc.) are retained.
  finalDf <- finalDf %>%
    dplyr::distinct(
      .data$person_id,
      .data$inferred_episode_end,
      .data$final_outcome_category,
      .keep_all = TRUE
    )

  finalDf
}

# Validate and normalize minimal cohort (person_id, inferred_episode_end) before upload.
# Ensures date column is Date type and within [dateMin, dateMax]; person_id integer.
# Rows with dates outside the range are dropped and a WARN log entry is written; returns validated tibble.
validateDeliveryModeCohort <- function(df,
                                      logger,
                                      dateCol = "inferred_episode_end",
                                      dateMin = as.Date("1900-01-01"),
                                      dateMax = Sys.Date()) {
  checkmate::assertClass(logger, "logger", null.ok = FALSE)
  checkmate::assertDate(dateMin, len = 1)
  checkmate::assertDate(dateMax, len = 1)
  if (dateMin > dateMax) {
    rlang::abort("validateDeliveryModeCohort: dateMin must be <= dateMax")
  }
  out <- dplyr::as_tibble(df)
  if (!"person_id" %in% names(out)) {
    rlang::abort("validateDeliveryModeCohort: required column 'person_id' is missing")
  }
  if (!dateCol %in% names(out)) {
    rlang::abort(sprintf("validateDeliveryModeCohort: required column '%s' is missing", dateCol))
  }
  if (!is.numeric(out$person_id) && !inherits(out$person_id, "integer64")) {
    rlang::abort("validateDeliveryModeCohort: person_id must be numeric or integer64")
  }
  out <- out %>%
    dplyr::mutate(
      "{dateCol}" := as.Date(.data[[dateCol]])
    )
  dateVals <- out[[dateCol]]
  outOfRange <- !is.na(dateVals) & (dateVals < dateMin | dateVals > dateMax)
  if (any(outOfRange, na.rm = TRUE)) {
    n <- sum(outOfRange, na.rm = TRUE)
    invalidDates <- sort(unique(dateVals[outOfRange]))
    invalidStr <- paste(format(invalidDates), collapse = ", ")
    if (length(invalidDates) > 10) {
      invalidStr <- paste0(paste(format(invalidDates[1:10]), collapse = ", "), " ... and ", length(invalidDates) - 10, " more")
    }
    log4r::warn(logger, sprintf(
      "%d row(s) dropped with %s outside [%s, %s]. Invalid dates: %s",
      n, dateCol, format(dateMin), format(dateMax), invalidStr
    ))
    out <- out[!outOfRange, ]
  }
  out
}

# Add delivery-mode flag/count columns. Uses a minimal cohort table (person_id + index date
# only) so we do not re-upload the full merged data; the DB does the concept intersect and we
# only pull back the aggregated result (no record-level domain download).
addDeliveryMode <- function(cdm, df, logger, intersectWindow = c(-30, 30)) {
  checkmate::assertClass(logger, "logger", null.ok = FALSE)
  dfColNames <- colnames(df)
  colnames(df) <- tolower(dfColNames)

  minimalCohort <- df %>%
    dplyr::select("person_id", "inferred_episode_end") %>%
    dplyr::filter(!is.na(.data$inferred_episode_end)) %>%
    dplyr::distinct()
  if (nrow(minimalCohort) > 0) {
    minimalCohort <- validateDeliveryModeCohort(minimalCohort, logger = logger)
  }
  tableName <- "esd_delivery_mode_cohort"
  cdm <- omopgenerics::insertTable(cdm = cdm, name = tableName, table = minimalCohort, overwrite = TRUE, temporary = FALSE)

  conceptSet <- suppressMessages(
    omopgenerics::importConceptSetExpression(
      path = system.file(package = "PregnancyIdentifier", "concepts/delivery_mode", mustWork = TRUE)
    )
  )
  conceptSet <- suppressMessages(
    omopgenerics::validateConceptSetArgument(conceptSet, cdm = cdm)
  )
  names(conceptSet) <- unlist(lapply(names(conceptSet), FUN = function(name) {
    unlist(strsplit(name, "^\\d+-"))[2]
  }))

  deliveryResult <- suppressWarnings(
    cdm[[tableName]] %>%
      PatientProfiles::addConceptIntersectFlag(
        conceptSet = conceptSet,
        indexDate = "inferred_episode_end",
        window = intersectWindow,
        nameStyle = "{concept_name}_{window_name}"
      ) %>%
      PatientProfiles::addConceptIntersectCount(
        conceptSet = conceptSet,
        indexDate = "inferred_episode_end",
        window = intersectWindow,
        nameStyle = "{concept_name}_{window_name}_count"
      ) %>%
      dplyr::collect()
  )

  deliveryCols <- setdiff(names(deliveryResult), c("person_id", "inferred_episode_end"))
  mergedDf <- df %>%
    dplyr::left_join(
      deliveryResult %>% dplyr::select("person_id", "inferred_episode_end", dplyr::all_of(deliveryCols)),
      by = c("person_id", "inferred_episode_end")
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(deliveryCols), ~ dplyr::coalesce(.x, 0)))
  mergedDf <- dplyr::as_tibble(mergedDf)
  colnames(mergedDf)[1:length(dfColNames)] <- dfColNames
  mergedDf
}
