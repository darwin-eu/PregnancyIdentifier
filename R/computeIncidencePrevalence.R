#' Compute incidence, prevalence, and cohort characteristics of pregnancy episodes
#'
#' After the pregnancy identification pipeline has run, this function loads the
#' final episode-level data, constructs outcome-specific cohort tables (HIP-only,
#' PPS-only, and combined HIPPS windows), generates denominator cohorts
#' stratified by age, and estimates incidence, period prevalence, and summary
#' characteristics.  Results are exported as CSV files suitable for the results
#' Shiny app.
#'
#' Persons with more than one observation period that overlaps a pregnancy
#' episode are excluded to avoid double-counting person-time.
#'
#' Three cohort definitions are created from each pregnancy episode:
#' \describe{
#'   \item{`hipp` (cohort_definition_id = 1)}{Episode window defined by
#'     `merge_pregnancy_start` to `hip_end_date`.}
#'   \item{`pps` (cohort_definition_id = 2)}{Episode window defined by
#'     `merge_pregnancy_start` to `pps_end_date`.}
#'   \item{`hipps` (cohort_definition_id = 3)}{Combined window using
#'     `hip_end_date` when available, falling back to `pps_end_date`.}
#' }
#'
#' Denominator cohorts are generated for females and both sexes, in the
#' following age groups: 0--150, observed-min to observed-max, 12--55, <35,
#' and >=35.
#'
#' Incidence and prevalence are estimated both overall and stratified by
#' pregnancy outcome category (e.g. LB, SB, AB, SA, ECT, DELIV).  This is
#' achieved by creating additional cohort definitions for each
#' window-type-by-outcome combination.
#'
#' @param cdm (`cdm_reference`) A CDM reference created by `CDMConnector`.
#' @param outputFolder (`character(1)`) Directory containing pipeline outputs,
#'   including `final_pregnancy_episodes.rds`.
#' @param exportFolder (`character(1)`) Directory where result CSV files are
#'   written.  Created recursively if it does not exist.
#' @param minCellCount (`integer(1)`) Minimum cell count for suppression when
#'   exporting summarised results.
#' @param logger A `log4r` logger object.  When `NULL` (the default), messages
#'   are printed via `message()`.
#'
#' @details
#' The function writes three CSV files to `exportFolder`:
#' \itemize{
#'   \item `{date}_{cdm_name}_incidence.csv` -- incidence estimates (overall
#'     and yearly).
#'   \item `{date}_{cdm_name}_prevalence.csv` -- period prevalence estimates
#'     (overall and yearly).
#'   \item `{date}_{cdm_name}_characteristics.csv` -- cohort characteristics
#'     stratified by age group and final outcome category.
#' }
#'
#' @return Invisibly returns `NULL`.  Called for its side effect of writing
#'   result CSV files.
#'
#' @noRd
computeIncidencePrevalence <- function(cdm,
                                       outputFolder,
                                       exportFolder,
                                       minCellCount,
                                       logger = NULL) {

  logMsg <- function(msg) {
    if (!is.null(logger)) {
      log4r::info(logger, msg)
    } else {
      message(msg)
    }
  }

  dir.create(exportFolder, showWarnings = FALSE, recursive = TRUE)

  # -- Load final pregnancy episodes ------------------------------------------
  identifiedPregnancies <- readRDS(
    file.path(outputFolder, "final_pregnancy_episodes.rds")
  )
  names(identifiedPregnancies) <- tolower(names(identifiedPregnancies))

  cdm <- omopgenerics::insertTable(
    cdm, "identified_pregnancies", identifiedPregnancies, overwrite = TRUE, temporary = FALSE
  )

  # -- Exclude persons with >1 observation period per episode -----------------
  toExclude <- cdm$observation_period %>%
    dplyr::right_join(cdm$identified_pregnancies, by = "person_id") %>%
    dplyr::group_by(.data$person_id, .data$merge_episode_number) %>%
    dplyr::count() %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::collect()

  logMsg(sprintf(
    "Excluding %s persons with multiple observation periods",
    nrow(toExclude)
  ))

  identifiedPregnancies <- identifiedPregnancies %>%
    dplyr::filter(!.data$person_id %in% toExclude$person_id)

  # -- Add age at pregnancy start ---------------------------------------------
  cdm$res_age <- cdm$person %>%
    dplyr::select("person_id", "gender_concept_id", "birth_datetime") %>%
    dplyr::right_join(cdm$identified_pregnancies, by = "person_id") %>%
    PatientProfiles::addAge(indexDate = "final_episode_start_date") %>%
    dplyr::rename(age_pregnancy_start = "age") %>%
    dplyr::compute(name = "res_age")

  # -- Build HIPPS cohort table -----------------------------------------------
  # Base cohort definitions: 1 = HIP window, 2 = PPS window, 3 = combined.

  # Collect distinct outcome categories for outcome-stratified cohorts.
  outcome_categories <- sort(unique(
    identifiedPregnancies$final_outcome_category[
      !is.na(identifiedPregnancies$final_outcome_category)
    ]
  ))

  # Build a helper that creates cohort rows for a given window type.
  # Returns rows for the "all outcomes" cohort and one cohort per outcome.
  make_window_cohorts <- function(base_tbl, end_date_col, base_id) {
    # All-outcome cohort (base_id)
    all_outcomes <- base_tbl %>%
      dplyr::select(
        subject_id = "person_id",
        cohort_start_date = "merge_pregnancy_start",
        cohort_end_date = dplyr::all_of(end_date_col),
        "final_outcome_category"
      ) %>%
      dplyr::mutate(cohort_definition_id = base_id)

    # Per-outcome cohorts (base_id + offset)
    outcome_cohorts <- lapply(seq_along(outcome_categories), function(i) {
      oc <- outcome_categories[i]
      all_outcomes %>%
        dplyr::filter(.data$final_outcome_category == oc) %>%
        dplyr::mutate(cohort_definition_id = base_id + i)
    })

    Reduce(dplyr::union_all, outcome_cohorts, init = all_outcomes)
  }

  n_outcomes <- length(outcome_categories)
  # IDs: hipp = 1, hipp_<outcome> = 2..1+n;  pps = 2+n, pps_<outcome> = ...
  stride <- 1L + n_outcomes
  hipp_base_id  <- 1L
  pps_base_id   <- 1L + stride
  hipps_base_id <- 1L + 2L * stride

  # For the combined (hipps) window, compute end date as hip_end_date
  # falling back to pps_end_date.
  hipps_src <- cdm$res_age %>%
    dplyr::mutate(
      hipps_end_date = dplyr::case_when(
        is.na(.data$hip_end_date) ~ .data$pps_end_date,
        .default = .data$hip_end_date
      )
    )

  combined <- make_window_cohorts(cdm$res_age, "hip_end_date", hipp_base_id) %>%
    dplyr::union_all(
      make_window_cohorts(cdm$res_age, "pps_end_date", pps_base_id)
    ) %>%
    dplyr::union_all(
      make_window_cohorts(hipps_src, "hipps_end_date", hipps_base_id)
    )

  # Build cohort set reference
  window_names <- c("hipp", "pps", "hipps")
  cohort_set_ref <- do.call(rbind, lapply(seq_along(window_names), function(w) {
    base <- 1L + (w - 1L) * stride
    data.frame(
      cohort_definition_id = c(base, base + seq_len(n_outcomes)),
      cohort_name = c(
        window_names[w],
        paste0(window_names[w], "_", tolower(outcome_categories))
      ),
      stringsAsFactors = FALSE
    )
  }))

  cdm$hipps_cohort_table <- combined %>%
    dplyr::mutate(
      cohort_start_date = as.Date(.data$cohort_start_date),
      cohort_end_date = as.Date(.data$cohort_end_date)
    ) %>%
    dplyr::filter(.data$cohort_start_date < .data$cohort_end_date) %>%
    omopgenerics::newCohortTable(
      cohortSetRef = cohort_set_ref,
      .softValidation = TRUE
    ) %>%
    PatientProfiles::addInObservation(
      indexDate = "cohort_start_date", nameStyle = "start_in_obs"
    ) %>%
    PatientProfiles::addInObservation(
      indexDate = "cohort_end_date", nameStyle = "end_in_obs"
    ) %>%
    dplyr::filter(
      .data$start_in_obs == 1,
      .data$end_in_obs == 1
    ) %>%
    dplyr::compute(name = "hipps_cohort_table") %>%
    omopgenerics::newCohortTable(
      cohortSetRef = cohort_set_ref,
      .softValidation = TRUE
    )

  # -- Determine age range for denominator ------------------------------------
  ages <- cdm$res_age %>%
    dplyr::pull(.data$age_pregnancy_start)

  minAge <- max(min(ages, na.rm = TRUE), 0L)
  maxAge <- max(max(ages, na.rm = TRUE), 0L)

  # -- Generate denominator cohorts -------------------------------------------
  logMsg("Generating denominator cohorts")
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    name = "hipps_denom",
    sex = c("Female", "Both"),
    ageGroup = list(
      `0 to 150`   = c(0L, 150L),
      `min to max` = c(minAge, maxAge),
      `12 to 55`   = c(12L, 55L),
      `<35`        = c(0L, 34L),
      `>=35`       = c(35L, 150L)
    )
  )

  # -- Estimate incidence -----------------------------------------------------
  logMsg("Estimating incidence")
  incRes <- IncidencePrevalence::estimateIncidence(
    cdm = cdm,
    denominatorTable = "hipps_denom",
    outcomeTable = "hipps_cohort_table",
    interval = c("overall", "years"),
    outcomeWashout = 0,
    repeatedEvents = TRUE
  )

  # -- Estimate period prevalence ---------------------------------------------
  # For prevalence, set cohort_start_date = cohort_end_date - 1 day so that
  # each episode is a point-like event anchored at its outcome date.
  logMsg("Building prevalence cohort table")
  cdm$hipps_prev_cohort <- cdm$hipps_cohort_table %>%
    dplyr::mutate(
      cohort_start_date = !!CDMConnector::dateadd("cohort_end_date", -1L, interval = "day")
    ) %>%
    dplyr::compute(name = "hipps_prev_cohort") %>%
    omopgenerics::newCohortTable(
      cohortSetRef = cohort_set_ref,
      .softValidation = TRUE
    )

  logMsg("Estimating period prevalence")
  prevRes <- IncidencePrevalence::estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "hipps_denom",
    outcomeTable = "hipps_prev_cohort",
    interval = c("overall", "years")
  )

  # -- Summarise cohort characteristics ---------------------------------------
  logMsg("Summarising cohort characteristics")
  charRes <- cdm$hipps_cohort_table %>%
    PatientProfiles::addAge(
      ageGroup = list(
        `0 - 9`   = c(0, 9),
        `10 - 19` = c(10, 19),
        `20 - 29` = c(20, 29),
        `30 - 39` = c(30, 39),
        `40 - 49` = c(40, 49),
        `>=50`    = c(50, 150)
      )
    ) %>%
    CohortCharacteristics::summariseCharacteristics(
      strata = list("age_group", "final_outcome_category")
    )

  # -- Export results ---------------------------------------------------------
  logMsg("Exporting incidence, prevalence, and characteristics")
  omopgenerics::exportSummarisedResult(
    incRes,
    minCellCount = minCellCount,
    fileName = file.path(exportFolder, "{date}_{cdm_name}_incidence.csv")
  )

  omopgenerics::exportSummarisedResult(
    prevRes,
    minCellCount = minCellCount,
    fileName = file.path(exportFolder, "{date}_{cdm_name}_prevalence.csv")
  )

  omopgenerics::exportSummarisedResult(
    charRes,
    minCellCount = minCellCount,
    fileName = file.path(exportFolder, "{date}_{cdm_name}_characteristics.csv")
  )

  # Clean up intermediate database table
  CDMConnector::dropSourceTable(cdm, "identified_pregnancies")

  invisible(NULL)
}
