addAge <- function(cdm, res) {
  cdm$person %>%
    dplyr::select("person_id", "gender_concept_id", "birth_datetime") %>%
    dplyr::right_join(res, by = c("person_id" = "person_id"), copy = TRUE) %>%
    dplyr::collect() %>%
    dplyr::mutate(age_pregnancy_start = as.Date(inferred_episode_start) - as.Date(birth_datetime)) %>%
    dplyr::mutate(
      age_pregnancy_start = as.numeric(.data$age_pregnancy_start)
    )
}

exportAgeSummary <- function(res, cdm, resPath, snap, runStart) {
  res %>%
    addAge(cdm = cdm) %>%
    summariseColumn("age_pregnancy_start") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ . / 365.25)) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    ) %>%
    write.csv(file.path(resPath, "age_summary.csv"), row.names = FALSE)
}

exportPrecisionDays <- function(res, resPath, snap, runStart) {
  d <- res |>
    dplyr::filter(!is.na(.data$precision_days)) |>
    dplyr::pull(.data$precision_days) |>
    density()

  precisionDaysRes <- data.frame(
    precision_days = d$x,
    density = d$y
  ) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    )

  write.csv(precisionDaysRes, file.path(resPath, "precision_days.csv"), row.names = FALSE)
}

exportEpisodeFrequency <- function(res, resPath, snap, runStart) {
  res %>%
    dplyr::summarise(
      total_episodes = dplyr::n(),
      total_individuals = dplyr::n_distinct(person_id)
    ) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    ) %>%
    write.csv(file.path(resPath, "episode_frequency.csv"), row.names = FALSE)
}

exportPregnancyFrequency <- function(res, resPath, snap, runStart) {
  res %>%
    dplyr::count(.data$person_id, name = "freq") %>%
    dplyr::count(.data$freq, name = "number_individuals") %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    ) %>%
    write.csv(file.path(resPath, "pregnancy_frequency.csv"), row.names = FALSE)
}

exportEpisodeFrequencySummary <- function(res, resPath, snap, runStart) {
  res %>%
    dplyr::count(.data$person_id, name = "freq") %>%
    summariseColumn("freq") %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    ) %>%
    write.csv(file.path(resPath, "episode_frequency_summary.csv"), row.names = FALSE)
}

exportGestationalAgeSummary <- function(res, resPath, snap, runStart) {
  gestAgeDaysSummary <- res %>%
    summariseColumn("gestational_age_days_calculated") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ .)) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    ) %>%
    write.csv(file.path(resPath, "gestational_age_days_summary.csv"), row.names = FALSE)
}

exportGestationalAgeCounts <- function(res, resPath, snap, runStart) {
  res %>%
    dplyr::summarise(
      less_1day = sum(.data$gestational_age_days_calculated < 1),
      over_308days = sum(.data$gestational_age_days_calculated > 308)) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    ) %>%
    write.csv(file.path(resPath, "gestational_age_days_counts.csv"), row.names = FALSE)
}

exportGestationalWeeksCounts <- function(res, resPath, snap, runStart) {
  res %>%
    dplyr::mutate(gestational_weeks = floor(.data$gestational_age_days_calculated / 7)) %>%
    dplyr::group_by(.data$gestational_weeks) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(
      pct = n / sum(n) * 100
    ) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    ) %>%
    write.csv(file.path(resPath, "gestational_weeks.csv"), row.names = FALSE)
}

exportGestationalDurationCounts <- function(res, resPath, snap, runStart) {
  gestDuration <- res %>%
    dplyr::group_by(.data$final_outcome_category) %>%
    summariseColumn("gestational_age_days_calculated") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ .)) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    )

  write.csv(gestDuration, file.path(resPath, "gestational_age_days_per_category_summary.csv"), row.names = FALSE)
}

exportTimeTrends <- function(res, resPath, snap, runStart) {
  date_cols <- c("pregnancy_start", "hip_end_date", "pps_end_date", "episode_min_date" , "episode_max_date", "recorded_episode_start",  "recorded_episode_end", "inferred_episode_start", "inferred_episode_end")
  # date_cols <- tolower(date_cols)

  res_time <- res %>%
    tidyr::pivot_longer(cols = date_cols, names_to = "column", values_to = "date") %>%
    dplyr::mutate(
      month = format(as.Date(date, format = "%Y-%m-%d"), "%m"),
      year = format(as.Date(date, format = "%Y-%m-%d"), "%Y")
    )

  res_time$month <- as.integer(res_time$month)
  res_time$year <- as.integer(res_time$year)

  # Year
  yearly_trends <- res_time %>%
    dplyr::group_by(column, year) %>%
    dplyr::summarise(count = n(), .groups = "drop")

  yearly_trends_missing <- yearly_trends %>%
    dplyr::filter(is.na(.data$year)) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    )

  write.csv(yearly_trends_missing, file.path(resPath, "yearly_trend_missing.csv"), row.names = FALSE)

  yearly_trends <- yearly_trends %>%
    dplyr::filter(!is.na(.data$year)) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    )

  write.csv(yearly_trends, file.path(resPath, "yearly_trend.csv"), row.names = FALSE)

  monthly_trends <- res_time %>%
    group_by(column, month) %>%
    summarise(count = n(), .groups = "drop")

  monthly_trends$month <- factor(base::month.name[monthly_trends$month], levels = month.name)

  missingMonth <- monthly_trends %>%
    dplyr::filter(is.na(.data$month)) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    )

  write.csv(missingMonth, file.path(resPath, "monthly_trend_missing.csv"), row.names = FALSE)

  monthly_trends <- monthly_trends %>%
    dplyr::filter(!is.na(.data$month)) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    )

  write.csv(monthly_trends, file.path(resPath, "monthly_trends.csv"), row.names = FALSE)
}

exportObservationPeriodRange <- function(res, resPath, snap, runStart) {
  dates <- cdm$observation_period %>%
    dplyr::summarise(
      min_obs = min(!!CDMConnector::datepart("observation_period_start_date", interval = "year"), na.rm = TRUE),
      max_obs = max(!!CDMConnector::datepart("observation_period_end_date", interval = "year"), na.rm = TRUE)
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    )

  write.csv(dates, file.path(resPath, "observation_period_range.csv"), row.names = FALSE)
}

exportPregnancyOverlapCounts <- function(res, resPath, snap, runStart) {
  overlap <- res %>%
    dplyr::add_count(person_id) %>%
    dplyr::filter(n > 1) %>% # only keep individuals with multiple episodes
    dplyr::arrange(person_id, inferred_episode_start, inferred_episode_end) %>%
    # slice(1:100) %>%
    dplyr::group_by(person_id) %>%
    dplyr::mutate(
      prev_end = dplyr::lag(inferred_episode_end),
      overlap = as.Date(inferred_episode_start, format = "%Y-%m-%d") <= as.Date(prev_end, format = "%Y-%m-%d")) %>%
    summariseColumn("overlap") %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    )
  # [,c("person_id", "inferred_episode_start", "inferred_episode_end", "overlap")]

  write.csv(overlap, file.path(resPath, "pregnancy_overlap_counts.csv"), row.names = FALSE)
}

exportDateConsistancy <- function(res, resPath, snap, runStart) {
  date_cols <- c("pregnancy_start", "HIP_end_date", "PPS_end_date", "episode_min_date" , "episode_max_date", "recorded_episode_start",  "recorded_episode_end", "inferred_episode_start", "inferred_episode_end")
  dateConsistancy <- res %>%
    dplyr::summarise(dplyr::across(dplyr::any_of(c(date_cols)), ~ mean(is.na(.)))) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    )
  # summarise(across(everything(), ~ mean(is.na(.))))
  write.csv(dateConsistancy, file.path(resPath, "date_consistancy.csv"), row.names = FALSE)
}

exportReversedDatesCounts <- function(res, resPath, snap, runStart) {
  revDates <- res %>%
    dplyr::mutate(
      rev_hip = .data$pregnancy_start > .data$hip_end_date,
      rev_pps = .data$pregnancy_start > .data$pps_end_date
    ) %>%
    dplyr::summarise(
      n_rev_hip = sum(rev_hip, na.rm = TRUE),
      n_rev_pps = sum(rev_pps, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    )

  write.csv(revDates, file.path(resPath, "swapped_dates.csv"), row.names = FALSE)
}

exportOutcomeCategoriesCounts <- function(res, resPath, snap, runStart) {
  outcomeCat <- res %>%
    dplyr::group_by(.data$hip_outcome_category) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(algorithm = "hip") %>%
    dplyr::rename(outcome_category = "hip_outcome_category") %>%
    dplyr::bind_rows(
      res %>%
        dplyr::group_by(.data$pps_outcome_category) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::mutate(algorithm = "pps") %>%
        dplyr::rename(outcome_category = "pps_outcome_category")
    ) %>%
    dplyr::bind_rows(
      res %>%
        dplyr::group_by(.data$final_outcome_category) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::mutate(algorithm = "hipps") %>%
        dplyr::rename(outcome_category = "final_outcome_category")
    ) %>%
    dplyr::group_by(.data$algorithm) %>%
    dplyr::mutate(pct = n / sum(n) * 100) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date
    )

  write.csv(outcomeCat, file.path(resPath, "outcome_categories_count.csv"), row.names = FALSE)
}

#' export
#'
#' Exports the patient level results to summarised share-able csv-files.
#'
#' @param cdm (`cdm_reference`) CDM-Reference
#' @param outputDir (`character(1)`) The path to the outputs generated by `runHipps()`
#' @param exportDir (`character(1)`) The path to the folder where the sharable outputs should be written to.
#'
#' @returns `NULL`
#' @export
export <- function(cdm, outputDir, exportDir) {
  runStart <- read.csv(file.path(outputDir, "runStart.csv"))$start

  dir.create(exportDir, showWarnings = FALSE, recursive = TRUE)
  snap <- CDMConnector::snapshot(cdm)

  write.csv(snap, file.path(exportDir, "cdm_source.csv"), row.names = FALSE)

  res <- readRDS(file.path(outputDir, "identified_pregancy_episodes.rds"))

  names(res) <- tolower(names(res))
  date_cols <- c("pregnancy_start", "HIP_end_date", "PPS_end_date", "episode_min_date" , "episode_max_date", "recorded_episode_start",  "recorded_episode_end", "inferred_episode_start", "inferred_episode_end")
  date_cols <- tolower(date_cols)

  file.copy(
    from = file.path(outputDir, "PPS-concept_counts.csv"),
    to = file.path(exportDir, "PPS-concept_counts.csv")
  )

  file.copy(
    from = file.path(outputDir, "log.txt"),
    to = file.path(exportDir, "log.txt")
  )

  exportAgeSummary(res = res, cdm = cdm, resPath = exportDir, snap = snap, runStart = runStart)
  exportPrecisionDays(res, exportDir, snap = snap, runStart = runStart)
  exportEpisodeFrequency(res, exportDir, snap = snap, runStart = runStart)
  exportPregnancyFrequency(res, exportDir, snap = snap, runStart = runStart)
  exportEpisodeFrequencySummary(res, exportDir, snap = snap, runStart = runStart)
  exportGestationalAgeSummary(res, exportDir, snap = snap, runStart = runStart)
  exportGestationalAgeCounts(res, exportDir, snap = snap, runStart = runStart)
  exportGestationalWeeksCounts(res, exportDir, snap = snap, runStart = runStart)
  exportGestationalDurationCounts(res, exportDir, snap = snap, runStart = runStart)
  exportTimeTrends(res, exportDir, snap = snap, runStart = runStart)
  exportObservationPeriodRange(res, exportDir, snap = snap, runStart = runStart)
  exportPregnancyOverlapCounts(res, exportDir, snap = snap, runStart = runStart)
  exportDateConsistancy(res, exportDir, snap = snap, runStart = runStart)
  exportReversedDatesCounts(res, exportDir, snap = snap, runStart = runStart)
  exportOutcomeCategoriesCounts(res, exportDir, snap = snap, runStart = runStart)

  message(sprintf("Files have been written to: %s", exportDir))
}
