addAge <- function(cdm, res) {
  cdm$person %>%
    dplyr::select("person_id", "gender_concept_id", "birth_datetime") %>%
    dplyr::right_join(res, by = c("person_id" = "person_id"), copy = TRUE) %>%
    dplyr::mutate(age_pregnancy_start = as.Date(inferred_episode_start) - as.Date(birth_datetime)) %>%
    dplyr::mutate(
      age_pregnancy_start = as.numeric(.data$age_pregnancy_start)
    ) %>%
    dplyr::compute(name = "res_age") %>%
    dplyr::collect()
}

exportAgeSummary <- function(res, cdm, resPath) {
  res %>%
    addAge(cdm = cdm) %>%
    summariseColumn("age_pregnancy_start") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ . / 365.25)) %>%
    write.csv(file.path(resPath, "age_summary.csv"))
}

exportPrecisionDays <- function(res) {
  d <- density(res$precision_days)

  data.frame(
    precision_days = d$x,
    density = d$y
  )
}

exportEpisodeFrequency <- function(res, resPath) {
  res %>%
    dplyr::summarise(
      total_episodes = dplyr::n(),
      total_individuals = dplyr::n_distinct(person_id)
    ) %>%
    write.csv(file.path(resPath, "episode_frequency.csv"))
}

exportPregnancyFrequency <- function(res, resPath) {
  res %>%
    dplyr::count(.data$person_id, name = "freq") %>%
    dplyr::count(.data$freq, name = "number_individuals") %>%
    write.csv(file.path(resPath, "pregnancy_frequency.csv"))
}

exportEpisodeFrequencySummary <- function(res, resPath) {
  res %>%
    dplyr::count(.data$person_id, name = "freq") %>%
    summariseColumn("freq") %>%
    write.csv(file.path(resPath, "episode_frequency_summary.csv"))
}

exportGestationalAgeSummary <- function(res, resPath) {
  gestAgeDaysSummary <- res %>%
    summariseColumn("gestational_age_days_calculated") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ .)) %>%
    write.csv(file.path(resPath, "gestational_age_days_summary.csv"))
}

exportGestationalAgeCounts <- function(res, resPath) {
  res %>%
    dplyr::summarise(
      less_1day = sum(.data$gestational_age_days_calculated < 1),
      over_308days = sum(.data$gestational_age_days_calculated > 308)) %>%
  write.csv(file.path(resPath, "gestational_age_days_counts.csv"))
}

exportGestationalWeeksCounts <- function(res, resPath) {
  res %>%
    dplyr::mutate(gestational_weeks = floor(gestational_age_days_calculated / 7)) %>%
    dplyr::group_by(.data$gestational_weeks) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(
      pct = n / sum(n) * 100
    ) %>%
    write.csv(file.path(resPath, "gestational_weeks.csv"))
}

exportGestationalDurationCounts <- function(res, resPath) {
  gestDuration <- res %>%
    dplyr::group_by(.data$final_outcome_category) %>%
    summariseColumn("gestational_age_days_calculated") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ .))

  write.csv(gestDuration, file.path(resPath, "gestational_age_days_per_category_summary.csv"))
}

exportTimeTrends <- function(res, resPath) {
  date_cols <- c("pregnancy_start", "HIP_end_date", "PPS_end_date", "episode_min_date" , "episode_max_date", "recorded_episode_start",  "recorded_episode_end", "inferred_episode_start", "inferred_episode_end")
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
    dplyr::filter(is.na(.data$year))

  write.csv(yearly_trends_missing, file.path(resPath, "yearly_trend_missing.csv"))

  yearly_trends <- yearly_trends %>%
    dplyr::filter(!is.na(.data$year))

  write.csv(yearly_trends, file.path(resPath, "yearly_trend.csv"))

  monthly_trends <- res_time %>%
    group_by(column, month) %>%
    summarise(count = n(), .groups = "drop")

  monthly_trends$month <- factor(base::month.name[monthly_trends$month], levels = month.name)

  missingMonth <- monthly_trends %>%
    dplyr::filter(is.na(.data$month))

  write.csv(missingMonth, file.path(resPath, "monthly_trend_missing.csv"))

  monthly_trends <- monthly_trends %>%
    dplyr::filter(!is.na(.data$month))

  write.csv(monthly_trends, file.path(resPath, "monthly_trends.csv"))
}

exportObservationPeriodRange <- function(res, resPath) {
  dates <- cdm$observation_period %>%
    dplyr::summarise(
      min_obs = min(!!CDMConnector::datepart("observation_period_start_date", interval = "year"), na.rm = TRUE),
      max_obs = max(!!CDMConnector::datepart("observation_period_end_date", interval = "year"), na.rm = TRUE)
    ) %>%
    dplyr::collect()

  write.csv(dates, file.path(resPath, "observation_period_range.csv"))
}

exportPregnancyOverlapCounts <- function(res, resPath) {
  overlap <- res %>%
    add_count(person_id) %>%
    filter(n > 1) %>% # only keep individuals with multiple episodes
    arrange(person_id, inferred_episode_start, inferred_episode_end) %>%
    # slice(1:100) %>%
    group_by(person_id) %>%
    mutate(prev_end = dplyr::lag(inferred_episode_end),
           overlap = as.Date(inferred_episode_start, format = "%Y-%m-%d")  <= as.Date(prev_end, format = "%Y-%m-%d")) %>%
    summariseColumn("overlap")
  # [,c("person_id", "inferred_episode_start", "inferred_episode_end", "overlap")]

  write.csv(overlap, file.path(resPath, "pregnancy_overlap_counts.csv"))
}

exportDateConsistancy <- function(res, resPath) {
  date_cols <- c("pregnancy_start", "HIP_end_date", "PPS_end_date", "episode_min_date" , "episode_max_date", "recorded_episode_start",  "recorded_episode_end", "inferred_episode_start", "inferred_episode_end")
  dateConsistancy <- res %>%
    dplyr::summarise(dplyr::across(dplyr::any_of(c(date_cols)), ~ mean(is.na(.))))
  # summarise(across(everything(), ~ mean(is.na(.))))
  write.csv(dateConsistancy, file.path(resPath, "date_consistancy.csv"))
}

exportReversedDatesCounts <- function(res, resPath) {
  revDates <- res %>%
    dplyr::mutate(
      rev_hip = .data$pregnancy_start > .data$HIP_end_date,
      rev_pps = .data$pregnancy_start > .data$PPS_end_date
    ) %>%
    dplyr::summarise(
      n_rev_hip = sum(rev_hip, na.rm = TRUE),
      n_rev_pps = sum(rev_pps, na.rm = TRUE)
    )

  write.csv(revDates, file.path(resPath, "swapped_dates.csv"))
}

exportOutcomeCategoriesCounts <- function(res, resPath) {
  outcomeCat <- res %>%
    dplyr::group_by(.data$HIP_outcome_category) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(algorithm = "hip") %>%
    dplyr::rename(outcome_category = "HIP_outcome_category") %>%
    dplyr::bind_rows(
      res %>%
        dplyr::group_by(.data$PPS_outcome_category) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::mutate(algorithm = "pps") %>%
        dplyr::rename(outcome_category = "PPS_outcome_category")
    ) %>%
    dplyr::bind_rows(
      res %>%
        dplyr::group_by(.data$final_outcome_category) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::mutate(algorithm = "hipps") %>%
        dplyr::rename(outcome_category = "final_outcome_category")
    ) %>%
    dplyr::group_by(.data$algorithm) %>%
    dplyr::mutate(pct = n / sum(n) * 100)

  write.csv(outcomeCat, file.path(resPath, "outcome_categories_count.csv"))
}

export <- function(cdm, identifiedPregnanciesFile) {
  snap <- CDMConnector::snapshot(cdm)

  res <- readRDS(identifiedPregnanciesFile)

  names(res) <- tolower(names(res))
  date_cols <- c("pregnancy_start", "HIP_end_date", "PPS_end_date", "episode_min_date" , "episode_max_date", "recorded_episode_start",  "recorded_episode_end", "inferred_episode_start", "inferred_episode_end")
  date_cols <- tolower(date_cols)
}

# res <- readRDS("./dev/output/result.rds")
#
# exportAgeSummary(res = res, cdm = cdm)
# exportPrecisionDays(res, "./dev/export/")
# exportEpisodeFrequency(res, "./dev/export/")
# exportPregnancyFrequency(res, "./dev/export/")
# exportEpisodeFrequencySummary(res, "./dev/export/")
# exportGestationalAgeSummary(res, "./dev/export/")
# exportGestationalAgeCounts(res, "./dev/export/")
# exportGestationalWeeksCounts(res, "./dev/export/")
# exportGestationalDurationCounts(res, "./dev/export/")
# exportTimeTrends(res, "./dev/export/")
# exportObservationPeriodRange(res, "./dev/export/")
# exportPregnancyOverlapCounts(res, "./dev/export/")
# exportDateConsistancy(res, "./dev/export/")
# exportReversedDatesCounts(res, "./dev/export/")
# exportOutcomeCategoriesCounts(res, "./dev/export/")
