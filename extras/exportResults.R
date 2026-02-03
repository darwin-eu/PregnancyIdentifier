library(dplyr)
library(ggplot2)

# IncidencePrevalence is required for some parts of this script
# install.packages("IncidencePrevalence")

# Same as outputDir
resPath <- "..."

cdmConnection <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  ...
)

cdm <- CDMConnector::cdmFromCon(
  con = cdmConnection,
  cdmSchema = "...",
  writeSchema = "..."
)

hipps <- readRDS(file.path(resPath, "..."))

# TODO: check which start & end dates to use
names(res) <- tolower(names(res))
date_cols <- c("pregnancy_start", "HIP_end_date", "PPS_end_date", "episode_min_date" , "episode_max_date", "recorded_episode_start",  "recorded_episode_end", "inferred_episode_start", "inferred_episode_end")
date_cols <- tolower(date_cols)

### Temp checks
res %>%
  summariseColumn("precision_days")

res %>%
  summariseColumn("pps_outcome_category")

res %>%
  as_tibble() %>%
  dplyr::filter(.data$precision_days < 50)

d <- density(res$precision_days)

df <- data.frame(
  precision_days = d$x,
  density = d$y
)

ggplot(data = df, mapping = aes(x = precision_days, y = density)) +
  geom_line()

df <- res %>%
  dplyr::group_by(.data$precision_category) %>%
  dplyr::summarise(n = n())

ggplot(data = df, mapping = aes(x = precision_category, y = n)) +
  geom_bar(stat = "identity")

df <- res %>%
  dplyr::group_by(.data$pps_outcome_category) %>%
  dplyr::summarise(n = dplyr::n())

ggplot(data = df, mapping = aes(x = pps_outcome_category, y = n)) +
  geom_bar(stat = "identity")

### Episode Frequency ###
print("Pregnancy frequency")
episodeFreq <- res %>%
  summarise(total_episodes=n(),
            total_individuals=n_distinct(person_id))

write.csv(episodeFreq, file.path(resPath, "episode_frequency.csv"))

print("Pregnancy frequency per person")
pregFreq <- res %>%
  count(person_id, name = "freq") %>%
  count(freq, name="number_individuals")

write.csv(pregFreq, file.path(resPath, "pregnancy_frequency.csv"))

episodeFreqSummary <- res %>%
  count(person_id, name = "freq") %>%
  summariseColumn("freq")

write.csv(episodeFreqSummary, file.path(resPath, "episode_frequency_summary.csv"))

print("Maternal age distribution (in years)")

cdm <- omopgenerics::insertTable(cdm, "res", res)

res_age <- cdm$person %>%
  select("person_id", "gender_concept_id", "birth_datetime") %>%
  right_join(cdm$res, by = c("person_id" = "person_id")) %>%
  mutate(age_pregnancy_start = as.Date(inferred_episode_start) - as.Date(birth_datetime)) %>%
  dplyr::mutate(
    age_pregnancy_start = as.numeric(.data$age_pregnancy_start)
  ) %>%
  dplyr::compute(name = "res_age") %>%
  dplyr::collect()

ageSummary <- res_age %>%
  summariseColumn("age_pregnancy_start") %>%
  mutate(across(where(is.numeric), ~ . / 365.25))

write.csv(ageSummary, file.path(resPath, "age_summary.csv"))

# TODO: pregnancy incidence rate
cdm$hipps_cohort_table <- cdm$res_age %>%
  dplyr::select(
    subject_id = person_id,
    cohort_start_date = "pregnancy_start",
    cohort_end_date = "hip_end_date"
  ) %>%
  dplyr::mutate(
    cohort_definition_id = 1
  ) %>%
  dplyr::union_all(
    cdm$res_age %>%
      dplyr::select(
        subject_id = person_id,
        cohort_start_date = "pregnancy_start",
        cohort_end_date = "pps_end_date"
      ) %>%
      dplyr::mutate(
        cohort_definition_id = 2
      )
  ) %>%
  dplyr::union_all(
    cdm$res_age %>%
      dplyr::mutate(
        cohort_definition_id = 3,
        cohort_end_date = dplyr::case_when(
          is.na(.data$hip_end_date) ~ .data$pps_end_date,
          .default = .data$hip_end_date
          )
        ) %>%
      dplyr::select(
        subject_id = person_id,
        cohort_start_date = "pregnancy_start",
        "cohort_end_date",
        "cohort_definition_id"
      )
  ) %>%
  dplyr::mutate(
    cohort_start_date = as.Date(.data$cohort_start_date),
    cohort_end_date = as.Date(.data$cohort_end_date)
  ) %>%
  dplyr::filter(.data$cohort_start_date < .data$cohort_end_date) %>%
  PatientProfiles::filterInObservation(indexDate = "cohort_start_date") %>%
  PatientProfiles::filterInObservation(indexDate = "cohort_end_date") %>%
  dplyr::compute(name = "hipps_cohort_table") %>%
  omopgenerics::newCohortTable(
    cohortSetRef = data.frame(
      cohort_definition_id = c(1, 2, 3),
      cohort_name = c("hipp", "pps", "hipps")
    )
  )

ages <- cdm$hipps_cohort_table %>%
  PatientProfiles::addAge() %>%
  summarise(
    min_age = min(.data$age, na.rm = TRUE),
    max_age = max(.data$age, na.rm = TRUE)
  ) %>%
  dplyr::collect()

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm,
  name = "hipps_denom",
  sex = c("Female"),
  ageGroup = list(
    `0 to 150` = c(0, 150),
    `min to max` = c(ages$min_age, ages$max_age),
    `<35` = c(0, 34),
    `>=35` = c(35, 150)
  )
)

incRes <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "hipps_denom",
  outcomeTable = "hipps_cohort_table"
)

omopgenerics::exportSummarisedResult(
  incRes,
  minCellCount = 5,
  fileName = file.path(resPath, "incidence.csv")
)

gg <- IncidencePrevalence::plotIncidence(
  result = incRes,
  facet = "denominator_age_group",
  colour = "outcome_cohort_name", line = TRUE
) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(file.path(resPath, "incidence_plot.png"), gg)

IncidencePrevalence::tableIncidence(
  result = incRes,
  groupColumn = "incidence_start_date"
) |>
  gt::gtsave(file.path(resPath, "incidence_table.docx"))

### Episode Construction ###
print("Gestational duration distribution (in weeks)")
gestAgeDaysSummary <- res %>%
  summariseColumn("gestational_age_days_calculated") %>%
  mutate(across(where(is.numeric), ~ . / 7))

write.csv(gestAgeDaysSummary, file.path(resPath, "gestational_age_days_summary.csv"))

gestAgeCounts <- res %>%
  summarise(less_1day = sum(gestational_age_days_calculated < 1),
            over_308days = sum(gestational_age_days_calculated > 308))

write.csv(gestAgeCounts, file.path(resPath, "gestational_age_days_counts.csv"))

print("Gestational duration bins")

gestWeeks <- res %>%
  dplyr::mutate(gestational_weeks = floor(gestational_age_days_calculated / 7)) %>%
  dplyr::group_by(.data$gestational_weeks) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(
    pct = n / sum(n) * 100
  )

write.csv(gestWeeks, file.path(resPath, "gestational_weeks.csv"))

gg <- ggplot(data = gestWeeks, mapping = aes(x = gestational_weeks, y = log(n))) +
  geom_bar(stat = "identity") +
  labs(
    x = "Gestational duration",
    y = "log(n)"
  )

ggsave(file.path(resPath, "gestational_weeks.png"), gg)

print("Seasonal and temporal patterns")

res_time <- res %>%
  tidyr::pivot_longer(cols = date_cols, names_to = "column", values_to = "date") %>%
  mutate(
    month = format(as.Date(date, format = "%Y-%m-%d"), "%m"),
    year = format(as.Date(date, format = "%Y-%m-%d"), "%Y")
  )

res_time$month <- as.integer(res_time$month)
res_time$year <- as.integer(res_time$year)

# Year
yearly_trends <- res_time %>%
  group_by(column, year) %>%
  summarise(count = n(), .groups = "drop")

yearly_trends_missing <- yearly_trends %>%
  dplyr::filter(is.na(.data$year))

write.csv(yearly_trends_missing, file.path(resPath, "yearly_trend_missing.csv"))

yearly_trends <- yearly_trends %>%
  dplyr::filter(!is.na(.data$year))

write.csv(yearly_trends, file.path(resPath, "yearly_trend.csv"))

dates <- cdm$observation_period %>%
  dplyr::summarise(
    min_obs = min(!!CDMConnector::datepart("observation_period_start_date", interval = "year"), na.rm = TRUE),
    max_obs = max(!!CDMConnector::datepart("observation_period_end_date", interval = "year"), na.rm = TRUE)
  ) %>%
  dplyr::collect()

write.csv(dates, file.path(resPath, "observation_period_range.csv"))

gg <- ggplot(data = yearly_trends, mapping = aes(x = year, y = count, color = column, group = column)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Count (N)") +
  geom_vline(mapping = aes(xintercept = dates$min_obs), linetype = "dashed")

ggsave(file.path(resPath, "yearly_trends.png"), gg)

# Month
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

gg <- ggplot(data = monthly_trends, mapping = aes(x = month, y = count, color = column, group = column)) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Count (N)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(file.path(resPath, "monthly_trends.png"), gg)

### Episode Duration ###
print("Overlapping pregnancy episodes")
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

print("Missing or inconsistent dates")
dateConsistancy <- res %>%
  summarise(across(any_of(c(date_cols)), ~ mean(is.na(.))))
# summarise(across(everything(), ~ mean(is.na(.))))
write.csv(dateConsistancy, file.path(resPath, "date_consistancy.csv"))

# TODO: check for reversed dates
revDates <- res %>%
  dplyr::mutate(
    rev_hip = .data$pregnancy_start > .data$hip_end_date,
    rev_pps = .data$pregnancy_start > .data$pps_end_date
  ) %>%
  dplyr::summarise(
    n_rev_hip = sum(rev_hip, na.rm = TRUE),
    n_rev_pps = sum(rev_pps, na.rm = TRUE)
  )

write.csv(revDates, file.path(resPath, "swapped_dates.csv"))

# TODO: control variables not used in algorithm

### Episode Outcomes ###

# TODO: mode of delivery

print("Proportion of pregnancy outcome events")

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
  dplyr::mutate(pct = n / sum(n) * 100)

write.csv(outcomeCat, file.path(resPath, "outcome_categories_count.csv"))

gg <- ggplot(data = outcomeCat, mapping = aes(y = pct, x = outcome_category)) +
  geom_bar(stat = "identity", position = "dodge", mapping = aes(fill = algorithm))

ggsave(file.path(resPath, "outcome_categories_count.png"), gg)

print("Gestational duration distribution (in weeks) for pregnancy outcome events")
gestDuration <- res %>%
  group_by(final_outcome_category) %>%
  summariseColumn("gestational_age_days_calculated") %>%
  mutate(across(where(is.numeric), ~ . / 7))

write.csv(gestDuration, file.path(resPath, "gestational_age_days_per_category_summary.csv"))
