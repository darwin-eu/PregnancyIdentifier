# server.R - Module server calls

server <- function(input, output, session) {
  # Always-present modules
  backgroundServer("background")
  databasesServer("databases")

  # Episode frequency
  episodeFrequencyServer("episode_frequency")
  pregnancyFrequencyServer("pregnancy_frequency")

  # Episode duration
  gestationalAgeServer("gestational_age")
  gestationalAgeBinnedServer("gestational_age_binned")
  gestationalAgeDaysPerCategoryServer("gestational_age_days")
  temporalPatternsServer("temporal_patterns")

  # Episode construction
  pregnancyOverlapServer("pregnancy_overlap")
  swappedDatesServer("swapped_dates")
  missingDatesServer("missing_dates")

  # Episode outcomes
  deliveryModeServer("delivery_mode")
  outcomeCategoriesServer("outcome_categories")

  # Cohort Characteristics
  if (has_characteristics) characteristicsServer("characteristics")

  # Incidence / Prevalence
  if (has_incidence) incidenceServer("incidence")
  if (has_prevalence) prevalenceServer("prevalence")

  # Observation Period
  observationPeriodServer("observation_period")

  # Concept counts
  if (has_esd_concepts) conceptCountsServer("esd_concepts", esdConceptCounts)
  if (has_hip_concepts) conceptCountsServer("hip_concepts", hipConceptCounts)
  if (has_pps_concepts) conceptCountsServer("pps_concepts", ppsConceptCounts)

  # Age
  if (has_age_summary) ageSummaryServer("age_summary")
  if (has_age_first_pregnancy) ageFirstPregnancyServer("age_first_pregnancy")
  if (has_age_first_pregnancy_end) ageFirstPregnancyEndServer("age_first_pregnancy_end")
  if (has_age_groups) ageGroupsServer("age_groups")

  # Attrition
  if (has_attrition) attritionServer("attrition", attritionEpisodes)
  if (has_attrition_cleanup) attritionServer("attrition_cleanup", attritionIfCleanup)

  # Concept check
  conceptCheckServer("concept_check")

  # Precision days
  precisionDaysServer("precision_days")

  # Quality check cleanup
  qualityCheckCleanupServer("quality_check")

  # PET comparison
  if (has_pet_comparison_sr) {
    petComparisonServer("pet_comparison")
  } else if (has_pet_legacy) {
    petComparisonLegacyContainerServer("pet_legacy")
  }

  # Version differences
  if (has_version_diff) versionDifferencesServer("version_diff", versionDifferences)

  # National statistics comparison
  if (has_national_stats) nationalStatsComparisonServer("national_stats")

  # Overview (last)
  overviewServer("overview")
}
