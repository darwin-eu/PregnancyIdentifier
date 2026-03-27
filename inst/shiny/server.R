# server.R - Module server calls with reactive version filtering

server <- function(input, output, session) {

  # --- Reactive data store filtered by selected versions ---
  rv <- reactiveValues()

  observe({
    vers <- input$version_select
    if (is.null(vers) || length(vers) == 0) vers <- allVersions

    # Filter helper: uses version column if present, else extracts from cdm_name.
    # For wide-format data (from dataToLong), filters columns by matching allDP.
    filterV <- function(df) {
      if (is.null(df) || !is.data.frame(df)) return(df)
      if ("version" %in% colnames(df)) {
        return(df %>% dplyr::filter(.data$version %in% vers))
      }
      if ("cdm_name" %in% colnames(df)) {
        return(df %>% dplyr::filter(gsub(".*_(v[0-9]+)$", "\\1", .data$cdm_name) %in% vers))
      }
      # Wide-format: keep 'name' column + only columns matching filtered allDP
      if ("name" %in% colnames(df) && ncol(df) > 1) {
        keepCols <- intersect(colnames(df), c("name", rv$allDP))
        if (length(keepCols) > 1) return(df[, keepCols, drop = FALSE])
      }
      df
    }

    # Core metadata
    rv$dbinfo <- filterV(dbinfo)
    rv$allDP <- sort(unique(rv$dbinfo$cdm_name))

    # Episode frequency
    rv$episodeFrequency <- filterV(episodeFrequency)
    rv$pregnancyFrequency <- filterV(pregnancyFrequency)

    # Episode duration
    rv$gestationalWeeksSummary <- filterV(gestationalWeeksSummary)
    rv$gestationalWeeksBinned <- filterV(gestationalWeeksBinned)
    rv$gestationalWeeksPlausibility <- filterV(gestationalWeeksPlausibility)
    rv$gestationalAgeDaysPerCategorySummary <- filterV(gestationalAgeDaysPerCategorySummary)
    rv$gestationalAgeDaysSummary <- filterV(gestationalAgeDaysSummary)
    rv$gestationalAgeDaysCounts <- filterV(gestationalAgeDaysCounts)
    rv$trendData <- filterV(trendData)
    rv$trendDataMissing <- filterV(trendDataMissing)

    # Episode construction
    rv$pregnancyOverlapCounts <- filterV(pregnancyOverlapCounts)
    rv$swappedDatesDisplay <- filterV(swappedDatesDisplay)
    rv$missingDates <- filterV(missingDates)

    # Episode outcomes
    rv$deliveryModeSummary <- filterV(deliveryModeSummary)
    rv$deliveryModeByYear <- filterV(deliveryModeByYear)
    rv$outcomeCategoriesCount <- filterV(outcomeCategoriesCount)

    # Characteristics / Incidence / Prevalence
    rv$characteristics <- filterV(characteristics)
    rv$incidence <- filterV(incidence)
    rv$prevalence <- filterV(prevalence)

    # Observation Period
    rv$observationPeriodRange <- filterV(observationPeriodRange)

    # Concept counts
    rv$esdConceptCounts <- filterV(esdConceptCounts)
    rv$hipConceptCounts <- filterV(hipConceptCounts)
    rv$ppsConceptCounts <- filterV(ppsConceptCounts)

    # Age
    rv$ageSummaryRaw <- filterV(ageSummaryRaw)
    rv$ageSummaryFirstPregnancy <- filterV(ageSummaryFirstPregnancy)
    rv$ageSummaryFirstPregnancyEnd <- filterV(ageSummaryFirstPregnancyEnd)
    rv$ageSummaryGroups <- filterV(ageSummaryGroups)

    # Attrition
    rv$attritionEpisodes <- filterV(attritionEpisodes)
    rv$attritionIfCleanup <- filterV(attritionIfCleanup)

    # Other
    rv$conceptCheck <- filterV(conceptCheck)
    rv$precisionDays <- filterV(precisionDays)
    rv$precisionDaysDenominators <- filterV(precisionDaysDenominators)
    rv$qualityCheckCleanup <- filterV(qualityCheckCleanup)

    # PET comparison
    rv$petComparisonSummarisedResult <- filterV(petComparisonSummarisedResult)
    rv$petUnmatchedLsc <- filterV(petUnmatchedLsc)

    # Observation period derived
    rv$minObservationPeriod <- if (exists("minObservationPeriod")) minObservationPeriod else 0
  })

  # --- Module calls ---

  # Always-present modules
  backgroundServer("background")
  databasesServer("databases", rv)

  # Episode frequency
  episodeFrequencyServer("episode_frequency", rv)
  pregnancyFrequencyServer("pregnancy_frequency", rv)

  # Episode duration
  gestationalAgeServer("gestational_age", rv)
  gestationalAgeBinnedServer("gestational_age_binned", rv)
  gestationalAgePlausibilityServer("gestational_age_plausibility", rv)
  gestationalAgeDaysPerCategoryServer("gestational_age_days", rv)
  temporalPatternsServer("temporal_patterns", rv)

  # Episode construction
  pregnancyOverlapServer("pregnancy_overlap", rv)
  swappedDatesServer("swapped_dates", rv)
  missingDatesServer("missing_dates", rv)

  # Episode outcomes
  deliveryModeServer("delivery_mode", rv)
  deliveryModeByYearServer("delivery_mode_by_year", rv)
  outcomeCategoriesServer("outcome_categories", rv)

  # Cohort Characteristics
  if (has_characteristics) characteristicsServer("characteristics", rv)

  # Incidence / Prevalence
  if (has_incidence) incidenceServer("incidence", rv)
  if (has_prevalence) prevalenceServer("prevalence", rv)

  # Observation Period
  observationPeriodServer("observation_period", rv)

  # Concept counts
  if (has_esd_concepts) conceptCountsServer("esd_concepts", rv, "esdConceptCounts")
  if (has_hip_concepts) conceptCountsServer("hip_concepts", rv, "hipConceptCounts")
  if (has_pps_concepts) conceptCountsServer("pps_concepts", rv, "ppsConceptCounts")

  # Age
  if (has_age_summary) ageSummaryServer("age_summary", rv)
  if (has_age_first_pregnancy) ageFirstPregnancyServer("age_first_pregnancy", rv)
  if (has_age_first_pregnancy_end) ageFirstPregnancyEndServer("age_first_pregnancy_end", rv)
  if (has_age_groups) ageGroupsServer("age_groups", rv)

  # Attrition
  if (has_attrition) attritionServer("attrition", rv, "attritionEpisodes")
  if (has_attrition_cleanup) attritionServer("attrition_cleanup", rv, "attritionIfCleanup")

  # Concept check
  conceptCheckServer("concept_check", rv)

  # Precision days
  precisionDaysServer("precision_days", rv)

  # Quality check cleanup
  qualityCheckCleanupServer("quality_check", rv)

  # PET comparison
  if (has_pet_comparison_sr) {
    petComparisonServer("pet_comparison", rv)
  } else if (has_pet_legacy) {
    petComparisonLegacyContainerServer("pet_legacy", rv)
  }

  # Version differences
  if (has_version_diff) versionDifferencesServer("version_diff", versionDifferences)

  # National statistics comparison
  if (has_national_stats) nationalStatsComparisonServer("national_stats", rv)

  # Overview (last)
  overviewServer("overview", rv)
}
