# ui.R - Dashboard layout with conditional sidebar tabs

header <- dashboardHeader(
  title = "PregnancyIdentifier",
  titleWidth = 300,
  tags$li(
    class = "dropdown",
    tags$div(
      class = "logo-container",
      style = "position: relative;",
      img(src = "__logo__.png", height = "40px", style = "margin-right:15px;")
    )
  )
)

sidebar <- dashboardSidebar(
  width = 230,
  shinyWidgets::pickerInput(
    "version_select", "Select version",
    choices = allVersions, selected = "v3",
    multiple = TRUE, options = opt
  ),
  sidebarMenu(
    id = "tabs",
    menuItem("Study background", tabName = "background", icon = icon("book")),
    menuItem("Database information", tabName = "databases", icon = icon("database")),
    if (has_version_diff) menuItem("Versions", tabName = "version_diff", icon = icon("code-branch")),

    # Episode frequency
    menuItem("Episode frequency", icon = icon("chart-bar"),
      menuSubItem("Episode frequency", tabName = "episode_frequency"),
      menuSubItem("Pregnancy frequency", tabName = "pregnancy_frequency")
    ),

    # Episode duration
    menuItem("Episode duration", icon = icon("clock"),
      menuSubItem("Gestational age weeks", tabName = "gestational_age"),
      menuSubItem("Gestational age binned", tabName = "gestational_age_binned"),
      menuSubItem("GW plausibility", tabName = "gestational_age_plausibility"),
      menuSubItem("Gestational age by category", tabName = "gestational_age_days"),
      menuSubItem("Temporal patterns", tabName = "temporal_patterns")
    ),

    # Episode construction
    menuItem("Episode construction", icon = icon("wrench"),
      menuSubItem("Pregnancy overlap", tabName = "pregnancy_overlap"),
      menuSubItem("Swapped dates", tabName = "swapped_dates"),
      menuSubItem("Missing dates", tabName = "missing_dates")
    ),

    # Episode outcomes
    menuItem("Episode outcomes", icon = icon("flag"),
      menuSubItem("Mode of delivery", tabName = "delivery_mode"),
      menuSubItem("Mode of delivery by year", tabName = "delivery_mode_by_year"),
      menuSubItem("Outcome categories", tabName = "outcome_categories")
    ),

    # Cohort Characteristics (conditional)
    if (has_characteristics) menuItem("Cohort Characteristics", tabName = "characteristics", icon = icon("users")),

    # Incidence / Prevalence (conditional)
    if (has_ip) menuItem("IncidencePrevalence", icon = icon("chart-line"),
      if (has_incidence) menuSubItem("Incidence", tabName = "incidence"),
      if (has_prevalence) menuSubItem("Prevalence", tabName = "prevalence")
    ),

    # Observation Period
    menuItem("Observation Period", tabName = "observation_period", icon = icon("calendar")),

    # Concept counts (conditional)
    if (has_concept_counts) menuItem("Concept counts", icon = icon("list"),
      if (has_esd_concepts) menuSubItem("ESD concept counts", tabName = "esd_concepts"),
      if (has_hip_concepts) menuSubItem("HIP concept counts", tabName = "hip_concepts"),
      if (has_pps_concepts) menuSubItem("PPS concept counts", tabName = "pps_concepts")
    ),

    # Age (conditional)
    if (has_age) menuItem("Age", icon = icon("user"),
      if (has_age_summary) menuSubItem("Age Summary by Outcome", tabName = "age_summary"),
      if (has_age_first_pregnancy) menuSubItem("Age at first pregnancy (start)", tabName = "age_first_pregnancy"),
      if (has_age_first_pregnancy_end) menuSubItem("Age at first pregnancy (end)", tabName = "age_first_pregnancy_end"),
      if (has_age_groups) menuSubItem("Age distribution", tabName = "age_groups")
    ),

    # Attrition (conditional)
    if (has_attrition || has_attrition_cleanup) menuItem("Attrition", icon = icon("filter"),
      if (has_attrition) menuSubItem("Attrition", tabName = "attrition"),
      if (has_attrition_cleanup) menuSubItem("Attrition if cleanup", tabName = "attrition_cleanup")
    ),

    # Concept check
    menuItem("Concept check", tabName = "concept_check", icon = icon("check")),

    # Precision days
    menuItem("Precision days", tabName = "precision_days", icon = icon("bullseye")),

    # Quality check cleanup
    menuItem("Quality check cleanup", tabName = "quality_check", icon = icon("broom")),

    # PET comparison (conditional)
    if (has_pet) menuItem("PET comparison", tabName = "pet_comparison", icon = icon("exchange-alt")),

    # National statistics comparison (conditional)
    if (has_national_stats) menuItem("National stats comparison", tabName = "national_stats", icon = icon("globe")),

    # Overview (last)
    menuItem("Overview", tabName = "overview", icon = icon("th-large"))
  )
)

body <- dashboardBody(
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),

  tabItems(
    tabItem(tabName = "background", backgroundUI("background")),
    tabItem(tabName = "databases", databasesUI("databases")),

    # Episode frequency
    tabItem(tabName = "episode_frequency", episodeFrequencyUI("episode_frequency")),
    tabItem(tabName = "pregnancy_frequency", pregnancyFrequencyUI("pregnancy_frequency")),

    # Episode duration
    tabItem(tabName = "gestational_age", gestationalAgeUI("gestational_age")),
    tabItem(tabName = "gestational_age_binned", gestationalAgeBinnedUI("gestational_age_binned")),
    tabItem(tabName = "gestational_age_plausibility", gestationalAgePlausibilityUI("gestational_age_plausibility")),
    tabItem(tabName = "gestational_age_days", gestationalAgeDaysPerCategoryUI("gestational_age_days")),
    tabItem(tabName = "temporal_patterns", temporalPatternsUI("temporal_patterns")),

    # Episode construction
    tabItem(tabName = "pregnancy_overlap", pregnancyOverlapUI("pregnancy_overlap")),
    tabItem(tabName = "swapped_dates", swappedDatesUI("swapped_dates")),
    tabItem(tabName = "missing_dates", missingDatesUI("missing_dates")),

    # Episode outcomes
    tabItem(tabName = "delivery_mode", deliveryModeUI("delivery_mode")),
    tabItem(tabName = "delivery_mode_by_year", deliveryModeByYearUI("delivery_mode_by_year")),
    tabItem(tabName = "outcome_categories", outcomeCategoriesUI("outcome_categories")),

    # Cohort Characteristics
    tabItem(tabName = "characteristics",
      if (has_characteristics) characteristicsUI("characteristics") else p("No characteristics data available.")
    ),

    # Incidence / Prevalence
    tabItem(tabName = "incidence",
      if (has_incidence) incidenceUI("incidence") else p("No incidence data available.")
    ),
    tabItem(tabName = "prevalence",
      if (has_prevalence) prevalenceUI("prevalence") else p("No prevalence data available.")
    ),

    # Observation Period
    tabItem(tabName = "observation_period", observationPeriodUI("observation_period")),

    # Concept counts
    tabItem(tabName = "esd_concepts",
      if (has_esd_concepts) conceptCountsUI("esd_concepts") else p("No ESD concept counts available.")
    ),
    tabItem(tabName = "hip_concepts",
      if (has_hip_concepts) conceptCountsUI("hip_concepts") else p("No HIP concept counts available.")
    ),
    tabItem(tabName = "pps_concepts",
      if (has_pps_concepts) conceptCountsUI("pps_concepts") else p("No PPS concept counts available.")
    ),

    # Age
    tabItem(tabName = "age_summary",
      if (has_age_summary) ageSummaryUI("age_summary") else p("No age summary data available.")
    ),
    tabItem(tabName = "age_first_pregnancy",
      if (has_age_first_pregnancy) ageFirstPregnancyUI("age_first_pregnancy") else p("No age at first pregnancy data available.")
    ),
    tabItem(tabName = "age_first_pregnancy_end",
      if (has_age_first_pregnancy_end) ageFirstPregnancyEndUI("age_first_pregnancy_end") else p("No age at first pregnancy end data available.")
    ),
    tabItem(tabName = "age_groups",
      if (has_age_groups) ageGroupsUI("age_groups") else p("No age distribution data available.")
    ),

    # Attrition
    tabItem(tabName = "attrition",
      if (has_attrition) attritionUI("attrition") else p("No attrition data available.")
    ),
    tabItem(tabName = "attrition_cleanup",
      if (has_attrition_cleanup) attritionUI("attrition_cleanup") else p("No attrition cleanup data available.")
    ),

    # Concept check
    tabItem(tabName = "concept_check", conceptCheckUI("concept_check")),

    # Precision days
    tabItem(tabName = "precision_days", precisionDaysUI("precision_days")),

    # Quality check cleanup
    tabItem(tabName = "quality_check", qualityCheckCleanupUI("quality_check")),

    # PET comparison
    tabItem(tabName = "pet_comparison",
      if (has_pet_comparison_sr) petComparisonUI("pet_comparison")
      else if (has_pet_legacy) petComparisonLegacyContainerUI("pet_legacy")
      else p("No PET comparison data available.")
    ),

    # Version differences
    tabItem(tabName = "version_diff",
      if (has_version_diff) versionDifferencesUI("version_diff")
      else p("No version differences data available.")
    ),

    # National statistics comparison
    tabItem(tabName = "national_stats",
      if (has_national_stats) nationalStatsComparisonUI("national_stats")
      else p("No national statistics data available.")
    ),

    # Overview (last)
    tabItem(tabName = "overview", overviewUI("overview"))
  ),

  customDarwinFooter()
)

# No-data fallback UI
if (!hasData || !exists("allDP")) {
  ui <- fluidPage(
    titlePanel("PregnancyIdentifier"),
    mainPanel(
      p("No data available. Add export zip files or one subfolder per database (with CSV files) inside the data folder to view results.")
    )
  )
} else if (!exists("cdmSource") || is.null(cdmSource) || nrow(cdmSource) == 0) {
  ui <- fluidPage(
    titlePanel("PregnancyIdentifier"),
    mainPanel(
      p("No valid database metadata (cdm_source) found in the data folder.")
    )
  )
} else {
  ui <- dashboardPage(header, sidebar, body)
}
