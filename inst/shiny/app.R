library(shiny)
library(DarwinShinyModules)
library(IncidencePrevalence)
library(ggplot2)
library(visOmopResults)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
# cleanup environment variables

rm(list = ls())

############################ Load modules ############################

sapply(list.files("utils", full.names = T), source)

############################ Load data ############################

if (!exists("shinySettings")) {
  # Allow tests to force a data folder via option or env var (e.g. shinytest2)
  opt_data <- getOption("shiny.data.folder", "")
  env_data <- Sys.getenv("SHINY_DATA_FOLDER", "")
  if (nzchar(opt_data)) {
    shinySettings <- list(dataFolder = opt_data)
  } else if (nzchar(env_data)) {
    shinySettings <- list(dataFolder = env_data)
  } else {
    dataFolder <- "data-test"
    if (file.exists(dataFolder)) {
      shinySettings <- list(dataFolder = dataFolder)
    } else {
      shinySettings <- list(dataFolder = paste0("./", dataFolder))
    }
  }
}

dataFolder <- shinySettings$dataFolder

zipFiles <- list.files(dataFolder, pattern = "\\.zip$", full.names = TRUE)
# Only process existing non-NA paths (exclude missing paths or directories)
zipFiles <- zipFiles[!is.na(zipFiles) & nzchar(zipFiles)]
if (length(zipFiles) > 0) {
  info <- file.info(zipFiles)
  zipFiles <- zipFiles[file.exists(zipFiles) & !is.na(info$size) & info$size > 0]
}

if (length(zipFiles) == 0) {
  # No export zip files: launch minimal app so shinytest can verify UI
  ui <- fluidPage(
    titlePanel("PregnancyIdentifier"),
    mainPanel(
      shiny::p("No data available. Add export zip files to the data folder to view results.")
    )
  )
  server <- function(input, output, session) {}
  shiny::shinyApp(ui = ui, server = server)
} else {
for (i in 1:length(zipFiles)) {
  writeLines(paste("Processing", zipFiles[i]))
  tempFolder <- tempfile()
  dir.create(tempFolder)
  unzip(zipFiles[i], exdir = tempFolder, junkpaths = TRUE)
  csvFiles <- list.files(tempFolder, pattern = ".csv")
  zipName <- gsub(".zip", "", basename(zipFiles[i]))
  zipNameParts <- unlist(strsplit(zipName, "-"))
  dbName <- zipNameParts[4]
  runDate <- gsub(paste0("-", dbName, ".*"), "", zipName)
  lapply(csvFiles, loadFile, dbName = dbName, runDate = runDate, folder = tempFolder, overwrite = (i == 1))
  unlink(tempFolder, recursive = TRUE)
}
# formatting
dbinfo <- cdmSource %>%
  dplyr::select_if(~ !all(is.na(.))) %>%
  dplyr::arrange(cdm_name)

allDP <- unique(dbinfo$cdm_name)
allDP <- allDP[order(allDP)]

gestationalAgeDaysCounts <- dataToLong(gestationalAgeDaysCounts)

swappedDates <- dataToLong(swappedDates) %>%
  dplyr::select(dplyr::all_of(c("name", allDP))) %>%
  dplyr::mutate(across(!name, as.numeric))

episodeCount <- pregnancyOverlapCounts %>%
  dplyr::mutate(total = as.numeric(total)) %>%
  dplyr::group_by(cdm_name, total) %>%
  dplyr::summarise(total = mean(total), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = "cdm_name", values_from = "total") %>%
  dplyr::mutate(name = "total") %>%
  dplyr::select(dplyr::all_of(c("name", allDP)))
swappedDates <- rbind(swappedDates, episodeCount)
percDF <- rbind(cbind(name = "rev_hip_perc", round(100*swappedDates[1, -1] / swappedDates[3, -1], 2)),
                cbind(name = "rev_pps_perc", round(100*swappedDates[2, -1] / swappedDates[3, -1], 2)))
colnames(percDF) <- colnames(swappedDates)
swappedDates <- rbind(swappedDates, percDF) %>%
  dplyr::arrange(match(name, c("rev_hip_perc", "rev_pps_perc", "n_rev_hip", "n_rev_pps", "total")))

ageSummary <- dataToLong(ageSummary, skipCols = c("cdm_name", "colName"))
numRound <- function(x) { round(100*as.numeric(x), 2) }
dateConsistancy <- dataToLong(dateConsistancy) %>%
  dplyr::mutate(across(!name, numRound))

observationPeriodRange <- dataToLong(observationPeriodRange)

pregnancyFrequencyList <- lapply(unique(pregnancyFrequency$cdm_name), FUN = function(name) {
  pregnancyFrequency %>%
    dplyr::filter(cdm_name == name) %>%
    dplyr::select(-"cdm_name") %>%
    dplyr::rename(!!name := number_individuals)})
pregnancyFrequencyList <- pregnancyFrequencyList[order(sapply(pregnancyFrequencyList, nrow), decreasing = T)]
pregnancyFrequency <- purrr::reduce(pregnancyFrequencyList, dplyr::left_join, by = "freq")
pregnancyFrequency <- pregnancyFrequency %>%
  dplyr::mutate(freq = factor(freq, levels = unique(pregnancyFrequency$freq))) %>%
  dplyr::mutate(across(!freq, as.numeric)) %>%
  suppressCounts(colNames = allDP)

episodeFrequency <- dataToLong(episodeFrequency %>% dplyr::left_join(episodeFrequencySummary), skipCols = c("cdm_name", "colName"))

gestationalAgeDaysSummary <- dataToLong(gestationalAgeDaysSummary, skipCols = c("colName", "cdm_name"))
gestationalAgeDaysPerCategorySummary <- gestationalAgeDaysPerCategorySummary %>%
  dplyr::mutate_at(vars(-(c("cdm_name", "final_outcome_category", "colName"))), as.numeric) %>%
  dplyr::mutate(final_outcome_category = factor(final_outcome_category, levels = c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG")))

minObservationPeriod <- observationPeriodRange %>%
  dplyr::filter(name == "min_obs") %>%
  dplyr::select(-c("name")) %>%
  as.numeric() %>%
  min()

pregnancyOverlapCounts <- pregnancyOverlapCounts %>%
  dplyr::select(-"colName") %>%
  dplyr::mutate(n = as.numeric(n),
                total = as.numeric(total),
                pct = as.numeric(pct))

summariseGestationalWeeks <- function(data, lowerBoundary, upperBoundary, label = NULL) {
  if (is.null(label)) {
    label <- glue::glue("{lowerBoundary}-{upperBoundary}")
  }
  data %>%
    dplyr::filter(gestational_weeks >= lowerBoundary & gestational_weeks < upperBoundary) %>%
    dplyr::select(-"gestational_weeks") %>%
    dplyr::group_by(cdm_name) %>%
    dplyr::summarise(n = sum(n),
                     pct = sum(pct)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gestational_weeks = label, .after = "cdm_name")
}

gestationalWeeks <- gestationalWeeks %>%
  dplyr::mutate(gestational_weeks = as.numeric(gestational_weeks),
                n = as.numeric(n),
                pct = as.numeric(pct))
maxWeeks <- round(max(gestationalWeeks$gestational_weeks, na.rm = T), -2)

gestationalWeeksSummary <- gestationalWeeks %>%
  dplyr::mutate(pct = round(pct, 1))

gestationalWeeksBinned <- rbind(summariseGestationalWeeks(gestationalWeeks, 0, 12, "<12"),
                                summariseGestationalWeeks(gestationalWeeks, 12, 28),
                                summariseGestationalWeeks(gestationalWeeks, 28, 32),
                                summariseGestationalWeeks(gestationalWeeks, 32, 37),
                                summariseGestationalWeeks(gestationalWeeks, 37, 42),
                                summariseGestationalWeeks(gestationalWeeks, 42, maxWeeks, ">42"))

gestationalWeeksBinned <- gestationalWeeksBinned %>%
  dplyr::mutate(gestational_weeks = factor(gestational_weeks, levels = c("<12", "12-28", "28-32", "32-37", "37-42", ">42")),
                pct = round(pct, 1))

# trend data
yearlyTrend <- yearlyTrend %>%
  dplyr::mutate(count = as.numeric(count),
                year = as.numeric(year),
                period = "year") %>%
  dplyr::rename(value = year)

yearlyTrendMissing <- yearlyTrendMissing %>%
  dplyr::mutate(count = as.numeric(count),
                year = as.numeric(year),
                period = "year") %>%
  dplyr::rename(value = year)

monthlyTrends <- monthlyTrends %>%
  dplyr::mutate(count = as.numeric(count),
                period = "month") %>%
  dplyr::rename(value = month)
monthlyTrendMissing <- monthlyTrendMissing %>%
  dplyr::mutate(count = as.numeric(count),
                period = "month") %>%
  dplyr::rename(value = month)

trendData <- rbind(yearlyTrend, monthlyTrends)
trendDataMissing <- rbind(yearlyTrendMissing, monthlyTrendMissing)

# outcome categories
outcomeCategoriesCount <- outcomeCategoriesCount %>%
  dplyr::mutate(n = as.numeric(n),
                pct = round(as.numeric(pct), 4)) %>%
  dplyr::mutate(outcome_category = factor(outcome_category, levels = c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG")))

# delivery mode
deliveryModeSummary <- deliveryModeSummary %>%
  dplyr::select(-c("cesarean_count", "vaginal_count")) %>%
  dplyr::rename(total = n)

deliveryModeSummary <- dplyr::left_join(
  deliveryModeSummary %>%
    tidyr::pivot_longer(cols = c("cesarean", "vaginal"), names_to = "mode", values_to = "n"),
  deliveryModeSummary %>%
    dplyr::select(-c("cesarean", "vaginal")) %>%
    dplyr::rename(vaginal = vaginal_pct,
                  cesarean = cesarean_pct) %>%
    tidyr::pivot_longer(cols = c("cesarean", "vaginal"), names_to = "mode", values_to = "pct")) %>%
  dplyr::mutate_at(vars(-(c("cdm_name", "final_outcome_category", "mode"))), as.numeric) %>%
  dplyr::mutate(final_outcome_category = factor(final_outcome_category, levels = c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG"))) %>%
  dplyr::select(c("cdm_name", "final_outcome_category", "mode", "total", "n", "pct"))

######### Shiny app ########
allDP <- unique(dbinfo$cdm_name)
allDP <- allDP[order(allDP)]
defaultPlotHeight <- "400px"
plotHeight <- "600px"

appStructure <- list(
  "Study background" = list(StudyBackground$new(
    background = "./background.md",
    EUPAS = "EUPAS"
  )),
  "Database information" = list(handleEmptyResult(object = FilterTableModule$new(dbinfo), result = dbinfo)),
  "Episode frequency" = list(
    "Episode frequency" = handleEmptyResult(object = EpisodeFrequencyModule$new(data = episodeFrequency, dp = allDP), result = episodeFrequency),
    "Incidence rate" = handleEmptyResult(object = IncidencePlot$new(data = incidence), result = incidence),
    "Pregnancy frequency" = handleEmptyResult(object = PregnancyFrequencyModule$new(data = pregnancyFrequency, dp = allDP), result = pregnancyFrequency),
    "Age summary" = handleEmptyResult(object = AgeSummaryModule$new(data = ageSummary, dp = allDP), result = ageSummary)
  ),
  "Episode duration" = list(
    "Gestational age weeks" = handleEmptyResult(object = GestationalAgeModule$new(data = gestationalWeeksSummary, daysData = gestationalAgeDaysCounts, dp = allDP), result = gestationalWeeksSummary),
    "Gestational age binned" = handleEmptyResult(object = GestationalAgeModule$new(data = gestationalWeeksBinned, dp = allDP, maxWeeksFilter = FALSE), result = gestationalWeeksSummary),
    "Gestational age days per category" = handleEmptyResult(object = GestationalAgeDaysPerCategoryModule$new(data = gestationalAgeDaysPerCategorySummary, dp = allDP), result = gestationalAgeDaysPerCategorySummary),
    "Temporal patterns" = handleEmptyResult(object = TemporalPatternsModule$new(data = trendData, missingData = trendDataMissing, dp = allDP), result = trendData)
  ),
  "Episode construction" = list(
    "Pregnancy overlap" = handleEmptyResult(object = EpisodeConstructionModule$new(data = pregnancyOverlapCounts,
                                                                                   dp = allDP,
                                                                                   yVar = "pct",
                                                                                   label = "n",
                                                                                   fillVar = "overlap",
                                                                                   title =  "Pregnancy overlap counts"), result = pregnancyOverlapCounts),
    "Swapped dates" = handleEmptyResult(object = EpisodeConstructionModule$new(data = swappedDates,
                                                                               dp = allDP,
                                                                               convertDataForPlot = TRUE,
                                                                               yVar = "value",
                                                                               fillVar = "name",
                                                                               title =  "Swapped dates"), result = swappedDates),
    "Date consistency" = handleEmptyResult(object = EpisodeConstructionModule$new(data = dateConsistancy,
                                                                                  dp = allDP,
                                                                                  convertDataForPlot = TRUE,
                                                                                  yVar = "value",
                                                                                  fillVar = "name",
                                                                                  title =  "Date consistency"), result = dateConsistancy)
  ),
  "Episode outcomes" = list(
    "Mode of delivery" = handleEmptyResult(object = DeliveryModeModule$new(data = deliveryModeSummary,
                                                                           dp = allDP,
                                                                           yVar = "pct",
                                                                           label = "n",
                                                                           fillVar = "mode",
                                                                           title =  "Delivery mode rates"), result = deliveryModeSummary),
    "Outcome categories" = handleEmptyResult(object = OutcomeCategoriesModule$new(data = outcomeCategoriesCount, dp = allDP), result = outcomeCategoriesCount)
  ),
  "Cohort Characteristics" = list(handleEmptyResult(object = Characteristics$new(characteristics), result = characteristics)),
  "IncidencePrevalence" = list(
    "Incidence" = list(handleEmptyResult(object = Incidence$new(incidence), result = incidence)),
    "Prevalence" = list(handleEmptyResult(object = Prevalence$new(prevalence, defaults = list()), result = prevalence))
  ),
  "Other" = list(
    "Observation period range" = handleEmptyResult(object = FilterTableModule$new(data = observationPeriodRange, dp = allDP), result = observationPeriodRange)
  )
)

# PET comparison tab (when comparePregnancyIdentifierWithPET outputs are loaded from zip)
if (exists("petComparisonEpisodeCounts", envir = .GlobalEnv)) {
  petComparisonEpisodeCounts <- get("petComparisonEpisodeCounts", envir = .GlobalEnv)
  appStructure[["PET comparison"]] <- list(
    "Episode counts" = handleEmptyResult(object = FilterTableModule$new(data = petComparisonEpisodeCounts), result = petComparisonEpisodeCounts)
  )
  if (exists("petComparisonVennCounts", envir = .GlobalEnv)) {
    petComparisonVennCounts <- get("petComparisonVennCounts", envir = .GlobalEnv)
    appStructure[["PET comparison"]][["Venn counts"]] <- handleEmptyResult(object = FilterTableModule$new(data = petComparisonVennCounts), result = petComparisonVennCounts)
  }
  if (exists("petComparisonPpvSensitivity", envir = .GlobalEnv)) {
    petComparisonPpvSensitivity <- get("petComparisonPpvSensitivity", envir = .GlobalEnv)
    appStructure[["PET comparison"]][["PPV and sensitivity"]] <- handleEmptyResult(object = FilterTableModule$new(data = petComparisonPpvSensitivity), result = petComparisonPpvSensitivity)
  }
  if (exists("petComparisonDateDifferenceSummary", envir = .GlobalEnv)) {
    petComparisonDateDifferenceSummary <- get("petComparisonDateDifferenceSummary", envir = .GlobalEnv)
    appStructure[["PET comparison"]][["Date difference summary"]] <- handleEmptyResult(object = FilterTableModule$new(data = petComparisonDateDifferenceSummary), result = petComparisonDateDifferenceSummary)
  }
  if (exists("petComparisonOutcomeConfusionMatrix", envir = .GlobalEnv)) {
    petComparisonOutcomeConfusionMatrix <- get("petComparisonOutcomeConfusionMatrix", envir = .GlobalEnv)
    appStructure[["PET comparison"]][["Outcome confusion matrix"]] <- handleEmptyResult(object = FilterTableModule$new(data = petComparisonOutcomeConfusionMatrix), result = petComparisonOutcomeConfusionMatrix)
  }
  if (exists("petComparisonOutcomeAccuracy", envir = .GlobalEnv)) {
    petComparisonOutcomeAccuracy <- get("petComparisonOutcomeAccuracy", envir = .GlobalEnv)
    appStructure[["PET comparison"]][["Outcome accuracy"]] <- handleEmptyResult(object = FilterTableModule$new(data = petComparisonOutcomeAccuracy), result = petComparisonOutcomeAccuracy)
  }
  if (exists("petComparisonDurationSummary", envir = .GlobalEnv)) {
    petComparisonDurationSummary <- get("petComparisonDurationSummary", envir = .GlobalEnv)
    appStructure[["PET comparison"]][["Duration summary"]] <- handleEmptyResult(object = FilterTableModule$new(data = petComparisonDurationSummary), result = petComparisonDurationSummary)
  }
  if (exists("petComparisonDurationDistribution", envir = .GlobalEnv)) {
    petComparisonDurationDistribution <- get("petComparisonDurationDistribution", envir = .GlobalEnv)
    appStructure[["PET comparison"]][["Duration distribution"]] <- handleEmptyResult(object = FilterTableModule$new(data = petComparisonDurationDistribution), result = petComparisonDurationDistribution)
  }
}

app <- DarwinDashboardApp$new(appStructure, title = "PregnancyIdentifier")
app$launch()
}
