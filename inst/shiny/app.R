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
  dataFolder <- "data"
  if (file.exists(dataFolder)) {
    shinySettings <- list(dataFolder = dataFolder)
  } else {
    shinySettings <- list(dataFolder = paste0("./", dataFolder))
  }
}

dataFolder <- shinySettings$dataFolder

zipFiles <- list.files(dataFolder, pattern = ".zip", full.names = TRUE)

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

gestationalAgeDaysCounts <- dataToLong(gestationalAgeDaysCounts)

swappedDates <- dataToLong(swappedDates)
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
  dplyr::mutate(freq = factor(freq, levels = unique(pregnancyFrequency$freq)))

episodeFrequency <- dataToLong(episodeFrequency %>% dplyr::left_join(episodeFrequencySummary), skipCols = c("cdm_name", "colName"))

gestationalAgeDaysSummary <- dataToLong(gestationalAgeDaysSummary, skipCols = c("colName", "cdm_name"))
gestationalAgeDaysPerCategorySummary <- gestationalAgeDaysPerCategorySummary %>%
  dplyr::mutate_at(vars(-(c("cdm_name", "final_outcome_category", "colName"))), as.numeric)

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
  #dplyr::filter(gestational_weeks > 0) %>%
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
                pct = round(as.numeric(pct), 4))

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

app <- DarwinDashboardApp$new(appStructure, title = "PregnancyIdentifier")
app$launch()
