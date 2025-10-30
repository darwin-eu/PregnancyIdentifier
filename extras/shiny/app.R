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

cachedResultsFile <- "data.rds"
if (!file.exists(cachedResultsFile)) {
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
    dbName <- basename(zipFiles[i])
    lapply(csvFiles, loadFile, dbName = dbName, folder = tempFolder, overwrite = (i == 1))
    unlink(tempFolder, recursive = TRUE)
  }
  # incidence
  saveRDS(list("incidence" = incidence,
               "ageSummary" = ageSummary,
               "dateConsistancy" = dateConsistancy,
               "episodeFrequency" = episodeFrequency,
               "episodeFrequencySummary" = episodeFrequencySummary,
               "gestationalAgeDaysCounts" = gestationalAgeDaysCounts,
               "gestationalAgeDaysPerCategorySummary" = gestationalAgeDaysPerCategorySummary,
               "gestationalAgeDaysSummary" = gestationalAgeDaysSummary,
               "gestationalWeeks" = gestationalWeeks,
               "monthlyTrends" = monthlyTrends,
               "monthlyTrendMissing" = monthlyTrendMissing,
               "observationPeriodRange" = observationPeriodRange,
               "outcomeCategoriesCount" = outcomeCategoriesCount,
               "pregnancyFrequency" = pregnancyFrequency,
               "pregnancyOverlapCounts" = pregnancyOverlapCounts,
               "swappedDates" = swappedDates,
               "yearlyTrend" = yearlyTrend,
               "yearlyTrendMissing" = yearlyTrendMissing
               ),
          cachedResultsFile)
} else {
  cachedData <- readRDS(cachedResultsFile)
  incidence <- cachedData$incidence
  ageSummary <- cachedData$ageSummary
  dateConsistancy <- cachedData$dateConsistancy
  episodeFrequency <- cachedData$episodeFrequency
  episodeFrequencySummary <- cachedData$episodeFrequencySummary
  gestationalAgeDaysCounts <- cachedData$gestationalAgeDaysCounts
  gestationalAgeDaysPerCategorySummary <- cachedData$gestationalAgeDaysPerCategorySummary
  gestationalAgeDaysSummary <- cachedData$gestationalAgeDaysSummary
  gestationalWeeks <- cachedData$gestationalWeeks
  monthlyTrends <- cachedData$monthlyTrends
  monthlyTrendMissing <- cachedData$monthlyTrendMissing
  observationPeriodRange <- cachedData$observationPeriodRange
  outcomeCategoriesCount <- cachedData$outcomeCategoriesCount
  pregnancyFrequency <- cachedData$pregnancyFrequency
  pregnancyOverlapCounts <- cachedData$pregnancyOverlapCounts
  swappedDates <- cachedData$swappedDates
  yearlyTrend <- cachedData$yearlyTrend
  yearlyTrendMissing <- cachedData$yearlyTrendMissing
}

######### Shiny app ########
appStructure <- list(
  "Study background" = list(StudyBackground$new(
    background = "./background.md",
    EUPAS = "EUPAS"
  )),
  "Incidence" = list(handleEmptyResult(object = Incidence$new(incidence), result = incidence)),
  "Tables" = list(
    "ageSummary" = handleEmptyResult(object = Table$new(data = ageSummary), result = ageSummary),
    "dateConsistancy" = handleEmptyResult(object = Table$new(data = dateConsistancy), result = dateConsistancy),
    "episodeFrequency" = handleEmptyResult(object = Table$new(data = episodeFrequency), result = episodeFrequency),
    "episodeFrequencySummary" = handleEmptyResult(object = Table$new(data = episodeFrequencySummary), result = episodeFrequencySummary),
    "gestationalAgeDaysCounts" = handleEmptyResult(object = Table$new(data = gestationalAgeDaysCounts), result = gestationalAgeDaysCounts),
    "gestationalAgeDaysPerCategorySummary" = handleEmptyResult(object = Table$new(data = gestationalAgeDaysPerCategorySummary), result = gestationalAgeDaysPerCategorySummary),
    "gestationalAgeDaysSummary" = handleEmptyResult(object = Table$new(data = gestationalAgeDaysSummary), result = gestationalAgeDaysSummary),
    "gestationalWeeks" = handleEmptyResult(object = Table$new(data = gestationalWeeks), result = gestationalWeeks),
    "incidence" = handleEmptyResult(object = Table$new(data = incidence), result = incidence),
    "monthlyTrends" = list(handleEmptyResult(object = Table$new(data = monthlyTrends), result = monthlyTrends),
                           handleEmptyResult(object = PlotPlotly$new(fun = monthTrendsPlot, args = list(data = monthlyTrends)), result = monthlyTrends)),
    "monthlyTrendMissing" = handleEmptyResult(object = Table$new(data = monthlyTrendMissing), result = monthlyTrendMissing),
    "observationPeriodRange" = handleEmptyResult(object = Table$new(data = observationPeriodRange), result = observationPeriodRange),
    "outcomeCategoriesCount" = handleEmptyResult(object = Table$new(data = outcomeCategoriesCount), result = outcomeCategoriesCount),
    "pregnancyFrequency" = handleEmptyResult(object = Table$new(data = pregnancyFrequency), result = pregnancyFrequency),
    "pregnancyOverlapCounts" = handleEmptyResult(object = Table$new(data = pregnancyOverlapCounts), result = pregnancyOverlapCounts),
    "swappedDates" = handleEmptyResult(object = Table$new(data = swappedDates), result = swappedDates),
    "yearlyTrend" = list(handleEmptyResult(object = Table$new(data = yearlyTrend), result = yearlyTrend),
                         handleEmptyResult(object = PlotPlotly$new(fun = yearTrendsPlot, args = list(data = yearlyTrend, xIntercept = observationPeriodRange$min_obs)), result = yearlyTrend)),
    "yearlyTrendMissing" = handleEmptyResult(object = Table$new(data = yearlyTrendMissing), result = yearlyTrendMissing)
  )
)

DarwinShinyModules::launchDarwinDashboardApp(appStructure, title = "PregnancyIdentifier")
