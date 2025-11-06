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
    dbName <- gsub(".zip", "", basename(zipFiles[i]))
    lapply(csvFiles, loadFile, dbName = dbName, folder = tempFolder, overwrite = (i == 1))
    unlink(tempFolder, recursive = TRUE)
  }
  # formatting
  dbinfo <- dbinfo %>%
    dplyr::select_if(~ !all(is.na(.))) %>%
    dplyr::rename(database = cdm_name)

  ageSummary <- ageSummary %>%
    tidyr::pivot_longer(cols = setdiff(colnames(.), c("colName", "cdm_name")), names_to = ageSummary$colName, values_to = ageSummary$cdm_name) %>%
    dplyr::select(-c("cdm_name", "colName"))

  dateConsistancy <- dateConsistancy %>%
    tidyr::pivot_longer(cols = setdiff(colnames(.), c("cdm_name")), names_to = "name", values_to = dateConsistancy$cdm_name) %>%
    dplyr::select(-"cdm_name")

  pregnancyFrequency <- do.call(rbind, lapply(unique(pregnancyFrequency$cdm_name), FUN = function(name) {
    pregnancyFrequency %>%
      dplyr::filter(cdm_name == name) %>%
      dplyr::select(-"cdm_name") %>%
      dplyr::rename(!!name := number_individuals)})
  )

  episodeFrequency <- episodeFrequency %>%
    dplyr::left_join(episodeFrequencySummary) %>%
    tidyr::pivot_longer(cols = setdiff(colnames(.), c("colName", "cdm_name")), names_to = "name", values_to = episodeFrequency$cdm_name) %>%
    dplyr::select(-c("cdm_name", "colName"))

  # incidence
  saveRDS(list("dbinfo" = dbinfo,
               "incidence" = incidence,
               "ageSummary" = ageSummary,
               "dateConsistancy" = dateConsistancy,
               "episodeFrequency" = episodeFrequency,
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
  dbinfo <- cachedData$dbinfo
  incidence <- cachedData$incidence
  ageSummary <- cachedData$ageSummary
  dateConsistancy <- cachedData$dateConsistancy
  episodeFrequency <- cachedData$episodeFrequency
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
  "Database information" = list(handleEmptyResult(object = DatabaseInfoModule$new(dbinfo), result = dbinfo)),
  "Checks" = list(
    "gestationalAgeDaysCounts" = handleEmptyResult(object = Table$new(data = gestationalAgeDaysCounts), result = gestationalAgeDaysCounts),
    "gestationalWeeks" = handleEmptyResult(object = Table$new(data = gestationalWeeks), result = gestationalWeeks),
    "pregnancyOverlapCounts" = handleEmptyResult(object = Table$new(data = pregnancyOverlapCounts), result = pregnancyOverlapCounts),
    "swappedDates" = handleEmptyResult(object = Table$new(data = swappedDates), result = swappedDates),
    "dateConsistency" = handleEmptyResult(object = Table$new(data = dateConsistancy), result = dateConsistancy)
  ),
  "Results" = list(
    "Pregnancy frequency" = handleEmptyResult(object = Table$new(data = pregnancyFrequency), result = pregnancyFrequency),
    "Episode frequency" = handleEmptyResult(object = Table$new(data = episodeFrequency), result = episodeFrequency),
    "ageSummary" = handleEmptyResult(object = Table$new(data = ageSummary), result = ageSummary),
    "observationPeriodRange" = handleEmptyResult(object = Table$new(data = observationPeriodRange), result = observationPeriodRange),
    "incidence" = list(handleEmptyResult(object = Incidence$new(incidence), result = incidence)),
    "gestationalAgeDaysPerCategorySummary" = handleEmptyResult(object = Table$new(data = gestationalAgeDaysPerCategorySummary), result = gestationalAgeDaysPerCategorySummary),
    "gestationalAgeDaysSummary" = handleEmptyResult(object = Table$new(data = gestationalAgeDaysSummary), result = gestationalAgeDaysSummary),
    "outcomeCategoriesCount" = handleEmptyResult(object = Table$new(data = outcomeCategoriesCount), result = outcomeCategoriesCount),
    "monthlyTrends" = list(handleEmptyResult(object = Table$new(data = monthlyTrends), result = monthlyTrends),
                           handleEmptyResult(object = PlotPlotly$new(fun = monthTrendsPlot, args = list(data = monthlyTrends)), result = monthlyTrends)),
    "monthlyTrendMissing" = handleEmptyResult(object = Table$new(data = monthlyTrendMissing), result = monthlyTrendMissing),
    "yearlyTrend" = list(handleEmptyResult(object = Table$new(data = yearlyTrend), result = yearlyTrend),
                         handleEmptyResult(object = PlotPlotly$new(fun = yearTrendsPlot, args = list(data = yearlyTrend, xIntercept = observationPeriodRange$min_obs)), result = yearlyTrend)),
    "yearlyTrendMissing" = handleEmptyResult(object = Table$new(data = yearlyTrendMissing), result = yearlyTrendMissing)
  )
)

DarwinShinyModules::launchDarwinDashboardApp(appStructure, title = "PregnancyIdentifier")
