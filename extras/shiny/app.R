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
    dplyr::select_if(~ !all(is.na(.)))

  gestationalAgeDaysCounts <- gestationalAgeDaysCounts %>%
    tidyr::pivot_longer(cols = setdiff(colnames(.), c("cdm_name")), names_to = "name", values_to = gestationalAgeDaysCounts$cdm_name) %>%
    dplyr::select(-"cdm_name")

  swappedDates <- swappedDates %>%
    tidyr::pivot_longer(cols = setdiff(colnames(.), c("cdm_name")), names_to = "name", values_to = swappedDates$cdm_name) %>%
    dplyr::select(-"cdm_name")

  ageSummary <- ageSummary %>%
    tidyr::pivot_longer(cols = setdiff(colnames(.), c("colName", "cdm_name")), names_to = ageSummary$colName, values_to = ageSummary$cdm_name) %>%
    dplyr::select(-c("cdm_name", "colName"))

  dateConsistancy <- dateConsistancy %>%
    tidyr::pivot_longer(cols = setdiff(colnames(.), c("cdm_name")), names_to = "name", values_to = dateConsistancy$cdm_name) %>%
    dplyr::select(-"cdm_name")

  observationPeriodRange <- observationPeriodRange %>%
    tidyr::pivot_longer(cols = setdiff(colnames(.), c("cdm_name")), names_to = "name", values_to = observationPeriodRange$cdm_name) %>%
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

  gestationalAgeDaysSummary <- gestationalAgeDaysSummary %>%
    tidyr::pivot_longer(cols = setdiff(colnames(.), c("colName", "cdm_name")), names_to = "name", values_to = gestationalAgeDaysSummary$cdm_name) %>%
    dplyr::select(-c("cdm_name", "colName"))

  gestationalAgeDaysPerCategorySummary <- do.call(rbind, lapply(unique(gestationalAgeDaysPerCategorySummary$cdm_name), FUN = function(name) {
    gestationalAgeDaysPerCategorySummary %>%
      dplyr::filter(cdm_name == name) %>%
      dplyr::select(-c("cdm_name", "colName")) %>%
      tidyr::pivot_longer(cols = setdiff(colnames(.), c("final_outcome_category")), names_to = "name", values_to = unique(gestationalAgeDaysPerCategorySummary$cdm_name))
  }))

  minObservationPeriod <- observationPeriodRange %>%
    dplyr::filter(name == "min_obs") %>%
    dplyr::select(-c("name")) %>%
    as.numeric() %>%
    min()

  pregnancyOverlapCounts <- pregnancyOverlapCounts %>%
    dplyr::select(-"colName")

  summariseGestationalWeeks <- function(data, lowerBoundary, upperBoundary) {
    data %>%
      dplyr::filter(gestational_weeks >= lowerBoundary & gestational_weeks <= upperBoundary) %>%
      dplyr::select(-"gestational_weeks") %>%
      dplyr::group_by(cdm_name) %>%
      dplyr::summarise(n = sum(n),
                       pct = sum(pct)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(gestational_weeks = glue::glue("{lowerBoundary}-{upperBoundary}"), .after = "cdm_name")
  }

  gestationalWeeks <- gestationalWeeks %>%
    dplyr::mutate(gestational_weeks = as.numeric(gestational_weeks),
                  n = as.numeric(n),
                  pct = as.numeric(pct))
  maxWeeks <- round(max(gestationalWeeks$gestational_weeks), -2)
  gestationalWeeksSummary <- rbind(gestationalWeeks %>% dplyr::filter(gestational_weeks <=50),
                                   summariseGestationalWeeks(gestationalWeeks, 51, 100))
  intervals <- seq(100, maxWeeks, 100)
  gestationalWeeksSummary <- rbind(gestationalWeeksSummary,
                                   do.call("rbind", lapply(intervals, FUN = function(weeks) {
                                     summariseGestationalWeeks(gestationalWeeks, weeks+1, weeks+100)
                                   }))) %>%
    dplyr::mutate(gestational_weeks = factor(x = gestational_weeks, levels = gestational_weeks))

  # trend data
  yearlyTrend <- yearlyTrend %>%
    dplyr::mutate(count = as.numeric(count),
                  year = as.numeric(year))
  monthlyTrends <- monthlyTrends %>%
    dplyr::mutate(count = as.numeric(count))

  # outcome categories
  outcomeCategoriesCount <- outcomeCategoriesCount %>%
    dplyr::mutate(n = as.numeric(n),
                  pct = round(as.numeric(pct), 4))

  # incidence
  saveRDS(list("dbinfo" = dbinfo,
               "incidence" = incidence,
               "ageSummary" = ageSummary,
               "dateConsistancy" = dateConsistancy,
               "episodeFrequency" = episodeFrequency,
               "episodeFrequencySummary" = episodeFrequencySummary,
               "gestationalAgeDaysCounts" = gestationalAgeDaysCounts,
               "gestationalAgeDaysPerCategorySummary" = gestationalAgeDaysPerCategorySummary,
               "gestationalAgeDaysSummary" = gestationalAgeDaysSummary,
               "gestationalWeeksSummary" = gestationalWeeksSummary,
               "monthlyTrends" = monthlyTrends,
               "monthlyTrendMissing" = monthlyTrendMissing,
               "observationPeriodRange" = observationPeriodRange,
               "minObservationPeriod" = minObservationPeriod,
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
  episodeFrequencySummary <- cachedData$episodeFrequencySummary
  gestationalAgeDaysCounts <- cachedData$gestationalAgeDaysCounts
  gestationalAgeDaysPerCategorySummary <- cachedData$gestationalAgeDaysPerCategorySummary
  gestationalAgeDaysSummary <- cachedData$gestationalAgeDaysSummary
  gestationalWeeksSummary <- cachedData$gestationalWeeksSummary
  monthlyTrends <- cachedData$monthlyTrends
  monthlyTrendMissing <- cachedData$monthlyTrendMissing
  observationPeriodRange <- cachedData$observationPeriodRange
  minObservationPeriod <- cachedData$minObservationPeriod
  outcomeCategoriesCount <- cachedData$outcomeCategoriesCount
  pregnancyFrequency <- cachedData$pregnancyFrequency
  pregnancyOverlapCounts <- cachedData$pregnancyOverlapCounts
  swappedDates <- cachedData$swappedDates
  yearlyTrend <- cachedData$yearlyTrend
  yearlyTrendMissing <- cachedData$yearlyTrendMissing
}

######### Shiny app ########
allDP <- unique(dbinfo$cdm_name)
plotHeight <- "600px"

appStructure <- list(
  "Study background" = list(StudyBackground$new(
    background = "./background.md",
    EUPAS = "EUPAS"
  )),
  "Database information" = list(handleEmptyResult(object = FilterTableModule$new(dbinfo), result = dbinfo)),
  "Checks" = list(
    "Gestational age days" = handleEmptyResult(object = FilterTableModule$new(data = gestationalAgeDaysCounts, dp = allDP), result = gestationalAgeDaysCounts),
    "Gestational weeks" = list(handleEmptyResult(object = FilterTableModule$new(data = gestationalWeeksSummary, dp = allDP), result = gestationalWeeksSummary),
                               handleEmptyResult(object = PlotPlotly$new(fun = gestationalDurationPlot,
                                                                         args = list(data = gestationalWeeksSummary),
                                                                         height = plotHeight,
                                                                         title = ""), result = gestationalWeeksSummary)),
    "Pregnancy overlap" = handleEmptyResult(object = FilterTableModule$new(data = pregnancyOverlapCounts, dp = allDP), result = pregnancyOverlapCounts),
    "Swapped dates" = handleEmptyResult(object = FilterTableModule$new(data = swappedDates, dp = allDP), result = swappedDates),
    "Date consistency" = handleEmptyResult(object = FilterTableModule$new(data = dateConsistancy, dp = allDP), result = dateConsistancy)
  ),
  "Results" = list(
    "Pregnancy frequency" = handleEmptyResult(object = FilterTableModule$new(data = pregnancyFrequency, dp = allDP), result = pregnancyFrequency),
    "Episode frequency" = list(handleEmptyResult(object = FilterTableModule$new(data = episodeFrequency, dp = allDP), result = episodeFrequency),
                               handleEmptyResult(object = PlotPlotly$new(fun = boxPlot,
                                                                         args = list(data = episodeFrequencySummary),
                                                                         height = plotHeight), result = episodeFrequencySummary)),
    "Age summary" = list(handleEmptyResult(object = FilterTableModule$new(data = ageSummary, dp = allDP), result = ageSummary),
                         handleEmptyResult(object = PlotPlotly$new(fun = boxPlot,
                                                                   args = list(data = ageSummary, transform = TRUE),
                                                                   height = plotHeight), result = ageSummary)),
    "Observation period range" = handleEmptyResult(object = FilterTableModule$new(data = observationPeriodRange, dp = allDP), result = observationPeriodRange),
    "Incidence" = list(handleEmptyResult(object = Incidence$new(incidence), result = incidence)),
    "Gestational age days per category" = handleEmptyResult(object = FilterTableModule$new(data = gestationalAgeDaysPerCategorySummary, dp = allDP), result = gestationalAgeDaysPerCategorySummary),
    "Gestational age days summary" = list(handleEmptyResult(object = FilterTableModule$new(data = gestationalAgeDaysSummary, dp = allDP), result = gestationalAgeDaysSummary),
                                          handleEmptyResult(object = PlotPlotly$new(fun = boxPlot,
                                                                                    args = list(data = gestationalAgeDaysSummary),
                                                                                    height = plotHeight), result = gestationalAgeDaysSummary)),
    "Outcome categories" = list(handleEmptyResult(object = FilterTableModule$new(data = outcomeCategoriesCount, dp = allDP), result = outcomeCategoriesCount),
                                handleEmptyResult(object = PlotPlotly$new(fun = outcomeCategoriesPlot,
                                                                          args = list(data = outcomeCategoriesCount),
                                                                          height = plotHeight), result = outcomeCategoriesCount)),
    "Monthly trends" = list(handleEmptyResult(object = FilterTableModule$new(data = monthlyTrends, dp = allDP), result = monthlyTrends),
                            handleEmptyResult(object = PlotPlotly$new(fun = monthTrendsPlot,
                                                                      args = list(data = monthlyTrends),
                                                                      height = plotHeight), result = monthlyTrends)),
    "Monthly trend missing" = handleEmptyResult(object = FilterTableModule$new(data = monthlyTrendMissing, dp = allDP), result = monthlyTrendMissing),
    "Yearly trend" = list(handleEmptyResult(object = FilterTableModule$new(data = yearlyTrend, dp = allDP), result = yearlyTrend),
                         handleEmptyResult(object = PlotPlotly$new(fun = yearTrendsPlot,
                                                                   args = list(data = yearlyTrend,
                                                                               xIntercept = minObservationPeriod),
                                                                   height = plotHeight), result = yearlyTrend)),
    "Yearly trend missing" = handleEmptyResult(object = FilterTableModule$new(data = yearlyTrendMissing, dp = allDP), result = yearlyTrendMissing)
  )
)

DarwinShinyModules::launchDarwinDashboardApp(appStructure, title = "PregnancyIdentifier")
