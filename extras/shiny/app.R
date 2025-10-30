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
  saveRDS(list("incidence" = incidence),
          cachedResultsFile)
} else {
  cachedData <- readRDS(cachedResultsFile)
  incidence <- cachedData$incidence
}

######### Shiny app ########
appStructure <- list(
  "Study background" = list(StudyBackground$new(
    background = "./background.md",
    EUPAS = "EUPAS"
  )),
  "Incidence" = list(handleEmptyResult(object = Incidence$new(incidence), result = incidence))
)

DarwinShinyModules::launchDarwinDashboardApp(appStructure, title = "PregnancyIdentifier")
