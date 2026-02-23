library(dplyr)
library(PregnancyIdentifier)
library(TestGenerator)
library(CDMConnector)

# TestGenerator::readPatients(
#   filePath = file.path(getwd(), "extras/TestData_P4_C5_002.1.xlsx"),
#   testName = "test",
#   outputPath = "inst/testCases",
#   extraTable = TRUE
# )

cdm <- TestGenerator::patientsCDM(
  pathJson = "inst/testCases",
  testName = "test",
  cdmVersion = "5.4")

# start run analysis
outputFolder <- outputFolder <- "./dev/output/"
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = T)
}

# debugonce(runPps)
# debugonce(runHip)

runPregnancyIdentifier(cdm, outputFolder, minCellCount = 0L)

# hipEpisodes <- readRDS(file.path(outputFolder, "hip_episodes.rds"))
# ppsMinMax <- readRDS(file.path(outputFolder, "pps_min_max_episodes.rds"))
# ppsEpisode <- readRDS(file.path(outputFolder, "pps_gest_timing_episodes.rds"))
# pps <- readRDS(file.path(outputFolder, "pps_episodes.rds"))
# hipps <- readRDS(file.path(outputFolder, "hipps_episodes.rds"))
# esd <- readRDS(file.path(outputFolder, "esd.rds"))
# final <- readRDS(file.path(outputFolder, "final_pregnancy_episodes.rds"))

exportPregnancies(cdm, outputFolder, exportDir = here::here("test_export"), minCellCount = 0L)


