library(dplyr)
library(PregnancyIdentifier)
library(TestGenerator)
library(CDMConnector)

TestGenerator::readPatients(
  filePath = file.path(getwd(), "extras/TestData_P4_C5_002.1.xlsx"),
  testName = "test",
  outputPath = "inst/testCases",
  extraTable = TRUE
)

cdm <- TestGenerator::patientsCDM(
  pathJson = "inst/testCases",
  testName = "test")


cdm$condition_occurrence %>%
  # filter(person_id == 32L)
  filter(person_id == 1L)

cdm <- cdmSubset(cdm, personId = 32L)

# cdmDisconnect(cdm)

cdm$condition_occurrence %>%
  mutate_all(as.character) %>%
  collect() %>%
  tidyr::gather() %>%
  print(n=100)

# start run analysis
outputDir <- outputDir <- "./dev/output/"
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = T)
}

debugonce(runPps)
debugonce(runHip)
runPregnancyIdentifier(cdm, outputDir, minCellCount = 0L)

hipEpisodes <- readRDS(file.path(outputDir, "hip_episodes.rds"))
ppsMinMax <- readRDS(file.path(outputDir, "pps_min_max_episodes.rds"))
ppsEpisode <- readRDS(file.path(outputDir, "pps_gest_timing_episodes.rds"))
pps <- readRDS(file.path(outputDir, "pps_episodes.rds"))
hipps <- readRDS(file.path(outputDir, "hipps_episodes.rds"))
esd <- readRDS(file.path(outputDir, "esd.rds"))
final <- readRDS(file.path(outputDir, "final_pregnancy_episodes.rds"))


exportPregnancies(cdm, outputDir, exportDir = here::here("test_export"), minCellCount = 0L)


