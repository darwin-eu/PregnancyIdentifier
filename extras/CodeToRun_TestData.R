library(dplyr)
library(PregnancyIdentifier)
library(TestGenerator)

# TestGenerator::readPatients(
#   filePath = file.path(getwd(), "extras/TestData_P4_C1_001.xlsx"),
#   testName = "test",
#   outputPath = "inst/testCases"
# )
cdm <- TestGenerator::patientsCDM(
  pathJson = "inst/testCases",
  testName = "test")

cdm <- PregnancyIdentifier:::uploadConceptSets(cdm)

outputDir <- "./dev/output/"

runHip(cdm = cdm, outputDir = outputDir, continue = TRUE)
runPps(cdm = cdm, outputDir = outputDir)

ppsMinMax <- readRDS(file.path(outputDir, "PPS_min_max_episodes.rds"))
ppsEpisode <- readRDS(file.path(outputDir, "PPS_gest_timing_episodes.rds"))
hipRes <- read.csv(file.path(outputDir, "hip_episodes.csv"))

mergeHipPps(
  cdm = cdm,
  HIP = hipRes,
  PPSEpisode = ppsEpisode,
  PPSMinMax = ppsMinMax,
  outputDir = "./dev/output/",
  fileName = "merge.csv"
)

cdm <- CDMConnector::readSourceTable(cdm = cdm, name = "initial_pregnant_cohort_df")

hippsRes <- readRDS(file.path(outputDir, "HIPPS_episodes.rds"))

runEsd(HIPPS = hippsRes, cdm = cdm, outputDir = outputDir)
