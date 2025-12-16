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

# start run analysis
outputDir <- "./dev/output/"
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = T)
}
exportDir <- "./dev/export/"
if (!dir.exists(exportDir)) {
  dir.create(exportDir, recursive = T)
}

# Run the HIPPS algorithm.
PregnancyIdentifier::runHipps(
  cdm = cdm,
  outputDir = outputDir,
  continue = TRUE
)

PregnancyIdentifier::export(
  cdm = cdm,
  outputDir = outputDir,
  exportDir = exportDir
)

# logger <- PregnancyIdentifier:::makeLogger(outputDir)
# cdm <- PregnancyIdentifier:::uploadConceptSets(cdm, logger)
#
# cdm <- runHip(cdm = cdm, outputDir = outputDir, logger = logger, continue = TRUE)
# cdm <- runPps(cdm = cdm, outputDir = outputDir, logger = logger)
#
# ppsMinMax <- readRDS(file.path(outputDir, "PPS_min_max_episodes.rds"))
# ppsEpisode <- readRDS(file.path(outputDir, "PPS_gest_timing_episodes.rds"))
# hipRes <- readRDS(file.path(outputDir, "HIP_episodes.rds"))
#
# mergeHipPps(
#   cdm = cdm,
#   HIP = hipRes,
#   PPSEpisode = ppsEpisode,
#   PPSMinMax = ppsMinMax,
#   outputDir = outputDir,
#   fileName = "merge.csv",
#   logger = logger
# )
#
# cdm <- CDMConnector::readSourceTable(cdm = cdm, name = "initial_pregnant_cohort_df")
#
# hippsRes <- readRDS(file.path(outputDir, "HIPPS_episodes.rds"))
#
# runEsd(HIPPS = hippsRes, cdm = cdm, outputDir = outputDir, logger = logger)
