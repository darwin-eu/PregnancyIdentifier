library(dplyr)
library(PregnancyIdentifier)

cdmConnection <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "...",
  host = "...",
  user = "...",
  password = "..."
)

cdm <- CDMConnector::cdmFromCon(
  con = cdmConnection,
  cdmSchema = "...",
  writeSchema = "..."
)

# add concept tables: hip_concepts, pps_concepts, matcho_outcome_limits, matcho_term_durations
cdm <- PregnancyIdentifier:::uploadConceptSets(cdm)

outputDir <- "./dev/output/"

runHip(cdm = cdm, outputDir = outputDir, continue = TRUE)
runPps(cdm = cdm, outputDir = outputDir)

ppsMinMax <- readRDS(file.path(outputDir, "PPS_min_max_episodes.rds"))
ppsEpisode <- readRDS(file.path(outputDir, "PPS_gest_timing_episodes.rds"))
hipRes <- readRDS(file.path(outputDir, "hip_episodes.rds"))

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
