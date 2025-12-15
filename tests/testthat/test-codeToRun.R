test_that("run analysis", {
  testPath <- getwd()
  # Read patients from JSON
  cdm <- TestGenerator::patientsCDM(
    pathJson = testPath,
    testName = "testData"
  )

  # start run analysis
  outputDir <- file.path(tempdir(), "Results")
  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = T)
  }

  logger <- PregnancyIdentifier:::makeLogger(outputDir)

  cdm <- PregnancyIdentifier:::uploadConceptSets(cdm, logger)
  cdm <- runHip(cdm = cdm, outputDir = outputDir, logger = logger, continue = TRUE)
  cdm <- runPps(cdm = cdm, outputDir = outputDir, logger = logger)

  ppsMinMax <- readRDS(file.path(outputDir, "PPS_min_max_episodes.rds"))
  ppsEpisode <- readRDS(file.path(outputDir, "PPS_gest_timing_episodes.rds"))
  hipRes <- readRDS(file.path(outputDir, "HIP_episodes.rds"))
  mergeHipPps(
    cdm = cdm,
    HIP = hipRes,
    PPSEpisode = ppsEpisode,
    PPSMinMax = ppsMinMax,
    outputDir = outputDir,
    fileName = "merge.csv",
    logger = logger
  )
  cdm <- CDMConnector::readSourceTable(cdm = cdm, name = "initial_pregnant_cohort_df")
  hippsRes <- readRDS(file.path(outputDir, "HIPPS_episodes.rds"))
  runEsd(HIPPS = hippsRes, cdm = cdm, outputDir = outputDir, logger = logger)
  # end analysis

  expect_true(dir.exists(outputDir))
  result <- list.files(outputDir)
  expect_length(result, 7)

  expect_equal(sort(result), sort(c(
    "PPS-concept_counts.csv",
    "ESD.rds",
    "HIPPS_episodes.rds",
    "HIP_episodes.rds",
    "PPS_gest_timing_episodes.rds",
    "PPS_min_max_episodes.rds",
    "log.txt"
  )))

  unlink(outputDir, recursive = TRUE)
})
