test_that("run analysis", {
  testPath <- getwd()
  # Read patients from JSON
  cdm <- TestGenerator::patientsCDM(
    pathJson = testPath,
    testName = "testData"
  )

  # start run analysis
  outputDir <- file.path(testPath, "Results")
  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = T)
  }
  cdm <- PregnancyIdentifier:::uploadConceptSets(cdm)
  cdm <- runHip(cdm = cdm, outputDir = outputDir, continue = TRUE)
  cdm <- runPps(cdm = cdm, outputDir = outputDir)

  ppsMinMax <- readRDS(file.path(outputDir, "PPS_min_max_episodes.rds"))
  ppsEpisode <- readRDS(file.path(outputDir, "PPS_gest_timing_episodes.rds"))
  hipRes <- readRDS(file.path(outputDir, "HIP_episodes.rds"))
  mergeHipPps(
    cdm = cdm,
    HIP = hipRes,
    PPSEpisode = ppsEpisode,
    PPSMinMax = ppsMinMax,
    outputDir = outputDir,
    fileName = "merge.csv"
  )
  cdm <- CDMConnector::readSourceTable(cdm = cdm, name = "initial_pregnant_cohort_df")
  hippsRes <- readRDS(file.path(outputDir, "HIPPS_episodes.rds"))
  runEsd(HIPPS = hippsRes, cdm = cdm, outputDir = outputDir)
  # end analysis

  expect_true(dir.exists(outputDir))
  result <- list.files(outputDir)
  expect_length(result, 5)

  expect_equal(sort(result), sort(c(
    "ESD.rds",
    "HIPPS_episodes.rds",
    "HIP_episodes.rds",
    "PPS_gest_timing_episodes.rds",
    "PPS_min_max_episodes.rds"
  )))

  unlink(outputDir, recursive = TRUE)
})
