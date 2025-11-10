test_that("runHipps", {
  testPath <- file.path(getwd(), "runHipps")
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

  PregnancyIdentifier::runHipps(
    cdm = cdm,
    outputDir = outputDir,
    fileName = "result",
    continue = TRUE
  )

  res <- readRDS(file.path(outputDir, "result.rds"))

  expect_true(dir.exists(outputDir))
  result <- list.files(outputDir)
  expect_length(result, 4)

  expect_equal(sort(result), sort(c(
    "result.rds",
    "ESD.rds",
    "HIP_episodes.rds",
    "PPS_gest_timing_episodes.rds",
    "PPS_min_max_episodes.rds"
  )))

  unlink(outputDir, recursive = TRUE)
})
