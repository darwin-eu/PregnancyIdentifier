test_that("runPps runs without error", {
  cdm <- mockPregnancyCdm()
  outputDir <- file.path(tempdir(), "test_runPps")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputDir, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)

  cdm <- runPps(
    cdm = cdm,
    outputDir = outputDir,
    logger = logger
  )

  expect_s3_class(cdm, "cdm_reference")
  expect_true(file.exists(file.path(outputDir, "pps_episodes.rds")))
  
  unlink(outputDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("runPps runs with custom parameters", {
  cdm <- mockPregnancyCdm()
  outputDir <- file.path(tempdir(), "test_runPps_custom")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputDir, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)

  cdm <- runPps(
    cdm = cdm,
    outputDir = outputDir,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2020-12-31"),
    logger = logger,
    debugMode = TRUE
  )

  expect_s3_class(cdm, "cdm_reference")
  expect_true(file.exists(file.path(outputDir, "pps_gest_timing_episodes.rds")))
  expect_true(file.exists(file.path(outputDir, "pps_min_max_episodes.rds")))
  expect_true(file.exists(file.path(outputDir, "pps_episodes.rds")))

  unlink(outputDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

