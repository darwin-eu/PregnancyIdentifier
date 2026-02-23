test_that("runPps runs without error", {
  cdm <- mockPregnancyCdm()
  outputFolder <- file.path(tempdir(), "test_runPps")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)

  cdm <- runPps(
    cdm = cdm,
    outputFolder = outputFolder,
    logger = logger
  )

  expect_s3_class(cdm, "cdm_reference")
  expect_true(file.exists(file.path(outputFolder, "pps_episodes.rds")))
  
  unlink(outputFolder, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("runPps runs with custom parameters", {
  cdm <- mockPregnancyCdm()
  outputFolder <- file.path(tempdir(), "test_runPps_custom")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)

  cdm <- runPps(
    cdm = cdm,
    outputFolder = outputFolder,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2020-12-31"),
    logger = logger,
    debugMode = TRUE
  )

  expect_s3_class(cdm, "cdm_reference")
  expect_true(file.exists(file.path(outputFolder, "pps_gest_timing_episodes.rds")))
  expect_true(file.exists(file.path(outputFolder, "pps_min_max_episodes.rds")))
  expect_true(file.exists(file.path(outputFolder, "pps_episodes.rds")))

  unlink(outputFolder, recursive = TRUE)
  cleanupCdmDb(cdm)
})

