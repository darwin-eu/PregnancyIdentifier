test_that("runEsd runs without error", {
  cdm <- mockPregnancyCdm()
  outputFolder <- file.path(tempdir(), "test_runEsd")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)

  # Run HIP, PPS, and merge first to create required files
  cdm <- runHip(cdm = cdm, outputFolder = outputFolder, logger = logger)
  cdm <- runPps(cdm = cdm, outputFolder = outputFolder, logger = logger)
  mergeHipps(outputFolder = outputFolder, logger = logger)

  # Now test runEsd
  runEsd(cdm = cdm, outputFolder = outputFolder, logger = logger)

  # expect_true(file.exists(file.path(outputFolder, "esd.rds")))
  expect_true(file.exists(file.path(outputFolder, "final_pregnancy_episodes.rds")))
  final <- readr::read_rds(file.path(outputFolder, "final_pregnancy_episodes.rds"))
  expect_true(is.data.frame(final))
  unlink(outputFolder, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("runEsd runs with custom parameters", {
  cdm <- mockPregnancyCdm()
  outputFolder <- file.path(tempdir(), "test_runEsd_custom")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)

  # Run HIP, PPS, and merge first
  cdm <- runHip(cdm = cdm, outputFolder = outputFolder, logger = logger)
  cdm <- runPps(cdm = cdm, outputFolder = outputFolder, logger = logger)
  mergeHipps(outputFolder = outputFolder, logger = logger)

  # Test with custom dates
  runEsd(
    cdm = cdm,
    outputFolder = outputFolder,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2020-12-31"),
    logger = logger,
    debugMode = TRUE
  )

  expect_true(file.exists(file.path(outputFolder, "esd.rds")))
  esd <- readr::read_rds(file.path(outputFolder, "esd.rds"))
  expect_true(is.data.frame(esd))

  unlink(outputFolder, recursive = TRUE)
  cleanupCdmDb(cdm)
})

