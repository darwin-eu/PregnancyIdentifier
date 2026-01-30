test_that("runEsd runs without error", {
  cdm <- mockPregnancyCdm()
  cdm <- initPregnancies(cdm)

  outputDir <- file.path(tempdir(), "test_runEsd")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputDir)

  # Run HIP, PPS, and merge first to create required files
  cdm <- runHip(cdm = cdm, outputDir = outputDir, logger = logger)
  cdm <- runPps(cdm = cdm, outputDir = outputDir, logger = logger)
  mergeHipps(outputDir = outputDir, logger = logger)

  # Now test runEsd
  runEsd(cdm = cdm, outputDir = outputDir, logger = logger)

  # expect_true(file.exists(file.path(outputDir, "ESD.rds")))
  expect_true(file.exists(file.path(outputDir, "final_pregnancy_episodes.rds")))
  final <- readr::read_rds(file.path(outputDir, "final_pregnancy_episodes.rds"))
  expect_true(is.data.frame(final))
  unlink(outputDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("runEsd runs with custom parameters", {
  cdm <- mockPregnancyCdm()
  cdm <- initPregnancies(cdm)

  outputDir <- file.path(tempdir(), "test_runEsd_custom")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputDir)

  # Run HIP, PPS, and merge first
  cdm <- runHip(cdm = cdm, outputDir = outputDir, logger = logger)
  cdm <- runPps(cdm = cdm, outputDir = outputDir, logger = logger)
  mergeHipps(outputDir = outputDir, logger = logger)

  # Test with custom dates
  runEsd(
    cdm = cdm,
    outputDir = outputDir,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2020-12-31"),
    logger = logger,
    debugMode = TRUE
  )

  expect_true(file.exists(file.path(outputDir, "ESD.rds")))
  esd <- readr::read_rds(file.path(outputDir, "ESD.rds"))
  expect_true(is.data.frame(esd))

  unlink(outputDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

