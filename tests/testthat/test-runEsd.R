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

test_that("runEsd default output includes quality flag columns", {
  cdm <- mockPregnancyCdm()
  outputFolder <- file.path(tempdir(), "test_runEsd_flags")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)
  cdm <- runHip(cdm = cdm, outputFolder = outputFolder, logger = logger)
  cdm <- runPps(cdm = cdm, outputFolder = outputFolder, logger = logger)
  mergeHipps(outputFolder = outputFolder, logger = logger)

  # Default: conformToValidation = FALSE — no records removed, but flags added
  runEsd(cdm = cdm, outputFolder = outputFolder, logger = logger, conformToValidation = FALSE)

  final <- readRDS(file.path(outputFolder, "final_pregnancy_episodes.rds"))
  expect_true(is.data.frame(final))

  # All 6 quality flag columns must be present
  expected_flags <- c(
    "is_start_outside_observation_period",
    "is_end_outside_observation_period",
    "is_start_gte_end",
    "is_zero_length",
    "is_too_long",
    "is_overlapping"
  )
  expect_true(
    all(expected_flags %in% names(final)),
    info = paste("Missing flag columns:", paste(setdiff(expected_flags, names(final)), collapse = ", "))
  )

  # Flag columns should be logical
  for (flag in expected_flags) {
    expect_true(
      is.logical(final[[flag]]) || all(is.na(final[[flag]])),
      info = paste(flag, "should be logical")
    )
  }

  unlink(outputFolder, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("runEsd conformToValidation=TRUE removes flagged episodes", {
  cdm <- mockPregnancyCdm()
  outputFolder_default <- file.path(tempdir(), "test_runEsd_noconform")
  outputFolder_conform <- file.path(tempdir(), "test_runEsd_conform")
  dir.create(outputFolder_default, recursive = TRUE, showWarnings = FALSE)
  dir.create(outputFolder_conform, recursive = TRUE, showWarnings = FALSE)

  # Run once without conformToValidation
  logger1 <- PregnancyIdentifier:::makeLogger(outputFolder_default, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger1)
  cdm <- runHip(cdm = cdm, outputFolder = outputFolder_default, logger = logger1)
  cdm <- runPps(cdm = cdm, outputFolder = outputFolder_default, logger = logger1)
  mergeHipps(outputFolder = outputFolder_default, logger = logger1)
  runEsd(cdm = cdm, outputFolder = outputFolder_default, logger = logger1, conformToValidation = FALSE)
  final_default <- readRDS(file.path(outputFolder_default, "final_pregnancy_episodes.rds"))

  # Copy pipeline artifacts to the conform folder and run ESD with conformToValidation = TRUE
  for (f in list.files(outputFolder_default, full.names = TRUE)) {
    file.copy(f, outputFolder_conform, overwrite = TRUE)
  }
  logger2 <- PregnancyIdentifier:::makeLogger(outputFolder_conform, outputLogToConsole = FALSE)
  runEsd(cdm = cdm, outputFolder = outputFolder_conform, logger = logger2, conformToValidation = TRUE)
  final_conform <- readRDS(file.path(outputFolder_conform, "final_pregnancy_episodes.rds"))

  # Conformed output should have <= records than default (may be equal if no bad records)
  expect_true(nrow(final_conform) <= nrow(final_default))

  # Both should have quality flag columns
  expect_true("is_start_outside_observation_period" %in% names(final_default))
  expect_true("is_start_outside_observation_period" %in% names(final_conform))

  # In conformed output, flagged episodes should be removed (flags all FALSE or NA)
  if (nrow(final_conform) > 0) {
    # After cleanup: no start_gte_end, no zero_length, no too_long
    expect_equal(sum(final_conform$is_start_gte_end, na.rm = TRUE), 0L)
    expect_equal(sum(final_conform$is_zero_length, na.rm = TRUE), 0L)
    expect_equal(sum(final_conform$is_too_long, na.rm = TRUE), 0L)
  }

  unlink(outputFolder_default, recursive = TRUE)
  unlink(outputFolder_conform, recursive = TRUE)
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

