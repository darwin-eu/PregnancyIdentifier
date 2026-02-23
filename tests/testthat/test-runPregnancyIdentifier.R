test_that("runPregnancyIdentifier runs without error", {
  cdm <- mockPregnancyCdm()

  outputFolder <- file.path(tempdir(), "test_runPregnancyIdentifier")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(
    cdm = cdm,
    outputFolder = outputFolder,
    outputLogToConsole = FALSE
  )

  expect_true(file.exists(file.path(outputFolder, "hip_episodes.rds")))
  expect_true(file.exists(file.path(outputFolder, "pps_episodes.rds")))
  expect_true(file.exists(file.path(outputFolder, "hipps_episodes.rds")))
  expect_true(file.exists(file.path(outputFolder, "final_pregnancy_episodes.rds")))

  unlink(outputFolder, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("runPregnancyIdentifier runs with custom parameters", {
  cdm <- mockPregnancyCdm()

  outputFolder <- file.path(tempdir(), "test_runPregnancyIdentifier_custom")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(
    cdm = cdm,
    outputFolder = outputFolder,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2020-12-31"),
    justGestation = FALSE,
    minCellCount = 0L,
    outputLogToConsole = FALSE
  )

  expect_true(file.exists(file.path(outputFolder, "hip_episodes.rds")))

  unlink(outputFolder, recursive = TRUE)
  cleanupCdmDb(cdm)
})

