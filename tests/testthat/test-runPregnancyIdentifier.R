test_that("runPregnancyIdentifier runs without error", {
  cdm <- mockPregnancyCdm()

  outputDir <- file.path(tempdir(), "test_runPregnancyIdentifier")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(
    cdm = cdm,
    outputDir = outputDir
  )

  expect_true(file.exists(file.path(outputDir, "HIP_episodes.rds")))
  expect_true(file.exists(file.path(outputDir, "PPS_episodes.rds")))
  expect_true(file.exists(file.path(outputDir, "HIPPS_episodes.rds")))
  expect_true(file.exists(file.path(outputDir, "final_pregnancy_episodes.rds")))

  unlink(outputDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("runPregnancyIdentifier runs with custom parameters", {
  cdm <- mockPregnancyCdm()

  outputDir <- file.path(tempdir(), "test_runPregnancyIdentifier_custom")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(
    cdm = cdm,
    outputDir = outputDir,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2020-12-31"),
    justGestation = FALSE,
    minCellCount = 0L
  )

  expect_true(file.exists(file.path(outputDir, "HIP_episodes.rds")))

  unlink(outputDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

