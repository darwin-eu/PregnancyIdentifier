test_that("Delivery mode is correct", {

  suppressWarnings(library(dplyr, warn.conflicts = FALSE))

  suppressMessages({
    cdm <- TestGenerator::patientsCDM(
      pathJson = system.file("testCases", package = "PregnancyIdentifier"),
      testName = 'TestData_P4_C5_002_DM',
      cdmVersion = "5.4",
      cdmName = "TestData_P4_C5_002_DM"
    )
  })

  outputFolder <- file.path(tempdir(), "testHipps")
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  runPregnancyIdentifier(cdm, outputFolder, outputLogToConsole = FALSE)

  df <- readRDS(file.path(outputFolder, "final_pregnancy_episodes.rds")) %>%
    dplyr::select(c("person_id", "final_outcome_category", dplyr::starts_with("cesarean"), dplyr::starts_with("vaginal"))) %>%
    dplyr::arrange(person_id)

  # Check delivery mode output
  testthat::expect_equal(df$cesarean_m30_to_30, c(1, 1, 0, 0, 0))
  testthat::expect_equal(df$cesarean_m30_to_30_count, c(1, 1, 0, 0, 0))
  testthat::expect_equal(df$vaginal_m30_to_30, c(0, 0, 1, 1, 0))
  testthat::expect_equal(df$vaginal_m30_to_30_count, c(0, 0, 1, 2, 0))

  cleanupCdmDb(cdm)
  unlink(outputFolder, recursive = TRUE)
})
