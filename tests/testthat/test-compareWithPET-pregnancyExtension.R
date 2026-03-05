# Test comparePregnancyIdentifierWithPET using mockPregnancyCdm and the
# pregnancy_extension table (PET) that exists in that database.
# mockPregnancyCdm() ensures main.pregnancy_extension has the required columns
# (person_id, pregnancy_start_date, pregnancy_end_date, pregnancy_outcome).

test_that("comparePregnancyIdentifierWithPET runs with pregnancy_extension (PET) from mockPregnancyCdm", {
  cdm <- mockPregnancyCdm()
  outputFolder <- file.path(tempdir(), "test_compareWithPET_extension_out")
  exportFolder <- file.path(tempdir(), "test_compareWithPET_extension_export")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  dir.create(exportFolder, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(outputFolder, recursive = TRUE)
    unlink(exportFolder, recursive = TRUE)
    cleanupCdmDb(cdm)
  }, add = TRUE)

  # Run pipeline to get final_pregnancy_episodes.rds
  runPregnancyIdentifier(
    cdm = cdm,
    outputFolder = outputFolder,
    outputLogToConsole = FALSE
  )
  expect_true(file.exists(file.path(outputFolder, "final_pregnancy_episodes.rds")))

  # Run comparison using PET table from mock (main.pregnancy_extension)
  comparePregnancyIdentifierWithPET(
    cdm = cdm,
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    petSchema = "main",
    petTable = "pregnancy_extension",
    minOverlapDays = 1L,
    outputLogToConsole = FALSE
  )

  csv_path <- file.path(exportFolder, "pet_comparison_summarised_result.csv")
  expect_true(file.exists(csv_path))
  expect_true(file.exists(file.path(exportFolder, "log.txt")))

  res <- omopgenerics::importSummarisedResult(csv_path)
  expect_s3_class(res, "summarised_result")

  vars <- unique(res$variable_name)
  expect_true("episode_counts" %in% vars)
  expect_true("person_overlap" %in% vars)
  expect_true("date_difference_summary" %in% vars)
})
