# Test comparePregnancyIdentifierWithPET using mockPregnancyCdm and the
# pregnancy_extension table (PET) that exists in that database.
# mockPregnancyCdm() ensures main.pregnancy_extension has the required columns
# (person_id, pregnancy_start_date, pregnancy_end_date, pregnancy_outcome).

test_that("comparePregnancyIdentifierWithPET runs with pregnancy_extension (PET) from mockPregnancyCdm", {
  cdm <- mockPregnancyCdm()
  outputDir <- file.path(tempdir(), "test_compareWithPET_extension")
  outputFolder <- file.path(tempdir(), "test_compareWithPET_extension_out")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(outputDir, recursive = TRUE)
    unlink(outputFolder, recursive = TRUE)
    cleanupCdmDb(cdm)
  }, add = TRUE)

  # Run pipeline to get final_pregnancy_episodes.rds
  runPregnancyIdentifier(
    cdm = cdm,
    outputDir = outputDir,
    outputLogToConsole = FALSE
  )
  expect_true(file.exists(file.path(outputDir, "final_pregnancy_episodes.rds")))

  # Run comparison using PET table from mock (main.pregnancy_extension)
  comparePregnancyIdentifierWithPET(
    cdm = cdm,
    outputDir = outputDir,
    outputFolder = outputFolder,
    petSchema = "main",
    petTable = "pregnancy_extension",
    minOverlapDays = 1L,
    outputLogToConsole = FALSE
  )

  csv_path <- file.path(outputFolder, "pet_comparison_summarised_result.csv")
  expect_true(file.exists(csv_path))
  expect_true(file.exists(file.path(outputFolder, "log.txt")))

  res <- omopgenerics::importSummarisedResult(csv_path)
  expect_s3_class(res, "summarised_result")

  vars <- unique(res$variable_name)
  expect_true("episode_counts" %in% vars)
  expect_true("person_overlap" %in% vars)
  expect_true("time_overlap_summary" %in% vars)
})
