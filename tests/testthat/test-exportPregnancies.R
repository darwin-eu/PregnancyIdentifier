test_that("exportPregnancies runs without error", {
  cdm <- mockPregnancyCdm()

  outputDir <- file.path(tempdir(), "test_exportPregnancies")
  exportDir <- file.path(tempdir(), "test_exportPregnancies_export")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  dir.create(exportDir, recursive = TRUE, showWarnings = FALSE)

  # Run full pipeline first to create required files
  runPregnancyIdentifier(
    cdm = cdm,
    outputDir = outputDir
  )

  # Now test exportPregnancies
  exportPregnancies(
    cdm = cdm,
    outputDir = outputDir,
    exportDir = exportDir
  )

  expect_true(dir.exists(exportDir))
  expect_true(file.exists(file.path(exportDir, "cdm_source.csv")))

  unlink(outputDir, recursive = TRUE)
  unlink(exportDir, recursive = TRUE)
})

test_that("exportPregnancies runs with custom minCellCount", {
  cdm <- mockPregnancyCdm()

  outputDir <- file.path(tempdir(), "test_exportPregnancies_custom")
  exportDir <- file.path(tempdir(), "test_exportPregnancies_export_custom")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  dir.create(exportDir, recursive = TRUE, showWarnings = FALSE)

  # Run full pipeline first
  runPregnancyIdentifier(
    cdm = cdm,
    outputDir = outputDir
  )

  # Test with custom minCellCount
  exportPregnancies(
    cdm = cdm,
    outputDir = outputDir,
    exportDir = exportDir,
    minCellCount = 0
  )

  expect_true(dir.exists(exportDir))

  expect_equal(list.files(exportDir, ".csv"), sort(c(
    # "pps_concept_counts.csv",
    "age_summary.csv",
    "age_summary_groups.csv",
    "cdm_source.csv",
    "date_consistency.csv",
    "episode_frequency.csv",
    "episode_frequency_summary.csv",
    "gestational_age_days_counts.csv",
    "gestational_age_days_per_category_summary.csv",
    "gestational_age_days_summary.csv",
    "gestational_weeks.csv",
    "monthly_trend_missing.csv",
    "monthly_trends.csv",
    "observation_period_range.csv",
    "outcome_categories_count.csv",
    "precision_days.csv",
    "pregnancy_frequency.csv",
    "pregnancy_overlap_counts.csv",
    "swapped_dates.csv",
    "yearly_trend.csv",
    "yearly_trend_missing.csv"
  )))

  unlink(outputDir, recursive = TRUE)
  unlink(exportDir, recursive = TRUE)
})

