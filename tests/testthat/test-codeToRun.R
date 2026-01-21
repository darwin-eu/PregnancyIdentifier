test_that("run mergeHipPps", {
  testPath <- system.file("testCases", mustWork = TRUE, package = "PregnancyIdentifier")
  # Read patients from JSON
  suppressMessages({
    cdm <- TestGenerator::patientsCDM(
      pathJson = testPath,
      testName = "testData"
    )
  })

  # setup
  outputDir <- file.path(tempdir(), "output")
  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = T)
  }
  logger <- PregnancyIdentifier:::makeLogger(outputDir)

  # start analysis
  cdm <- initPregnancies(cdm)

  cdm <- runHip(cdm = cdm, outputDir = outputDir, logger = logger)
  hipRes <- readRDS(file.path(outputDir, "HIP_episodes.rds"))

  cdm <- runPps(cdm = cdm, outputDir = outputDir, logger = logger)
  ppsMinMax <- readRDS(file.path(outputDir, "pps_min_max_episodes.rds"))
  ppsEpisode <- readRDS(file.path(outputDir, "pps_gest_timing_episodes.rds"))

  mergeHips(cdm = cdm, outputDir = outputDir, logger = logger)
  hippsRes <- readRDS(file.path(outputDir, "HIPPS_episodes.rds"))

  runEsd(cdm = cdm, outputDir = outputDir, logger = logger)
  identified_pregnancy_episodes <- readRDS(file.path(outputDir, "identified_pregnancy_episodes.rds"))

  expect_true(dir.exists(outputDir))
  result <- list.files(outputDir)
  expect_length(result, 8)

  expect_equal(sort(result), sort(c(
    "pps_concept_counts.csv",
    "ESD.rds",
    "HIPPS_episodes.rds",
    "HIP_episodes.rds",
    "pps_gest_timing_episodes.rds",
    "pps_min_max_episodes.rds",
    "log.txt",
    "identified_pregnancy_episodes.rds"
  )))

  unlink(outputDir, recursive = TRUE)
})

test_that("run runHipps", {
  testPath <- system.file("testCases", mustWork = TRUE, package = "PregnancyIdentifier")
  # Read patients from JSON
  cdm <- TestGenerator::patientsCDM(
    pathJson = testPath,
    testName = "testData"
  )

  # setup
  outputDir <- file.path(tempdir(), "output")
  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = T)
  }
  exportDir <- file.path(tempdir(), "export")
  if (!dir.exists(exportDir)) {
    dir.create(exportDir, recursive = T)
  }

  # start analysis
  PregnancyIdentifier::runPregnancyIdentifier(
    cdm = cdm,
    outputDir = outputDir
  )

  PregnancyIdentifier::exportPregnancies(
    cdm = cdm,
    outputDir = outputDir,
    exportDir = exportDir
  )
  # end analysis

  expect_true(dir.exists(outputDir))
  result <- list.files(outputDir)
  expect_length(result, 9)

  expect_equal(sort(result), sort(c(
    "HIP_episodes.rds",
    "ESD.rds",
    "HIPPS_episodes.rds",
    "pps_concept_counts.csv",
    "pps_gest_timing_episodes.rds",
    "pps_min_max_episodes.rds",
    "identified_pregnancy_episodes.rds",
    "log.txt",
    "runStart.csv"
  )))

  expect_true(dir.exists(exportDir))
  csvResult <- list.files(exportDir, pattern = "*.csv")
  expect_length(csvResult, 20)
  zipResult <- list.files(exportDir, pattern = "*.zip")
  expect_length(zipResult, 1)

  expect_equal(sort(csvResult), sort(c(
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

  unlink(exportDir, recursive = TRUE)
  unlink(outputDir, recursive = TRUE)
})
