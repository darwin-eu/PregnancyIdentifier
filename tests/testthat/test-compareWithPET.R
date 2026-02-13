# Test comparePregnancyIdentifierWithPET using mockPregnancyCdm, pipeline output,
# and a simulated PET table written to the same database.

test_that("comparePregnancyIdentifierWithPET runs and writes output", {
  cdm <- mockPregnancyCdm()
  outputDir <- file.path(tempdir(), "test_compareWithPET")
  outputFolder <- file.path(tempdir(), "test_compareWithPET_out")
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

  # Load algorithm output and build a simulated PET table (subset with same structure)
  alg <- readRDS(file.path(outputDir, "final_pregnancy_episodes.rds"))
  names(alg) <- tolower(names(alg))
  # Map algorithm outcome to PET outcome concept_id (LB=4092289, SB=443213, AB=4081422, SA=4067106)
  outcome_to_concept <- function(x) {
    dplyr::case_when(
      x == "LB" ~ 4092289L,
      x == "SB" ~ 443213L,
      x == "AB" ~ 4081422L,
      x == "SA" ~ 4067106L,
      x == "DELIV" ~ 4092289L,
      TRUE ~ 4092289L
    )
  }
  pet_df <- alg %>%
    dplyr::select(
      "person_id",
      final_episode_start_date = "final_episode_start_date",
      final_episode_end_date = "final_episode_end_date",
      final_outcome_category = "final_outcome_category"
    ) %>%
    dplyr::mutate(
      pregnancy_start_date = as.Date(.data$final_episode_start_date),
      pregnancy_end_date = as.Date(.data$final_episode_end_date),
      pregnancy_outcome = outcome_to_concept(.data$final_outcome_category)
    ) %>%
    dplyr::select("person_id", "pregnancy_start_date", "pregnancy_end_date", "pregnancy_outcome")

  # Write PET table to the same database (DuckDB default schema is "main")
  con <- CDMConnector::cdmCon(cdm)
  pet_schema <- "main"
  DBI::dbWriteTable(
    con,
    DBI::Id(schema = pet_schema, table = "pregnancy_episode"),
    as.data.frame(pet_df),
    overwrite = TRUE
  )

  # Run comparison (use same outputFolder for outputs)
  res <- comparePregnancyIdentifierWithPET(
    cdm = cdm,
    outputDir = outputDir,
    outputFolder = outputFolder,
    petSchema = pet_schema,
    petTable = "pregnancy_episode",
    minOverlapDays = 1L,
    outputLogToConsole = FALSE
  )

  # Check return structure
  expect_type(res, "list")
  expect_named(res, c(
    "episode_counts", "venn_counts", "ppv_sensitivity", "duration_summary",
    "date_differences", "outcome_confusion", "outcome_accuracy", "duration_distribution"
  ))
  expect_s3_class(res$episode_counts, "data.frame")
  expect_s3_class(res$venn_counts, "data.frame")
  expect_s3_class(res$ppv_sensitivity, "data.frame")
  expect_s3_class(res$duration_summary, "data.frame")
  expect_s3_class(res$duration_distribution, "data.frame")
  expect_s3_class(res$outcome_accuracy, "data.frame")

  # Episode counts: algorithm and PET
  expect_equal(nrow(res$episode_counts), 2L)
  expect_true("source" %in% names(res$episode_counts))
  expect_true("n_episodes" %in% names(res$episode_counts))
  expect_true("n_persons" %in% names(res$episode_counts))

  # Venn counts
  expect_true(nrow(res$venn_counts) >= 1L)
  expect_true("category" %in% names(res$venn_counts))

  # PPV/sensitivity
  expect_true("metric" %in% names(res$ppv_sensitivity))
  expect_true("value" %in% names(res$ppv_sensitivity))

  # Duration summary: algorithm and pet
  expect_equal(nrow(res$duration_summary), 2L)
  expect_true("source" %in% names(res$duration_summary))

  # Output files exist
  expect_true(file.exists(file.path(outputFolder, "pet_comparison_episode_counts.csv")))
  expect_true(file.exists(file.path(outputFolder, "pet_comparison_venn_counts.csv")))
  expect_true(file.exists(file.path(outputFolder, "pet_comparison_ppv_sensitivity.csv")))
  expect_true(file.exists(file.path(outputFolder, "pet_comparison_duration_summary.csv")))
  expect_true(file.exists(file.path(outputFolder, "pet_comparison_duration_distribution.csv")))
  expect_true(file.exists(file.path(outputFolder, "pet_comparison_outcome_accuracy.csv")))
  expect_true(file.exists(file.path(outputFolder, "log.txt")))

  # When we built PET from algorithm, there should be matches
  expect_true(res$episode_counts$n_episodes[res$episode_counts$source == "algorithm"] >= 1)
  expect_true(res$episode_counts$n_episodes[res$episode_counts$source == "pet"] >= 1)
  expect_true(sum(res$venn_counts$n_episodes) >= 1)
})

test_that("comparePregnancyIdentifierWithPET errors when final_pregnancy_episodes.rds is missing", {
  cdm <- mockPregnancyCdm()
  outputDir <- file.path(tempdir(), "test_compareWithPET_nofile")
  outputFolder <- file.path(tempdir(), "test_compareWithPET_nofile_out")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(outputDir, recursive = TRUE)
    unlink(outputFolder, recursive = TRUE)
    cleanupCdmDb(cdm)
  }, add = TRUE)

  # No pipeline run -> no final_pregnancy_episodes.rds
  expect_error(
    comparePregnancyIdentifierWithPET(
      cdm = cdm,
      outputDir = outputDir,
      outputFolder = outputFolder,
      petSchema = "main",
      petTable = "pregnancy_episode",
      outputLogToConsole = FALSE
    ),
    "Algorithm output not found"
  )
})
