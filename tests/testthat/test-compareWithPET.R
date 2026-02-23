# Test comparePregnancyIdentifierWithPET using mockPregnancyCdm, pipeline output,
# and a simulated PET table written to the same database.

test_that("comparePregnancyIdentifierWithPET runs and writes output", {
  cdm <- mockPregnancyCdm()
  outputFolder <- file.path(tempdir(), "test_compareWithPET")
  outputFolder <- file.path(tempdir(), "test_compareWithPET_out")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(outputFolder, recursive = TRUE)
    unlink(outputFolder, recursive = TRUE)
    cleanupCdmDb(cdm)
  }, add = TRUE)

  # Run pipeline to get final_pregnancy_episodes.rds
  runPregnancyIdentifier(
    cdm = cdm,
    outputFolder = outputFolder,
    outputLogToConsole = FALSE
  )
  expect_true(file.exists(file.path(outputFolder, "final_pregnancy_episodes.rds")))

  # Load algorithm output and build a simulated PET table (subset with same structure)
  alg <- readRDS(file.path(outputFolder, "final_pregnancy_episodes.rds"))
  names(alg) <- tolower(names(alg))
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)
  PregnancyIdentifier:::validateEpisodePeriods(
    alg, "person_id", "final_episode_start_date", "final_episode_end_date", logger
  )
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

  # Run comparison (use same outputFolder for outputs); function writes summarised result only, returns nothing
  comparePregnancyIdentifierWithPET(
    cdm = cdm,
    outputFolder = outputFolder,
    outputFolder = outputFolder,
    petSchema = pet_schema,
    petTable = "pregnancy_episode",
    minOverlapDays = 1L,
    outputLogToConsole = FALSE
  )

  # SummarisedResult is written to export
  csv_path <- file.path(outputFolder, "pet_comparison_summarised_result.csv")
  expect_true(file.exists(csv_path))
  expect_true(file.exists(file.path(outputFolder, "log.txt")))

  res <- omopgenerics::importSummarisedResult(csv_path)
  expect_s3_class(res, "summarised_result")

  # Check that key variables are present in the summarised result
  vars <- unique(res$variable_name)
  expect_true("episode_counts" %in% vars)
  expect_true("venn_counts" %in% vars)
  expect_true("ppv_sensitivity" %in% vars)
  expect_true("person_overlap" %in% vars)
  expect_true("time_overlap_summary" %in% vars)
  expect_true("duration_summary" %in% vars)
  expect_true("protocol_summary" %in% vars)

  # When we built PET from algorithm, there should be episode counts for both sources
  ep <- res |> dplyr::filter(.data$variable_name == "episode_counts")
  expect_true(nrow(ep) >= 1)
})

test_that("comparePregnancyIdentifierWithPET errors when final_pregnancy_episodes.rds is missing", {
  cdm <- mockPregnancyCdm()
  outputFolder <- file.path(tempdir(), "test_compareWithPET_nofile")
  outputFolder <- file.path(tempdir(), "test_compareWithPET_nofile_out")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(outputFolder, recursive = TRUE)
    unlink(outputFolder, recursive = TRUE)
    cleanupCdmDb(cdm)
  }, add = TRUE)

  # No pipeline run -> no final_pregnancy_episodes.rds
  expect_error(
    comparePregnancyIdentifierWithPET(
      cdm = cdm,
      outputFolder = outputFolder,
      outputFolder = outputFolder,
      petSchema = "main",
      petTable = "pregnancy_episode",
      outputLogToConsole = FALSE
    ),
    "Algorithm output not found"
  )
})
