# Test comparePregnancyIdentifierWithPET using mockPregnancyCdm and the
# pregnancy_extension table (PET) that exists in that database.
# The mock PET has pregnancy_outcome_id (string); we build a table with
# pregnancy_outcome (concept_id) for the comparison function.

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

  # Build PET-format table from pregnancy_extension: comparison expects
  # person_id, pregnancy_start_date, pregnancy_end_date, pregnancy_outcome (concept_id).
  # Mock has pregnancy_outcome_id (e.g. "livebirth") -> map to concept_id.
  pet_from_extension <- cdm$pregnancy_extension %>%
    dplyr::collect() %>%
    dplyr::mutate(
      pregnancy_start_date = as.Date(.data$pregnancy_start_date),
      pregnancy_end_date = as.Date(.data$pregnancy_end_date),
      pregnancy_outcome = dplyr::case_when(
        tolower(.data$pregnancy_outcome_id) == "livebirth" ~ 4092289L,
        tolower(.data$pregnancy_outcome_id) == "miscarriage" ~ 4067106L,
        tolower(.data$pregnancy_outcome_id) == "elective termination" ~ 4081422L,
        tolower(.data$pregnancy_outcome_id) == "stillbirth" ~ 443213L,
        TRUE ~ 4092289L
      )
    ) %>%
    dplyr::select(
      "person_id",
      "pregnancy_start_date",
      "pregnancy_end_date",
      "pregnancy_outcome",
      dplyr::any_of("gestational_length_in_day")
    )

  # Write to same database so comparePregnancyIdentifierWithPET can read it
  con <- CDMConnector::cdmCon(cdm)
  pet_schema <- "main"
  DBI::dbWriteTable(
    con,
    DBI::Id(schema = pet_schema, table = "pet_from_pregnancy_extension"),
    as.data.frame(pet_from_extension),
    overwrite = TRUE
  )

  # Run comparison using PET table built from pregnancy_extension
  res <- comparePregnancyIdentifierWithPET(
    cdm = cdm,
    outputDir = outputDir,
    outputFolder = outputFolder,
    petSchema = pet_schema,
    petTable = "pet_from_pregnancy_extension",
    minOverlapDays = 1L,
    outputLogToConsole = FALSE
  )

  expect_type(res, "list")
  expect_named(res, c(
    "episode_counts", "protocol_summary", "person_overlap", "venn_counts", "time_overlap_summary",
    "confusion_2x2", "ppv_sensitivity", "duration_summary", "duration_matched_summary",
    "date_differences", "outcome_confusion", "outcome_accuracy", "outcome_by_year", "duration_distribution"
  ))

  expect_s3_class(res$episode_counts, "data.frame")
  expect_equal(nrow(res$episode_counts), 2L)
  expect_true("algorithm" %in% res$episode_counts$source)
  expect_true("pet" %in% res$episode_counts$source)

  expect_s3_class(res$person_overlap, "data.frame")
  expect_true("raw_person_overlap" %in% res$person_overlap$metric)

  expect_s3_class(res$time_overlap_summary, "data.frame")
  expect_equal(nrow(res$time_overlap_summary), 4L)

  expect_true(file.exists(file.path(outputFolder, "pet_comparison_episode_counts.csv")))
  expect_true(file.exists(file.path(outputFolder, "pet_comparison_person_overlap.csv")))
  expect_true(file.exists(file.path(outputFolder, "pet_comparison_time_overlap_summary.csv")))
  expect_true(file.exists(file.path(outputFolder, "pet_comparison_outcome_by_year.csv")))
  expect_true(file.exists(file.path(outputFolder, "log.txt")))
})
