# CSV contents (gestation_visits.csv): person_id;concept_id;visit_date;value_as_number;concept_name;category;gest_value;date_of_birth;date_diff;age
# 12 rows for person_id=1: gestation visits 2021-05-27 through 2022-01-10 (episode 1), 2023-06-26 through 2024-04-20 (episodes 2 and 3).
# buildGestationEpisodes(minDays=70, bufferDays=28) groups these into 3 distinct episodes.

test_that("gestational episodes from preg_hip_records", {
  cdm <- mockPregnancyCdm()
  expect_s3_class(cdm, "cdm_reference")

  outputDir <- file.path(tempdir(), "test_episodes")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputDir)

  cdm <- initPregnancies(cdm)

  # Overwrite preg_hip_records with custom gestation visits (columns required by buildGestationEpisodes)
  gestation_visits <- utils::read.csv(
    testthat::test_path("gestation_visits.csv"),
    sep = ";"
  ) %>%
    dplyr::mutate(
      visit_date = as.Date(.data$visit_date)
    ) %>%
    dplyr::select("person_id", "concept_id", "visit_date", "value_as_number", "category")

  cdm <- CDMConnector::insertTable(
    cdm,
    name = "preg_hip_records",
    table = gestation_visits,
    overwrite = TRUE
  )

  cdm <- PregnancyIdentifier:::buildGestationEpisodes(
    cdm,
    logger = logger,
    minDays = 70,
    bufferDays = 28
  )

  episodes <- dplyr::collect(cdm$gest_episodes_df)
  testthat::expect_equal(length(unique(episodes$episode)), 3L)

  unlink(outputDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})
