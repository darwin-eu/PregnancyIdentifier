test_that("initPregnancies runs without error", {
  cdm <- mockPregnancyCdm()
  logger <- makeLogger(tempdir(), outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)
  expect_s3_class(cdm, "cdm_reference")

  expect_true(
    all(
      c("preg_hip_concepts", "preg_pps_concepts","preg_matcho_term_durations", "preg_hip_records", "preg_pps_records")
      %in%  names(cdm)
    )
  )
  cleanupCdmDb(cdm)
})

test_that("initPregnancies includes PREG-only concepts without gest_value", {
  # Regression test: a person whose only pregnancy record is a PREG-category
  # concept (no gest_value) must still appear in preg_hip_records.
  # This guards against re-introducing the v3.0.0 filter that dropped
  # PREG-only concepts at intake.
  cdm <- mockPregnancyCdm()
  con <- CDMConnector::cdmCon(cdm)
  write_schema <- CDMConnector::cdmWriteSchema(cdm)

  # concept_id 443700 = "Pregnancy" — PREG category, no gest_value in the HIP
  # concepts file. Pick a person_id that doesn't collide with test data.
  preg_concept_id <- 443700L
  test_person_id  <- 99999L

  # Insert a female person born 1990 (age ~20 at event date 2010-06-15)
  new_person <- data.frame(
    person_id              = test_person_id,
    gender_concept_id      = 8532L,
    year_of_birth          = 1990L,
    month_of_birth         = 1L,
    day_of_birth           = 1L,
    birth_datetime         = as.POSIXct("1990-01-01"),
    race_concept_id        = 0L,
    ethnicity_concept_id   = 0L,
    location_id            = NA_integer_,
    provider_id            = NA_integer_,
    care_site_id           = NA_integer_,
    person_source_value    = NA_character_,
    gender_source_value    = NA_character_,
    gender_source_concept_id   = 0L,
    race_source_value          = NA_character_,
    race_source_concept_id     = 0L,
    ethnicity_source_value     = NA_character_,
    ethnicity_source_concept_id = 0L
  )
  DBI::dbAppendTable(con, DBI::Id(schema = write_schema, table = "person"), new_person)

  # Insert a single condition_occurrence with the PREG-only concept
  new_condition <- data.frame(
    condition_occurrence_id    = 99999L,
    person_id                  = test_person_id,
    condition_concept_id       = preg_concept_id,
    condition_start_date       = as.Date("2010-06-15"),
    condition_start_datetime   = as.POSIXct("2010-06-15"),
    condition_end_date         = NA,
    condition_end_datetime     = NA,
    condition_type_concept_id  = 32817L,
    condition_status_concept_id = 0L,
    stop_reason                = NA_character_,
    provider_id                = NA_integer_,
    visit_occurrence_id        = NA_integer_,
    visit_detail_id            = NA_integer_,
    condition_source_value     = NA_character_,
    condition_source_concept_id = 0L,
    condition_status_source_value = NA_character_
  )
  DBI::dbAppendTable(con, DBI::Id(schema = write_schema, table = "condition_occurrence"), new_condition)

  logger <- makeLogger(tempdir(), outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)

  # The PREG-only person must appear in preg_hip_records
  preg_records <- cdm$preg_hip_records %>%
    dplyr::filter(.data$person_id == test_person_id) %>%
    dplyr::collect()

  expect_true(nrow(preg_records) > 0, info = "PREG-only concept record must not be filtered out")
  expect_true(all(preg_records$category == "PREG"))

  cleanupCdmDb(cdm)
})

test_that("PREG-outcome person appears in hip_records and final output", {
  # A person with a PREG concept (no gest_value) plus a live-birth concept
  # must appear in preg_hip_records AND in the final pipeline output.
  cdm <- mockPregnancyCdm()
  con <- CDMConnector::cdmCon(cdm)
  write_schema <- CDMConnector::cdmWriteSchema(cdm)

  test_person_id <- 99998L
  preg_concept_id <- 443700L  # PREG category, no gest_value
  lb_concept_id   <- 439136L  # LB category (Asphyxia in liveborn infant)

  new_person <- data.frame(
    person_id              = test_person_id,
    gender_concept_id      = 8532L,
    year_of_birth          = 1990L,
    month_of_birth         = 1L,
    day_of_birth           = 1L,
    birth_datetime         = as.POSIXct("1990-01-01"),
    race_concept_id        = 0L,
    ethnicity_concept_id   = 0L,
    location_id            = NA_integer_,
    provider_id            = NA_integer_,
    care_site_id           = NA_integer_,
    person_source_value    = NA_character_,
    gender_source_value    = NA_character_,
    gender_source_concept_id   = 0L,
    race_source_value          = NA_character_,
    race_source_concept_id     = 0L,
    ethnicity_source_value     = NA_character_,
    ethnicity_source_concept_id = 0L
  )
  DBI::dbAppendTable(con, DBI::Id(schema = write_schema, table = "person"), new_person)

  # observation_period is required for ESD's observation-period filter
  new_obs_period <- data.frame(
    observation_period_id     = 99998L,
    person_id                 = test_person_id,
    observation_period_start_date = as.Date("2005-01-01"),
    observation_period_end_date   = as.Date("2020-12-31"),
    period_type_concept_id        = 44814724L
  )
  DBI::dbAppendTable(con, DBI::Id(schema = write_schema, table = "observation_period"), new_obs_period)

  # PREG concept ~5 months into pregnancy
  new_conditions <- data.frame(
    condition_occurrence_id     = c(99998L, 99997L),
    person_id                   = c(test_person_id, test_person_id),
    condition_concept_id        = c(preg_concept_id, lb_concept_id),
    condition_start_date        = as.Date(c("2010-03-15", "2010-06-15")),
    condition_start_datetime    = as.POSIXct(c("2010-03-15", "2010-06-15")),
    condition_end_date          = as.Date(c(NA, NA)),
    condition_end_datetime      = as.POSIXct(c(NA, NA)),
    condition_type_concept_id   = c(32817L, 32817L),
    condition_status_concept_id = c(0L, 0L),
    stop_reason                 = c(NA_character_, NA_character_),
    provider_id                 = c(NA_integer_, NA_integer_),
    visit_occurrence_id         = c(NA_integer_, NA_integer_),
    visit_detail_id             = c(NA_integer_, NA_integer_),
    condition_source_value      = c(NA_character_, NA_character_),
    condition_source_concept_id = c(0L, 0L),
    condition_status_source_value = c(NA_character_, NA_character_)
  )
  DBI::dbAppendTable(con, DBI::Id(schema = write_schema, table = "condition_occurrence"), new_conditions)

  outputFolder <- file.path(tempdir(), "test_preg_final")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(
    cdm = cdm,
    outputFolder = outputFolder,
    outputLogToConsole = FALSE
  )

  # preg_hip_records is dropped during pipeline cleanup, so verify the person

  # Person must appear in final_pregnancy_episodes.rds
  final <- readRDS(file.path(outputFolder, "final_pregnancy_episodes.rds"))
  person_episodes <- final[final$person_id == test_person_id, ]
  expect_true(nrow(person_episodes) > 0, info = "Person with PREG concept must appear in final output")

  unlink(outputFolder, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("initPregnancies runs with custom parameters", {
  cdm <- mockPregnancyCdm()
  logger <- makeLogger(tempdir(), outputLogToConsole = FALSE)
  cdm <- initPregnancies(
    cdm,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2020-12-31"),
    ageBounds = c(18L, 50L),
    logger = logger
  )
  expect_s3_class(cdm, "cdm_reference")
  expect_true(
    all(
      c("preg_hip_concepts", "preg_pps_concepts","preg_matcho_term_durations", "preg_hip_records", "preg_pps_records")
      %in%  names(cdm)
    )
  )
  cleanupCdmDb(cdm)
})

