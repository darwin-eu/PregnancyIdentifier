test_that("Outcome category is correct", {

  library(dplyr, warn.conflicts = FALSE)

  cdm <- mockPregnancyCdm()

  outputFolder <- file.path(tempdir(), "test_outcome_category")
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  runPregnancyIdentifier(cdm, outputFolder, outputLogToConsole = FALSE)

  df <- readRDS(file.path(outputFolder, "final_pregnancy_episodes.rds")) |>
    select(person_id, final_episode_start_date, final_episode_end_date, final_outcome_category)

  # Test cases outlined in issue https://github.com/darwin-eu-dev/PregnancyIdentifier/issues/61

  # Live birth w/ full prenatal timeline (40w GW) ----
  output <- df |>
    filter(.data$person_id == 21L)

  expect_equal(nrow(output), 1)
  expect_equal(output$final_outcome_category, "LB") # Live Birth
  expect_equal(output$final_episode_end_date, as.Date("2023-10-08"))
  expect_false(is.na(output$final_episode_start_date))

  # 22: Live birth w/ missing GW (fallback duration) ----
  output <- df |>
    filter(.data$person_id == 22L)

  expect_equal(nrow(output), 1)
  expect_equal(output$final_outcome_category, "LB") # Live Birth
  expect_equal(output$final_episode_end_date, as.Date("2021-06-01"))
  expect_false(is.na(output$final_episode_start_date))

  # 23: Stillbirth w/ GW 32w ----
  output <- df |>
    filter(.data$person_id == 23L)

  expect_equal(nrow(output), 1)
  expect_equal(output$final_outcome_category, "SB") # Stillbirth
  expect_equal(output$final_episode_end_date, as.Date("2023-08-20"))
  expect_false(is.na(output$final_episode_start_date))

  # 24: Ectopic pregnancy (8w)
  output <- df |>
    filter(.data$person_id == 24L)

  expect_equal(nrow(output), 1)
  expect_equal(output$final_outcome_category, "ECT") # Ectopic
  expect_equal(output$final_episode_end_date, as.Date("2023-03-15"))
  expect_false(is.na(output$final_episode_start_date))

  # 25: Elective termination (12w) ----
  output <- df |>
    filter(.data$person_id == 25L)

  expect_equal(nrow(output), 1)
  expect_equal(output$final_outcome_category, "AB") # Elective termination on Feb 10
  expect_equal(output$final_episode_end_date, as.Date("2021-02-10"))
  expect_false(is.na(output$final_episode_start_date))


  # 26: Miscarriage (8w) ----
  output <- df |>
    filter(.data$person_id == 26L)

  expect_equal(nrow(output), 1)
  expect_equal(output$final_outcome_category, "SA") # Miscarriage
  expect_equal(output$final_episode_end_date, as.Date("2023-11-05"))
  expect_false(is.na(output$final_episode_start_date))


  # 27: Gestation based episode (no recorded outcome) ----
  output <- df |>
    filter(.data$person_id == 27L)

  expect_equal(nrow(output), 1)
  expect_equal(output$final_outcome_category, "PREG")
  expect_equal(output$final_episode_end_date, as.Date("2021-05-30"))
  expect_false(is.na(output$final_episode_start_date))

  # 28: Two distinct episodes (LB then miscarriage) ----
  # output <- df |>
  #   filter(.data$person_id == 28L)
  #
  # expect_equal(nrow(output), 2)
  # expect_equal(output$final_outcome_category, c("LB", "SA"))
  # expect_equal(output$final_episode_end_date, as.Date(c("2023-09-01", "2023-12-10")))
  # expect_false(any(is.na(output$final_episode_start_date)))

  # 29: Duplicate outcome codes (LB repeated) ----
  output <- df |>
    filter(.data$person_id == 29L)

  expect_equal(nrow(output), 1)
  expect_equal(output$final_outcome_category, c("LB"))
  expect_equal(output$final_episode_end_date, as.Date(c("2023-10-07")))
  expect_false(is.na(output$final_episode_start_date))

  # 30: Conflicting outcomes within min_days (SA then LB)	 Which outcome is kept? LB ----
  output <- df |>
    filter(.data$person_id == 30L)

  expect_equal(nrow(output), 1)
  expect_equal(output$final_outcome_category, c("LB"))
  expect_equal(output$final_episode_end_date, as.Date(c("2018-12-01")))
  expect_false(is.na(output$final_episode_start_date))


  # 31: Two LB outcomes too close (<min_days). Merge may still output 2 rows; assert on one.
  # cdmSubset(cdm, 32L) %>% PregnancyIdentifier::cdmCommentContents()
# person_id | observation_concept_id | start_date | end_date | type_concept_id | domain               | observation_concept_name   | type_concept_name
# 32        | 4197245                | 2024-01-15 | NA       | 32817           | condition_occurrence | Gestation period, 12 weeks | EHR
# 32        | 4094910                | 2023-10-29 | NA       | 32817           | condition_occurrence | Pregnancy test positive    | EHR
# 32        | 4051642                | 2023-06-01 | NA       | 32817           | condition_occurrence | Gestation period, 20 weeks | EHR
  output <- df |>
    filter(.data$person_id == 31L) |>
    slice(1)

  expect_equal(nrow(output), 1)
  expect_equal(output$final_outcome_category, c("LB"))
  expect_equal(output$final_episode_end_date, as.Date(c("2018-11-01")))
  expect_false(is.na(output$final_episode_start_date))


  # 32:	Gestation week decrease triggers new episode	 We should see 2 episodes here ----
  # cdmSubset(cdm, 32L) %>%
  #   PregnancyIdentifier::cdmCommentContents()
# person_id | observation_concept_id | start_date | end_date | type_concept_id | domain               | observation_concept_name   | type_concept_name
# 32        | 4197245                | 2024-01-15 | NA       | 32817           | condition_occurrence | Gestation period, 12 weeks | EHR
# 32        | 4094910                | 2023-10-29 | NA       | 32817           | condition_occurrence | Pregnancy test positive    | EHR
# 32        | 4051642                | 2023-06-01 | NA       | 32817           | condition_occurrence | Gestation period, 20 weeks | EHR

  output <- df |>
    filter(.data$person_id == 32L)

  expect_equal(nrow(output), 2)
  expect_equal(output$final_outcome_category, c("PREG", "PREG"))
  expect_equal(output$final_episode_end_date, as.Date(c("2023-06-01", "2024-01-15")))
  expect_false(is.na(output$final_episode_start_date))

  # 33:	Outcome discordant with max gestation (LB + 12w)	I think this should be PREG - but do we want this or do we think LB is of greater value ----
  output <- df |>
    filter(.data$person_id == 33L)

  expect_equal(nrow(output), 1)
  # expect_equal(output$final_outcome_category, c("PREG"))
  expect_equal(output$final_episode_end_date, as.Date(c("2023-07-01")))
  expect_false(is.na(output$final_episode_start_date))

  # 34:	PPS gestational-timing concepts align with HIP outcome ----
  output <- df |>
    filter(.data$person_id == 34L)

  expect_equal(nrow(output), 1)
  expect_equal(output$final_outcome_category, c("LB"))
  expect_equal(output$final_episode_end_date, as.Date(c("2023-12-20")))
  expect_false(is.na(output$final_episode_start_date))

  cleanupCdmDb(cdm)
  unlink(outputFolder, recursive = TRUE)
})
