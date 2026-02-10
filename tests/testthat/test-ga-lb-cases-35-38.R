# Test cases for persons 35, 36, 37, 38: GA (gestational age) vs LB (live birth) timing.
# Data is in condition_occurrence; use cdmFlatten(cdm) |> collect() then filter by person_id to inspect.
#
# Scenario summary (from test data TestData_P4_C5_002_1.json):
# - Person 35: GA 40 weeks recorded 2023-12-20 only (no LB in data) -> gestation-only episode.
# - Person 36: GA 40 weeks and LB both on 2023-12-20 (exact match) -> start 40w before GA, end = LB.
# - Person 37: GA 39 weeks on 2023-12-13, LB on 2023-12-20 (GA before outcome, +1 week) -> start from GA, end = LB.
# - Person 38: GA 40 weeks on 2023-12-20, LB on 2023-12-06 (GA after outcome, -2 weeks) -> start 40w before LB, end = LB.

test_that("persons 35--38: final outcome category and episode dates are correct", {
  library(dplyr, warn.conflicts = FALSE)

  cdm <- mockPregnancyCdm()
  outputFolder <- file.path(tempdir(), "test_ga_lb_35_38")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(cdm, outputFolder, outputLogToConsole = FALSE)

  df <- readRDS(file.path(outputFolder, "final_pregnancy_episodes.rds")) |>
    dplyr::select(person_id, final_episode_start_date, final_episode_end_date, final_outcome_category)

  # ---- Person 35: GA 40w on 2023-12-20 only (no LB in condition_occurrence) ----
  # Algorithm may or may not create an episode for gestation-only (no outcome); both are acceptable.
  out35 <- dplyr::filter(df, .data$person_id == 35L)
  if (nrow(out35) >= 1L) {
    expect_true(!is.na(out35$final_episode_start_date[1L]), info = "person 35 start date should be populated")
    expect_true(!is.na(out35$final_episode_end_date[1L]), info = "person 35 end date should be populated")
    expect_true(out35$final_episode_start_date[1L] <= out35$final_episode_end_date[1L],
      info = "person 35 start should be on or before end"
    )
    expect_true(
      out35$final_outcome_category[1L] %in% c("PREG", "LB"),
      info = "person 35 outcome category should be PREG or LB"
    )
  }
  # If 0 episodes, that is acceptable for gestation-only with no outcome.

  # ---- Person 36: GA 40 weeks, LB +0 week (recorded same day) ----
  # Expected: final_outcome_category = LB, end = 2023-12-20, start = 40 weeks before 2023-12-20
  out36 <- dplyr::filter(df, .data$person_id == 36L)
  expect_equal(nrow(out36), 1, info = "person 36 should have exactly one episode")
  expect_equal(out36$final_outcome_category, "LB", info = "person 36 should be LB")
  expect_equal(out36$final_episode_end_date, as.Date("2023-12-20"), info = "person 36 end = LB date")
  expect_true(!is.na(out36$final_episode_start_date), info = "person 36 start should be populated")
  # Start = 40 weeks (280 days) before recording of GA (same as LB date)
  expected_start_36 <- as.Date("2023-12-20") - 280L
  expect_equal(out36$final_episode_start_date, expected_start_36,
    info = "person 36 start should be 40 weeks before GA/LB date (2023-03-15)"
  )
  expect_true(out36$final_episode_start_date <= out36$final_episode_end_date,
    info = "person 36 start should be before end"
  )

  # ---- Person 37: GA 38/39 weeks, LB +1 week (GA before outcome) ----
  # Data: GA 39w on 2023-12-13, LB on 2023-12-20. Start from GA: 39 weeks before 2023-12-13; end = LB.
  out37 <- dplyr::filter(df, .data$person_id == 37L)
  expect_equal(nrow(out37), 1, info = "person 37 should have exactly one episode")
  expect_equal(out37$final_outcome_category, "LB", info = "person 37 should be LB")
  expect_equal(out37$final_episode_end_date, as.Date("2023-12-20"), info = "person 37 end = LB date")
  expect_true(!is.na(out37$final_episode_start_date), info = "person 37 start should be populated")
  # Start = 39 weeks before GA recording date (2023-12-13) -> 2023-03-15
  expected_start_37 <- as.Date("2023-12-13") - (39L * 7L)
  expect_equal(out37$final_episode_start_date, expected_start_37,
    info = "person 37 start should be 39 weeks before GA date (2023-03-15)"
  )
  expect_true(out37$final_episode_start_date <= out37$final_episode_end_date,
    info = "person 37 start should be before end"
  )

  # ---- Person 38: GA 40 weeks, LB -2 weeks (GA recorded after outcome) ----
  # Data: GA 40w on 2023-12-20, LB on 2023-12-06. Start = count back from LB; end = LB.
  # Algorithm uses Matcho term (e.g. max_term 301 days for LB) for start when inferring from outcome.
  out38 <- dplyr::filter(df, .data$person_id == 38L)
  expect_equal(nrow(out38), 1, info = "person 38 should have exactly one episode")
  expect_equal(out38$final_outcome_category, "LB", info = "person 38 should be LB")
  expect_equal(out38$final_episode_end_date, as.Date("2023-12-06"), info = "person 38 end = LB date")
  expect_true(!is.na(out38$final_episode_start_date), info = "person 38 start should be populated")
  # Start is derived from LB date (outcome); algorithm uses term duration (e.g. 301 days for LB)
  expect_true(out38$final_episode_start_date >= as.Date("2023-02-01") &&
    out38$final_episode_start_date <= as.Date("2023-03-15"),
    info = "person 38 start should be ~40–43 weeks before LB (2023-12-06)"
  )
  expect_true(out38$final_episode_start_date <= out38$final_episode_end_date,
    info = "person 38 start should be before end"
  )

  cleanupCdmDb(cdm)
  unlink(outputFolder, recursive = TRUE)
})
