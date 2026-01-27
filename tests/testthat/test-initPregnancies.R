test_that("initPregnancies runs without error", {
  cdm <- mockPregnancyCdm()
  cdm <- initPregnancies(cdm)
  expect_s3_class(cdm, "cdm_reference")

  expect_true(
    all(
      c("preg_hip_concepts", "preg_pps_concepts","preg_matcho_term_durations", "preg_initial_cohort")
      %in%  names(cdm)
    )
  )
  CDMConnector::cdmDisconnect(cdm)
})

test_that("initPregnancies runs with custom parameters", {
  cdm <- mockPregnancyCdm()
  cdm <- initPregnancies(
    cdm,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2020-12-31"),
    ageBounds = c(18L, 50L)
  )
  expect_s3_class(cdm, "cdm_reference")
  expect_true(
    all(
      c("preg_hip_concepts", "preg_pps_concepts","preg_matcho_term_durations", "preg_initial_cohort")
      %in%  names(cdm)
    )
  )
  CDMConnector::cdmDisconnect(cdm)
})

