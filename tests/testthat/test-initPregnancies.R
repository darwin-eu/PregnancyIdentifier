test_that("initPregnancies runs without error", {
  cdm <- mockPregnancyCdm()
  logger <- makeLogger(tempdir())
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
      c("preg_hip_concepts", "preg_pps_concepts","preg_matcho_term_durations", "preg_hip_records", "preg_pps_records")
      %in%  names(cdm)
    )
  )
  cleanupCdmDb(cdm)
})

