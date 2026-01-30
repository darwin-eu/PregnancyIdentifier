test_that("mockPregnancyCdm runs without error", {
  cdm <- mockPregnancyCdm()
  expect_s3_class(cdm, "cdm_reference")
  cleanupCdmDb(cdm)
})

