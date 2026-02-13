test_that("mockPregnancyCdm runs without error", {
  cdm <- mockPregnancyCdm()
  expect_s3_class(cdm, "cdm_reference")
  cleanupCdmDb(cdm)
})

test_that("mockPregnancyCdm runs without error", {
  cdm <- mockPregnancyCdm(fullVocab = FALSE)
  expect_s3_class(cdm, "cdm_reference")
  expect_true(dplyr::tally(cdm$concept) %>% dplyr::pull("n") < 1000)
  cleanupCdmDb(cdm)
})

