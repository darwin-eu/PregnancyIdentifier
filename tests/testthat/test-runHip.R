test_that("runHip runs without error", {
  cdm <- mockPregnancyCdm()
  cdm <- initPregnancies(cdm)
  
  outputDir <- file.path(tempdir(), "test_runHip")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputDir)
  
  cdm <- runHip(
    cdm = cdm,
    outputDir = outputDir,
    logger = logger
  )
  
  expect_s3_class(cdm, "cdm_reference")
  expect_true(file.exists(file.path(outputDir, "HIP_episodes.rds")))
  
  unlink(outputDir, recursive = TRUE)
  CDMConnector::cdmDisconnect(cdm)
})

test_that("runHip runs with custom parameters", {
  cdm <- mockPregnancyCdm()
  cdm <- initPregnancies(cdm)
  
  outputDir <- file.path(tempdir(), "test_runHip_custom")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputDir)
  
  cdm <- runHip(
    cdm = cdm,
    outputDir = outputDir,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2020-12-31"),
    justGestation = FALSE,
    logger = logger
  )
  
  expect_s3_class(cdm, "cdm_reference")
  
  unlink(outputDir, recursive = TRUE)
  CDMConnector::cdmDisconnect(cdm)
})

