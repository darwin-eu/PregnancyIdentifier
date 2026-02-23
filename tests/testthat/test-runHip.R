test_that("runHip runs without error", {
  cdm <- mockPregnancyCdm()
  expect_s3_class(cdm, "cdm_reference")

  outputFolder <- file.path(tempdir(), "test_runHip")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)



  suppressWarnings({
    library(CDMConnector)
    library(dplyr, warn.conflicts = FALSE)
  })

# cdmCommentContents(cdm, 12)
# person_id | observation_concept_id | start_date | end_date | type_concept_id | domain               | observation_concept_name | type_concept_name
# 12        | 4014295                | 2017-12-15 | NA       | 32817           | condition_occurrence | Single live birth        | EHR

 #
 cdm <- CDMConnector::cdmSubset(cdm, 12L)
 #
 #  debugonce(initPregnancies)
 cdm <- initPregnancies(cdm, logger = logger)
 #
 #  cdm$preg_hip_records
 #
  # debugonce(runHip)
  # debugonce(attachGestationAndLength)
  cdm <- runHip(
    cdm = cdm,
    outputFolder = outputFolder,
    logger = logger
  )


  expect_true(file.exists(file.path(outputFolder, "hip_episodes.rds")))

  hip <- readRDS(file.path(outputFolder, "hip_episodes.rds"))
  expect_true(is.data.frame(hip))

  unlink(outputFolder, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("runHip runs with custom parameters", {
  cdm <- mockPregnancyCdm()
  outputFolder <- file.path(tempdir(), "test_runHip_custom")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)

  cdm <- runHip(
    cdm = cdm,
    outputFolder = outputFolder,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2020-12-31"),
    justGestation = FALSE,
    logger = logger
  )

  expect_s3_class(cdm, "cdm_reference")

  unlink(outputFolder, recursive = TRUE)
  cleanupCdmDb(cdm)
})

