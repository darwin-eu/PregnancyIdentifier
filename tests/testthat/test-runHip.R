test_that("runHip runs without error", {
  cdm <- mockPregnancyCdm()
  expect_s3_class(cdm, "cdm_reference")

  outputDir <- file.path(tempdir(), "test_runHip")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputDir, outputLogToConsole = FALSE)



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
    outputDir = outputDir,
    logger = logger
  )


  expect_true(file.exists(file.path(outputDir, "hip_episodes.rds")))

  hip <- readRDS(file.path(outputDir, "hip_episodes.rds"))
  expect_true(is.data.frame(hip))

  unlink(outputDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("runHip runs with custom parameters", {
  cdm <- mockPregnancyCdm()
  outputDir <- file.path(tempdir(), "test_runHip_custom")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputDir, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)

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
  cleanupCdmDb(cdm)
})

