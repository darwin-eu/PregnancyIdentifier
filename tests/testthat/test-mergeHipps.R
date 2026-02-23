test_that("mergeHipps runs without error", {
  cdm <- mockPregnancyCdm()
  outputFolder <- file.path(tempdir(), "test_mergeHipps")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- makeLogger(outputFolder, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)

  # Run HIP and PPS first to create required files
  cdm <- runHip(cdm = cdm, outputFolder = outputFolder, logger = logger)
  expect_equal(c("hip_episodes.rds", "log.txt"), sort(list.files(outputFolder)))

  cdm <- runPps(cdm = cdm, outputFolder = outputFolder, logger = logger)
  expect_true(all(
    c("pps_concept_counts.csv", "pps_episodes.rds") %in% list.files(outputFolder)
  ))

  # intermediate outputs for debugging
  expect_false(any(
    c("pps_min_max_episodes.rds", "pps_gest_timing_episodes.rds") %in% list.files(outputFolder)
  ))

  cdm <- runPps(cdm = cdm, outputFolder = outputFolder, logger = logger, debugMode = TRUE)
  expect_true(all(
    c("pps_concept_counts.csv", "pps_gest_timing_episodes.rds",
      "pps_min_max_episodes.rds", "pps_episodes.rds") %in% list.files(outputFolder)
  ))


  # Now test mergeHipps
  mergeHipps(outputFolder = outputFolder, logger = logger)
  expect_true("hipps_episodes.rds" %in% list.files(outputFolder))

  expect_no_error({
    purrr::map(list.files(outputFolder, pattern = "rds"), ~readRDS(file.path(outputFolder, .))) %>%
      setNames(list.files(outputFolder, pattern = "rds"))
  })

  expect_no_error(
    read.csv(file.path(outputFolder, "pps_concept_counts.csv")) %>% dplyr::tibble()
  )

  unlink(outputFolder, recursive = TRUE)
  cleanupCdmDb(cdm)
})

