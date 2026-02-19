test_that("mergeHipps runs without error", {
  cdm <- mockPregnancyCdm()
  outputDir <- file.path(tempdir(), "test_mergeHipps")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  logger <- makeLogger(outputDir, outputLogToConsole = FALSE)
  cdm <- initPregnancies(cdm, logger = logger)

  # Run HIP and PPS first to create required files
  cdm <- runHip(cdm = cdm, outputDir = outputDir, logger = logger)
  expect_equal(c("hip_episodes.rds", "log.txt"), sort(list.files(outputDir)))

  cdm <- runPps(cdm = cdm, outputDir = outputDir, logger = logger)
  expect_true(all(
    c("pps_concept_counts.csv", "pps_episodes.rds") %in% list.files(outputDir)
  ))

  # intermediate outputs for debugging
  expect_false(any(
    c("pps_min_max_episodes.rds", "pps_gest_timing_episodes.rds") %in% list.files(outputDir)
  ))

  cdm <- runPps(cdm = cdm, outputDir = outputDir, logger = logger, debugMode = TRUE)
  expect_true(all(
    c("pps_concept_counts.csv", "pps_gest_timing_episodes.rds",
      "pps_min_max_episodes.rds", "pps_episodes.rds") %in% list.files(outputDir)
  ))


  # Now test mergeHipps
  mergeHipps(outputDir = outputDir, logger = logger)
  expect_true("hipps_episodes.rds" %in% list.files(outputDir))

  expect_no_error({
    purrr::map(list.files(outputDir, pattern = "rds"), ~readRDS(file.path(outputDir, .))) %>%
      setNames(list.files(outputDir, pattern = "rds"))
  })

  expect_no_error(
    read.csv(file.path(outputDir, "pps_concept_counts.csv")) %>% dplyr::tibble()
  )

  unlink(outputDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

