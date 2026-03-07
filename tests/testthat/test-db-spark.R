# Database test: DATABRICKS/SPARK Copies minimal mock CDM with copyCdmTo, runs pipeline, checks files.

test_that("runPregnancyIdentifier on SPARK/Databricks", {
  skip_on_cran()
  skip_if(tolower(Sys.getenv("SKIP_DATABASE_TESTING", "")) == "true", "SKIP_DATABASE_TESTING is set")
  skip_if_not_installed("odbc")
  skip_if(Sys.getenv("DATABRICKS_HTTPPATH") == "")

  con <- get_connection("spark")
  omopgenerics::uniqueId()

  writeSchema <- c(schema = "pregnancy_test_cdm", prefix = prefix())
  cdmSchema <- "pregnancy_test_cdm"

  # only do this once
  # cdm_src <- mockPregnancyCdm(fullVocab = FALSE)

  # cdm_src %>%
  #   purrr::map_dbl(~dplyr::tally(.) %>% dplyr::pull(n))
  #
  # cdm <- CDMConnector::copyCdmTo(
  #   con = con,
  #   cdm = cdm_src,
  #   schema = cdmSchema,
  #   overwrite = TRUE
  # )
  # cleanupCdmDb(cdm_src)

  cdm <- CDMConnector::cdmFromCon(
    con,
    cdmSchema = cdmSchema,
    writeSchema = writeSchema,
    cdmName = "preg_spark_test"
  )

  outputFolder <- file.path(tempdir(), "test_db_spark")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(
    cdm = cdm,
    outputFolder = outputFolder,
    outputLogToConsole = FALSE
  )

  expect_true(file.exists(file.path(outputFolder, "hip_episodes.rds")),
              label = "hip_episodes.rds exists")
  expect_true(file.exists(file.path(outputFolder, "pps_episodes.rds")),
              label = "pps_episodes.rds exists")
  expect_true(file.exists(file.path(outputFolder, "hipps_episodes.rds")),
              label = "hipps_episodes.rds exists")
  expect_true(file.exists(file.path(outputFolder, "final_pregnancy_episodes.rds")),
              label = "final_pregnancy_episodes.rds exists")

  final <- readRDS(file.path(outputFolder, "final_pregnancy_episodes.rds"))

  expect_true(nrow(final) > 0)

  unlink(outputFolder, recursive = TRUE)
  CDMConnector::cdmDisconnect(cdm)
})
