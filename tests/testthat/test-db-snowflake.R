# Database test: Snowflake. Copies minimal mock CDM with copyCdmTo, runs pipeline, checks files.
# Runs only when SNOWFLAKE_* env vars are set. Runs in parallel with other test-db-* files.
# See CDMConnector vignette a04_DBI_connection_examples (odbc).

test_that("runPregnancyIdentifier on Snowflake (copyCdmTo) produces result files", {
  skip_if(tolower(Sys.getenv("SKIP_DATABASE_TESTING", "")) == "true", "SKIP_DATABASE_TESTING is set")
  skip_if_not_installed("odbc")
  skip_if(Sys.getenv("SNOWFLAKE_SERVER") == "")

  con <- get_connection("snowflake")

  # DBI::dbExecute(con, "create schema pregnancy_cdm")
  # DBI::dbGetQuery(con, "select current_role() as role, current_database() as db, current_schema() as schema")
  # DBI::dbGetQuery(con, "create database if not exists SCRATCH;")
  # DBI::dbGetQuery(con, "grant ownership on database SCRATCH to role ACCOUNTADMIN;")

  # only need to do this once
  # cdm_src <- mockPregnancyCdm(fullVocab = FALSE)
  # cdm <- CDMConnector::copyCdmTo(
  #   con = con,
  #   cdm = cdm_src,
  #   schema = "PREGNANCY_CDM",
  #   overwrite = TRUE
  # )
  # cleanupCdmDb(cdm_src)

  writeSchema <- c(schema = "PREGNANCY_CDM", prefix = prefix())
  cdmSchema <- c("PREGNANCY_CDM")

  cdm <- CDMConnector::cdmFromCon(
    con,
    cdmSchema = cdmSchema,
    writeSchema = writeSchema,
    cdmName = "preg_snowflake_test"
  )

  outputDir <- file.path(tempdir(), "test_db_snowflake")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(
    cdm = cdm,
    outputDir = outputDir,
    outputLogToConsole = FALSE
  )

  expect_true(file.exists(file.path(outputDir, "hip_episodes.rds")),
              label = "hip_episodes.rds exists")
  expect_true(file.exists(file.path(outputDir, "pps_episodes.rds")),
              label = "pps_episodes.rds exists")
  expect_true(file.exists(file.path(outputDir, "hipps_episodes.rds")),
              label = "hipps_episodes.rds exists")
  expect_true(file.exists(file.path(outputDir, "final_pregnancy_episodes.rds")),
              label = "final_pregnancy_episodes.rds exists")

  final <- readRDS(file.path(outputDir, "final_pregnancy_episodes.rds"))

  expect_true(nrow(final) > 0)

  unlink(outputDir, recursive = TRUE)
  CDMConnector::cdmDisconnect(cdm)
})
