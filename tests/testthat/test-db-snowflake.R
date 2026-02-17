# Database test: Snowflake. Copies minimal mock CDM with copyCdmTo, runs pipeline, checks files.
# Runs only when SNOWFLAKE_* env vars are set. Runs in parallel with other test-db-* files.
# See CDMConnector vignette a04_DBI_connection_examples (odbc).

test_that("runPregnancyIdentifier on Snowflake (copyCdmTo) produces result files", {
  skip_if_not_installed("odbc")
  skip_if(Sys.getenv("SNOWFLAKE_SERVER") == "")

  con <- get_connection("snowflake")

  # DBI::dbGetQuery(con, "select current_role() as role, current_database() as db, current_schema() as schema")
  # DBI::dbGetQuery(con, "create database if not exists SCRATCH;")
  # DBI::dbGetQuery(con, "grant ownership on database SCRATCH to role ACCOUNTADMIN;")
  # DBI::dbExecute(con, "create schema preg;")

  writeSchema <- get_write_schema("snowflake", prefix = "preg_")

  # only need to do this once
  # cdm_src <- mockPregnancyCdm(fullVocab = FALSE)
  # cdm <- CDMConnector::copyCdmTo(
  #   con = con,
  #   cdm = cdm_src,
  #   schema = writeSchema,
  #   overwrite = TRUE
  # )
  # cleanupCdmDb(cdm_src)

  cdm <- CDMConnector::cdmFromCon(
    con,
    cdmSchema = writeSchema,
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
