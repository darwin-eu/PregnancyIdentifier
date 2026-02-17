# Database test: PostgreSQL. Copies minimal mock CDM with copyCdmTo, runs pipeline, checks files.
# Runs only when PG_* env vars are set. Runs in parallel with other test-db-* files.

test_that("runPregnancyIdentifier runs on SQL Server", {
  skip_if(tolower(Sys.getenv("SKIP_DATABASE_TESTING", "")) == "true", "SKIP_DATABASE_TESTING is set")
  skip_if_not_installed("odbc")
  skip_if(Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE") == "")
  con <- get_connection("sqlserver")
  writeSchema <- get_write_schema("sqlserver", prefix = "preg_")

  existingTables <- CDMConnector::listTables(con, writeSchema)

  if (!any(grepl("pregnancy_extension", existingTables, ignore.case = TRUE))) {
    # only do this once. on sqlserver tempdb is cleaned up on restart so we may need to copy
    # the pregnancy tables if they are not there. Luckily on sql server this is fast.
    cdm_src <- mockPregnancyCdm(fullVocab = FALSE)

    cdm <- CDMConnector::copyCdmTo(
      con = con,
      cdm = cdm_src,
      schema = writeSchema,
      overwrite = TRUE
    )
    cleanupCdmDb(cdm_src)
  }

  cdm <- CDMConnector::cdmFromCon(
    con,
    cdmSchema = writeSchema,
    writeSchema = writeSchema,
    cdmName = "preg_postgres_test"
  )

  outputDir <- file.path(tempdir(), "test_db_sqlserver")
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
