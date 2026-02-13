# Database test: Spark (Databricks). Copies minimal mock CDM with copyCdmTo, runs pipeline, checks files.
# Runs only when DATABRICKS_* env vars are set. Runs in parallel with other test-db-* files.
# See CDMConnector vignette a04_DBI_connection_examples (odbc::databricks).

test_that("runPregnancyIdentifier on Spark/Databricks (copyCdmTo) produces result files", {
  skip_if_not_installed("odbc")
  httpPath <- Sys.getenv("DATABRICKS_HTTPPATH", NA_character_)
  if (is.na(httpPath) || !nzchar(httpPath)) {
    skip("Spark/Databricks test skipped: set DATABRICKS_HTTPPATH, DATABRICKS_HOST, DATABRICKS_TOKEN to run")
  }
  host <- Sys.getenv("DATABRICKS_HOST", "")
  token <- Sys.getenv("DATABRICKS_TOKEN", "")
  schema <- Sys.getenv("DATABRICKS_SCHEMA", "scratch")
  if (!nzchar(host) || !nzchar(token)) {
    skip("Spark/Databricks test skipped: set DATABRICKS_HOST and DATABRICKS_TOKEN to run")
  }

  cdm_src <- mockPregnancyCdm()

  con <- DBI::dbConnect(
    odbc::databricks(),
    httpPath = httpPath,
    useNativeQuery = FALSE
  )
  on.exit(
    {
      if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    },
    add = TRUE
  )

  cdm <- CDMConnector::copyCdmTo(
    con = con,
    cdm = cdm_src,
    schema = schema,
    overwrite = TRUE
  )
  cleanupCdmDb(cdm_src)

  outputDir <- file.path(tempdir(), "test_db_spark")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(outputDir, recursive = TRUE), add = TRUE)

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

  CDMConnector::cdmDisconnect(cdm)
})
