# Database test: Snowflake. Copies minimal mock CDM with copyCdmTo, runs pipeline, checks files.
# Runs only when SNOWFLAKE_* env vars are set. Runs in parallel with other test-db-* files.
# See CDMConnector vignette a04_DBI_connection_examples (odbc).

test_that("runPregnancyIdentifier on Snowflake (copyCdmTo) produces result files", {
  skip_if_not_installed("odbc")
  server <- Sys.getenv("SNOWFLAKE_SERVER", NA_character_)
  if (is.na(server) || !nzchar(server)) {
    skip("Snowflake test skipped: set SNOWFLAKE_SERVER, SNOWFLAKE_USER, SNOWFLAKE_PASSWORD, SNOWFLAKE_DATABASE, SNOWFLAKE_WAREHOUSE, SNOWFLAKE_DRIVER to run")
  }
  user <- Sys.getenv("SNOWFLAKE_USER", "")
  password <- Sys.getenv("SNOWFLAKE_PASSWORD", "")
  database <- Sys.getenv("SNOWFLAKE_DATABASE", "")
  warehouse <- Sys.getenv("SNOWFLAKE_WAREHOUSE", "")
  driver <- Sys.getenv("SNOWFLAKE_DRIVER", "SnowflakeDSIIDriver")
  schema <- Sys.getenv("SNOWFLAKE_SCHEMA", "PUBLIC")
  if (!nzchar(user)) {
    skip("Snowflake test skipped: set SNOWFLAKE_USER to run")
  }

  cdm_src <- mockPregnancyCdm()

  con <- DBI::dbConnect(
    odbc::odbc(),
    SERVER = server,
    UID = user,
    PWD = password,
    DATABASE = database,
    WAREHOUSE = warehouse,
    DRIVER = driver
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

  outputDir <- file.path(tempdir(), "test_db_snowflake")
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
