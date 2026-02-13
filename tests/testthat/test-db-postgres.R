# Database test: PostgreSQL. Copies minimal mock CDM with copyCdmTo, runs pipeline, checks files.
# Runs only when PG_* env vars are set. Runs in parallel with other test-db-* files.

test_that("runPregnancyIdentifier on PostgreSQL (copyCdmTo) produces result files", {
  skip_if_not_installed("RPostgres")
  dbname <- Sys.getenv("PG_DBNAME", NA_character_)
  if (is.na(dbname) || !nzchar(dbname) || dbname == "...") {
    skip("PostgreSQL test skipped: set PG_DBNAME (and PG_HOST, PG_USER, PG_PASSWORD, PG_SCHEMA) to run")
  }
  host <- Sys.getenv("PG_HOST", "localhost")
  port <- as.integer(Sys.getenv("PG_PORT", "5432"))
  user <- Sys.getenv("PG_USER", "")
  password <- Sys.getenv("PG_PASSWORD", "")
  schema <- Sys.getenv("PG_SCHEMA", "public")
  if (!nzchar(user)) {
    skip("PostgreSQL test skipped: set PG_USER to run")
  }

  cdm_src <- mockPregnancyCdm()

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = dbname,
    host = host,
    port = port,
    user = user,
    password = password
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

  outputDir <- file.path(tempdir(), "test_db_postgres")
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
