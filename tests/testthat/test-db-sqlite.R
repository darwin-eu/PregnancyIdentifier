# Database test: SQLite. Copies minimal mock CDM with copyCdmTo, runs pipeline, checks files.
# Runs in parallel with other test-db-* files.
# Skipped when CDMConnector does not support SQLite for copyCdmTo (listTables).

test_that("runPregnancyIdentifier on SQLite (copyCdmTo) produces result files", {
  skip_if_not_installed("RSQLite")
  skip("CDMConnector copyCdmTo does not support SQLite (listTables); run DuckDB and Postgres DB tests instead")
  cdm_src <- mockPregnancyCdm()

  db_path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(
    {
      if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
      if (file.exists(db_path)) unlink(db_path)
    },
    add = TRUE
  )

  cdm <- CDMConnector::copyCdmTo(
    con = con,
    cdm = cdm_src,
    schema = "main",
    overwrite = TRUE
  )
  cleanupCdmDb(cdm_src)

  outputDir <- file.path(tempdir(), "test_db_sqlite")
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
