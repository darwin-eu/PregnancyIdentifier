# Database test: PostgreSQL. Copies minimal mock CDM with copyCdmTo, runs pipeline, checks files.
# Runs only when PG_* env vars are set. Runs in parallel with other test-db-* files.

test_that("runPregnancyIdentifier on PostgreSQL (copyCdmTo) produces result files", {
  skip_if_not_installed("RPostgres")
  skip_if(Sys.getenv("CDM5_POSTGRESQL_DBNAME") == "")
  con <- get_connection("postgres")
  writeSchema <- get_write_schema("postgres", prefix = "preg_")

  # only do this once
  # cdm_src <- mockPregnancyCdm(fullVocab = FALSE)
  #
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
    cdmName = "preg_postgres_test"
  )


  outputDir <- file.path(tempdir(), "test_db_postgres")
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
  disconnect(con)
  CDMConnector::cdmDisconnect(cdm)
})
