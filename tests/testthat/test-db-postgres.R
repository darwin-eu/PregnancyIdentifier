# Database test: PostgreSQL. Copies minimal mock CDM with copyCdmTo, runs pipeline, checks files.
# Runs only when PG_* env vars are set. Runs in parallel with other test-db-* files.

test_that("runPregnancyIdentifier on PostgreSQL (copyCdmTo) produces result files", {
  skip_on_cran()
  skip_if(tolower(Sys.getenv("SKIP_DATABASE_TESTING", "")) == "true", "SKIP_DATABASE_TESTING is set")
  skip_if_not_installed("RPostgres")
  skip_if(Sys.getenv("CDM5_POSTGRESQL_DBNAME") == "")
  con <- get_connection("postgres")

  writeSchema <- c(schema = "pregnancy_cdm", prefix = prefix())
  cdmSchema <- c("pregnancy_cdm")

  # DBI::dbExecute(con, "create schema pregnancy_cdm;")
  # only do this once
  # cdm_src <- mockPregnancyCdm(fullVocab = FALSE)
  #
  # cdm <- CDMConnector::copyCdmTo(
  #   con = con,
  #   cdm = cdm_src,
  #   schema = "pregnancy_cdm",
  #   overwrite = TRUE
  # )
  # cleanupCdmDb(cdm_src)


  cdm <- CDMConnector::cdmFromCon(
    con,
    cdmSchema = cdmSchema,
    writeSchema = writeSchema,
    cdmName = "preg_postgres_test"
  )

  outputFolder <- file.path(tempdir(), "test_db_postgres")
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
