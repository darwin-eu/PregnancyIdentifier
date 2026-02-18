
test_that("runPregnancyIdentifier runs on SQL Server", {
  skip_if(tolower(Sys.getenv("SKIP_DATABASE_TESTING", "")) == "true", "SKIP_DATABASE_TESTING is set")
  skip_if_not_installed("odbc")
  skip_if(Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE") == "")
  con <- get_connection("sqlserver")

  writeSchema <- c("pregnancy_cdm")
  cdmSchema <- c("pregnancy_cdm")

  existingTables <- CDMConnector::listTables(con, writeSchema)

    # # only do this once.
    # cdm_src <- mockPregnancyCdm(fullVocab = FALSE)
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
