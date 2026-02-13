# Database test: DuckDB (mockPregnancyCdm is DuckDB-backed).
# Runs in parallel with other test-db-* files.

test_that("runPregnancyIdentifier on DuckDB (mock CDM) produces result files", {
  cdm <- mockPregnancyCdm()
  outputDir <- file.path(tempdir(), "test_db_duckdb")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(outputDir, recursive = TRUE)
    cleanupCdmDb(cdm)
  }, add = TRUE)

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
})
