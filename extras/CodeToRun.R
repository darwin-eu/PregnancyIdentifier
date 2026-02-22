# CodeToRun.R — Full workflow: connect, run pipeline with export, optional PET comparison,
# optional Shiny viewer, optional ZIP for sharing.
#
# Connection setup follows the same pattern as the PostgreSQL database tests
# (see tests/testthat/test-db-postgres.R and tests/testthat/helper-db.R).
# Set environment variables for your database (e.g. in .Renviron or in the session):
#   CDM5_POSTGRESQL_DBNAME, CDM5_POSTGRESQL_HOST, CDM5_POSTGRESQL_USER,
#   CDM5_POSTGRESQL_PASSWORD, and optionally CDM5_POSTGRESQL_PORT (default 5432).

library(dplyr)
library(PregnancyIdentifier)

# -----------------------------------------------------------------------------
# 1. Connect to the CDM (PostgreSQL example)
# -----------------------------------------------------------------------------
# Uses env vars: CDM5_POSTGRESQL_DBNAME, CDM5_POSTGRESQL_HOST,
#                CDM5_POSTGRESQL_USER, CDM5_POSTGRESQL_PASSWORD,
#                CDM5_POSTGRESQL_PORT (optional, default 5432).

dbname <- Sys.getenv("CDM5_POSTGRESQL_DBNAME")
host   <- Sys.getenv("CDM5_POSTGRESQL_HOST")
user   <- Sys.getenv("CDM5_POSTGRESQL_USER")
password <- Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
port   <- as.integer(Sys.getenv("CDM5_POSTGRESQL_PORT", "5432"))

cdmConnection <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = dbname,
  host     = host,
  port     = port,
  user     = user,
  password = password
)

cdmSchema    <- "..."   # e.g. "main" or "cdm"
writeSchema  <- "..."   # schema where the package can create tables (e.g. "scratch")

cdm <- CDMConnector::cdmFromCon(
  con        = cdmConnection,
  cdmSchema  = cdmSchema,
  writeSchema = writeSchema
)

# -----------------------------------------------------------------------------
# 2. Paths
# -----------------------------------------------------------------------------
# Pipeline writes intermediate RDS/logs here; shareable CSVs go to exportDir.
# The Shiny app expects either ZIP files or one subfolder per database with CSVs,
# so we pass outputDir (which contains the "export" subfolder) when launching it.
outputDir  <- "./output/"
exportDir  <- file.path(outputDir, "export")

# -----------------------------------------------------------------------------
# 3. Run the pipeline with export
# -----------------------------------------------------------------------------
# Writes shareable summary CSVs to exportDir. No ZIP is created here; use
# zipExportFolder() later if you want to bundle everything for sharing.
runPregnancyIdentifier(
  cdm                   = cdm,
  outputDir             = outputDir,
  runExport             = TRUE,
  minCellCount          = 5L,
  conformToValidation   = FALSE
)

# -----------------------------------------------------------------------------
# 4. (Optional) PET comparison — write comparison tables into the export folder
# -----------------------------------------------------------------------------
# Run only if the Pregnancy Extension Table (PET) is available in your database.
# Comparison CSVs are written to exportDir so they sit alongside the other
# shareable outputs and can be included in the same ZIP and viewed in Shiny.

runPetComparison <- FALSE   # set to TRUE when PET is available

if (runPetComparison) {
  petSchema <- "..."   # schema containing the PET table (e.g. "omop_cmbd")
  petTable  <- "..."   # PET table name (e.g. "pregnancy_episode" or "pregnancy_extension")

  comparePregnancyIdentifierWithPET(
    cdm                          = cdm,
    outputDir                    = outputDir,
    outputFolder                 = exportDir,
    petSchema                    = petSchema,
    petTable                     = petTable,
    minOverlapDays               = 1L,
    removeWithinSourceOverlaps   = FALSE,
    outputLogToConsole           = TRUE
  )
}

# -----------------------------------------------------------------------------
# 5. (Optional) Launch the Shiny app to explore results
# -----------------------------------------------------------------------------
# Pass the parent of the export folder (outputDir) so the app finds the
# "export" subfolder and loads its CSVs. Launch in browser with launch.browser = TRUE.

launchShiny <- FALSE   # set to TRUE to open the viewer

if (launchShiny) {
  viewResults(dataFolder = outputDir, launch.browser = TRUE)
}

# -----------------------------------------------------------------------------
# 6. (Optional) Create a ZIP of the export folder for sharing
# -----------------------------------------------------------------------------
# Do this after export (and after PET comparison if run) so the ZIP contains
# all shareable CSVs and PET comparison tables. Customise zipPath if you want
# a specific filename (e.g. including your CDM name).

createZip <- FALSE   # set to TRUE to create the ZIP

if (createZip) {
  zipExportFolder(exportDir = exportDir)
  # Or with a custom path:
  # zipExportFolder(exportDir = exportDir,
  #                 zipPath = file.path(exportDir, "2026-02-20-3.0.1-MyCDM-results.zip"))
}

# -----------------------------------------------------------------------------
# 7. Disconnect
# -----------------------------------------------------------------------------
CDMConnector::cdmDisconnect(cdm)
