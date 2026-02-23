# Code to run the full PregnancyIdentifier pipeline with PET comparison
# -----------------------------------------------------------------------
# Prerequisites:
#   - PET (Pregnancy Extension Table) is already in your database.
#   - You have a DBI connection to the CDM and (if PET is in another schema)
#     access to the schema where the PET table lives.
#
# If your CDMConnector version supports including the PET table in the CDM
# object (e.g. via additionalTable = "pregnancy_extension" or cohortTables),
# you can add it when building the CDM so it is available as cdm$pregnancy_extension.
# comparePregnancyIdentifierWithPET() reads the PET table by schema and table
# name, so the CDM object does not need to contain the PET table as long as
# the connection can access it.

library(dplyr)
library(PregnancyIdentifier)

# ---- 1. Database connection and CDM ----
# Replace with your connection details. Include the PET table in the CDM if
# your CDMConnector supports it (e.g. additionalTable = "pregnancy_extension").
cdmConnection <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "...",
  host = "...",
  user = "...",
  password = "..."
)

cdm <- CDMConnector::cdmFromCon(
  con = cdmConnection,
  cdmSchema = "...",
  writeSchema = "..."
  # If supported: additionalTable = "pregnancy_extension"
  # or: cohortTables = "pregnancy_extension"
)

# ---- 2. Paths ----
# outputDir = person-level and episode-level pipeline outputs (RDS, logs).
# exportDir = shareable aggregated CSVs (default: outputDir/export); use as Shiny input.
outputDir   <- "./output/"
exportDir   <- file.path(outputDir, "export")

# ---- 3. Run full pipeline (export always runs; CSVs go to exportFolder) ----
# exportFolder defaults to outputDir/export; pass explicitly to match variable below.
runPregnancyIdentifier(
  cdm                   = cdm,
  outputDir             = outputDir,
  exportFolder          = exportDir,
  minCellCount          = 5L,
  conformToValidation   = FALSE
)

# ---- 4. PET comparison (writes to outputFolder; pipeline reads from outputDir) ----
# Schema and table where the PET (Pregnancy Extension Table) lives.
petSchema <- "..."   # e.g. same as cdmSchema or "omop_cmbd"
petTable  <- "..."   # e.g. "pregnancy_episode" or "pregnancy_extension"

comparePregnancyIdentifierWithPET(
  cdm                          = cdm,
  outputFolder                  = outputDir,
  exportFolder                  = exportDir,
  petSchema                     = petSchema,
  petTable                      = petTable,
  minOverlapDays                = 1L,
  removeWithinSourceOverlaps    = FALSE,
  outputLogToConsole            = TRUE
)

# ---- 5. (Optional) Create a single ZIP of the export folder ----
# Do this after export and PET comparison so the ZIP contains both
# shareable CSVs and PET comparison tables.
zipExportFolder(exportDir = exportDir)

# Optional: specify the ZIP path explicitly (e.g. include your CDM name)
# zipExportFolder(exportDir = exportDir, zipPath = file.path(exportDir, "2026-02-20-3.0.1-MyCDM-results.zip"))

DBI::dbDisconnect(cdmConnection)
