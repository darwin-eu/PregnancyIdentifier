# codeToRunExport.R
#
# Run ONLY the export step (Step 7) against an existing final_pregnancy_episodes.rds,
# using the updated PregnancyIdentifier package (>= 3.3.2):
#   * exportPregnancies() no longer uploads the episode table to the database
#     (the copy = TRUE per-row insert that hung on Spark/Databricks is gone), and
#   * it logs detailed per-step START/DONE timing to the console and to
#     export_log.txt in exportFolder, so any slow/stuck step is obvious.
#
# Prerequisites:
#   * Reinstall the package first (R CMD INSTALL .) and restart R so this picks up
#     the 3.3.2 fix. Confirm with: packageVersion("PregnancyIdentifier")
#   * outputFolder must contain final_pregnancy_episodes.rds and runStart.csv.
#
# Required env vars: DATABRICKS_HTTPPATH, DATABRICKS_CDM_SCHEMA,
#                    DATABRICKS_SCRATCH_SCHEMA

library(PregnancyIdentifier)

stopifnot(utils::packageVersion("PregnancyIdentifier") >= "3.3.2")

# ---- Fill these in ----------------------------------------------------------
outputFolder <- "..."   # folder with final_pregnancy_episodes.rds + runStart.csv
exportFolder <- "..."   # where the export CSVs (and export_log.txt) are written
cdmName      <- "BIFAP"
minCellCount <- 5L
makeZip      <- TRUE     # create the results zip after export

# ---- Confirm the input exists -----------------------------------------------
esdOutput <- file.path(outputFolder, "final_pregnancy_episodes.rds")
if (!file.exists(esdOutput)) {
  stop("final_pregnancy_episodes.rds not found at: ", esdOutput)
}

# ---- Connect (fresh connection) ---------------------------------------------
con <- DBI::dbConnect(
  odbc::databricks(),
  httpPath = Sys.getenv("DATABRICKS_HTTPPATH"),
  useNativeQuery = FALSE
)
on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

cdm <- CDMConnector::cdmFromCon(
  con         = con,
  cdmSchema   = Sys.getenv("DATABRICKS_CDM_SCHEMA"),
  writeSchema = Sys.getenv("DATABRICKS_SCRATCH_SCHEMA"),
  cdmName     = cdmName
)

# ---- Export (logs every step itself; no upload) -----------------------------
PregnancyIdentifier::exportPregnancies(
  cdm          = cdm,
  outputFolder = outputFolder,
  exportFolder = exportFolder,
  minCellCount = minCellCount
)

if (isTRUE(makeZip)) {
  PregnancyIdentifier::zipExportFolder(exportFolder)
}

message("Export complete. CSVs + export_log.txt are in: ", exportFolder)
