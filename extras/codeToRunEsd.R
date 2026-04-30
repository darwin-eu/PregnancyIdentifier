# codeToRunEsd.R
#
# Resume script for BIFAP on Spark/Databricks: skips Steps 1–4
# (initPregnancies, HIP, PPS, merge) and picks up from Step 5 (ESD)
# using the intermediate .rds files already written to outputFolder.
#
# IMPORTANT: Before running this script, install the development version of
# CDMConnector which contains the Spark INSERT fix:
#
#   remotes::install_github("darwin-eu/CDMConnector", upgrade = "never")
#
# The fix uses batched INSERT INTO ... SELECT UNION ALL instead of row-by-row
# INSERT VALUES, which avoids the Spark ODBC HY000 driver reconnect error.
#
# Prerequisites:
#   - The previous run completed Steps 1–4 so these files exist in outputFolder:
#       * hipps_episodes.rds
#       * runStart.csv
#   - The development version of CDMConnector is installed (see above).

library(PregnancyIdentifier)

# ---- Connection parameters ---------------------------------------------------
cdmSchema    <- "omop_2024"
writeSchema  <- c(catalog = "hive_metastore", schema = "omop_2024_results")
cdmName      <- "BIFAP"

# Output Parameters
outputFolder  <- "E:/DARWIN-Studies/P4-C5-002 - Obj2  PREGNANCY/Study/P4-C5-002-main/P4-C5-002-main/results_patients"
exportFolder  <- "E:/DARWIN-Studies/P4-C5-002 - Obj2  PREGNANCY/Study/P4-C5-002-main/P4-C5-002-main/results"

minCellCount <- 5

# Study period for BIFAP
startDate <- as.Date("2010-01-01")
endDate   <- as.Date("2024-12-31")

# ---- Validate that the previous run's intermediate files exist ---------------
requiredFiles <- c("hipps_episodes.rds", "runStart.csv")
missing <- requiredFiles[!file.exists(file.path(outputFolder, requiredFiles))]
if (length(missing) > 0) {
  stop(
    "Cannot resume: the following files are missing from outputFolder:\n",
    paste(" -", missing, collapse = "\n"), "\n",
    "These are produced by Steps 1-4. You need a complete initial run up to the merge step."
  )
}

# ---- Verify CDMConnector has the Spark fix -----------------------------------
if (!exists(".dbWriteTableSafe", where = asNamespace("CDMConnector"))) {
  stop(
    "CDMConnector does not contain the Spark INSERT fix.\n",
    "Please install the development version:\n",
    "  remotes::install_github(\"darwin-eu/CDMConnector\", upgrade = \"never\")"
  )
}
message("CDMConnector Spark fix verified.")

# ---- Connect to Spark/Databricks --------------------------------------------
con <- DBI::dbConnect(odbc::databricks(),
                      Driver          = "Simba Spark ODBC Driver",
                      Host            = "adb-8258457304257519.19.azuredatabricks.net",
                      Port            = 443,
                      AuthMech        = 3,
                      HTTPPath        = "sql/protocolv1/o/8258457304257519/0122-121530-8os6ljem",
                      Protocol        = "https",
                      ThriftTransport = 2,
                      SSL             = 1,
                      UID             = "token",
                      PWD             = Sys.getenv("DATABRICKS_TOKEN"),
                      catalog         = "hive_metastore"
)

cdm <- CDMConnector::cdmFromCon(
  con         = con,
  cdmSchema   = cdmSchema,
  writeSchema = writeSchema,
  cdmName     = cdmName
)

# ---- Re-insert the concept lookup tables that ESD needs ----------------------
# ESD queries cdm$preg_pps_concepts (for gestational timing concept ranges).
# initPregnancies() normally creates these, but since we're skipping it we
# insert them here from the package's bundled Excel files.

message("Inserting pregnancy concept tables into the CDM...")

ppsConcepts <- suppressMessages(readxl::read_excel(
  system.file("concepts", "PPS_concepts_reviewed1702026.xlsx",
              package = "PregnancyIdentifier", mustWork = TRUE)
)) |>
  dplyr::rename_with(tolower) |>
  dplyr::mutate(pps_concept_id = suppressWarnings(as.integer(.data$pps_concept_id)))

cdm <- omopgenerics::insertTable(cdm, "preg_pps_concepts", ppsConcepts,
                                 overwrite = TRUE, temporary = FALSE)

message("Inserted preg_pps_concepts (", nrow(ppsConcepts), " rows)")

# ---- Set up logger -----------------------------------------------------------
logger <- PregnancyIdentifier::makeLogger(outputFolder)
pkgVersion <- as.character(utils::packageVersion("PregnancyIdentifier"))
log4r::info(logger, paste0("PregnancyIdentifier version: ", pkgVersion))
log4r::info(logger, paste0("CDMConnector version: ", utils::packageVersion("CDMConnector")))
log4r::info(logger, paste0("CDM name: ", cdmName))
log4r::info(logger, "RESUME RUN — skipping Steps 1-4, starting from ESD")

# ---- Step 5: ESD refinement -------------------------------------------------
log4r::info(logger, "Running ESD")
PregnancyIdentifier::runEsd(
  cdm          = cdm,
  outputFolder = outputFolder,
  startDate    = startDate,
  endDate      = endDate,
  logger       = logger,
  debugMode    = FALSE,
  conformToValidation = FALSE
)

# ---- Step 6: Incidence, prevalence & characteristics -------------------------
log4r::info(logger, "Running computeIncidencePrevalence")
PregnancyIdentifier:::computeIncidencePrevalence(
  cdm          = cdm,
  outputFolder = outputFolder,
  exportFolder = exportFolder,
  minCellCount = minCellCount,
  logger       = logger
)

# ---- Step 7: Export aggregated summaries -------------------------------------
log4r::info(logger, "Running exportPregnancies")
PregnancyIdentifier::exportPregnancies(
  cdm          = cdm,
  outputFolder = outputFolder,
  exportFolder = exportFolder,
  minCellCount = minCellCount
)

# ---- Create results ZIP ------------------------------------------------------
PregnancyIdentifier::zipExportFolder(exportFolder)

# ---- Cleanup -----------------------------------------------------------------
log4r::info(logger, "Resume run complete")
DBI::dbDisconnect(con)

message("Done! Results are in: ", exportFolder)
