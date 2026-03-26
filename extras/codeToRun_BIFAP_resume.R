# codeToRun_BIFAP_resume.R
#
# Resume script for BIFAP: skips Steps 1–4 (initPregnancies, HIP, PPS, merge)
# and picks up from Step 5 (ESD) using the intermediate .rds files already
# written to outputFolder by the previous (failed) run.
#
# Prerequisites:
#   - The previous run completed Steps 1–4 so these files exist in outputFolder:
#       * hipps_episodes.rds
#       * runStart.csv
#       * (optionally) attrition.csv, hip_concept_counts.csv, pps_concept_counts.csv
#   - The CDM connection is still valid (same database, same schemas).

library(PregnancyIdentifier)

# ---- Connection parameters (BIFAP — fill these in) --------------------------
cdmSchema    <- "..."
writeSchema  <- c(catalog = "...", schema = "...", prefix = "...")
cdmName      <- "BIFAP"

# Folders from the original run (must point to the SAME directories)
outputFolder <- "..."
exportFolder <- "..."

minCellCount <- 5L

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

# ---- Connect to CDM ---------------------------------------------------------
con <- DBI::dbConnect(
  # Fill in your ODBC / JDBC driver details here, e.g.:
  # drv = odbc::odbc(),
  # dsn = "...",
  ...
)

cdm <- CDMConnector::cdmFromCon(
  con       = con,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  cdmName   = cdmName
)

# ---- Re-insert the concept lookup tables that ESD needs ---------------------
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
