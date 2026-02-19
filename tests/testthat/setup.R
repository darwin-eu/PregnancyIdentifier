# Helper run before tests (testthat loads helper.R automatically).
# Ensure TestGenerator uses cached Eunomia data when EUNOMIA_DATA_FOLDER is in .Renviron
# (e.g. R CMD check / some test runners don't load .Renviron, so we set it here).
if (!nzchar(Sys.getenv("EUNOMIA_DATA_FOLDER")) || !dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) {
  renv_path <- path.expand("~/.Renviron")
  if (file.exists(renv_path)) {
    renv_content <- readLines(renv_path, warn = FALSE)
    for (line in renv_content) {
      if (grepl("^EUNOMIA_DATA_FOLDER\\s*=", line)) {
        value <- sub("^EUNOMIA_DATA_FOLDER\\s*=", "", line)
        value <- trimws(value)
        value <- gsub("^['\"]|['\"]$", "", value)
        if (nzchar(value) && dir.exists(value)) {
          Sys.setenv(EUNOMIA_DATA_FOLDER = value)
        }
        break
      }
    }
  }
}

# Clean up duckdb file after disconnecting a mock CDM.

#' Disconnect from a CDM and remove its duckdb file if file-based.
#'
#' Gets the db file path from the connection (CDMConnector::cdmCon(cdm)),
#' disconnects, then unlinks the file so tests do not leave duckdb files behind.
#'
#' @param cdm A cdm_reference object (e.g. from mockPregnancyCdm() or TestGenerator::patientsCDM()).
#' @return Invisible NULL.
cleanupCdmDb <- function(cdm) {
  con <- CDMConnector::cdmCon(cdm)
  dbfile <- DBI::dbGetInfo(con)$dbname
  CDMConnector::cdmDisconnect(cdm)
  if (nzchar(dbfile) && file.exists(dbfile)) {
    unlink(dbfile)
  }
  invisible(NULL)
}
