# Helper run before tests (testthat loads helper.R automatically).
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
