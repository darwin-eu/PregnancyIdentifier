getTblRowCount <- function(tbl) {
  tbl %>%
    dplyr::count() %>%
    dplyr::pull(.data$n)
}

cdmTableExists <- function(cdm, tableName) {
  DBI::dbExistsTable(conn = attr(cdm, "dbcon"), name = tableName)
}


#' Create a mock pregnancy cdm for examples and testing
#'
#' @returns A cdm reference with some example pregnancy data in it
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockPregnancyCdm()
#' }
mockPregnancyCdm <- function() {
  suppressMessages({
    cdm <- TestGenerator::patientsCDM(
      pathJson = system.file("testCases", package = "PregnancyIdentifier"),
      testName = 'TestData_P4_C5_002_1',
      cdmVersion = "5.3",
      cdmName = "TestData_P4_C5_002_1"
    )
  })
}


# Log helpers that make having a logger optional
logInfo <- function(logger, x) {
  if (!is.null(logger)) {
    log4r::info(logger, x)
  } else {
    rlang::inform(x)
  }
}

logWarn <- function(logger, x) {
  if (!is.null(logger)) {
    log4r::warn(logger, x)
  } else {
    rlang::warn(x)
  }
}

