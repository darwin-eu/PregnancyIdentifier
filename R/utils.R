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

  cdm$pregnancy_extension <- cdm$pregnancy_extension %>%
    dplyr::mutate(
      pregnancy_start_date = as.Date(.data$pregnancy_start_date),
      pregnancy_end_date = as.Date(.data$pregnancy_end_date)
    ) %>%
    dplyr::compute(name = "pregnancy_extension", temporary = FALSE, overwrite = TRUE)

  return(cdm)
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

# Add age and sex to a cdm table
# `tbl` is a cdm table, (e.g. cdm$pregnancy)
# `dateColumn` is a character that matches a Date column in the cdm table
addAgeSex <- function(tbl, dateColumn) {
  checkmate::assertClass(tbl, "cdm_table")
  checkmate::assertCharacter(dateColumn, len = 1, any.missing = FALSE)
  checkmate::assertTRUE("person_id" %in% colnames(tbl))
  checkmate::assertTRUE(dateColumn %in% colnames(tbl))
  cdm_local <- attr(tbl, "cdm_reference")
  checkmate::assertClass(cdm_local, "cdm_reference")

  person <- cdm_local$person %>%
    dplyr::select("person_id", "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth")

  tbl %>%
    dplyr::inner_join(person, by = "person_id") %>%
    dplyr::mutate(
      day_of_birth   = dplyr::coalesce(.data$day_of_birth, 1L),
      month_of_birth = dplyr::coalesce(.data$month_of_birth, 1L)
    ) %>%
    dplyr::mutate(
      date_of_birth  = as.Date(paste0(.data$year_of_birth, "-", .data$month_of_birth, "-", .data$day_of_birth))
    ) %>%
    dplyr::mutate(
      age = !!CDMConnector::datediff("date_of_birth", dateColumn) / 365.25,
      sex = dplyr::case_when(
        gender_concept_id == 8532L ~ "female",
        gender_concept_id == 8507L ~ "male",
        TRUE ~ "unknown")
    ) %>%
    dplyr::select(-c("day_of_birth", "month_of_birth", "year_of_birth", "gender_concept_id", "date_of_birth"))
}


# helpful print function for debugging
printLong <- function(x) {
  df <- x %>%
    dplyr::collect() %>%
    dplyr::mutate_all(as.character)

  out <- cbind(
    column = colnames(df),
    as.data.frame(t(df), stringsAsFactors = FALSE)
  )
  colnames(out)[-1] <- paste0("row", seq_len(ncol(out) - 1))
  print(dplyr::tibble(out), n = 1000)
}

