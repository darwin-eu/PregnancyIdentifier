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
#' Convert a data.frame to an aligned, copy-pasteable R comment block
#'
#' @param df A data.frame / tibble
#' @return Invisibly returns df (printed as comments)
dfToComment <- function(df) {
  df_chr <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)

  widths <- vapply(
    names(df_chr),
    function(col) max(nchar(c(col, df_chr[[col]])), na.rm = TRUE),
    integer(1)
  )

  pad <- function(x, w) sprintf(paste0("%-", w, "s"), x)

  header <- paste(mapply(pad, names(df_chr), widths), collapse = " | ")
  cat("# ", header, "\n", sep = "")

  for (i in seq_len(nrow(df_chr))) {
    row <- paste(mapply(pad, df_chr[i, ], widths), collapse = " | ")
    cat("# ", row, "\n", sep = "")
  }

  invisible(df)
}

#' Flatten a CDM, optionally filter by person_id, and print as an R comment block
#'
#' @param cdm A CDM reference object
#' @param personIds Optional numeric vector of person_id values to filter on (default NULL = no filter)
#' @return Invisibly returns the collected data.frame (also printed as comments)
cdmFlatPrint <- function(cdm, personIds = NULL) {

  flat <- CDMConnector::cdmFlatten(cdm) %>%
    dplyr::collect(flat)

  if (!is.null(personIds)) {
    if (!is.numeric(personIds)) {
      stop("`personIds` must be a numeric vector (or NULL).")
    }
    flat <- dplyr::filter(flat, .data$person_id %in% personIds)
  }

  flat <- dplyr::arrange(
    flat,
    .data$person_id,
    dplyr::desc(.data$start_date),
    dplyr::desc(.data$end_date)
  )

  dfToComment(flat)
  invisible(flat)
}

#' Insert flattened CDM records as an aligned R comment in the active editor
#'
#' Flattens a CDM using [CDMConnector::cdmFlatten()], collects the data into R,
#' optionally filters by one or more `person_id` values, and inserts an aligned,
#' copy-pasteable comment block directly below the current cursor line in the
#' active RStudio document.
#'
#' This is intended as a lightweight debugging and documentation helper when
#' inspecting patient-level timelines (e.g. cohort inclusion, outcome validation,
#' or study review notes).
#'
#' @param cdm A CDM reference object created with CDMConnector.
#' @param personIds Optional numeric vector of `person_id` values to filter on.
#'   If `NULL`, all persons in the flattened CDM are used (use with care).
#'
#' @return Invisibly returns the collected flattened CDM as a data.frame.
#'   The primary side effect is insertion of commented text into the active
#'   RStudio source editor.
#'
#' @details
#' The inserted output is formatted as aligned R comments:
#'
#' \preformatted{
#' # person_id | observation_concept_id | start_date | end_date   | domain
#' # 12        | 2211751                 | 2021-01-13 | 2021-01-13 | procedure_occurrence
#' }
#'
#' The function requires an interactive RStudio session and will error if
#' `rstudioapi` is not available.
#'
#' @seealso
#' [CDMConnector::cdmFlatten()]
#'
#' @examples
#' \dontrun{
#' # Insert patient timeline directly into your script
#' cdmCommentContents(cdm, 12)
#'
#' # Insert multiple patients
#' cdmCommentContents(cdm, c(12, 22))
#' }
#'
#' @export
cdmCommentContents <- function(cdm, personIds = NULL) {
  # This function only makes sense in interactive use.
  if (!interactive()) return(invisible(NULL))

  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("Package 'rstudioapi' is required.", call. = FALSE)
  }
  if (!rstudioapi::isAvailable()) {
    stop("RStudio is required (rstudioapi is not available in this session).", call. = FALSE)
  }

  if (!is.null(personIds) && !is.numeric(personIds)) {
    stop("`personIds` must be a numeric vector (or NULL).", call. = FALSE)
  }

  # 1) Run the pipeline (cdmFlatten -> collect -> optional filter -> arrange)
  flat <- CDMConnector::cdmFlatten(cdm) |>
    dplyr::collect()

  if (!is.null(personIds)) {
    flat <- dplyr::filter(flat, .data$person_id %in% personIds)
  }

  flat <- dplyr::arrange(
    flat,
    .data$person_id,
    dplyr::desc(.data$start_date),
    dplyr::desc(.data$end_date)
  )

  # 2) Convert to aligned comment text (like your first format)
  df_chr <- as.data.frame(lapply(flat, as.character), stringsAsFactors = FALSE)

  widths <- vapply(
    names(df_chr),
    function(col) max(nchar(c(col, df_chr[[col]])), na.rm = TRUE),
    integer(1)
  )

  pad <- function(x, w) sprintf(paste0("%-", w, "s"), x)

  lines <- character()
  header <- paste(mapply(pad, names(df_chr), widths), collapse = " | ")
  lines <- c(lines, paste0("# ", header))

  for (i in seq_len(nrow(df_chr))) {
    row <- paste(mapply(pad, df_chr[i, ], widths), collapse = " | ")
    lines <- c(lines, paste0("# ", row))
  }

  # 3) Insert directly below the current line in the active RStudio document
  ctx <- rstudioapi::getActiveDocumentContext()
  contents <- ctx$contents

  # RStudio rows are 1-based
  cursor_row <- ctx$selection[[1]]$range$start[["row"]]

  # Find the most recent call line above (or at) the cursor
  call_row <- cursor_row
  while (call_row >= 1 && !grepl("\\bcdmCommentContents\\s*\\(", contents[[call_row]])) {
    call_row <- call_row - 1
  }

  if (call_row < 1) {
    stop("Couldn't find a line containing `cdmCommentContents(` above the cursor.", call. = FALSE)
  }

  # Insert *immediately below* the call line
  rstudioapi::insertText(
    location = rstudioapi::document_position(call_row + 1, 1),
    text = paste0(paste(lines, collapse = "\n"), "\n")
  )

  invisible(flat)
}

