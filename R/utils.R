getTblRowCount <- function(tbl) {
  tbl %>%
    dplyr::ungroup() %>%
    dplyr::count() %>%
    dplyr::pull(.data$n)
}

#' Validate episode periods: overlaps within person and max duration
#'
#' Checks a dataframe of episode periods (start/end dates per person) for two
#' conditions and logs warnings: (1) number of records that overlap with another
#' record within the same person; (2) number of records whose span (end - start
#' in days) exceeds a maximum (default 308 days).
#'
#' @param df A dataframe with at least person id and start/end date columns.
#' @param personIdCol Character. Name of the person identifier column.
#' @param startDateCol Character. Name of the period start date column.
#' @param endDateCol Character. Name of the period end date column.
#' @param logger A log4r logger object (e.g. from `makeLogger()`).
#' @param maxDays Numeric. Threshold in days for the second warning (default 308).
#' @return Invisibly returns `df` unchanged.
validateEpisodePeriods <- function(df,
                                   personIdCol,
                                   startDateCol,
                                   endDateCol,
                                   logger,
                                   maxDays = 308) {
  checkmate::assertDataFrame(df, min.rows = 0)
  checkmate::assertString(personIdCol)
  checkmate::assertString(startDateCol)
  checkmate::assertString(endDateCol)
  checkmate::assertClass(logger, "logger", null.ok = FALSE)
  checkmate::assertNumber(maxDays, lower = 0, finite = TRUE)
  stopifnot(
    personIdCol %in% names(df),
    startDateCol %in% names(df),
    endDateCol %in% names(df)
  )

  df <- dplyr::ungroup(df)
  complete <- df %>%
    dplyr::filter(
      !is.na(.data[[startDateCol]]) & !is.na(.data[[endDateCol]])
    )
  if (nrow(complete) == 0) {
    return(invisible(df))
  }

  # 1) Overlaps within person: sweep-line O(k) per person instead of O(k^2) self-join
  # Sort by person, start, end; track prior max end and min start after to detect overlaps
  complete <- complete %>%
    dplyr::arrange(.data[[personIdCol]], .data[[startDateCol]], .data[[endDateCol]]) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(personIdCol))) %>%
    dplyr::mutate(
      .end_num = as.numeric(as.Date(.data[[endDateCol]])),
      .start_num = as.numeric(as.Date(.data[[startDateCol]])),
      .prior_max_end = dplyr::lag(cummax(.data$.end_num), default = NA_real_),
      .min_start_after = dplyr::lead(rev(cummin(rev(.data$.start_num))), default = NA_real_)
    ) %>%
    dplyr::ungroup()
  # Row overlaps a previous interval iff start < prior_max_end; overlapped by a later iff min_start_after < end
  overlapping <- (!is.na(complete$.prior_max_end) & complete$.start_num < complete$.prior_max_end) |
    (!is.na(complete$.min_start_after) & complete$.min_start_after < complete$.end_num)
  nOverlapping <- sum(overlapping, na.rm = TRUE)
  if (nOverlapping > 0) {
    log4r::warn(
      logger,
      sprintf(
        "validateEpisodePeriods: %d record(s) overlap with another record within the same person.",
        nOverlapping
      )
    )
  } else {
    log4r::info(logger, "validation: no overlapping episodes found")
  }

  # 2) Span > maxDays
  daysSpan <- as.numeric(
    as.Date(complete[[endDateCol]]) - as.Date(complete[[startDateCol]])
  )
  nOverMax <- sum(daysSpan > maxDays, na.rm = TRUE)
  if (nOverMax > 0) {
    log4r::warn(
      logger,
      sprintf(
        "validateEpisodePeriods: %d record(s) have period length greater than %d days.",
        nOverMax,
        maxDays
      )
    )
  } else {
    log4r::info(
      logger,
      sprintf("validation: all episodes are less than %d days", maxDays)
    )
  }

  invisible(df)
}

#' Conform episode periods: remove overlaps and too-long episodes
#'
#' Optionally applied after \code{validateEpisodePeriods}. Within each person,
#' drops records that overlap an earlier episode (keeps earliest-start-first
#' non-overlapping set) and drops records whose span exceeds \code{maxDays}.
#' Used when the caller wants to modify output to satisfy validation rules
#' rather than only log issues.
#'
#' @param df A dataframe with at least person id and start/end date columns.
#' @param personIdCol Character. Name of the person identifier column.
#' @param startDateCol Character. Name of the period start date column.
#' @param endDateCol Character. Name of the period end date column.
#' @param maxDays Numeric. Maximum allowed span in days (default 308). Records with span > maxDays are dropped.
#' @return Data frame with overlapping and too-long records removed. Rows with NA in start or end are retained unchanged.
#' @noRd
conformEpisodePeriods <- function(df,
                                  personIdCol,
                                  startDateCol,
                                  endDateCol,
                                  maxDays = 308) {
  checkmate::assertDataFrame(df, min.rows = 0)
  checkmate::assertString(personIdCol)
  checkmate::assertString(startDateCol)
  checkmate::assertString(endDateCol)
  checkmate::assertNumber(maxDays, lower = 0, finite = TRUE)
  stopifnot(
    personIdCol %in% names(df),
    startDateCol %in% names(df),
    endDateCol %in% names(df)
  )
  df <- dplyr::ungroup(df)
  incomplete <- df %>%
    dplyr::filter(
      is.na(.data[[startDateCol]]) | is.na(.data[[endDateCol]])
    )
  complete <- df %>%
    dplyr::filter(
      !is.na(.data[[startDateCol]]) & !is.na(.data[[endDateCol]])
    )
  if (nrow(complete) == 0) {
    return(dplyr::bind_rows(complete, incomplete))
  }
  # Within person: sort by start; keep row i iff start_i >= last_kept_end (greedy non-overlapping)
  complete <- complete %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(personIdCol))) %>%
    dplyr::arrange(.data[[startDateCol]], .by_group = TRUE) %>%
    dplyr::mutate(
      .prev_end = dplyr::lag(.data[[endDateCol]], default = as.Date("1900-01-01") - 1L),
      .cum_prev_end = cummax(as.numeric(.data$.prev_end))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(as.numeric(.data[[startDateCol]]) >= .data$.cum_prev_end) %>%
    dplyr::select(-dplyr::any_of(c(".prev_end", ".cum_prev_end")))
  # Drop records with span > maxDays
  complete <- complete %>%
    dplyr::filter(
      as.numeric(as.Date(.data[[endDateCol]]) - as.Date(.data[[startDateCol]])) <= maxDays
    )
  dplyr::bind_rows(complete, incomplete)
}

cdmTableExists <- function(cdm, tableName) {
  DBI::dbExistsTable(conn = attr(cdm, "dbcon"), name = tableName)
}

#' Create a mock pregnancy cdm for examples and testing
#'
#' @param fullVocab If `TRUE` (default), the CDM includes the full vocabulary
#'   tables. If `FALSE`, concept, concept_relationship, concept_ancestor, and
#'   concept_synonym are subset to only concept IDs that appear in the data
#'   (from [CDMConnector::cdmFlatten()] \code{observation_concept_id}), for
#'   lighter testing on different database systems.
#' @returns A cdm reference with some example pregnancy data in it
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockPregnancyCdm()
#' cdm_small_vocab <- mockPregnancyCdm(fullVocab = FALSE)
#' }
mockPregnancyCdm <- function(fullVocab = TRUE) {
  suppressMessages({
    cdm <- TestGenerator::patientsCDM(
      pathJson = system.file("testCases", package = "PregnancyIdentifier"),
      testName = 'TestData_P4_C5_002_1',
      cdmVersion = "5.4",
      cdmName = "TestData_P4_C5_002_1"
    )
  })

  cdm$pregnancy_extension <- cdm$pregnancy_extension %>%
    dplyr::mutate(
      pregnancy_start_date = as.Date(.data$pregnancy_start_date),
      pregnancy_end_date = as.Date(.data$pregnancy_end_date)
    ) %>%
    dplyr::compute(name = "pregnancy_extension", temporary = FALSE, overwrite = TRUE)

  if (!fullVocab) {
    used_ids <- CDMConnector::cdmFlatten(cdm) %>%
      dplyr::select("observation_concept_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull("observation_concept_id")
    used_ids <- as.integer(used_ids[!is.na(used_ids)])

    if (length(used_ids) > 0L) {
      if ("concept" %in% names(cdm)) {
        cdm$concept <- cdm$concept %>%
          dplyr::filter(.data$concept_id %in% .env$used_ids) %>%
          dplyr::compute(name = "concept")
      }
      if ("concept_relationship" %in% names(cdm)) {
        cdm$concept_relationship <- cdm$concept_relationship %>%
          dplyr::filter(
            .data$concept_id_1 %in% .env$used_ids |
            .data$concept_id_2 %in% .env$used_ids
          ) %>%
          dplyr::compute(name = "concept_relationship")
      }
      if ("concept_ancestor" %in% names(cdm)) {
        cdm$concept_ancestor <- cdm$concept_ancestor %>%
          dplyr::filter(
            .data$ancestor_concept_id %in% .env$used_ids |
            .data$descendant_concept_id %in% .env$used_ids
          ) %>%
          dplyr::compute(name = "concept_ancestor")
      }
      if ("concept_synonym" %in% names(cdm)) {
        cdm$concept_synonym <- cdm$concept_synonym %>%
          dplyr::filter(.data$concept_id %in% .env$used_ids) %>%
          dplyr::compute(name = "concept_synonym")
      }
      if ("drug_strength" %in% names(cdm)) {
        cdm$drug_strength <- cdm$drug_strength %>%
          dplyr::filter(.data$drug_concept_id %in% .env$used_ids) %>%
          dplyr::compute(name = "drug_strength")
      }
      if ("relationship" %in% names(cdm)) {
        cdm$relationship <- cdm$relationship %>%
          dplyr::semi_join(cdm$concept_relationship, by = "relationship_id") %>%
          dplyr::compute(name = "relationship")
      }

      cdm_src$relationship
    }
  }

  return(cdm)
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

