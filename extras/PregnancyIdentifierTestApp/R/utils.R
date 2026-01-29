# utils.R
# Shared utility functions used across modules (dates, logging, copy, error results).

#' Null coalescing operator
#'
#' @param x First value (any type).
#' @param y Fallback value when \code{x} is NULL.
#' @return \code{x} if not NULL, otherwise \code{y}.
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Compare two IDs robustly (handles integer/numeric/character after import)
#'
#' @param a First ID (person_id, etc.).
#' @param b Second ID.
#' @return TRUE if both coerce to the same integer, FALSE otherwise.
#' @export
same_id <- function(a, b) {
  if (is.null(a) || is.null(b)) return(FALSE)
  ai <- suppressWarnings(as.integer(a))
  bi <- suppressWarnings(as.integer(b))
  !is.na(ai) && !is.na(bi) && ai == bi
}

#' Deep copy a list/object
#'
#' Uses base R serialization to preserve R-specific types (Date, factor,
#' attributes, classes) without the lossy conversion that happens with JSON.
#'
#' @param x Object to copy (any type).
#' @return Deep copy of \code{x} (list, etc.).
#' @export
deep_copy <- function(x) {
  unserialize(serialize(x, NULL))
}

#' Safely coerce values to integer
#'
#' Performs integer coercion with a warning when values cannot be represented
#' as 32-bit integers (e.g., > .Machine$integer.max) or when non-numeric values
#' are present.
#'
#' @param x Vector to coerce (numeric or character).
#' @param field_name Character(1); name of the field (used in warning messages).
#' @return Integer vector (may contain NA where coercion failed).
#' @export
safe_as_integer <- function(x, field_name = "value") {
  result <- suppressWarnings(as.integer(x))
  bad <- is.na(result) & !is.na(x)
  if (any(bad)) {
    warning(
      sprintf(
        "%s contains values that could not be safely coerced to integer (e.g., > %d or non-numeric).",
        field_name,
        .Machine$integer.max
      ),
      call. = FALSE
    )
  }
  result
}

#' Parse a date safely
#'
#' Tries to parse a variety of date-like inputs while avoiding hard failures.
#'
#' @param x Value to parse: Date, character(1), or NULL.
#' @param default Default value to return when parsing fails.
#' @return Date object, or \code{default} if parsing fails or \code{x} is NULL.
#' @export
parse_date_safe <- function(x, default = NULL) {
  if (is.null(x)) {
    return(default)
  }
  if (inherits(x, "Date")) {
    return(x)
  }
  tryCatch(
    {
      as.Date(x)
    },
    error = function(e) {
      warning("Failed to parse date value: ", x, call. = FALSE)
      default
    }
  )
}

#' Format date as OMOP string (YYYY-MM-DD)
#'
#' @param x Date or date-like value (Date, character(1), or NULL).
#' @return Character(1) date in YYYY-MM-DD format, or NULL if input is NULL.
#' @export
format_date_omop <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  as.character(as.Date(x))
}

#' Extract date of birth from person record
#'
#' @param person_rec Person record (list with birth_datetime or year_of_birth, etc.).
#' @return Date object or NULL if not available.
#' @export
person_dob <- function(person_rec) {
  if (!is.null(person_rec$birth_datetime)) {
    return(parse_date_safe(person_rec$birth_datetime))
  }
  if (!is.null(person_rec$year_of_birth)) {
    y <- person_rec$year_of_birth
    m <- person_rec$month_of_birth %||% 1L
    d <- person_rec$day_of_birth %||% 1L
    return(parse_date_safe(sprintf("%d-%02d-%02d", y, m, d)))
  }
  NULL
}

#' Debug logging function (only logs if app.debug option is TRUE)
#'
#' @param ... Arguments to pass to cat()
#' @return NULL (invisibly)
#' @export
debug_log <- function(...) {
  if (isTRUE(getOption("app.debug"))) {
    cat(...)
  }
  invisible(NULL)
}

#' Create a standardized error result
#'
#' @param message Human-readable error message
#' @param code Short error code (e.g., 'request_failed')
#' @param details Optional detailed data (string, list, etc.)
#' @param ... Additional fields to include in the result
#' @return List with \code{success = FALSE}, \code{error}, \code{message}, \code{details}, and any \code{...} fields.
#' @export
make_error_result <- function(message, code = "error", details = NULL, ...) {
  c(
    list(
      success = FALSE,
      error = code,
      message = message,
      details = details
    ),
    list(...)
  )
}

#' Create a standardized success result
#'
#' @param data Primary payload
#' @param message Human-readable success message
#' @param ... Additional fields to include in the result
#' @return List with \code{success = TRUE}, \code{message}, \code{data}, and any \code{...} fields.
#' @export
make_success_result <- function(data = NULL, message = "Success", ...) {
  c(
    list(
      success = TRUE,
      message = message,
      data = data
    ),
    list(...)
  )
}
