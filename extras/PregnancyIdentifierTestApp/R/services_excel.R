# services_excel.R
# Excel import/export: state_to_excel(), excel_to_state() matching canonical
# OMOP CDM column schemas; validation of sheet names and expected columns.

# Define canonical column schemas for each domain
# These MUST match the sample Excel file exactly
CANONICAL_COLUMNS <- list(
  person = c(
    "person_id", "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth",
    "birth_datetime", "race_concept_id", "ethnicity_concept_id", "location_id",
    "provider_id", "care_site_id", "person_source_value", "gender_source_value",
    "race_source_value", "ethnicity_source_value"
  ),
  observation_period = c(
    "observation_period_id", "person_id", "observation_period_start_date",
    "observation_period_end_date", "period_type_concept_id"
  ),
  visit_occurrence = c(
    "visit_occurrence_id", "person_id", "visit_concept_id", "visit_start_date",
    "visit_start_datetime", "visit_end_date", "visit_end_datetime",
    "visit_type_concept_id", "provider_id", "care_site_id", "visit_source_value",
    "visit_source_concept_id", "admitting_source_concept_id", "admitting_source_value",
    "discharge_to_concept_id", "discharge_to_source_value", "preceding_visit_occurrence_id"
  ),
  visit_detail = c(
    "visit_detail_id", "person_id", "visit_detail_concept_id", "visit_detail_start_date",
    "visit_detail_start_datetime", "visit_detail_end_date", "visit_detail_end_datetime",
    "visit_detail_type_concept_id", "provider_id", "care_site_id", "admitting_source_concept_id",
    "discharge_to_concept_id", "preceding_visit_detail_id", "visit_detail_source_value",
    "visit_detail_source_concept_id", "admitting_source_value", "discharge_to_source_value",
    "visit_detail_parent_id", "visit_occurrence_id"
  ),
  drug_exposure = c(
    "drug_exposure_id", "person_id", "drug_concept_id", "drug_exposure_start_date",
    "drug_exposure_start_datetime", "drug_exposure_end_date", "drug_exposure_end_datetime",
    "verbatim_end_date", "drug_type_concept_id", "stop_reason", "refills", "quantity",
    "days_supply", "sig", "route_concept_id", "lot_number", "provider_id",
    "visit_occurrence_id", "visit_detail_id", "drug_source_value", "drug_source_concept_id",
    "route_source_value", "dose_unit_source_value"
  ),
  condition_occurrence = c(
    "condition_occurrence_id", "person_id", "condition_concept_id", "condition_start_date",
    "condition_start_datetime", "condition_end_date", "condition_end_datetime",
    "condition_type_concept_id", "stop_reason", "provider_id", "visit_occurrence_id",
    "visit_detail_id", "condition_source_value", "condition_source_concept_id",
    "condition_status_source_value", "condition_status_concept_id"
  ),
  procedure_occurrence = c(
    "procedure_occurrence_id", "person_id", "procedure_concept_id", "procedure_date",
    "procedure_datetime", "procedure_type_concept_id", "modifier_concept_id", "quantity",
    "provider_id", "visit_occurrence_id", "visit_detail_id", "procedure_source_value",
    "procedure_source_concept_id", "modifier_source_value"
  ),
  measurement = c(
    "measurement_id", "person_id", "measurement_concept_id", "measurement_date",
    "measurement_datetime", "measurement_type_concept_id", "operator_concept_id",
    "value_as_number", "value_as_concept_id", "unit_concept_id", "range_low",
    "range_high", "provider_id", "visit_occurrence_id", "visit_detail_id",
    "measurement_source_value", "measurement_source_concept_id", "unit_source_value",
    "value_source_value"
  )
)

#' Convert state to Excel format (matching canonical OMOP CDM columns)
#'
#' @param state List: OMOP CDM state (domain -> list of records).
#' @param file_path Character(1); path to save Excel file (e.g. \code{"data.xlsx"}).
#' @return Invisible \code{file_path}.
#' @export
state_to_excel <- function(state, file_path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx package is required. Install with: install.packages('openxlsx')")
  }
  if (!is.list(state)) {
    stop("state must be a list, got ", paste(class(state), collapse = "/"))
  }
  if (!is.character(file_path) || length(file_path) != 1 || is.na(file_path) || nchar(file_path) == 0) {
    stop("file_path must be a non-empty character string")
  }

  # Create workbook
  wb <- openxlsx::createWorkbook()

  # Process each domain in canonical order
  domain_order <- c(
    "person", "observation_period", "visit_occurrence", "visit_detail",
    "drug_exposure", "condition_occurrence", "procedure_occurrence", "measurement"
  )

  for (domain in domain_order) {
    # Get canonical columns for this domain
    canonical_cols <- CANONICAL_COLUMNS[[domain]]
    if (is.null(canonical_cols)) {
      warning(sprintf("No canonical columns defined for domain '%s', skipping", domain))
      next
    }

    # Create sheet (always, even if empty)
    openxlsx::addWorksheet(wb, domain)

    # Get data for this domain
    domain_data <- state[[domain]]

    if (is.null(domain_data) || length(domain_data) == 0) {
      # Empty sheet with headers only
      df <- data.frame(matrix(ncol = length(canonical_cols), nrow = 0))
      names(df) <- canonical_cols
    } else {
      # Convert list of records to data frame
      # Get all unique field names
      all_fields <- unique(unlist(lapply(domain_data, names)))

      # Build data frame
      df_list <- lapply(canonical_cols, function(col) {
        values <- lapply(domain_data, function(rec) {
          val <- rec[[col]]
          if (is.null(val)) {
            return(NA)
          }
          # Convert dates to character if needed
          if (inherits(val, "Date")) {
            return(as.character(val))
          }
          if (inherits(val, "POSIXct")) {
            return(as.character(val))
          }
          return(val)
        })
        # Unlist to vector (handles mixed types by converting to character if needed)
        return(unlist(values, use.names = FALSE))
      })
      names(df_list) <- canonical_cols
      df <- as.data.frame(df_list, stringsAsFactors = FALSE)

      # Ensure all canonical columns exist (add NA if missing)
      for (col in canonical_cols) {
        if (!col %in% names(df)) {
          df[[col]] <- NA
        }
      }

      # Reorder columns to match canonical order
      df <- df[, canonical_cols, drop = FALSE]
    }

    # Write data
    openxlsx::writeData(wb, domain, df)

    # Auto-size columns
    if (ncol(df) > 0) {
      openxlsx::setColWidths(wb, domain, cols = 1:ncol(df), widths = "auto")
    }
  }

  # Save workbook
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  return(invisible(file_path))
}

#' Convert Excel file to state (matching canonical OMOP CDM columns)
#'
#' @param file_path Character(1); path to Excel file.
#' @return List: OMOP CDM state (domain -> list of records). Unknown sheets are skipped; validation warnings may be issued.
#' @export
excel_to_state <- function(file_path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx package is required. Install with: install.packages('openxlsx')")
  }
  if (!is.character(file_path) || length(file_path) != 1 || is.na(file_path)) {
    stop("file_path must be a length-1 character path")
  }
  if (!file.exists(file_path)) {
    stop(
      sprintf(
        "File not found: %s. Check that the path is correct and the file exists.",
        file_path
      )
    )
  }

  # Read workbook
  wb <- openxlsx::loadWorkbook(file_path)
  sheet_names <- openxlsx::getSheetNames(file_path)

  state <- list()

  # Known OMOP domains this app understands
  known_domains <- c(
    "person", "observation_period", "visit_occurrence",
    "visit_detail", "drug_exposure", "condition_occurrence",
    "procedure_occurrence", "measurement"
  )

  # Process each sheet
  for (sheet_name in sheet_names) {
    # Validate sheet name is a known domain
    if (!sheet_name %in% known_domains) {
      warning(
        sprintf(
          "Unknown domain '%s' in Excel file, skipping this sheet.",
          sheet_name
        ),
        call. = FALSE
      )
      next
    }

    # Read sheet
    df <- openxlsx::readWorkbook(wb, sheet = sheet_name)

    # Validate expected columns exist for known domains
    expected_cols <- CANONICAL_COLUMNS[[sheet_name]]
    if (!is.null(expected_cols)) {
      missing_cols <- setdiff(expected_cols, names(df))
      if (length(missing_cols) > 0) {
        warning(
          sprintf(
            "Domain '%s' is missing expected columns: %s",
            sheet_name,
            paste(missing_cols, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }

    if (nrow(df) == 0) {
      # Empty sheet
      state[[sheet_name]] <- list()
      next
    }

    # Convert data frame to list of records
    records <- lapply(1:nrow(df), function(i) {
      rec <- as.list(df[i, , drop = FALSE])
      # Remove NA values (but keep 0 and empty strings)
      rec <- lapply(rec, function(val) {
        if (is.na(val)) {
          return(NULL)
        }
        return(val)
      })
      # Convert date columns back to character strings
      for (col in names(rec)) {
        if (inherits(rec[[col]], "Date")) {
          rec[[col]] <- as.character(rec[[col]])
        } else if (inherits(rec[[col]], "POSIXct")) {
          rec[[col]] <- as.character(rec[[col]])
        }
      }
      return(rec)
    })

    state[[sheet_name]] <- records
  }

  # Validate state without stopping; surface any issues via warnings
  validation <- state_validate(state, stop_on_error = FALSE)
  if (!validation$valid) {
    warning(
      "State validation failed after Excel import:\n",
      paste(validation$errors, collapse = "\n"),
      call. = FALSE
    )
  }

  return(state)
}

#' Get canonical column order for a domain
#'
#' @param domain Character(1); domain name (e.g. \code{"person"}).
#' @return Character vector of column names in canonical order, or NULL if unknown domain.
#' @export
get_canonical_columns <- function(domain) {
  CANONICAL_COLUMNS[[domain]]
}
