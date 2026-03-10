#' Append PregnancyIdentifier version to cdm_name in result CSVs (idempotent)
#'
#' For each result folder in data/, reads log.txt, extracts the exact package
#' version, then for each CSV in that folder updates cdm_name (or database)
#' by appending the version unless it already ends with a version number.
#' Run this script from the inst/shiny directory.
#'
#' Usage: source("extras/append_version_to_cdm_name.R"); append_version_to_cdm_name()

# Regex to extract version from log line: "INFO  [2026-03-03 17:55:04] PregnancyIdentifier version: 3.0.5"
VERSION_LINE_PATTERN <- "PregnancyIdentifier version:\\s*([0-9]+\\.[0-9]+(?:\\.[0-9]+)?)"

# Regex to detect if cdm_name already ends with a version number (e.g. "3.0.5" or "3.1.1")
ALREADY_VERSIONED_PATTERN <- "[0-9]+\\.[0-9]+(?:\\.[0-9]+)?\\s*$"

append_version_to_cdm_name <- function(data_dir = "data") {
  data_path <- data_dir
  if (!dir.exists(data_path)) {
    stop("Data directory not found: ", data_path)
  }

  result_folders <- list.dirs(data_path, full.names = TRUE, recursive = FALSE)
  if (length(result_folders) == 0) {
    message("No result folders found in ", data_path)
    return(invisible(NULL))
  }

  for (result_folder in result_folders) {
    log_file <- file.path(result_folder, "log.txt")
    if (!file.exists(log_file)) {
      message("Skipping ", basename(result_folder), ": no log.txt")
      next
    }

    log_lines <- readLines(log_file, warn = FALSE)
    version_line <- grep("PregnancyIdentifier version:", log_lines, value = TRUE)
    if (length(version_line) == 0) {
      message("Skipping ", basename(result_folder), ": no version line in log.txt")
      next
    }

    version <- regmatches(version_line[1], regexec(VERSION_LINE_PATTERN, version_line[1]))
    if (length(version) == 0 || length(version[[1]]) < 2) {
      message("Skipping ", basename(result_folder), ": could not parse version from log")
      next
    }
    version <- version[[1]][2]

    csv_files <- list.files(result_folder, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0) {
      message("No CSV files in ", basename(result_folder))
      next
    }

    for (csv_path in csv_files) {
      tryCatch({
        d <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
        if (nrow(d) == 0) next

        col_to_update <- NULL
        if ("cdm_name" %in% names(d)) {
          col_to_update <- "cdm_name"
        } else if ("database" %in% names(d)) {
          col_to_update <- "database"
        }

        if (is.null(col_to_update)) next

        vals <- d[[col_to_update]]
        already_versioned <- grepl(ALREADY_VERSIONED_PATTERN, vals, perl = TRUE)
        to_append <- !already_versioned
        if (!any(to_append)) next

        new_vals <- ifelse(to_append, paste(vals, version), vals)
        d[[col_to_update]] <- new_vals
        utils::write.csv(d, csv_path, row.names = FALSE)
        message("Updated ", col_to_update, " in ", basename(csv_path), " (version ", version, ")")
      }, error = function(e) {
        warning("Error processing ", csv_path, ": ", conditionMessage(e))
      })
    }
  }

  invisible(NULL)
}

append_version_to_cdm_name()
