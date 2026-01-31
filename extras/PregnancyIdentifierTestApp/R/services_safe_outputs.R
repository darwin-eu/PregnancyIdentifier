# services_safe_outputs.R
# Canonical expected outputs and schemas; ensure_outputs_exist(); detect_hip_zero().
# Derived from package R/: runPregnancyIdentifier.R, runHIP.R, runPPS.R, mergeHIPPS.R, runESD.R, exportPregnancies.R.

#' Expected output artifact names (canonical list from package code).
#'
#' @return List with rds, rds_debug, csv, export_dir, export_csv, export_zip.
#' @noRd
expected_outputs <- function() {
  list(
    rds = c(
      "hip_episodes.rds",
      "pps_episodes.rds",
      "hipps_episodes.rds",
      "final_pregnancy_episodes.rds"
    ),
    rds_debug = c(
      "pps_gest_timing_episodes.rds",
      "pps_min_max_episodes.rds",
      "esd.rds"
    ),
    csv = c("runStart.csv"),
    csv_optional = c("pps_concept_counts.csv"),
    export_dir = "export",
    export_csv = c(
      "cdm_source.csv",
      "age_summary.csv",
      "episode_frequency.csv",
      "pregnancy_frequency.csv",
      "gestational_age_days_summary.csv",
      "date_consistency.csv",
      "outcome_categories_count.csv"
    ),
    export_zip = character(0)
  )
}

#' Schema registry: column name -> type for 0-row safe data.frames.
#' Types: "integer", "numeric", "character", "Date", "logical".
#' @noRd
schema_registry <- function() {
  list(
    hip_episodes.rds = list(
      person_id = "integer",
      episode = "integer",
      estimated_start_date = "Date",
      visit_date = "Date",
      gest_date = "Date",
      category = "character"
    ),
    pps_episodes.rds = list(
      person_id = "integer",
      person_episode_number = "integer",
      episode_min_date = "Date",
      episode_max_date = "Date",
      episode_max_date_plus_two_months = "Date",
      algo2_category = "character",
      algo2_outcome_date = "Date"
    ),
    hipps_episodes.rds = list(
      person_id = "integer",
      episode_number = "integer",
      pregnancy_start = "Date",
      HIP_end_date = "Date",
      PPS_end_date = "Date",
      recorded_episode_start = "Date",
      recorded_episode_end = "Date",
      recorded_episode_length = "numeric",
      HIP_outcome_category = "character",
      PPS_outcome_category = "character",
      HIP_flag = "integer",
      PPS_flag = "integer"
    ),
    final_pregnancy_episodes.rds = list(
      person_id = "integer",
      episode_number = "integer",
      pregnancy_start = "Date",
      recorded_episode_start = "Date",
      recorded_episode_end = "Date",
      inferred_episode_start = "Date",
      inferred_episode_end = "Date",
      final_outcome_category = "character",
      precision_days = "integer",
      precision_category = "character",
      gestational_age_days_calculated = "integer"
    ),
    runStart.csv = list(start = "integer"),
    pps_gest_timing_episodes.rds = list(
      person_id = "integer",
      episode_number = "integer",
      inferred_episode_start = "Date",
      precision_days = "numeric",
      precision_category = "character"
    ),
    pps_min_max_episodes.rds = list(
      person_id = "integer",
      person_episode_number = "integer",
      episode_min_date = "Date",
      episode_max_date = "Date"
    ),
    esd.rds = list(
      person_id = "integer",
      episode_number = "integer",
      inferred_episode_start = "Date",
      precision_days = "numeric",
      precision_category = "character"
    )
  )
}

#' Build a 0-row data.frame from a schema (name -> type).
#' @param schema Named list of column names to type strings.
#' @return data.frame with 0 rows and correct column types.
#' @noRd
empty_from_schema <- function(schema) {
  if (length(schema) == 0) {
    return(data.frame())
  }
  cols <- lapply(names(schema), function(name) {
    type <- schema[[name]]
    switch(type,
      integer = integer(0),
      numeric = numeric(0),
      character = character(0),
      Date = structure(double(0), class = "Date"),
      logical = logical(0),
      integer(0)
    )
  })
  names(cols) <- names(schema)
  as.data.frame(cols, stringsAsFactors = FALSE)
}

#' Return a 0-row data.frame with same column names and types as template.
#' @param template_df A data.frame (can have 0 or more rows).
#' @return data.frame with 0 rows.
#' @noRd
empty_like <- function(template_df) {
  if (!is.data.frame(template_df) || nrow(template_df) == 0) {
    if (is.data.frame(template_df) && ncol(template_df) > 0) {
      return(template_df[integer(0), , drop = FALSE])
    }
    if (is.data.frame(template_df)) {
      return(template_df)
    }
    return(data.frame())
  }
  template_df[integer(0), , drop = FALSE]
}

#' Ensure all expected output files exist with correct schema (0-row if missing).
#' Creates outputDir and outputDir/export; writes safe empty RDS/CSV for any missing artifact.
#' Only creates debug RDS/ESD when debugMode is TRUE.
#' @param outputDir Character. Top-level output directory.
#' @param debugMode Logical. If TRUE, ensure debug RDS files exist.
#' @param log_append_cb Optional function(line) called to append a log line (e.g. for UI).
#' @param run_failed Logical. If TRUE, log that placeholders were created because the run failed.
#' @noRd
ensure_outputs_exist <- function(outputDir,
                                 debugMode = FALSE,
                                 log_append_cb = NULL,
                                 run_failed = FALSE) {
  if (is.null(outputDir) || is.na(outputDir) || outputDir == "") {
    return(invisible(NULL))
  }
  log_line <- function(msg) {
    if (is.function(log_append_cb)) {
      tryCatch(log_append_cb(msg), error = function(e) NULL)
    }
  }
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  export_dir <- file.path(outputDir, expected_outputs()$export_dir)
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  reg <- schema_registry()
  exp <- expected_outputs()

  if (run_failed) {
    log_line("[WARN] Run failed; creating placeholder outputs so browsers and exports do not error.")
  }

  for (rds_name in exp$rds) {
    path <- file.path(outputDir, rds_name)
    schema <- reg[[rds_name]]
    if (is.null(schema)) {
      next
    }
    if (file.exists(path)) {
      obj <- tryCatch(readRDS(path), error = function(e) NULL)
      if (is.null(obj) || !is.data.frame(obj)) {
        log_line(sprintf("[WARN] %s invalid or not a data.frame; rewriting safe empty.", rds_name))
        saveRDS(empty_from_schema(schema), path)
      }
    } else {
      log_line(sprintf("[INFO] Creating missing %s (0 rows).", rds_name))
      saveRDS(empty_from_schema(schema), path)
    }
  }

  if (isTRUE(debugMode)) {
    for (rds_name in exp$rds_debug) {
      path <- file.path(outputDir, rds_name)
      schema <- reg[[rds_name]]
      if (is.null(schema)) next
      if (file.exists(path)) {
        obj <- tryCatch(readRDS(path), error = function(e) NULL)
        if (is.null(obj) || !is.data.frame(obj)) {
          log_line(sprintf("[WARN] %s invalid; rewriting safe empty.", rds_name))
          saveRDS(empty_from_schema(schema), path)
        }
      } else {
        log_line(sprintf("[INFO] Creating missing debug %s (0 rows).", rds_name))
        saveRDS(empty_from_schema(schema), path)
      }
    }
  }

  for (csv_name in exp$csv) {
    path <- file.path(outputDir, csv_name)
    if (!file.exists(path)) {
      schema <- reg[[csv_name]]
      if (!is.null(schema)) {
        log_line(sprintf("[INFO] Creating missing %s (0 rows).", csv_name))
        df <- empty_from_schema(schema)
        utils::write.csv(df, path, row.names = FALSE)
      } else if (csv_name == "runStart.csv") {
        df <- data.frame(start = as.integer(Sys.time()))
        utils::write.csv(df, path, row.names = FALSE)
      }
    }
  }

  for (csv_name in exp$export_csv) {
    path <- file.path(export_dir, csv_name)
    if (!file.exists(path)) {
      log_line(sprintf("[INFO] Creating missing export %s (0 rows).", csv_name))
      if (csv_name == "cdm_source.csv") {
        df <- data.frame(
          cdm_name = character(0),
          snapshot_date = character(0),
          stringsAsFactors = FALSE
        )
      } else {
        df <- data.frame(placeholder = character(0))
      }
      utils::write.csv(df, path, row.names = FALSE)
    }
  }

  log_line("[INFO] ensure_outputs_exist finished.")
  invisible(NULL)
}

#' Detect number of HIP episodes (0 when no HIP episodes found).
#' @param outputDir Character. Top-level output directory.
#' @return Integer number of rows, or NA if file missing/cannot be read.
#' @noRd
detect_hip_zero <- function(outputDir) {
  if (is.null(outputDir) || is.na(outputDir) || outputDir == "") {
    return(NA_integer_)
  }
  path <- file.path(outputDir, "hip_episodes.rds")
  if (!file.exists(path)) {
    return(NA_integer_)
  }
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj)) {
    return(NA_integer_)
  }
  as.integer(nrow(obj))
}
