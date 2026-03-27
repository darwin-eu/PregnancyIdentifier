# 03_validate.R — Validate the processed results
# Checks file presence, schema consistency, and flags anomalies.

source("00_config.R")

if (!exists("manifest")) {
  cli_abort("Run 01_discover_runs.R first to build the manifest")
}

cli_h1("Validating processed results")

STUDY_DIR <- file.path(OUT_DIR, "study_results")
IP_EXPECTED_COLS <- c("result_id", "cdm_name", "group_name", "group_level",
                       "strata_name", "strata_level", "variable_name",
                       "variable_level", "estimate_name", "estimate_type",
                       "estimate_value", "additional_name", "additional_level")

checks <- tibble(
  db_key = character(),
  run_date = character(),
  check = character(),
  status = character(),
  detail = character()
)

add_check <- function(checks, db_key, run_date, check, status, detail = "") {
  add_row(checks, db_key = db_key, run_date = run_date,
          check = check, status = status, detail = detail)
}

for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, ]
  db_key <- row$db_key
  run_date <- row$run_date
  run_dir <- file.path(STUDY_DIR, db_key, run_date)

  # Check 1: ip-result files exist and have correct schema
  if (row$has_ip_results) {
    for (ip_type in c("characteristics", "incidence", "prevalence")) {
      ip_path <- file.path(run_dir, paste0(ip_type, ".csv"))
      if (!file.exists(ip_path)) {
        checks <- add_check(checks, db_key, run_date,
                            paste0("ip_file_exists_", ip_type), "FAIL",
                            "File missing")
      } else if (file.size(ip_path) == 0) {
        checks <- add_check(checks, db_key, run_date,
                            paste0("ip_file_exists_", ip_type), "FAIL",
                            "File empty")
      } else {
        # Check schema
        header <- names(read_csv(ip_path, n_max = 0, show_col_types = FALSE))
        if (identical(header, IP_EXPECTED_COLS)) {
          checks <- add_check(checks, db_key, run_date,
                              paste0("ip_schema_", ip_type), "PASS")
        } else {
          missing <- setdiff(IP_EXPECTED_COLS, header)
          extra <- setdiff(header, IP_EXPECTED_COLS)
          checks <- add_check(checks, db_key, run_date,
                              paste0("ip_schema_", ip_type), "WARN",
                              paste0("missing: ", paste(missing, collapse = ","),
                                     " extra: ", paste(extra, collapse = ",")))
        }
      }
    }
  }

  # Check 2: cdm_name consistency
  if (row$has_study_results) {
    cdm_path <- file.path(run_dir, "cdm_source.csv")
    if (file.exists(cdm_path)) {
      cdm_df <- read_csv(cdm_path, show_col_types = FALSE, n_max = 1)
      cdm_name_source <- if ("cdm_name" %in% names(cdm_df)) cdm_df$cdm_name[1] else NA
      checks <- add_check(checks, db_key, run_date, "cdm_source_exists", "PASS",
                          paste0("cdm_name=", cdm_name_source))
    } else {
      checks <- add_check(checks, db_key, run_date, "cdm_source_exists", "WARN",
                          "cdm_source.csv missing")
    }
  }

  # Check 3: expected file count for latest runs
  if (row$is_latest_run && row$has_study_results) {
    present_files <- list.files(run_dir, pattern = "\\.(csv|txt)$")
    present_files <- setdiff(present_files, "_run_metadata.csv")
    canonical_present <- intersect(present_files, c(CANONICAL_FILES, OPTIONAL_FILES,
                                                     "characteristics.csv", "incidence.csv", "prevalence.csv"))
    n_expected <- length(CANONICAL_FILES) + 3  # 33 study + 3 ip
    if (length(canonical_present) >= n_expected - 5) {
      checks <- add_check(checks, db_key, run_date, "file_completeness", "PASS",
                          paste0(length(canonical_present), "/", n_expected, " canonical files"))
    } else {
      missing <- setdiff(c(CANONICAL_FILES, "characteristics.csv", "incidence.csv", "prevalence.csv"),
                         present_files)
      checks <- add_check(checks, db_key, run_date, "file_completeness", "WARN",
                          paste0(length(canonical_present), "/", n_expected,
                                 " missing: ", paste(head(missing, 5), collapse = ", ")))
    }
  }
}

# --- Write report ---
write_csv(checks, file.path(OUT_DIR, "validation_report.csv"))

cli_h1("Validation Summary")
checks %>%
  count(status) %>%
  walk2(.$status, .$n, ~ {
    if (.x == "PASS") cli_alert_success("{.y} checks passed")
    else if (.x == "WARN") cli_alert_warning("{.y} warnings")
    else cli_alert_danger("{.y} failures")
  })

# Show any non-PASS checks
issues <- filter(checks, status != "PASS")
if (nrow(issues) > 0) {
  cli_h2("Issues")
  issues %>% select(db_key, run_date, check, status, detail) %>% print(n = Inf)
}

cli_alert_success("Validation report written to {file.path(OUT_DIR, 'validation_report.csv')}")
