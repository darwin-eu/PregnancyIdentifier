# 02_copy_and_normalize.R — Copy files from raw_results to clean processed structure
# Reads the manifest (with list columns) from 01_discover_runs.R and copies files
# into processed_results/study_results/{db_key}/{run_date}/

source("00_config.R")

# Check that manifest exists in memory (from 01_discover_runs.R)
if (!exists("manifest")) {
  cli_abort("Run 01_discover_runs.R first to build the manifest")
}

cli_h1("Copying and normalizing results")

STUDY_DIR <- file.path(OUT_DIR, "study_results")

# Clean slate — remove previous output (but preserve manifest.csv)
if (dir.exists(STUDY_DIR)) {
  cli_alert_warning("Removing existing {STUDY_DIR}")
  dir_delete(STUDY_DIR)
}
dir_create(STUDY_DIR)

# Track all copy operations
copy_log <- tibble(
  db_key = character(),
  run_date = character(),
  source_path = character(),
  dest_path = character(),
  canonical_name = character(),
  file_type = character()
)

for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, ]
  db_key <- row$db_key
  run_date <- row$run_date
  run_out_dir <- file.path(STUDY_DIR, db_key, run_date)
  dir_create(run_out_dir)

  cli_alert_info("{db_key} / {run_date}")

  # --- Copy ip-result files (strip date/db prefix) ---
  ip_files <- row$ip_file_paths[[1]]
  for (ip_file in ip_files) {
    result_type <- extract_result_type(ip_file)
    if (is.na(result_type)) next
    dest <- file.path(run_out_dir, paste0(result_type, ".csv"))
    file_copy(ip_file, dest, overwrite = TRUE)
    copy_log <- add_row(copy_log,
      db_key = db_key, run_date = run_date,
      source_path = ip_file, dest_path = dest,
      canonical_name = paste0(result_type, ".csv"),
      file_type = "ip_result"
    )
  }

  # --- Copy study-result files (apply renames) ---
  study_map <- row$study_file_map[[1]]
  for (j in seq_along(study_map)) {
    canonical_name <- names(study_map)[j]
    source_path <- study_map[j]
    dest <- file.path(run_out_dir, canonical_name)
    file_copy(source_path, dest, overwrite = TRUE)
    copy_log <- add_row(copy_log,
      db_key = db_key, run_date = run_date,
      source_path = source_path, dest_path = dest,
      canonical_name = canonical_name,
      file_type = "study_result"
    )
  }

  # --- Write _run_metadata.csv ---
  metadata <- tibble(
    db_key = db_key,
    db_folder = row$db_folder,
    run_date = run_date,
    cdm_name = row$cdm_name,
    pkg_version = row$pkg_version,
    n_ip_files = row$n_ip_files,
    n_study_files = row$n_study_files,
    has_ip_results = row$has_ip_results,
    has_study_results = row$has_study_results,
    is_latest_run = row$is_latest_run,
    source_dir = row$run_dir
  )
  write_csv(metadata, file.path(run_out_dir, "_run_metadata.csv"))
}

# --- Write copy log ---
write_csv(copy_log, file.path(OUT_DIR, "copy_log.csv"))

cli_h1("Copy Summary")
cli_alert_success("Copied {nrow(copy_log)} files across {nrow(manifest)} runs")
cli_alert_success("Copy log written to {file.path(OUT_DIR, 'copy_log.csv')}")

# Summary by db
copy_log %>%
  count(db_key, file_type) %>%
  pivot_wider(names_from = file_type, values_from = n, values_fill = 0) %>%
  print(n = Inf)
