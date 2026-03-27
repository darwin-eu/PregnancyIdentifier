# 07_build_version_sets.R — Create unified result sets per major version
#
# For each major version (v1, v2), selects the latest run per database and
# copies study results + IP results into a single folder per version.
# Output structure:
#   processed_results/version_sets/v1/<db_key>/{study files + ip files}
#   processed_results/version_sets/v2/<db_key>/{study files + ip files}

source("00_config.R")

STUDY_DIR <- file.path(OUT_DIR, "study_results")
VERSION_SET_DIR <- file.path(OUT_DIR, "version_sets")

# ─── 1. Load manifest and select latest run per (db_key, major_version) ──────

manifest <- read_csv(file.path(OUT_DIR, "manifest.csv"), show_col_types = FALSE)

# For each database × major_version, pick the latest run_date
latest_runs <- manifest %>%
  filter(major_version %in% c("v1", "v2")) %>%
  group_by(db_key, major_version) %>%
  slice_max(run_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(major_version, db_key)

cli_h1("Building unified version sets")

for (ver in c("v1", "v2")) {
  ver_runs <- latest_runs %>% filter(major_version == ver)
  cli_h2("{ver}: {nrow(ver_runs)} databases")

  if (nrow(ver_runs) == 0) {
    cli_alert_warning("No runs found for {ver}, skipping")
    next
  }

  ver_dir <- file.path(VERSION_SET_DIR, ver)
  if (dir_exists(ver_dir)) dir_delete(ver_dir)
  dir_create(ver_dir)

  for (i in seq_len(nrow(ver_runs))) {
    row <- ver_runs[i, ]
    db_key <- row$db_key
    run_date <- as.character(row$run_date)

    # Source: study results
    study_src <- file.path(STUDY_DIR, db_key, run_date)
    # Destination
    db_dest <- file.path(ver_dir, db_key)
    dir_create(db_dest)

    # Copy study results (skip _run_metadata.csv)
    if (dir_exists(study_src)) {
      study_files <- dir_ls(study_src, type = "file")
      study_files <- study_files[!str_detect(basename(study_files), "^_")]
      file_copy(study_files, file.path(db_dest, basename(study_files)))
      cli_alert_success("{db_key}: copied {length(study_files)} study files from {run_date}")
    } else {
      cli_alert_warning("{db_key}: no study results for {run_date}")
    }

    # IP files (characteristics, incidence, prevalence) are already included
    # in the study results folder from the normalization step
  }
}

# ─── 2. Summary ──────────────────────────────────────────────────────────────

cli_h2("Summary")
for (ver in c("v1", "v2")) {
  ver_dir <- file.path(VERSION_SET_DIR, ver)
  if (dir_exists(ver_dir)) {
    dbs <- dir_ls(ver_dir, type = "directory")
    cli_alert_success("{ver}: {length(dbs)} databases — {paste(basename(dbs), collapse = ', ')}")
  }
}

# Write a small manifest for the version sets
version_set_manifest <- latest_runs %>%
  select(major_version, db_key, run_date, pkg_version, version_source, cdm_name)

write_csv(version_set_manifest, file.path(VERSION_SET_DIR, "version_set_manifest.csv"))
cli_alert_success("Wrote {file.path(VERSION_SET_DIR, 'version_set_manifest.csv')}")
