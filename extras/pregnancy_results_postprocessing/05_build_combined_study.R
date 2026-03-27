# 05_build_combined_study.R — Stack study-result CSVs across all latest runs
# For each canonical file, reads from all databases where it exists and row-binds.

source("00_config.R")

if (!exists("manifest")) {
  cli_abort("Run 01_discover_runs.R first to build the manifest")
}

cli_h1("Building combined study results")

STUDY_DIR <- file.path(OUT_DIR, "study_results")
COMBINED_DIR <- file.path(OUT_DIR, "combined")
dir_create(COMBINED_DIR)

latest_runs <- manifest %>%
  filter(is_latest_run, has_study_results)

cli_alert_info("Combining study results from {nrow(latest_runs)} databases")

# Combine each canonical file across databases
files_to_combine <- c(CANONICAL_FILES, OPTIONAL_FILES)
# Exclude log.txt — not a data file
files_to_combine <- setdiff(files_to_combine, "log.txt")

for (filename in files_to_combine) {
  paths <- map_chr(seq_len(nrow(latest_runs)), function(i) {
    row <- latest_runs[i, ]
    file.path(STUDY_DIR, row$db_key, row$run_date, filename)
  })
  existing <- paths[file.exists(paths)]

  if (length(existing) == 0) next

  cli_alert_info("Combining {filename} ({length(existing)} databases)")

  combined <- map_dfr(existing, function(path) {
    # Extract db_key and run_date from path
    parts <- str_split(path, "/")[[1]]
    n <- length(parts)
    run_date <- parts[n - 1]
    db_key <- parts[n - 2]

    df <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
    df %>% mutate(db_key = db_key, run_date = run_date)
  })

  out_path <- file.path(COMBINED_DIR, paste0("latest_", filename))
  write_csv(combined, out_path)
  cli_alert_success("  {filename}: {nrow(combined)} rows from {length(existing)} DBs")
}

cli_alert_success("Combined study results written to {COMBINED_DIR}")
