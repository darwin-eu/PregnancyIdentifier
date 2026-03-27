# 04_build_combined_ip.R — Stack ip-results (characteristics, incidence, prevalence)
# across all latest runs into combined CSVs.

source("00_config.R")

if (!exists("manifest")) {
  cli_abort("Run 01_discover_runs.R first to build the manifest")
}

cli_h1("Building combined ip-results")

STUDY_DIR <- file.path(OUT_DIR, "study_results")
COMBINED_DIR <- file.path(OUT_DIR, "combined")
dir_create(COMBINED_DIR)

latest_runs <- manifest %>%
  filter(is_latest_run, has_ip_results)

cli_alert_info("Combining ip-results from {nrow(latest_runs)} databases")

for (ip_type in c("characteristics", "incidence", "prevalence")) {
  cli_h2("Stacking {ip_type}")

  combined <- map_dfr(seq_len(nrow(latest_runs)), function(i) {
    row <- latest_runs[i, ]
    path <- file.path(STUDY_DIR, row$db_key, row$run_date, paste0(ip_type, ".csv"))

    if (!file.exists(path)) {
      cli_alert_warning("  Missing: {row$db_key}/{row$run_date}/{ip_type}.csv")
      return(tibble())
    }

    cli_alert_info("  Reading {row$db_key} ({row$run_date})")
    df <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
    df <- df %>%
      mutate(
        db_key = row$db_key,
        run_date = row$run_date
      )
    df
  })

  out_path <- file.path(COMBINED_DIR, paste0("latest_", ip_type, ".csv"))
  write_csv(combined, out_path)
  cli_alert_success("  Wrote {nrow(combined)} rows to {basename(out_path)}")
}

cli_alert_success("Combined ip-results written to {COMBINED_DIR}")
