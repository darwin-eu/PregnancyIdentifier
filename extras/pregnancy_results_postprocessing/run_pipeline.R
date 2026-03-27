# run_pipeline.R — Master script to run the full normalization pipeline
# Usage: source("run_pipeline.R") or Rscript run_pipeline.R

cli::cli_h1("Pregnancy Results Post-Processing Pipeline")
cli::cli_alert_info("Start time: {Sys.time()}")

source("00_unzip_all.R")
source("01_discover_runs.R")
source("02_copy_and_normalize.R")
source("03_validate.R")
source("04_build_combined_ip.R")
source("05_build_combined_study.R")
source("07_build_version_sets.R")

cli::cli_h1("Pipeline Complete")
cli::cli_alert_success("End time: {Sys.time()}")
cli::cli_alert_info("Output directory: {OUT_DIR}")
