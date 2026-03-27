# 00_unzip_all.R — Rebuild raw_results from the master zip file
#
# Treats pregnancy_dtz_export.zip as the single source of truth.
# 1. Wipes and recreates raw_results/
# 2. Extracts the master zip
# 3. Recursively extracts all nested zips, removing each zip after extraction
# 4. Repeats until no zips remain
#
# Result: a fully-extracted directory tree with only CSV/TXT/log files (no zips).
# Idempotent: safe to re-run at any time.

library(fs)
library(cli)

PROJECT_DIR <- here::here()
MASTER_ZIP  <- path(PROJECT_DIR, "pregnancy_dtz_export.zip")
RAW_DIR     <- path(PROJECT_DIR, "raw_results")

stopifnot(file_exists(MASTER_ZIP))

# ─── 1. Clean slate ─────────────────────────────────────────────────────────

cli_h1("Rebuilding raw_results from {path_file(MASTER_ZIP)}")

if (dir_exists(RAW_DIR)) {
  cli_alert_warning("Removing existing raw_results/")
  unlink(RAW_DIR, recursive = TRUE, force = TRUE)
}
dir_create(RAW_DIR)

# ─── 2. Extract master zip ──────────────────────────────────────────────────

cli_alert_info("Extracting master zip...")
utils::unzip(MASTER_ZIP, exdir = RAW_DIR, overwrite = TRUE)

# The master zip puts everything under P4-C5-002/ — check and flatten if needed
p4_dir <- path(RAW_DIR, "P4-C5-002")
if (dir_exists(p4_dir)) {
  cli_alert_info("Moving contents out of P4-C5-002/ wrapper")
  children <- dir_ls(p4_dir)
  for (child in children) {
    dest <- path(RAW_DIR, path_file(child))
    if (dir_exists(dest)) {
      # Merge: move contents into existing dir
      sub_children <- dir_ls(child)
      for (sc in sub_children) {
        file_move(sc, path(dest, path_file(sc)))
      }
    } else {
      file_move(child, dest)
    }
  }
  dir_delete(p4_dir)
}

cli_alert_success("Master zip extracted")

# ─── 3. Recursively extract all nested zips ──────────────────────────────────

total_extracted <- 0L
pass <- 0L

repeat {
  pass <- pass + 1L

  zip_files <- dir_ls(RAW_DIR, glob = "*.zip", recurse = TRUE, type = "file")

  if (length(zip_files) == 0) break

  extracted_this_pass <- 0L

  for (zf in zip_files) {
    # Extract into a sibling directory named after the zip
    dest <- path_ext_remove(zf)
    dir_create(dest)

    result <- tryCatch(
      utils::unzip(zf, exdir = dest, overwrite = TRUE),
      error = function(e) {
        cli_alert_warning("Failed: {path_file(zf)} — {e$message}")
        character(0)
      }
    )

    if (length(result) > 0) {
      extracted_this_pass <- extracted_this_pass + 1L
    }

    # Remove the zip after extraction
    file_delete(zf)
  }

  total_extracted <- total_extracted + extracted_this_pass
  cli_alert_info("Pass {pass}: extracted and removed {extracted_this_pass} zip(s)")
}

# ─── 4. Summary ─────────────────────────────────────────────────────────────

remaining_zips <- dir_ls(RAW_DIR, glob = "*.zip", recurse = TRUE, type = "file")
n_csvs <- length(dir_ls(RAW_DIR, glob = "*.csv", recurse = TRUE, type = "file"))
n_dirs <- length(dir_ls(RAW_DIR, type = "directory", recurse = TRUE))

cli_h1("Extraction Complete")
cli_alert_success("Extracted {total_extracted} zip(s) across {pass} pass(es)")
cli_alert_info("{n_csvs} CSV files across {n_dirs} directories")
if (length(remaining_zips) > 0) {
  cli_alert_danger("{length(remaining_zips)} zip(s) could not be extracted!")
} else {
  cli_alert_success("No zip files remaining — fully extracted")
}
