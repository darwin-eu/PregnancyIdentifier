# 01_discover_runs.R — Scan raw_results and build the manifest
#
# Strategy: recursively find all *_characteristics.csv files under each database's
# search directories, group by run_date, and collect sibling study-result CSVs.
# Since all zips have been recursively extracted, everything is on disk.

source("00_config.R")

cli_h1("Discovering runs in raw_results")

# --- Helpers ---

#' Build a study_file_map from a directory containing study CSVs
build_study_map <- function(dir_path) {
  all_files <- list.files(dir_path, full.names = TRUE)
  csvs <- all_files[str_detect(basename(all_files), "\\.(csv|txt)$")]
  csvs <- csvs[!str_detect(basename(csvs), IP_RESULT_REGEX)]
  csvs <- csvs[!map_lgl(csvs, is_cohort_diag_file)]
  study_csvs <- csvs[map_lgl(csvs, is_study_file)]
  if (length(study_csvs) == 0) return(set_names(character(0), character(0)))
  set_names(as.character(study_csvs), map_chr(study_csvs, normalize_filename))
}

#' Given a directory with ip-result files, find the best sibling/parent directory
#' that has study-result CSVs. Searches: same dir, parent dir, sibling dirs.
find_study_dir <- function(ip_dir) {
  # 1. Same directory
  study_map <- build_study_map(ip_dir)
  if (length(study_map) > 0) return(list(dir = ip_dir, map = study_map))

  # 2. Parent directory
  parent <- dirname(ip_dir)
  study_map <- build_study_map(parent)
  if (length(study_map) > 0) return(list(dir = parent, map = study_map))

  # 3. Sibling directories (e.g., ip-results in one subdir, study-results in another)
  siblings <- list.dirs(parent, recursive = FALSE, full.names = TRUE)
  siblings <- setdiff(siblings, ip_dir)
  for (sib in siblings) {
    study_map <- build_study_map(sib)
    if (length(study_map) > 0) return(list(dir = sib, map = study_map))
  }

  # 4. Nothing found
  list(dir = ip_dir, map = set_names(character(0), character(0)))
}

#' Discover all runs under a search directory by recursively finding characteristics files
discover_runs_in_dir <- function(db_folder, db_key, search_dir) {
  base_path <- file.path(RAW_DIR, db_folder)
  dir_path <- if (search_dir == ".") base_path else file.path(base_path, search_dir)

  if (!dir.exists(dir_path)) {
    cli_alert_warning("Directory not found: {dir_path}")
    return(tibble())
  }

  # Recursively find ALL characteristics files
  all_csvs <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  char_files <- all_csvs[str_detect(basename(all_csvs), IP_RESULT_REGEX) &
                          str_detect(basename(all_csvs), "characteristics")]

  if (length(char_files) == 0) {
    # Check for study-only directories (e.g., CPRD GOLD Main-Obj2/export/)
    study_map <- build_study_map(dir_path)
    if (length(study_map) > 0) {
      cdm_path <- file.path(dir_path, "cdm_source.csv")
      cdm_name <- extract_cdm_name(cdm_path)
      log_files <- list.files(dir_path, pattern = "^(log\\.txt|run\\.log)$", full.names = TRUE)
      log_path <- if (length(log_files) > 0) log_files[1] else ""
      run_date <- NA_character_
      if (file.exists(cdm_path)) {
        df <- read_csv(cdm_path, show_col_types = FALSE, n_max = 1)
        if ("snapshot_date" %in% names(df) && !is.na(df$snapshot_date[1]))
          run_date <- as.character(df$snapshot_date[1])
      }
      if (is.na(run_date) && log_path != "") {
        lines <- readLines(log_path, n = 1, warn = FALSE)
        date_match <- str_extract(lines[1], "\\d{4}-\\d{2}-\\d{2}")
        if (!is.na(date_match)) run_date <- date_match
      }
      if (is.na(run_date)) run_date <- "unknown"
      ver <- resolve_version(dir_path, log_path, run_date)
      return(tibble(
        db_folder = db_folder, db_key = db_key, run_date = run_date,
        run_dir = dir_path, cdm_name = cdm_name,
        pkg_version = ver$pkg_version,
        major_version = ver$major_version,
        version_source = ver$version_source,
        n_study_files = length(study_map), n_ip_files = 0L,
        has_ip_results = FALSE, has_study_results = TRUE,
        ip_file_paths = list(character(0)), study_file_map = list(study_map),
        notes = "study-only directory"
      ))
    }
    return(tibble())
  }

  # Extract run_date and parent dir for each characteristics file
  char_info <- tibble(
    path = char_files,
    dir = dirname(char_files),
    run_date = map_chr(char_files, extract_run_date)
  )

  # Deduplicate: for the same run_date, prefer the shallowest path (closest to search root)
  # This avoids counting the same run extracted in multiple nested locations
  char_info <- char_info %>%
    mutate(depth = str_count(dir, "/")) %>%
    group_by(run_date) %>%
    slice_min(depth, n = 1, with_ties = FALSE) %>%
    ungroup()

  # Build a row for each unique run found via ip-results
  ip_runs <- map_dfr(seq_len(nrow(char_info)), function(i) {
    row <- char_info[i, ]
    ip_dir <- row$dir

    # Find all 3 ip-result files in the same directory
    ip_files_here <- list.files(ip_dir, pattern = "\\.csv$", full.names = TRUE)
    ip_files_here <- ip_files_here[str_detect(basename(ip_files_here), IP_RESULT_REGEX)]

    # Find study files (same dir, parent, or sibling)
    study_result <- find_study_dir(ip_dir)
    study_map <- study_result$map
    study_source <- study_result$dir

    # Read metadata
    cdm_path <- file.path(study_source, "cdm_source.csv")
    cdm_name <- extract_cdm_name(cdm_path)
    log_files <- list.files(study_source, pattern = "^(log\\.txt|run\\.log)$", full.names = TRUE)
    log_path <- if (length(log_files) > 0) log_files[1] else ""

    # Resolve version: folder name > log > date inference
    ver <- resolve_version(ip_dir, log_path, row$run_date)

    tibble(
      db_folder = db_folder, db_key = db_key, run_date = row$run_date,
      run_dir = study_source, cdm_name = cdm_name,
      pkg_version = ver$pkg_version,
      major_version = ver$major_version,
      version_source = ver$version_source,
      n_study_files = as.integer(length(study_map)),
      n_ip_files = length(ip_files_here),
      has_ip_results = TRUE,
      has_study_results = length(study_map) > 0,
      ip_file_paths = list(as.character(ip_files_here)),
      study_file_map = list(study_map),
      notes = ""
    )
  })

  # Also find study-only directories (have episode_frequency.csv but no ip-results)
  # This catches runs like BIFAP 2026-01-22 that only have study CSVs
  ep_freq_files <- all_csvs[basename(all_csvs) == "episode_frequency.csv"]
  ep_freq_dirs <- unique(dirname(ep_freq_files))
  # Exclude dirs already covered by ip-result runs
  ip_run_dirs <- unique(c(char_info$dir, dirname(char_info$dir)))
  study_only_dirs <- setdiff(ep_freq_dirs, ip_run_dirs)
  # Also exclude dirs whose study files are already claimed by an ip-run
  if (nrow(ip_runs) > 0) {
    claimed_dirs <- unique(ip_runs$run_dir)
    study_only_dirs <- setdiff(study_only_dirs, claimed_dirs)
  }

  study_only_runs <- map_dfr(study_only_dirs, function(sdir) {
    study_map <- build_study_map(sdir)
    if (length(study_map) == 0) return(tibble())

    cdm_path <- file.path(sdir, "cdm_source.csv")
    cdm_name <- extract_cdm_name(cdm_path)
    log_files <- list.files(sdir, pattern = "^(log\\.txt|run\\.log)$", full.names = TRUE)
    log_path <- if (length(log_files) > 0) log_files[1] else ""
    # Infer run date from log or cdm_source
    run_date <- NA_character_
    if (file.exists(cdm_path)) {
      df <- read_csv(cdm_path, show_col_types = FALSE, n_max = 1)
      if ("snapshot_date" %in% names(df) && !is.na(df$snapshot_date[1]))
        run_date <- as.character(df$snapshot_date[1])
    }
    if (is.na(run_date) && log_path != "") {
      lines <- readLines(log_path, n = 1, warn = FALSE)
      date_match <- str_extract(lines[1], "\\d{4}-\\d{2}-\\d{2}")
      if (!is.na(date_match)) run_date <- date_match
    }
    # Try to extract date from directory name
    if (is.na(run_date)) {
      dir_date <- str_extract(basename(sdir), "\\d{4}-\\d{2}-\\d{2}")
      if (!is.na(dir_date)) run_date <- dir_date
    }
    if (is.na(run_date)) run_date <- "unknown"

    ver <- resolve_version(sdir, log_path, run_date)

    tibble(
      db_folder = db_folder, db_key = db_key, run_date = run_date,
      run_dir = sdir, cdm_name = cdm_name,
      pkg_version = ver$pkg_version,
      major_version = ver$major_version,
      version_source = ver$version_source,
      n_study_files = as.integer(length(study_map)), n_ip_files = 0L,
      has_ip_results = FALSE, has_study_results = TRUE,
      ip_file_paths = list(character(0)), study_file_map = list(study_map),
      notes = "study-only (no ip-results)"
    )
  })

  bind_rows(ip_runs, study_only_runs)
}

# --- Run discovery across all databases ---

manifest <- db_search_config %>%
  pmap_dfr(function(db_folder, db_key, search_dirs) {
    cli_h2("Scanning {db_key}")
    runs <- map_dfr(search_dirs, function(sd) {
      cli_alert_info("  Searching: {sd}")
      discover_runs_in_dir(db_folder, db_key, sd)
    })

    # Deduplicate across search_dirs: same run_date should appear only once
    if (nrow(runs) > 1) {
      runs <- runs %>%
        group_by(run_date) %>%
        # Prefer the row with more study files
        slice_max(n_study_files, n = 1, with_ties = FALSE) %>%
        ungroup()
    }

    if (nrow(runs) == 0) {
      cli_alert_warning("  No runs found for {db_key}!")
    } else {
      cli_alert_success("  Found {nrow(runs)} run(s), {sum(runs$has_study_results)} with study data")
    }
    runs
  })

# --- Mark latest run per db_key ---

manifest <- manifest %>%
  group_by(db_key) %>%
  mutate(is_latest_run = run_date == max(run_date)) %>%
  ungroup() %>%
  arrange(db_key, run_date)

# --- Write manifest ---

manifest_flat <- manifest %>%
  mutate(
    ip_file_paths_str = map_chr(ip_file_paths, ~ paste(.x, collapse = "|")),
    study_files_str = map_chr(study_file_map, ~ paste(names(.x), collapse = "|"))
  ) %>%
  select(-ip_file_paths, -study_file_map)

dir_create(OUT_DIR)
write_csv(manifest_flat, file.path(OUT_DIR, "manifest.csv"))

cli_h1("Manifest Summary")
cli_alert_success("Total runs discovered: {nrow(manifest)}")
cli_alert_info("Databases: {n_distinct(manifest$db_key)}")
cli_alert_info("Runs with ip-results: {sum(manifest$has_ip_results)}")
cli_alert_info("Runs with study results: {sum(manifest$has_study_results)}")
cli_alert_info("Latest runs: {sum(manifest$is_latest_run)}")

manifest %>%
  group_by(db_key) %>%
  summarise(
    n_runs = n(),
    n_with_study = sum(has_study_results),
    latest_date = max(run_date),
    versions = paste(sort(unique(na.omit(major_version))), collapse = ", "),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# --- Version coverage summary ---
cli_h2("Version coverage")
version_coverage <- manifest %>%
  filter(!is.na(major_version)) %>%
  distinct(db_key, major_version) %>%
  mutate(present = TRUE) %>%
  pivot_wider(names_from = major_version, values_from = present, values_fill = FALSE) %>%
  arrange(db_key)

missing_versions <- manifest %>%
  filter(!is.na(major_version)) %>%
  distinct(db_key, major_version) %>%
  group_by(db_key) %>%
  summarise(versions = list(major_version), .groups = "drop") %>%
  mutate(
    missing = map_chr(versions, function(v) {
      m <- setdiff(c("v1", "v2", "v3"), v)
      if (length(m) == 0) "-" else paste(m, collapse = ", ")
    })
  ) %>%
  filter(missing != "-")

if (nrow(missing_versions) > 0) {
  cli_alert_warning("Databases with missing versions:")
  walk2(missing_versions$db_key, missing_versions$missing, function(db, m) {
    cli_alert_info("  {db}: missing {m}")
  })
} else {
  cli_alert_success("All databases have v1, v2, and v3 runs!")
}

cli_alert_success("Manifest written to {file.path(OUT_DIR, 'manifest.csv')}")
