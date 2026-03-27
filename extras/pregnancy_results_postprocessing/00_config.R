# 00_config.R — Shared constants, helpers, and database search configuration
# This file is sourced by all other pipeline scripts.

library(tidyverse)
library(fs)
library(cli)

# --- Paths ---
PROJECT_DIR <- here::here()
RAW_DIR <- file.path(PROJECT_DIR, "raw_results")
OUT_DIR <- file.path(PROJECT_DIR, "processed_results")

# --- IP-result filename pattern ---
# Matches: 2026_03_19_NLHR@UIO_characteristics.csv
IP_RESULT_REGEX <- "^(\\d{4})_(\\d{2})_(\\d{2})_(.+)_(characteristics|incidence|prevalence)\\.csv$"

# --- Canonical study-result file list (target format, Mar 2026) ---
CANONICAL_FILES <- c(
  "age_summary.csv",
  "age_summary_first_pregnancy.csv",
  "age_summary_first_pregnancy_end.csv",
  "age_summary_groups.csv",
  "attrition.csv",
  "attrition_if_cleanup.csv",
  "cdm_source.csv",
  "concept_check.csv",
  "delivery_mode_by_year.csv",
  "delivery_mode_summary.csv",
  "episode_frequency.csv",
  "episode_frequency_summary.csv",
  "esd_concept_counts.csv",
  "gestational_age_days_counts.csv",
  "gestational_age_days_per_category_summary.csv",
  "gestational_age_days_summary.csv",
  "gestational_weeks.csv",
  "hip_concept_counts.csv",
  "log.txt",
  "missing_dates.csv",
  "monthly_trend_missing.csv",
  "monthly_trends.csv",
  "observation_period_range.csv",
  "outcome_categories_count.csv",
  "pps_concept_counts.csv",
  "precision_days.csv",
  "precision_days_denominators.csv",
  "pregnancy_frequency.csv",
  "pregnancy_overlap_counts.csv",
  "quality_check_cleanup.csv",
  "swapped_dates.csv",
  "yearly_trend.csv",
  "yearly_trend_missing.csv"
)

# Optional files that some databases produce
OPTIONAL_FILES <- c(
  "pet_comparison_summarised_result.csv",
  "pet_unmatched_lsc.csv"
)

# --- File rename map (old name -> canonical name) ---
RENAME_MAP <- c(
  "date_consistancy.csv" = "missing_dates.csv",
  "PPS-concept_counts.csv" = "pps_concept_counts.csv"
)

# Also accept run.log as log.txt
LOG_ALIASES <- c("run.log" = "log.txt")

# --- CohortDiagnostics files to exclude ---
COHORT_DIAG_FILES <- c(
  "cohort.csv", "cohort_count.csv", "cohort_inclusion.csv",
  "cohort_inc_result.csv", "cohort_inc_stats.csv",
  "cohort_relationships.csv", "cohort_summary_stats.csv",
  "concept.csv", "concept_ancestor.csv", "concept_relationship.csv",
  "concept_sets.csv", "concept_synonym.csv",
  "covariate_ref.csv", "covariate_value.csv",
  "database.csv", "domain.csv",
  "executionTimes.csv",
  "incidence_rate.csv", "index_event_breakdown.csv",
  "metadata.csv", "relationship.csv",
  "resolved_concepts.csv",
  "temporal_covariate_ref.csv", "temporal_covariate_value.csv",
  "time_distribution.csv", "vocabulary.csv",
  "visit_context.csv", "included_source_concept.csv",
  "orphan_concept.csv"
)

# --- Database search configuration ---
# Each row maps a database folder to a cleaned key and the subdirectories to search.
# The discovery script will look for *_characteristics.csv in these dirs.
db_search_config <- tribble(
  ~db_folder,                          ~db_key,              ~search_dirs,
  "BIFAP - P4-C5-002",                "BIFAP",              list(c(".")),
  "CDW Bordeaux - P4-C5-002",         "CDW_Bordeaux",       list(c(".")),
  "CPRD GOLD - P4-C5-002",            "CPRD_GOLD",          list(c(".")),
  "DK-DHR - P4-C5-002",               "DK_DHR",             list(c(".")),
  "EMDB-ULSGE",                        "EMDB_ULSGE",        list(c(".")),
  "FinOMOP-THL - P4-C5-002",          "FinOMOP_THL",        list(c(".")),
  "FinOMOP-TaUH Pirha - P4-C5-002",   "FinOMOP_TaUH",      list(c(".")),
  "HI-SPEED - P4-C5-002",             "HI_SPEED",           list(c(".")),
  "IMASIS - P4-C5-002",               "IMASIS",             list(c(".")),
  "InGef RDB - P4-C5-002",            "InGef_RDB",          list(c(".")),
  "IPCI - P4-C5-002",                 "IPCI",               list(c(".")),
  "NAJS - P4-C5-002",                 "NAJS",               list(c(".")),
  "NLHR - P4-C5-002",                 "NLHR",               list(c(".")),
  "SIDIAP - P4-C5-002",               "SIDIAP",             list(c("Study", "OMOP2024")),
  "SUCD - P4-C5-002",                 "SUCD",               list(c("."))
) %>%
  mutate(search_dirs = map(search_dirs, ~ .x[[1]]))

# --- Helper functions ---

#' Extract run date from an ip-result filename
#' @param filename e.g. "2026_03_19_NLHR@UIO_characteristics.csv"
#' @return Date or NA
extract_run_date <- function(filename) {
  m <- str_match(basename(filename), IP_RESULT_REGEX)
  if (is.na(m[1, 1])) return(NA_character_)
  paste(m[1, 2], m[1, 3], m[1, 4], sep = "-")
}

#' Extract result type from an ip-result filename
#' @return "characteristics", "incidence", or "prevalence"
extract_result_type <- function(filename) {
  m <- str_match(basename(filename), IP_RESULT_REGEX)
  m[1, 6]
}

#' Parse pkg_version from log.txt
#' Looks for "PregnancyIdentifier version: X.Y.Z"
extract_pkg_version <- function(log_path) {
  if (!file.exists(log_path)) return(NA_character_)
  lines <- readLines(log_path, n = 5, warn = FALSE)
  ver_line <- str_subset(lines, "PregnancyIdentifier version:")
  if (length(ver_line) == 0) return(NA_character_)
  str_extract(ver_line[1], "\\d+\\.\\d+\\.\\d+")
}

#' Extract package version from folder path (e.g. "2026-01-22-2.0.0-BIFAP-results")
#' Returns semver string or NA
extract_version_from_path <- function(dir_path) {
  # Check the directory name and its ancestors for a semver pattern
  # Pattern: date-X.Y.Z-name or name-X.Y.Z-something
  parts <- unlist(strsplit(dir_path, "/"))
  for (part in rev(parts)) {
    # Match patterns like "2026-01-22-2.0.0-BIFAP-results" or "p4-c5-002-1.0.0-timestamp"
    ver <- str_extract(part, "(?<=-)\\d+\\.\\d+\\.\\d+(?=-|$)")
    if (!is.na(ver)) return(ver)
    # Also check for V1/V2/V3 parent folder hints
    if (str_detect(part, "^V[123]$")) {
      return(paste0(str_extract(part, "\\d"), ".0.0"))
    }
  }
  # Also check immediate child directories (e.g. ip-results sibling has version in name)
  if (dir.exists(dir_path)) {
    children <- list.dirs(dir_path, recursive = FALSE, full.names = FALSE)
    for (child in children) {
      ver <- str_extract(child, "(?<=-)\\d+\\.\\d+\\.\\d+(?=-|$)")
      if (!is.na(ver)) return(ver)
    }
  }
  NA_character_
}

# --- Release date boundaries for date-based version inference ---
# From https://github.com/darwin-eu-dev/PregnancyIdentifier/releases
# v1.0.0: 2025-11-17, v2.0.0: 2025-12-17, v3.0.0: 2026-02-19
VERSION_BOUNDARIES <- tribble(
  ~major_version, ~release_date,
  "v1",           as.Date("2025-11-17"),
  "v2",           as.Date("2025-12-17"),
  "v3",           as.Date("2026-02-19")
)

#' Infer major version from run date when no explicit version is available.
#' Returns "v1", "v2", or "v3" (or NA if date is before v1).
infer_major_version_from_date <- function(run_date_str) {
  if (is.na(run_date_str) || run_date_str == "unknown") return(NA_character_)
  d <- as.Date(run_date_str)
  if (d >= as.Date("2026-02-19")) return("v3")
  if (d >= as.Date("2025-12-17")) return("v2")
  if (d >= as.Date("2025-11-17")) return("v1")
  NA_character_
}

#' Get the major version string from a semver (e.g. "2.0.1" -> "v2")
major_version_from_semver <- function(semver) {
  if (is.na(semver)) return(NA_character_)
  paste0("v", str_extract(semver, "^\\d+"))
}

#' Resolve package version for a run: folder name > log.txt > date inference.
#' Returns a list with $pkg_version (semver or NA), $major_version ("v1"/"v2"/"v3"),
#' and $version_source ("folder_name", "log", "date_inferred").
resolve_version <- function(dir_path, log_path, run_date) {
  # 1. Try folder name
  folder_ver <- extract_version_from_path(dir_path)
  if (!is.na(folder_ver)) {
    return(list(
      pkg_version = folder_ver,
      major_version = major_version_from_semver(folder_ver),
      version_source = "folder_name"
    ))
  }
  # 2. Try log.txt
  log_ver <- extract_pkg_version(log_path)
  if (!is.na(log_ver)) {
    return(list(
      pkg_version = log_ver,
      major_version = major_version_from_semver(log_ver),
      version_source = "log"
    ))
  }
  # 3. Fall back to date inference (major version only)
  major <- infer_major_version_from_date(run_date)
  return(list(
    pkg_version = NA_character_,
    major_version = major,
    version_source = if (!is.na(major)) "date_inferred" else NA_character_
  ))
}

#' Read cdm_name from cdm_source.csv
extract_cdm_name <- function(cdm_source_path) {
  if (!file.exists(cdm_source_path)) return(NA_character_)
  df <- read_csv(cdm_source_path, show_col_types = FALSE, n_max = 1)
  if ("cdm_name" %in% names(df)) return(df$cdm_name[1])
  if ("cdm_source_name" %in% names(df)) return(df$cdm_source_name[1])
  NA_character_
}

#' Normalize a filename using the rename map
normalize_filename <- function(filename) {
  bn <- basename(filename)
  if (bn %in% names(RENAME_MAP)) return(RENAME_MAP[[bn]])
  if (bn %in% names(LOG_ALIASES)) return(LOG_ALIASES[[bn]])
  bn
}

#' Check if a file is a known study-result file (canonical or old name)
is_study_file <- function(filename) {
  bn <- basename(filename)
  normalized <- normalize_filename(filename)
  normalized %in% c(CANONICAL_FILES, OPTIONAL_FILES)
}

#' Check if a file is a CohortDiagnostics file
is_cohort_diag_file <- function(filename) {
  basename(filename) %in% COHORT_DIAG_FILES
}
