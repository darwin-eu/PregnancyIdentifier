# over_308_days_by_version.R
#
# Extracts total_episodes and over_308_days for every CDM across v1, v2, v3.
#   - v3: reads over_308days directly from gestational_age_days_counts.csv
#   - v1, v2: gestational_age_days_counts.csv has many NAs, so compute
#     over_308_days from gestational_weeks.csv by summing n where
#     gestational_weeks > 44 (44 weeks = 308 days per the app's convention,
#     so "over 308 days" means strictly > 44 weeks). This is an integer-week
#     approximation — day-level precision within week 44 is not recoverable
#     from the weekly aggregate.
#
# total_episodes comes from episode_frequency.csv for all versions.
#
# Output: inst/shiny/over_308_days_by_version.csv
#
# Run from the inst/shiny folder:
#   Rscript over_308_days_by_version.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
})

shiny_dir <- if (basename(getwd()) == "shiny") getwd() else "inst/shiny"
v1_dir <- file.path(shiny_dir, "data2", "v1")
v2_dir <- file.path(shiny_dir, "data2", "v2")
v3_dir <- file.path(shiny_dir, "data")

# Canonical CDM names (v1/v2 directory names are already the canonical form).
# v3 directories carry "_results_<date>" suffixes and some variant spellings;
# strip the suffix, normalize separators, and apply this lookup for the rest.
v3_name_map <- c(
  "CDW_Bordeaux" = "CDW_Bordeaux",  # v3 "CDW Bordeaux"
  "CPRD"         = "CPRD_GOLD",     # v3 "CPRD"
  "DK_DHR"       = "DK_DHR",        # v3 "DK-DHR"
  "FINOMOP_TaUH" = "FinOMOP_TaUH",  # v3 "FINOMOP_TaUH"
  "FinOMOP_THL"  = "FinOMOP_THL",   # v3 "FinOMOP-THL"
  "HI_SPEED"     = "HI_SPEED",      # v3 "HI-SPEED"
  "INGEF"        = "InGef_RDB",     # v3 "INGEF"
  "IMASIS"       = "IMASIS",
  "IPCI"         = "IPCI",
  "NAJS"         = "NAJS",
  "NLHR"         = "NLHR",
  "SIDIAP"       = "SIDIAP",
  "SUCD"         = "SUCD"
)

normalize_cdm_name <- function(dir_name, version) {
  if (version %in% c("v1", "v2")) return(dir_name)
  # v3: strip "_results_<date>" suffix, then normalize separators
  stem <- sub("_results_.*$", "", dir_name)
  key <- gsub("[ -]", "_", stem)
  if (key %in% names(v3_name_map)) return(unname(v3_name_map[key]))
  warning("Unmapped v3 directory: ", dir_name, " (stem: ", stem, ")")
  key
}

read_one <- function(cdm_dir, version) {
  ef_file <- file.path(cdm_dir, "episode_frequency.csv")
  if (!file.exists(ef_file)) return(NULL)
  ef <- suppressMessages(read_csv(ef_file, show_col_types = FALSE))
  total_episodes <- suppressWarnings(as.numeric(ef$total_episodes[1]))
  cdm_name <- normalize_cdm_name(basename(cdm_dir), version)

  if (version == "v3") {
    gad_file <- file.path(cdm_dir, "gestational_age_days_counts.csv")
    over_308 <- if (file.exists(gad_file)) {
      gad <- suppressMessages(read_csv(gad_file, show_col_types = FALSE))
      suppressWarnings(as.numeric(gad$over_308days[1]))
    } else NA_real_
    source_col <- "gestational_age_days_counts.csv::over_308days"
  } else {
    # v1 / v2: compute from gestational_weeks.csv
    gw_file <- file.path(cdm_dir, "gestational_weeks.csv")
    over_308 <- if (file.exists(gw_file)) {
      gw <- suppressMessages(read_csv(gw_file, show_col_types = FALSE))
      gw <- gw %>% mutate(
        gestational_weeks = suppressWarnings(as.numeric(gestational_weeks)),
        n = suppressWarnings(as.numeric(n))
      )
      sum(gw$n[!is.na(gw$gestational_weeks) & gw$gestational_weeks > 44],
          na.rm = TRUE)
    } else NA_real_
    source_col <- "gestational_weeks.csv::sum(n | weeks > 44)"
  }

  tibble(
    version = version,
    cdm_name = cdm_name,
    total_episodes = total_episodes,
    over_308_days = over_308,
    pct_over_308 = ifelse(is.na(total_episodes) | total_episodes == 0,
                          NA_real_,
                          round(100 * over_308 / total_episodes, 3)),
    source = source_col
  )
}

collect_version <- function(base_dir, version) {
  if (!dir.exists(base_dir)) return(tibble())
  cdm_dirs <- list.dirs(base_dir, recursive = FALSE)
  # v3 base_dir has stray CSVs at the top; filter to real dirs only
  cdm_dirs <- cdm_dirs[dir.exists(cdm_dirs)]
  map_dfr(cdm_dirs, read_one, version = version)
}

results <- bind_rows(
  collect_version(v1_dir, "v1"),
  collect_version(v2_dir, "v2"),
  collect_version(v3_dir, "v3")
) %>% arrange(cdm_name, version)

out_path <- file.path(shiny_dir, "over_308_days_by_version.csv")
write_csv(results, out_path)

message("Wrote ", nrow(results), " rows to ", out_path)
print(results, n = Inf)
