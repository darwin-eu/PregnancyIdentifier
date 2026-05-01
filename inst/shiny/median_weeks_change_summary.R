# median_weeks_change_summary.R
#
# Computes per-database median gestational age in WEEKS (from
# gestational_age_days_summary.csv, converting days -> weeks) for v1/v2/v3,
# then summarizes version-to-version change grouped by whether the
# database's % of episodes > 308 days increased or decreased
# (classification from over_308_days_by_version.csv).
#
# Outputs two CSVs in inst/shiny:
#   median_weeks_change_detail.csv  - per-database medians, deltas, direction
#   median_weeks_change_summary.csv - median / mean change in weeks per group
#
# Run from inst/shiny:
#   Rscript median_weeks_change_summary.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(tidyr)
})

shiny_dir <- if (basename(getwd()) == "shiny") getwd() else "inst/shiny"
v1_dir <- file.path(shiny_dir, "data2", "v1")
v2_dir <- file.path(shiny_dir, "data2", "v2")
v3_dir <- file.path(shiny_dir, "data")

# --- CDM name normalization (same convention as over_308_days_by_version.R) ---
v3_name_map <- c(
  "CDW_Bordeaux" = "CDW_Bordeaux", "CPRD"         = "CPRD_GOLD",
  "DK_DHR"       = "DK_DHR",       "FINOMOP_TaUH" = "FinOMOP_TaUH",
  "FinOMOP_THL"  = "FinOMOP_THL",  "HI_SPEED"     = "HI_SPEED",
  "INGEF"        = "InGef_RDB",    "IMASIS"       = "IMASIS",
  "IPCI"         = "IPCI",         "NAJS"         = "NAJS",
  "NLHR"         = "NLHR",         "SIDIAP"       = "SIDIAP",
  "SUCD"         = "SUCD"
)
normalize_cdm_name <- function(dir_name, version) {
  if (version %in% c("v1", "v2")) return(dir_name)
  stem <- sub("_results_.*$", "", dir_name)
  key  <- gsub("[ -]", "_", stem)
  if (key %in% names(v3_name_map)) unname(v3_name_map[key]) else key
}

# --- Extract median gestational age (days) per CDM/version ---
read_median_days <- function(base_dir, version) {
  if (!dir.exists(base_dir)) return(tibble())
  dirs <- list.dirs(base_dir, recursive = FALSE)
  dirs <- dirs[dir.exists(dirs)]
  map_dfr(dirs, function(d) {
    f <- file.path(d, "gestational_age_days_summary.csv")
    if (!file.exists(f)) return(NULL)
    x <- suppressMessages(read_csv(f, show_col_types = FALSE,
                                   col_types = cols(.default = "c")))
    # Use the first (and usually only) row per file
    med_days <- suppressWarnings(as.numeric(x$median[1]))
    tibble(
      version        = version,
      cdm_name       = normalize_cdm_name(basename(d), version),
      median_days    = med_days,
      median_weeks   = med_days / 7
    )
  })
}

medians <- bind_rows(
  read_median_days(v1_dir, "v1"),
  read_median_days(v2_dir, "v2"),
  read_median_days(v3_dir, "v3")
)

wide_med <- medians %>%
  select(cdm_name, version, median_weeks) %>%
  pivot_wider(names_from = version, values_from = median_weeks,
              names_prefix = "median_wk_") %>%
  mutate(
    delta_weeks_v1_v2 = median_wk_v2 - median_wk_v1,
    delta_weeks_v2_v3 = median_wk_v3 - median_wk_v2
  )

# --- Pull the over-308 direction classification ---
over308_path <- file.path(shiny_dir, "over_308_days_by_version.csv")
if (!file.exists(over308_path)) {
  stop("Missing ", over308_path,
       " - run over_308_days_by_version.R first.")
}
over308 <- read_csv(over308_path, show_col_types = FALSE) %>%
  select(version, cdm_name, pct_over_308) %>%
  pivot_wider(names_from = version, values_from = pct_over_308,
              names_prefix = "pct308_") %>%
  mutate(
    direction_v1_v2 = case_when(
      is.na(pct308_v1) | is.na(pct308_v2) ~ "missing",
      pct308_v2 <  pct308_v1 ~ "decreased",
      pct308_v2 >  pct308_v1 ~ "increased",
      TRUE                   ~ "unchanged"
    ),
    direction_v2_v3 = case_when(
      is.na(pct308_v2) | is.na(pct308_v3) ~ "missing",
      pct308_v3 <  pct308_v2 ~ "decreased",
      pct308_v3 >  pct308_v2 ~ "increased",
      TRUE                   ~ "unchanged"
    )
  )

detail <- wide_med %>%
  left_join(over308, by = "cdm_name") %>%
  arrange(cdm_name)

detail_path <- file.path(shiny_dir, "median_weeks_change_detail.csv")
write_csv(detail, detail_path)
message("Wrote per-database detail -> ", detail_path)

# --- Summary: median/mean change in weeks grouped by direction ---
summarize_block <- function(deltas) {
  d <- deltas[!is.na(deltas)]
  tibble(
    n      = length(d),
    min    = if (length(d)) min(d)                            else NA_real_,
    q1     = if (length(d)) quantile(d, 0.25, names = FALSE)  else NA_real_,
    median = if (length(d)) median(d)                         else NA_real_,
    mean   = if (length(d)) mean(d)                           else NA_real_,
    q3     = if (length(d)) quantile(d, 0.75, names = FALSE)  else NA_real_,
    max    = if (length(d)) max(d)                            else NA_real_
  )
}

rows <- list(
  summarize_block(detail$delta_weeks_v1_v2[detail$direction_v1_v2 == "decreased"]) %>%
    mutate(comparison = "v1_to_v2", direction_of_pct_over_308 = "decreased"),
  summarize_block(detail$delta_weeks_v1_v2[detail$direction_v1_v2 == "increased"]) %>%
    mutate(comparison = "v1_to_v2", direction_of_pct_over_308 = "increased"),
  summarize_block(detail$delta_weeks_v2_v3[detail$direction_v2_v3 == "decreased"]) %>%
    mutate(comparison = "v2_to_v3", direction_of_pct_over_308 = "decreased"),
  summarize_block(detail$delta_weeks_v2_v3[detail$direction_v2_v3 == "increased"]) %>%
    mutate(comparison = "v2_to_v3", direction_of_pct_over_308 = "increased")
)

summary_tbl <- bind_rows(rows) %>%
  select(comparison, direction_of_pct_over_308,
         n, min, q1, median, mean, q3, max) %>%
  mutate(across(c(min, q1, median, mean, q3, max), ~ round(., 3)))

summary_path <- file.path(shiny_dir, "median_weeks_change_summary.csv")
write_csv(summary_tbl, summary_path)
message("Wrote summary -> ", summary_path)

cat("\n--- Change in per-database median gestational age (weeks) ---\n")
cat("    grouped by direction of the % > 308 days change\n\n")
print(summary_tbl, n = Inf)

cat("\n--- Per-database detail ---\n")
print(detail %>%
        select(cdm_name,
               median_wk_v1, median_wk_v2, median_wk_v3,
               delta_weeks_v1_v2, delta_weeks_v2_v3,
               direction_v1_v2, direction_v2_v3) %>%
        mutate(across(where(is.numeric), ~ round(., 2))),
      n = Inf, width = 200)
