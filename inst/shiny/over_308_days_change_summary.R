# over_308_days_change_summary.R
#
# Summarizes the magnitude of version-to-version change in
# % of episodes with gestational age > 308 days, per database.
# Uses the wide per-database percentages produced by
# over_308_days_by_version.R (re-runs it if the CSV is missing).
#
# Outputs two CSVs in the same folder:
#   over_308_days_change_detail.csv  - per-database deltas and direction
#   over_308_days_change_summary.csv - summary stats per comparison & direction
#
# Run from inst/shiny:
#   Rscript over_308_days_change_summary.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

shiny_dir <- if (basename(getwd()) == "shiny") getwd() else "inst/shiny"
src_csv  <- file.path(shiny_dir, "over_308_days_by_version.csv")
src_r    <- file.path(shiny_dir, "over_308_days_by_version.R")

if (!file.exists(src_csv)) {
  if (!file.exists(src_r))
    stop("Source CSV and source script both missing: ", src_csv)
  message("Source CSV missing; running ", src_r)
  old_wd <- getwd(); setwd(shiny_dir); on.exit(setwd(old_wd), add = TRUE)
  source(src_r, local = new.env())
}

long <- read_csv(src_csv, show_col_types = FALSE) %>%
  select(version, cdm_name, pct_over_308)

wide <- long %>%
  pivot_wider(names_from = version, values_from = pct_over_308) %>%
  mutate(
    delta_v1_v2 = v2 - v1,
    delta_v2_v3 = v3 - v2,
    direction_v1_v2 = case_when(
      is.na(v1) | is.na(v2) ~ "missing",
      v2 <  v1 ~ "decreased",
      v2 >  v1 ~ "increased",
      TRUE     ~ "unchanged"
    ),
    direction_v2_v3 = case_when(
      is.na(v2) | is.na(v3) ~ "missing",
      v3 <  v2 ~ "decreased",
      v3 >  v2 ~ "increased",
      TRUE     ~ "unchanged"
    )
  ) %>%
  arrange(cdm_name)

detail_path <- file.path(shiny_dir, "over_308_days_change_detail.csv")
write_csv(wide, detail_path)
message("Wrote per-database detail -> ", detail_path)

# Summary stats per comparison & direction (including "all changes" rows)
summarize_block <- function(deltas) {
  d <- deltas[!is.na(deltas)]
  tibble(
    n      = length(d),
    min    = if (length(d)) min(d)            else NA_real_,
    q1     = if (length(d)) quantile(d, 0.25, names = FALSE) else NA_real_,
    median = if (length(d)) median(d)         else NA_real_,
    mean   = if (length(d)) mean(d)           else NA_real_,
    q3     = if (length(d)) quantile(d, 0.75, names = FALSE) else NA_real_,
    max    = if (length(d)) max(d)            else NA_real_,
    sum    = if (length(d)) sum(d)            else NA_real_
  )
}

build_summary <- function(delta, direction, label) {
  bind_rows(
    summarize_block(delta[direction == "decreased"])            %>% mutate(direction = "decreased"),
    summarize_block(delta[direction == "increased"])            %>% mutate(direction = "increased"),
    summarize_block(delta[direction %in% c("decreased",
                                            "increased",
                                            "unchanged")])      %>% mutate(direction = "all_comparable"),
    summarize_block(abs(delta[direction %in% c("decreased",
                                                "increased",
                                                "unchanged")])) %>% mutate(direction = "abs_all_comparable")
  ) %>% mutate(comparison = label, .before = 1)
}

summary_tbl <- bind_rows(
  build_summary(wide$delta_v1_v2, wide$direction_v1_v2, "v1_to_v2"),
  build_summary(wide$delta_v2_v3, wide$direction_v2_v3, "v2_to_v3")
) %>%
  select(comparison, direction, n, min, q1, median, mean, q3, max, sum) %>%
  mutate(across(c(min, q1, median, mean, q3, max, sum), ~ round(., 3)))

summary_path <- file.path(shiny_dir, "over_308_days_change_summary.csv")
write_csv(summary_tbl, summary_path)
message("Wrote summary stats -> ", summary_path)

cat("\n--- Summary (percentage-point change in % over 308 days) ---\n")
print(summary_tbl, n = Inf)
