# 06_episode_quality_table.R — Episode quality metrics table across databases and runs
#
# Reads pre-computed summary CSVs from the processed results and assembles a
# table with: episode count, plausibility %, >308 days %, overlap %, unknown outcome %.
#
# Outputs: CSV + gt table in .docx format.

library(tidyverse)
library(gt)
library(fs)

# Point rmarkdown/gt at the pandoc bundled with Quarto
Sys.setenv(RSTUDIO_PANDOC = "/Applications/quarto/bin/tools/aarch64")

STUDY_DIR <- file.path("processed_results", "study_results")
OUT_DIR <- "processed_results"

# ─── 1. Discover runs that have study results ────────────────────────────────

run_dirs <- dir_ls(STUDY_DIR, type = "directory") %>%

  map_dfr(function(db_path) {
    db_key <- path_file(db_path)
    dir_ls(db_path, type = "directory") %>%
      map_dfr(function(run_path) {
        run_date <- path_file(run_path)
        # Only include runs that have episode_frequency.csv (i.e., study results)
        if (file_exists(path(run_path, "episode_frequency.csv"))) {
          tibble(db_key = db_key, run_date = run_date, run_path = as.character(run_path))
        } else {
          tibble()
        }
      })
  }) %>%
  arrange(db_key, run_date)

cat(sprintf("Found %d runs with study data across %d databases\n",
            nrow(run_dirs), n_distinct(run_dirs$db_key)))

# ─── 2. Safe CSV reader ─────────────────────────────────────────────────────

read_summary <- function(run_path, filename) {
  fp <- path(run_path, filename)
  if (!file_exists(fp)) return(NULL)
  read_csv(fp, show_col_types = FALSE)
}

# ─── 3. Extract metrics for each run ─────────────────────────────────────────

# Read manifest for version info
manifest_df <- read_csv(path(OUT_DIR, "manifest.csv"), show_col_types = FALSE)

metrics <- run_dirs %>%
  pmap_dfr(function(db_key, run_date, run_path) {

    # --- Total episodes ---
    ep <- read_summary(run_path, "episode_frequency.csv")
    total_episodes <- if (!is.null(ep)) ep$total_episodes[1] else NA_integer_

    # --- Plausibility & >308 days ---
    # Primary source: gestational_age_days_counts.csv
    # Fallback: compute from gestational_weeks.csv (week 44 = 308 days)
    ga <- read_summary(run_path, "gestational_age_days_counts.csv")
    ga_valid <- !is.null(ga) && !is.na(ga$over_308days[1])

    if (ga_valid && !is.null(total_episodes) && total_episodes > 0) {
      over_308 <- as.numeric(ga$over_308days[1])
      pct_plausible <- (total_episodes - over_308) / total_episodes * 100
      pct_over_308 <- over_308 / total_episodes * 100
    } else if (!is.null(total_episodes) && total_episodes > 0) {
      # Fallback: use gestational_weeks.csv
      # Week > 44 corresponds to >308 days (44 * 7 = 308)
      gw_fallback <- read_summary(run_path, "gestational_weeks.csv")
      if (!is.null(gw_fallback)) {
        over_44w <- gw_fallback %>% filter(gestational_weeks > 44)
        if (nrow(over_44w) > 0) {
          n_over_308 <- sum(as.numeric(ifelse(over_44w$n == "<5", "2", over_44w$n)), na.rm = TRUE)
          pct_over_308 <- n_over_308 / total_episodes * 100
          pct_plausible <- (total_episodes - n_over_308) / total_episodes * 100
        } else {
          pct_over_308 <- 0
          pct_plausible <- 100
        }
      } else {
        pct_plausible <- NA_real_
        pct_over_308 <- NA_real_
      }
    } else {
      pct_plausible <- NA_real_
      pct_over_308 <- NA_real_
    }

    # --- Overlap ---
    ov <- read_summary(run_path, "pregnancy_overlap_counts.csv")
    if (!is.null(ov) && !is.null(total_episodes) && total_episodes > 0) {
      overlap_true <- ov %>% filter(overlap == TRUE)
      pct_overlap <- if (nrow(overlap_true) > 0) {
        as.numeric(overlap_true$n[1]) / total_episodes * 100
      } else {
        0
      }
    } else {
      pct_overlap <- NA_real_
    }

    # --- Outcome categories: % DELIV and % PREG (using "hip" algorithm) ---
    oc <- read_summary(run_path, "outcome_categories_count.csv")
    if (!is.null(oc) && !is.null(total_episodes) && total_episodes > 0) {
      # Use "hip" algorithm for outcome breakdown
      algo <- if ("hip" %in% oc$algorithm) "hip" else "hipps"
      oc_algo <- oc %>% filter(algorithm == algo)

      deliv_row <- oc_algo %>% filter(outcome_category == "DELIV")
      pct_deliv <- if (nrow(deliv_row) > 0) {
        as.numeric(deliv_row$n[1]) / total_episodes * 100
      } else {
        0
      }

      preg_row <- oc_algo %>% filter(outcome_category == "PREG")
      pct_preg <- if (nrow(preg_row) > 0) {
        as.numeric(preg_row$n[1]) / total_episodes * 100
      } else {
        0
      }
    } else {
      pct_deliv <- NA_real_
      pct_preg <- NA_real_
    }

    # --- >52 weeks (>364 days) from gestational_weeks.csv ---
    gw <- read_summary(run_path, "gestational_weeks.csv")
    if (!is.null(gw) && !is.null(total_episodes) && total_episodes > 0) {
      # Weeks > 52 correspond to >364 days
      over_52w <- gw %>% filter(gestational_weeks > 52)
      if (nrow(over_52w) > 0) {
        # n column may contain "<5" (small-cell suppression) — replace with midpoint 2
        n_over_52w <- sum(as.numeric(ifelse(over_52w$n == "<5", "2", over_52w$n)), na.rm = TRUE)
        pct_over_364 <- n_over_52w / total_episodes * 100
      } else {
        pct_over_364 <- 0
      }
    } else {
      pct_over_364 <- NA_real_
    }

    # --- Package version from manifest ---
    manifest_row <- manifest_df %>%
      filter(.data$db_key == .env$db_key, .data$run_date == .env$run_date)
    if (nrow(manifest_row) > 0) {
      pkg_version <- manifest_row$major_version[1]
      pkg_version_exact <- manifest_row$pkg_version[1]
      version_source <- manifest_row$version_source[1]
      version_inferred <- !is.na(version_source) && version_source == "date_inferred"
      # Use exact version if available, otherwise major version
      if (!is.na(pkg_version_exact) && pkg_version_exact != "NA") {
        pkg_version <- paste0("v", pkg_version_exact)
      }
    } else {
      pkg_version <- NA_character_
      version_inferred <- TRUE
    }

    tibble(
      db_key = db_key,
      run_date = run_date,
      pkg_version = pkg_version,
      version_inferred = version_inferred,
      total_episodes = as.integer(total_episodes),
      pct_plausible = pct_plausible,
      pct_over_308 = pct_over_308,
      pct_over_364 = pct_over_364,
      pct_overlap = pct_overlap,
      pct_deliv = pct_deliv,
      pct_preg = pct_preg,
      pct_deliv_preg = pct_deliv + pct_preg
    )
  })

# ─── 4. Keep only the latest run per (database, major version) ────────────

# Extract major version for grouping (e.g. "v1.0.0" -> "v1", "v2" stays "v2")
metrics <- metrics %>%
  mutate(
    major_version = str_extract(pkg_version, "v\\d+")
  ) %>%
  filter(!is.na(major_version)) %>%
  group_by(db_key, major_version) %>%
  # Keep the latest run_date per (db, major version)
  slice_max(run_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(db_key, major_version)

# ─── 5. Write CSV ───────────────────────────────────────────────────────────

csv_out <- metrics %>%
  select(
    database = db_key,
    major_version,
    run_date,
    pkg_version,
    version_inferred,
    episodes_count = total_episodes,
    plausible_pct = pct_plausible,
    over_308_days_pct = pct_over_308,
    over_364_days_pct = pct_over_364,
    overlap_pct = pct_overlap,
    deliv_pct = pct_deliv,
    preg_pct = pct_preg,
    deliv_preg_pct = pct_deliv_preg
  )

write_csv(csv_out, path(OUT_DIR, "episode_quality_metrics.csv"))
cat("CSV written to processed_results/episode_quality_metrics.csv\n")

# ─── 6. Build gt table ──────────────────────────────────────────────────────

# Prettify database names for display
display_names <- c(
  BIFAP = "BIFAP", CDW_Bordeaux = "CDW Bordeaux", CPRD_GOLD = "CPRD GOLD",
  DK_DHR = "DK-DHR", EMDB_ULSGE = "EMDB-ULSGE", FinOMOP_THL = "FinOMOP-THL",
  FinOMOP_TaUH = "FinOMOP-TaUH Pirha", HI_SPEED = "HI-SPEED",
  IMASIS = "IMASIS", InGef_RDB = "InGef RDB", IPCI = "IPCI",
  NAJS = "NAJS", NLHR = "NLHR",
  SIDIAP = "SIDIAP",
  SUCD = "SUCD"
)

gt_data <- metrics %>%
  mutate(
    database = coalesce(display_names[db_key], db_key),
    # Mark inferred versions with a dagger
    version_label = if_else(version_inferred, paste0(major_version, "\u2020"), major_version)
  ) %>%
  select(
    database,
    version_label,
    run_date,
    total_episodes,
    pct_plausible,
    pct_over_308,
    pct_over_364,
    pct_overlap,
    pct_deliv,
    pct_preg,
    pct_deliv_preg
  )

tbl <- gt_data %>%
  gt(groupname_col = "database") %>%
  cols_label(
    version_label          = "Version",
    run_date               = "Run Date",
    total_episodes  = "Episodes",
    pct_plausible   = "Plausible (0\u2013308d)",
    pct_over_308    = ">308 days",
    pct_over_364    = ">52 weeks",
    pct_overlap     = "Overlapping",
    pct_deliv       = "DELIV",
    pct_preg        = "PREG",
    pct_deliv_preg  = "DELIV+PREG"
  ) %>%
  fmt_integer(columns = total_episodes) %>%
  fmt_number(
    columns = c(pct_plausible, pct_over_308, pct_over_364, pct_overlap, pct_deliv, pct_preg, pct_deliv_preg),
    decimals = 1,
    suffixing = FALSE
  ) %>%
  cols_align(align = "right", columns = c(total_episodes, pct_plausible, pct_over_308, pct_over_364, pct_overlap, pct_deliv, pct_preg, pct_deliv_preg)) %>%
  cols_align(align = "left", columns = c(version_label, run_date)) %>%
  tab_spanner(
    label = "Percentage of Total Episodes (%)",
    columns = c(pct_plausible, pct_over_308, pct_over_364, pct_overlap)
  ) %>%
  tab_spanner(
    label = "Outcome (%)",
    columns = c(pct_deliv, pct_preg, pct_deliv_preg)
  ) %>%
  tab_header(
    title = "Episode Quality Metrics",
    subtitle = "Pre-cleanup metrics across databases and package versions"
  ) %>%
  tab_footnote(
    footnote = "\u2020 Version inferred from run date using release history (v1: 2025-11-17, v2: 2025-12-17, v3: 2026-02-19).",
    locations = cells_column_labels(columns = version_label)
  ) %>%
  tab_footnote(
    footnote = "Plausible: episode duration \u2265 0 and \u2264 308 days.",
    locations = cells_column_labels(columns = pct_plausible)
  ) %>%
  tab_footnote(
    footnote = ">52 weeks: episodes with gestational age >364 days. Approximate due to small-cell suppression (<5 replaced with 2).",
    locations = cells_column_labels(columns = pct_over_364)
  ) %>%
  tab_footnote(
    footnote = "Overlap: episodes sharing any date range within the same person.",
    locations = cells_column_labels(columns = pct_overlap)
  ) %>%
  tab_footnote(
    footnote = "DELIV: episodes with 'DELIV' outcome category (HIP algorithm). PREG: episodes with 'PREG' (ongoing/unresolved pregnancy) outcome.",
    locations = cells_column_spanners(spanners = "Outcome (%)")
  ) %>%
  sub_missing(missing_text = "\u2014") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_options(
    row_group.font.weight = "bold",
    table.font.size = px(11),
    data_row.padding = px(3)
  ) %>%
  # Highlight concerning values
  tab_style(
    style = cell_text(color = "#c0392b"),
    locations = cells_body(
      columns = pct_over_308,
      rows = pct_over_308 > 5
    )
  ) %>%
  tab_style(
    style = cell_text(color = "#c0392b"),
    locations = cells_body(
      columns = pct_over_364,
      rows = pct_over_364 > 5
    )
  ) %>%
  tab_style(
    style = cell_text(color = "#c0392b"),
    locations = cells_body(
      columns = pct_overlap,
      rows = pct_overlap > 10
    )
  ) %>%
  tab_style(
    style = cell_text(color = "#c0392b"),
    locations = cells_body(
      columns = pct_preg,
      rows = pct_preg > 30
    )
  )

# ─── 7. Save as docx ────────────────────────────────────────────────────────

docx_path <- path(OUT_DIR, "episode_quality_metrics.docx")
gtsave(tbl, docx_path)
cat(sprintf("gt table saved to %s\n", docx_path))

# Also save as HTML for quick viewing
html_path <- path(OUT_DIR, "episode_quality_metrics.html")
gtsave(tbl, html_path)
cat(sprintf("gt table saved to %s\n", html_path))

# ─── 8. Print summary to console ────────────────────────────────────────────

cat("\n=== Episode Quality Metrics Summary ===\n")
print(csv_out, n = Inf)
