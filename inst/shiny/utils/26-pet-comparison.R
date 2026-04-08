# 26-pet-comparison.R - PET comparison module (standard Shiny module)
# Data: petComparisonSummarisedResult (a summarised_result)
# Complex module with Overview/Plot/Person-level/Table/Summarised-result tabs.

# ---- Overview markdown ----
PET_COMPARISON_OVERVIEW_MD <- "
## What is compared

This section compares **pregnancy episodes from the PregnancyIdentifier algorithm** with **pregnancy episodes in the OMOP Pregnancy Extension Table (PET)**. The PET is treated as the **reference (gold standard) for positive episodes** only: there is no negative class (no non-pregnancy episodes), so we can only compute **sensitivity** and **positive predictive value (PPV)**. Specificity and NPV are not defined here.

---

## How the comparison is done

1. **Sources**
   - **PET:** Episodes from the PET table (person, start/end dates, outcome).
   - **Algorithm:** Episodes from the pipeline output (e.g. `final_pregnancy_episodes.rds`).

2. **Matching**
   - Within each person, every PET episode is paired with every algorithm episode whose date range **overlaps by at least a minimum number of days** (e.g. 1 day).
   - **One-to-one assignment:** Among these candidate pairs, a greedy algorithm assigns each PET episode to at most one algorithm episode (and vice versa), choosing pairs by **largest overlap first**. This avoids double-counting and gives consistent counts for Venn and confusion metrics.

3. **Resulting counts**
   - **Both:** PET episode and algorithm episode matched (one-to-one).
   - **PET only:** PET episode with no matched algorithm episode.
   - **Algorithm only:** Algorithm episode with no matched PET episode.

---

## What is calculated

| Metric | Meaning |
|--------|--------|
| **Episode counts** | Total episodes in PET and total from the algorithm (optionally filtered, e.g. gestation 0-308 days, end >= start). |
| **Venn counts** | Number of episodes in **both**, **PET only**, and **algorithm only** (from the one-to-one matching). |
| **Sensitivity** | Among all PET episodes, the proportion that have a matched algorithm episode: *TP / (TP + FN)*. |
| **PPV** | Among all algorithm episodes, the proportion that are matched to a PET episode (true positives): *TP / (TP + FP)*, where FP = algorithm-only episodes. |
| **Start date difference** | For matched pairs: PET start minus algorithm start (days). Positive = algorithm starts too early, negative = algorithm starts too late. Reported as min/q25/median/q75/max. |
| **End date difference** | For matched pairs: PET end minus algorithm end (days). Positive = algorithm ends too early, negative = algorithm ends too late. |
| **Duration difference** | For matched pairs: PET duration minus algorithm duration (days). Positive = algorithm episodes are shorter, negative = algorithm episodes are longer. |
| **Date differences by outcome** | Same start/end/duration differences stratified by algorithm outcome category (LB, SB, SA, AB, PREG, etc.). Useful because different outcomes have very different expected durations. |
| **Confusion 2x2** | Counts for TP, FN, FP; TN is not defined (no gold-standard negatives). |
| **Outcome accuracy** | Among matched pairs, how often the algorithm outcome category agrees with the PET outcome. |
| **Duration** | Distribution of pregnancy duration (days) for PET and algorithm episodes; for matched pairs, duration of PET vs algorithm. |

---

## Person-level comparison

Persons are classified into three groups:
- **Both HIPPS and PET**: Persons who have episodes in both sources.
- **HIPPS only**: Persons who have algorithm episodes but no PET episodes at all.
- **PET only**: Persons who have PET episodes but no algorithm episodes at all.

For each group, the Person-level tab shows:
- Number of distinct persons (Venn diagram)
- Distribution of pregnancies per person (mean, median, min, max, IQR)

---

## Sub-tabs in this section

- **Overview** (this page): Methodology and interpretation.
- **Plot:** Venn diagram by database (PET episodes vs algorithm episodes, overlap = both).
- **Alignment:** Distribution of date differences between matched PET and algorithm episodes. Shows how well start dates, end dates, and durations align, with an option to stratify by algorithm outcome. Requires result files generated with v3.0.6+.
- **Person-level:** Person-level Venn diagram and episodes-per-person summary.
- **Table:** Formatted table of all comparison metrics (visOmopResults), with a **Database** filter.
- **Summarised result:** Raw summarised result table (download as CSV).
"

# ---- Display label remapping (shiny only, does not affect exports) ----
# Backward compatibility: old result files (pre-v3.0.6) used short labels for
# time_overlap_summary and date_difference_summary. New output files already use
# descriptive labels in the source. This mapping ensures old files display correctly.
PET_LEGACY_LABELS <- c(
  # Time overlap labels (removed from new output, but old files may still have them)
  "PET -> IPE 0 day overlap required"  = "PET -> Algorithm: All PET episodes (incl. unmatched)",
  "PET -> IPE 1 day overlap required"  = "PET -> Algorithm: Matched PET episodes only",
  "IPE -> PET 0 day overlap required"  = "Algorithm -> PET: All algorithm episodes (incl. unmatched)",
  "IPE -> PET 1 day overlap required"  = "Algorithm -> PET: Matched algorithm episodes only",
  # Date difference labels (old files used short names)
  "start_diff_days"    = "Start date difference (PET - Algorithm, days)",
  "end_diff_days"      = "End date difference (PET - Algorithm, days)",
  "duration_diff_days" = "Duration difference (PET - Algorithm, days)"
)

#' Remap legacy labels for display in shiny. Ensures old result files
#' (with short variable_level names) display with descriptive labels.
#' New result files already have descriptive labels and pass through unchanged.
remap_pet_display_labels <- function(sr) {
  if (!"variable_level" %in% names(sr)) return(sr)
  # Exact matches for legacy labels
  sr <- sr %>%
    dplyr::mutate(
      variable_level = dplyr::if_else(
        .data$variable_level %in% names(PET_LEGACY_LABELS),
        PET_LEGACY_LABELS[.data$variable_level],
        .data$variable_level
      )
    )
  # Legacy outcome-stratified: "start_diff_days__LB" -> "Start date difference (PET - Algorithm, days) [LB]"
  outcome_pattern <- "^(start_diff_days|end_diff_days|duration_diff_days)__(.+)$"
  has_outcome <- grepl(outcome_pattern, sr$variable_level)
  if (any(has_outcome)) {
    base <- sub(outcome_pattern, "\\1", sr$variable_level[has_outcome])
    outcome <- sub(outcome_pattern, "\\2", sr$variable_level[has_outcome])
    pretty_base <- dplyr::if_else(base %in% names(PET_LEGACY_LABELS), PET_LEGACY_LABELS[base], base)
    sr$variable_level[has_outcome] <- paste0(pretty_base, " [", outcome, "]")
  }
  sr
}

# ---- Venn helper ----
#' Build Venn diagram data for amVennDiagram5 from counts.
pet_venn_data_from_counts <- function(both, pet_only, algorithm_only) {
  both <- max(0L, as.integer(both))
  pet_only <- max(0L, as.integer(pet_only))
  algorithm_only <- max(0L, as.integer(algorithm_only))
  n_pet <- pet_only + both
  n_alg <- algorithm_only + both
  if (n_pet == 0 && n_alg == 0) {
    return(amVennDiagram5::makeVennData(list(
      "PET" = integer(0),
      "HIPPS" = integer(0)
    )))
  }
  pet_ids <- if (n_pet > 0) seq_len(n_pet) else integer(0)
  alg_ids <- if (n_alg > 0) (pet_only + 1L):(pet_only + both + algorithm_only) else integer(0)
  amVennDiagram5::makeVennData(list(
    "PET" = pet_ids,
    "HIPPS" = alg_ids
  ))
}

#' Extract Venn counts (both, pet_only, algorithm_only) per cdm_name from summarised result.
extract_venn_data_from_sr <- function(sr, variable = "venn_counts", estimate = "n_episodes") {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  vennWide <- tbl %>%
    dplyr::filter(.data$variable_name == variable, .data$estimate_name == estimate) %>%
    dplyr::select("cdm_name", "variable_level", "estimate_value")
  if (nrow(vennWide) == 0) return(NULL)
  take_first <- function(x) { x <- unlist(x, use.names = FALSE); if (length(x) == 0) NA else x[1] }
  pw <- vennWide %>%
    tidyr::pivot_wider(names_from = "variable_level", values_from = "estimate_value", values_fn = take_first)
  for (col in c("both", "pet_only", "algorithm_only")) {
    if (!col %in% names(pw)) pw[[col]] <- NA_character_
  }
  pw %>%
    dplyr::mutate(
      both = suppressWarnings(as.integer(dplyr::coalesce(.data$both, "0"))),
      pet_only = suppressWarnings(as.integer(dplyr::coalesce(.data$pet_only, "0"))),
      algorithm_only = suppressWarnings(as.integer(dplyr::coalesce(.data$algorithm_only, "0")))
    ) %>%
    dplyr::select("cdm_name", "both", "pet_only", "algorithm_only")
}

#' Extract person-level episodes-per-person summary from summarised result.
extract_person_epp_from_sr <- function(sr) {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  epp <- tbl %>%
    dplyr::filter(.data$variable_name == "person_episodes_per_person") %>%
    dplyr::select("cdm_name", "variable_level", "estimate_name", "estimate_value")
  if (nrow(epp) == 0) return(NULL)
  take_first <- function(x) { x <- unlist(x, use.names = FALSE); if (length(x) == 0) NA else x[1] }
  epp %>%
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value", values_fn = take_first) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("mean", "median", "sd", "min", "q25", "q75", "max")),
                                ~ suppressWarnings(as.numeric(.x)))) %>%
    dplyr::mutate(
      group = dplyr::case_when(
        .data$variable_level == "both:algorithm" ~ "Both (HIPPS episodes)",
        .data$variable_level == "both:pet" ~ "Both (PET episodes)",
        .data$variable_level == "algorithm_only" ~ "HIPPS only",
        .data$variable_level == "pet_only" ~ "PET only",
        TRUE ~ .data$variable_level
      )
    )
}

#' Extract binned date-difference distributions from summarised result.
#' Returns a data.frame with columns: cdm_name, measure, bin, n
#' @param sr summarised_result
#' @param variable_name "date_difference_distribution" or "date_difference_distribution_by_outcome"
extract_date_diff_distribution_from_sr <- function(sr, variable_name = "date_difference_distribution") {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  rows <- tbl %>%
    dplyr::filter(.data$variable_name == .env$variable_name,
                  .data$estimate_name == "n") %>%
    dplyr::select("cdm_name", "variable_level", "estimate_value")
  if (nrow(rows) == 0) return(NULL)

  bin_order <- c("\u2264 -30", "-29 to -15", "-14 to -8", "-7 to -1",
                 "0", "1 to 7", "8 to 14", "15 to 29", "\u2265 30")
  measure_labels <- c(
    "start_diff" = "Start date difference",
    "end_diff" = "End date difference",
    "duration_diff" = "Duration difference"
  )

  if (variable_name == "date_difference_distribution") {
    # Format: "measure::bin"
    parsed <- rows %>%
      dplyr::mutate(
        measure = sub("^(.*?)::(.*?)$", "\\1", .data$variable_level),
        bin = sub("^(.*?)::(.*?)$", "\\2", .data$variable_level),
        n = suppressWarnings(as.integer(.data$estimate_value))
      ) %>%
      dplyr::mutate(
        measure_label = dplyr::if_else(
          .data$measure %in% names(measure_labels),
          measure_labels[.data$measure],
          .data$measure
        ),
        bin = factor(.data$bin, levels = bin_order)
      ) %>%
      dplyr::select("cdm_name", "measure", "measure_label", "bin", "n")
  } else {
    # Format: "measure::outcome::bin"
    parsed <- rows %>%
      dplyr::mutate(
        measure = sub("^(.*?)::(.*?)::(.*?)$", "\\1", .data$variable_level),
        outcome = sub("^(.*?)::(.*?)::(.*?)$", "\\2", .data$variable_level),
        bin = sub("^(.*?)::(.*?)::(.*?)$", "\\3", .data$variable_level),
        n = suppressWarnings(as.integer(.data$estimate_value))
      ) %>%
      dplyr::mutate(
        measure_label = dplyr::if_else(
          .data$measure %in% names(measure_labels),
          measure_labels[.data$measure],
          .data$measure
        ),
        bin = factor(.data$bin, levels = bin_order)
      ) %>%
      dplyr::select("cdm_name", "measure", "measure_label", "outcome", "bin", "n")
  }
  parsed
}

#' Extract gestational time summary by group (matched, algorithm_only, pet_only).
#' Returns a data.frame with columns: cdm_name, group, stat, value.
extract_gestational_time_from_sr <- function(sr) {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  rows <- tbl %>%
    dplyr::filter(.data$variable_name == "group_gestational_time") %>%
    dplyr::select("cdm_name", "variable_level", "estimate_name", "estimate_value")
  if (nrow(rows) == 0) return(NULL)
  take_first <- function(x) { x <- unlist(x, use.names = FALSE); if (length(x) == 0) NA else x[1] }
  rows %>%
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value", values_fn = take_first) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("n", "mean", "median", "sd", "min", "q25", "q75", "max")),
                                ~ suppressWarnings(as.numeric(.x)))) %>%
    dplyr::mutate(
      group = dplyr::case_when(
        .data$variable_level == "matched" ~ "Matched",
        .data$variable_level == "algorithm_only" ~ "HIPPS only (unmatched)",
        .data$variable_level == "pet_only" ~ "PET only (unmatched)",
        TRUE ~ .data$variable_level
      )
    )
}

#' Extract outcome distribution by group (matched, algorithm_only, pet_only).
#' Returns a data.frame with columns: cdm_name, group, outcome, n, pct.
extract_group_outcome_from_sr <- function(sr) {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  rows <- tbl %>%
    dplyr::filter(.data$variable_name == "group_outcome") %>%
    dplyr::select("cdm_name", "variable_level", "estimate_name", "estimate_value")
  if (nrow(rows) == 0) return(NULL)
  take_first <- function(x) { x <- unlist(x, use.names = FALSE); if (length(x) == 0) NA else x[1] }
  wide <- rows %>%
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value", values_fn = take_first) %>%
    dplyr::mutate(
      n = suppressWarnings(as.numeric(.data$n)),
      pct = suppressWarnings(as.numeric(.data$pct))
    )
  # Parse "group:outcome" from variable_level
  wide %>%
    dplyr::mutate(
      group_raw = sub(":.*$", "", .data$variable_level),
      outcome = sub("^[^:]+:", "", .data$variable_level),
      group = dplyr::case_when(
        .data$group_raw == "matched" ~ "Matched",
        .data$group_raw == "algorithm_only" ~ "HIPPS only",
        .data$group_raw == "pet_only" ~ "PET only",
        TRUE ~ .data$group_raw
      )
    ) %>%
    dplyr::select("cdm_name", "group", "outcome", "n", "pct")
}

#' Extract PET-only HIP/PPS concept coverage from summarised result.
#' Returns a data.frame with columns: cdm_name, metric, value.
extract_pet_only_concept_coverage_from_sr <- function(sr) {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  coverage_vars <- c("pet_only_hip_coverage", "pet_only_pps_coverage", "pet_only_any_record_coverage")
  rows <- tbl %>%
    dplyr::filter(.data$variable_name %in% coverage_vars) %>%
    dplyr::select("cdm_name", "variable_name", "estimate_name", "estimate_value")
  if (nrow(rows) == 0) return(NULL)
  rows %>%
    dplyr::mutate(estimate_value = suppressWarnings(as.numeric(.data$estimate_value)))
}

#' Extract PET-only HIP/PPS record count distributions from summarised result.
extract_pet_only_record_counts_from_sr <- function(sr) {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  count_vars <- c("pet_only_hip_record_count", "pet_only_pps_record_count")
  rows <- tbl %>%
    dplyr::filter(.data$variable_name %in% count_vars) %>%
    dplyr::select("cdm_name", "variable_name", "estimate_name", "estimate_value")
  if (nrow(rows) == 0) return(NULL)
  take_first <- function(x) { x <- unlist(x, use.names = FALSE); if (length(x) == 0) NA else x[1] }
  rows %>%
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value", values_fn = take_first) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("n", "mean", "median", "sd", "min", "q25", "q75", "max")),
                                ~ suppressWarnings(as.numeric(.x)))) %>%
    dplyr::mutate(
      record_type = dplyr::case_when(
        .data$variable_name == "pet_only_hip_record_count" ~ "HIP records",
        .data$variable_name == "pet_only_pps_record_count" ~ "PPS records",
        TRUE ~ .data$variable_name
      )
    )
}

#' Extract PET-only HIP category breakdown from summarised result.
extract_pet_only_hip_categories_from_sr <- function(sr) {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  rows <- tbl %>%
    dplyr::filter(.data$variable_name == "pet_only_hip_category") %>%
    dplyr::select("cdm_name", "variable_level", "estimate_name", "estimate_value")
  if (nrow(rows) == 0) return(NULL)
  take_first <- function(x) { x <- unlist(x, use.names = FALSE); if (length(x) == 0) NA else x[1] }
  rows %>%
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value", values_fn = take_first) %>%
    dplyr::mutate(
      n = suppressWarnings(as.numeric(.data$n)),
      pct = suppressWarnings(as.numeric(.data$pct))
    ) %>%
    dplyr::rename(category = "variable_level")
}

#' Extract delivery mode summary from summarised result.
#' Returns a data.frame with columns: cdm_name, group, period, n, n_cesarean, n_vaginal, n_known, pct_cesarean, pct_vaginal
extract_delivery_mode_from_sr <- function(sr) {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  rows <- tbl %>%
    dplyr::filter(.data$variable_name == "delivery_mode") %>%
    dplyr::select("cdm_name", "variable_level", "estimate_name", "estimate_value")
  if (nrow(rows) == 0) return(NULL)
  take_first <- function(x) { x <- unlist(x, use.names = FALSE); if (length(x) == 0) NA else x[1] }
  wide <- rows %>%
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value", values_fn = take_first) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("n", "n_cesarean", "n_vaginal", "n_known", "pct_cesarean", "pct_vaginal")),
                                ~ suppressWarnings(as.numeric(.x))))
  # Parse "group:period" from variable_level
  wide %>%
    dplyr::mutate(
      group_raw = sub(":.*$", "", .data$variable_level),
      period = sub("^[^:]+:", "", .data$variable_level),
      group = dplyr::case_when(
        .data$group_raw == "matched" ~ "Matched",
        .data$group_raw == "algorithm_only" ~ "HIPPS only",
        TRUE ~ .data$group_raw
      )
    ) %>%
    dplyr::select("cdm_name", "group", "period", dplyr::any_of(c("n", "n_cesarean", "n_vaginal", "n_known", "pct_cesarean", "pct_vaginal")))
}

#' Extract unmatched PET boundary metrics (first/last year of study period).
#' Returns a data.frame with columns: cdm_name, variable_level, n, pct, boundary_date
extract_pet_only_boundary_metrics_from_sr <- function(sr) {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  rows <- tbl %>%
    dplyr::filter(.data$variable_name == "pet_only_boundary_metrics") %>%
    dplyr::select("cdm_name", "variable_level", "estimate_name", "estimate_value")
  if (nrow(rows) == 0) return(NULL)
  take_first <- function(x) { x <- unlist(x, use.names = FALSE); if (length(x) == 0) NA else x[1] }
  rows %>%
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value", values_fn = take_first) %>%
    dplyr::mutate(
      n = suppressWarnings(as.numeric(.data$n)),
      pct = suppressWarnings(as.numeric(.data$pct))
    )
}

# ---- Main PET comparison module ----
petComparisonUI <- function(id) {
  ns <- NS(id)

  # Pre-compute data availability for conditional UI
  has_venn <- FALSE
  has_person_venn <- FALSE
  has_alignment <- FALSE
  cdm_names <- character(0)

  if (exists("petComparisonSummarisedResult") && !is.null(petComparisonSummarisedResult) &&
      nrow(petComparisonSummarisedResult) > 0) {
    venn_data <- extract_venn_data_from_sr(petComparisonSummarisedResult, "venn_counts", "n_episodes")
    has_venn <- !is.null(venn_data) && nrow(venn_data) > 0
    person_venn_data <- extract_venn_data_from_sr(petComparisonSummarisedResult, "person_venn_counts", "n_persons")
    has_person_venn <- !is.null(person_venn_data) && nrow(person_venn_data) > 0
    alignment_data <- extract_date_diff_distribution_from_sr(petComparisonSummarisedResult, "date_difference_distribution")
    has_alignment <- !is.null(alignment_data) && nrow(alignment_data) > 0
    gest_data <- extract_gestational_time_from_sr(petComparisonSummarisedResult)
    outcome_data <- extract_group_outcome_from_sr(petComparisonSummarisedResult)
    has_unmatched <- (!is.null(gest_data) && nrow(gest_data) > 0) ||
                     (!is.null(outcome_data) && nrow(outcome_data) > 0)
    tbl <- as.data.frame(petComparisonSummarisedResult)
    if ("cdm_name" %in% names(tbl)) cdm_names <- unique(tbl$cdm_name)
  }

  db_choices <- stats::setNames(cdm_names, cdm_names)

  # Overview tab
  overview_ui <- tagList(
    tags$style(paste(
      ".pet-comparison-overview h2 { margin-top: 1.2em; margin-bottom: 0.5em; font-size: 1.25em; }",
      ".pet-comparison-overview h2:first-child { margin-top: 0; }",
      ".pet-comparison-overview h3 { margin-top: 1em; margin-bottom: 0.4em; font-size: 1.1em; }",
      ".pet-comparison-overview p { margin-bottom: 0.8em; line-height: 1.5; }",
      ".pet-comparison-overview ul { margin-bottom: 0.8em; padding-left: 1.5em; }",
      ".pet-comparison-overview hr { margin: 1.5em 0; border: 0; border-top: 1px solid #ddd; }",
      ".pet-comparison-overview table { border-collapse: collapse; width: 100%; margin: 1em 0; }",
      ".pet-comparison-overview th, .pet-comparison-overview td { border: 1px solid #ddd; padding: 0.5em 0.75em; text-align: left; }",
      ".pet-comparison-overview th { background: #f5f5f5; font-weight: 600; }",
      sep = " "
    )),
    div(
      class = "pet-comparison-overview",
      style = "max-width: 900px; margin-bottom: 2em; line-height: 1.5; color: #333;",
      HTML(markdown::markdownToHTML(text = PET_COMPARISON_OVERVIEW_MD, fragment.only = TRUE))
    )
  )

  # Plot tab
  plot_ui <- if (has_venn) {
    tagList(
      if (length(cdm_names) > 1) {
        fluidRow(
          column(3, selectInput(ns("database"), "Database",
                               choices = db_choices, selected = db_choices[1]))
        )
      },
      fluidRow(
        column(12, amVennDiagram5::amVennDiagramOutput(ns("venn"), width = "100%", height = "450px") %>% withSpinner())
      )
    )
  } else {
    p("No Venn data available.")
  }

  # Person-level tab
  person_ui <- if (has_person_venn) {
    tagList(
      if (length(cdm_names) > 1) {
        fluidRow(
          column(3, selectInput(ns("person_database"), "Database",
                               choices = db_choices, selected = db_choices[1]))
        )
      },
      h4("Person-level Venn diagram"),
      p("Persons grouped by whether they have episodes in both HIPPS and PET, HIPPS only, or PET only."),
      fluidRow(
        column(12, amVennDiagram5::amVennDiagramOutput(ns("person_venn"), width = "100%", height = "400px") %>% withSpinner())
      ),
      hr(),
      h4("Episodes per person by group"),
      DT::DTOutput(ns("person_epp_table")) %>% withSpinner(),
      downloadButton(ns("download_person_epp_csv"), "Download table (.csv)")
    )
  } else {
    p("No person-level comparison data available. Re-run the PET comparison to generate this data.")
  }

  # Table tab
  table_ui <- tagList(
    fluidRow(
      column(3, pickerInput(ns("tableCdm"), "Database",
                           choices = cdm_names, selected = cdm_names,
                           multiple = TRUE, options = opt))
    ),
    br(),
    gt::gt_output(ns("visTable")) %>% withSpinner(),
    downloadButton(ns("download_vis_docx"), "Download table (.docx)")
  )

  # Alignment tab
  alignment_ui <- if (has_alignment) {
    tagList(
      div(class = "tab-help-text",
          "Distribution of date differences (PET \u2212 Algorithm) for matched episode pairs.",
          " Bars centred at 0 indicate perfect alignment; negative = algorithm date is later",
          " than PET; positive = algorithm date is earlier than PET."),
      fluidRow(
        column(3, if (length(cdm_names) > 1) {
          selectInput(ns("align_database"), "Database",
                      choices = db_choices, selected = db_choices[1])
        }),
        column(3, selectInput(ns("align_measure"), "Measure",
                              choices = c("Start date" = "start_diff",
                                          "End date" = "end_diff",
                                          "Duration" = "duration_diff",
                                          "All (faceted)" = "all"),
                              selected = "all")),
        column(3, checkboxInput(ns("align_by_outcome"), "Stratify by outcome", value = FALSE))
      ),
      plotly::plotlyOutput(ns("alignmentPlot"), height = "520px") %>% withSpinner(),
      h4("Download figure"),
      fluidRow(
        column(3, textInput(ns("align_download_height"), "Height (cm)", value = "14")),
        column(3, textInput(ns("align_download_width"), "Width (cm)", value = "24")),
        column(3, textInput(ns("align_download_dpi"), "Resolution (dpi)", value = "300"))
      ),
      downloadButton(ns("download_alignment_plot"), "Download plot (PNG)")
    )
  } else {
    p("No episode alignment data available. Re-run the PET comparison (v3.0.6+) to generate alignment distributions.")
  }

  # Unmatched characterization tab
  unmatched_ui <- if (has_unmatched) {
    tagList(
      div(class = "tab-help-text",
          "Characterize episodes that exist in only one source (HIPPS-only or PET-only).",
          " Compare gestational length, outcome distribution, and HIP/PPS concept",
          " presence in unmatched PET episodes."),
      if (length(cdm_names) > 1) {
        fluidRow(
          column(3, selectInput(ns("unmatched_database"), "Database",
                                choices = db_choices, selected = db_choices[1]))
        )
      },
      h4("Gestational length by matching group"),
      p("Duration in days from episode start to end, summarised for matched, HIPPS-only, and PET-only episodes."),
      DT::DTOutput(ns("gest_table")) %>% withSpinner(),
      hr(),
      h4("Outcome distribution by matching group"),
      plotly::plotlyOutput(ns("outcome_plot"), height = "450px") %>% withSpinner(),
      DT::DTOutput(ns("outcome_table")) %>% withSpinner(),
      hr(),
      h4("Delivery mode by matching group"),
      p("Cesarean vs vaginal delivery mode for matched and HIPPS-only algorithm episodes, overall and by year."),
      uiOutput(ns("delivery_mode_ui")),
      hr(),
      h4("Study period boundary analysis of unmatched PET episodes"),
      p("Proportion of unmatched PET episodes near the study period boundaries, which may indicate episodes that lack algorithm coverage due to data truncation."),
      uiOutput(ns("boundary_metrics_ui")),
      hr(),
      h4("HIP/PPS concept presence in unmatched PET episodes"),
      p("How many PET-only episodes contain HIP (pregnancy record) or PPS (pregnancy-related condition) concepts within their date range."),
      uiOutput(ns("concept_coverage_ui")),
      hr(),
      h4("HIP category breakdown in unmatched PET episodes"),
      uiOutput(ns("hip_category_ui"))
    )
  } else {
    p("No unmatched characterization data available. Re-run the PET comparison to generate this data.")
  }

  # Unmatched LSC tab
  has_lsc <- exists("petUnmatchedLsc") && is.data.frame(petUnmatchedLsc) && nrow(petUnmatchedLsc) > 0
  lsc_ui <- if (has_lsc) {
    lsc_cdm_names <- unique(as.data.frame(petUnmatchedLsc)$cdm_name)
    tagList(
      div(class = "tab-help-text",
          "Large-scale characteristics of unmatched PET episodes (conditions, drugs, procedures,",
          " observations, measurements) in the 365 days prior to the pregnancy end date."),
      if (length(lsc_cdm_names) > 1) {
        fluidRow(
          column(3, selectInput(ns("lsc_database"), "Database",
                                choices = stats::setNames(lsc_cdm_names, lsc_cdm_names),
                                selected = lsc_cdm_names[1]))
        )
      },
      DT::DTOutput(ns("lsc_table")) %>% withSpinner(),
      downloadButton(ns("download_lsc_csv"), "Download table (.csv)")
    )
  } else {
    p("No large-scale characteristics data available. Re-run the PET comparison with a CDM connection to generate this data.")
  }

  # Summary table tab (cross-database overview)
  summary_table_ui <- tagList(
    div(class = "tab-help-text",
        "Cross-database summary of PET comparison metrics: episode counts, sensitivity,",
        " PPV, date alignment, and outcome agreement."),
    DT::DTOutput(ns("petSummaryTable")) %>% withSpinner(),
    downloadButton(ns("download_summary_csv"), "Download table (.csv)")
  )

  # Summarised result tab
  summarised_ui <- tagList(
    DT::DTOutput(ns("rawTable")) %>% withSpinner(),
    downloadButton(ns("download_raw_csv"), "Download table (.csv)")
  )

  tabsetPanel(
    id = ns("petTabs"),
    type = "tabs",
    tabPanel("Overview", overview_ui),
    tabPanel("Person-level agreement", person_ui),
    tabPanel("Episode level agreement", plot_ui),
    tabPanel("Alignment of episodes", alignment_ui),
    tabPanel("Unmatched characterization", unmatched_ui),
    tabPanel("Unmatched LSC", lsc_ui),
    tabPanel("Summary table", summary_table_ui),
    tabPanel("Table of metrics", table_ui),
    tabPanel("Raw results", summarised_ui)
  )
}

petComparisonServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    result <- reactive({
      r <- rv$petComparisonSummarisedResult
      if (is.null(r)) return(NULL)
      # Ensure result is a proper summarised_result (CSV load yields plain tibble)
      if (!inherits(r, "summarised_result")) {
        r <- tryCatch(
          omopgenerics::newSummarisedResult(tibble::as_tibble(r)),
          error = function(e) r
        )
      }
      r
    })

    display_result <- reactive({
      r <- result()
      if (is.null(r)) return(NULL)
      # Remap labels for display only (exports keep original labels)
      # Preserve summarised_result class through remap (dplyr/as.data.frame strips it)
      sr_class <- class(r)
      sr_settings <- if (inherits(r, "summarised_result")) omopgenerics::settings(r) else NULL
      dr <- remap_pet_display_labels(as.data.frame(r))
      dr <- tibble::as_tibble(dr)
      class(dr) <- sr_class
      if (!is.null(sr_settings)) attr(dr, "settings") <- sr_settings
      dr
    })

    cdm_names <- reactive({
      dr <- display_result()
      if (is.null(dr)) return(character(0))
      tbl <- as.data.frame(dr)
      if (is.data.frame(tbl) && "cdm_name" %in% names(tbl)) unique(tbl$cdm_name) else character(0)
    })

    venn_data <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_venn_data_from_sr(r, "venn_counts", "n_episodes") })
    person_venn_data <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_venn_data_from_sr(r, "person_venn_counts", "n_persons") })
    person_epp_data <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_person_epp_from_sr(r) })

    # Plot: Venn
    chosen_db <- reactive({
      cn <- cdm_names()
      if (length(cn) == 1) cn[1] else input$database
    })
    venn_plot_data <- reactive({
      vd <- venn_data()
      if (is.null(vd) || nrow(vd) == 0) return(NULL)
      db <- chosen_db()
      if (is.null(db)) return(NULL)
      row <- vd %>% dplyr::filter(.data$cdm_name == .env$db)
      if (nrow(row) == 0) return(NULL)
      pet_venn_data_from_counts(row$both[1], row$pet_only[1], row$algorithm_only[1])
    })
    output$venn <- amVennDiagram5::renderAmVennDiagram({
      vd <- venn_plot_data()
      if (is.null(vd)) return(NULL)
      amVennDiagram5::amVennDiagram(vd, theme = "default", legendPosition = "bottom")
    })

    # Person-level: Venn + EPP table
    chosen_person_db <- reactive({
      cn <- cdm_names()
      if (length(cn) == 1) cn[1] else input$person_database
    })
    person_venn_plot_data <- reactive({
      pvd <- person_venn_data()
      if (is.null(pvd) || nrow(pvd) == 0) return(NULL)
      db <- chosen_person_db()
      if (is.null(db)) return(NULL)
      row <- pvd %>% dplyr::filter(.data$cdm_name == .env$db)
      if (nrow(row) == 0) return(NULL)
      pet_venn_data_from_counts(row$both[1], row$pet_only[1], row$algorithm_only[1])
    })
    output$person_venn <- amVennDiagram5::renderAmVennDiagram({
      vd <- person_venn_plot_data()
      if (is.null(vd)) return(NULL)
      amVennDiagram5::amVennDiagram(vd, theme = "default", legendPosition = "bottom")
    })
    personEppTableData <- reactive({
      db <- chosen_person_db()
      ped <- person_epp_data()
      if (is.null(ped) || is.null(db)) return(NULL)
      ped %>%
        dplyr::filter(.data$cdm_name == .env$db) %>%
        dplyr::select("group", dplyr::any_of(c("mean", "median", "sd", "min", "q25", "q75", "max"))) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2)))
    })
    output$person_epp_table <- DT::renderDT({
      epp_tbl <- personEppTableData()
      if (is.null(epp_tbl) || nrow(epp_tbl) == 0) return(NULL)
      DT::datatable(epp_tbl, rownames = FALSE, options = list(dom = "t", pageLength = 10))
    })
    output$download_person_epp_csv <- downloadHandler(
      filename = function() { "pet_person_episodes_per_person.csv" },
      content = function(file) {
        d <- personEppTableData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )

    # Alignment: butterfly histogram of date difference distributions
    alignment_overall <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_date_diff_distribution_from_sr(r, "date_difference_distribution") })
    alignment_by_outcome <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_date_diff_distribution_from_sr(r, "date_difference_distribution_by_outcome") })

    chosen_align_db <- reactive({
      cn <- cdm_names()
      if (length(cn) == 1) cn[1] else input$align_database
    })

    alignment_ggplot <- reactive({
      db <- chosen_align_db()
      by_outcome <- isTRUE(input$align_by_outcome)
      measure_filter <- input$align_measure

      src <- if (by_outcome && !is.null(alignment_by_outcome())) {
        alignment_by_outcome()
      } else if (!is.null(alignment_overall())) {
        alignment_overall()
      } else {
        return(NULL)
      }

      if (!is.null(db)) src <- src %>% dplyr::filter(.data$cdm_name == .env$db)
      if (nrow(src) == 0) return(NULL)

      # Filter to selected measure(s)
      if (!is.null(measure_filter) && measure_filter != "all") {
        src <- src %>% dplyr::filter(.data$measure == .env$measure_filter)
      }

      if (nrow(src) == 0) return(NULL)

      # Compute percentage within each measure (and outcome if present)
      group_cols <- c("measure_label")
      if ("outcome" %in% names(src) && by_outcome) group_cols <- c(group_cols, "outcome")

      src <- src %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::mutate(
          total = sum(.data$n, na.rm = TRUE),
          pct = dplyr::if_else(.data$total > 0, round(100 * .data$n / .data$total, 1), 0)
        ) %>%
        dplyr::ungroup()

      # Colour palette: gradient from red (far from 0) through yellow to green (at 0)
      bin_colours <- c(
        "\u2264 -30"    = "#d73027",
        "-29 to -15" = "#f46d43",
        "-14 to -8"  = "#fdae61",
        "-7 to -1"   = "#fee08b",
        "0"          = "#66bd63",
        "1 to 7"     = "#fee08b",
        "8 to 14"    = "#fdae61",
        "15 to 29"   = "#f46d43",
        "\u2265 30"     = "#d73027"
      )

      p <- ggplot2::ggplot(src, ggplot2::aes(
        x = .data$bin, y = .data$pct, fill = .data$bin,
        text = paste0(.data$measure_label,
                      if ("outcome" %in% names(src) && by_outcome) paste0(" [", .data$outcome, "]") else "",
                      "\nBin: ", .data$bin,
                      "\nN: ", format(.data$n, big.mark = ","),
                      "\nDenom: ", format(.data$total, big.mark = ","),
                      "\n", round(.data$pct, 1), "%")
      )) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = bin_colours, drop = FALSE) +
        ggplot2::labs(
          x = "Date difference (PET \u2212 Algorithm, days)",
          y = "Percentage of matched episodes (%)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
          strip.text = ggplot2::element_text(face = "bold", size = 10),
          panel.grid.minor = ggplot2::element_blank()
        )

      # Faceting
      if (measure_filter == "all" && by_outcome && "outcome" %in% names(src)) {
        p <- p + ggplot2::facet_grid(outcome ~ measure_label)
      } else if (measure_filter == "all") {
        p <- p + ggplot2::facet_wrap(~ measure_label, ncol = 1)
      } else if (by_outcome && "outcome" %in% names(src)) {
        p <- p + ggplot2::facet_wrap(~ outcome, ncol = 2)
      }

      p
    })

    output$alignmentPlot <- plotly::renderPlotly({
      p <- alignment_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No alignment data available for selected filters."))
      plotly::ggplotly(p, tooltip = "text")
    })

    output$download_alignment_plot <- downloadHandler(
      filename = function() { "pet_alignment_plot.png" },
      content = function(file) {
        p <- alignment_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = as.numeric(input$align_download_width),
            height = as.numeric(input$align_download_height),
            dpi = as.numeric(input$align_download_dpi),
            units = "cm"
          )
        }
      }
    )

    # Table: visOmopTable with database filter (uses display_result for renamed labels)
    filtered_result <- reactive({
      dr <- display_result()
      if (is.null(dr)) return(NULL)
      sel <- input$tableCdm
      if (is.null(sel) || length(sel) == 0) sel <- cdm_names()
      res <- dr %>% dplyr::filter(.data$cdm_name %in% sel)
      if (!inherits(res, "summarised_result")) {
        res <- tibble::as_tibble(res)
        class(res) <- class(dr)
        attr(res, "settings") <- attr(dr, "settings")
      }
      res
    })

    vis_table_gt <- reactive({
      req(filtered_result())
      sr <- filtered_result()
      if (nrow(as.data.frame(sr)) == 0) {
        return(gt::gt(tibble::tibble(Message = "No data for selected database(s).")))
      }
      # Drop version column — not part of summarised_result spec and causes visOmopTable errors
      if ("version" %in% colnames(sr)) {
        sr_class <- class(sr)
        sr_settings <- attr(sr, "settings")
        sr <- sr[, colnames(sr) != "version", drop = FALSE]
        class(sr) <- sr_class
        if (!is.null(sr_settings)) attr(sr, "settings") <- sr_settings
      }
      tryCatch(
        visOmopResults::visOmopTable(
          result = sr,
          header = c("cdm_name"),
          groupColumn = c("variable_name", "variable_level"),
          rename = c("Database" = "cdm_name"),
          type = "gt"
        ),
        error = function(e) {
          gt::gt(tibble::tibble(Message = paste("Error rendering table:", e$message)))
        }
      )
    })
    output$visTable <- gt::render_gt({
      vis_table_gt()
    })
    # Summary table (cross-database overview)
    pet_summary_data <- reactive({
      r <- result()
      if (is.null(r)) return(NULL)
      extract_pet_summary_table(r)
    })
    pet_summary_display <- reactive({
      d <- pet_summary_data()
      if (is.null(d)) return(NULL)
      format_pet_summary_table(d)
    })
    output$petSummaryTable <- DT::renderDT({
      d <- pet_summary_display()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      DT::datatable(d, rownames = FALSE,
                    options = list(dom = "t", pageLength = 25, scrollX = TRUE),
                    class = "compact stripe")
    })
    output$download_summary_csv <- downloadHandler(
      filename = function() { "pet_comparison_summary.csv" },
      content = function(file) {
        d <- pet_summary_display()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )

    output$download_vis_docx <- downloadHandler(
      filename = function() { "pet_comparison_table.docx" },
      content = function(file) {
        tbl <- vis_table_gt()
        if (!is.null(tbl)) gt::gtsave(tbl, file)
      }
    )

    # Unmatched characterization
    gest_data <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_gestational_time_from_sr(r) })
    outcome_data <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_group_outcome_from_sr(r) })
    concept_coverage_data <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_pet_only_concept_coverage_from_sr(r) })
    record_count_data <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_pet_only_record_counts_from_sr(r) })
    hip_category_data <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_pet_only_hip_categories_from_sr(r) })

    chosen_unmatched_db <- reactive({
      cn <- cdm_names()
      if (length(cn) == 1) cn[1] else input$unmatched_database
    })

    # Gestational length table
    output$gest_table <- DT::renderDT({
      db <- chosen_unmatched_db()
      if (is.null(gest_data()) || is.null(db)) return(NULL)
      d <- gest_data() %>%
        dplyr::filter(.data$cdm_name == .env$db) %>%
        dplyr::select("group", dplyr::any_of(c("n", "mean", "median", "sd", "min", "q25", "q75", "max"))) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 1)))
      if (nrow(d) == 0) return(NULL)
      DT::datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 10),
                    colnames = c("Group", "N", "Mean (days)", "Median", "SD", "Min", "Q25", "Q75", "Max"))
    })

    # Outcome distribution plot
    output$outcome_plot <- plotly::renderPlotly({
      db <- chosen_unmatched_db()
      if (is.null(outcome_data()) || is.null(db)) {
        return(emptyPlotlyMessage("No outcome data available."))
      }
      d <- outcome_data() %>% dplyr::filter(.data$cdm_name == .env$db)
      if (nrow(d) == 0) return(emptyPlotlyMessage("No outcome data for selected database."))

      d$group <- factor(d$group, levels = c("Matched", "HIPPS only", "PET only"))

      p <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$group, y = .data$pct, fill = .data$outcome,
        text = paste0(.data$group, " - ", .data$outcome,
                      "\nN: ", format(round(.data$n), big.mark = ","),
                      "\n", round(.data$pct, 1), "%")
      )) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::labs(x = NULL, y = "Percentage (%)", fill = "Outcome") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 11),
          legend.position = "right"
        )
      plotly::ggplotly(p, tooltip = "text")
    })

    # Outcome distribution table
    output$outcome_table <- DT::renderDT({
      db <- chosen_unmatched_db()
      if (is.null(outcome_data()) || is.null(db)) return(NULL)
      d <- outcome_data() %>%
        dplyr::filter(.data$cdm_name == .env$db) %>%
        dplyr::mutate(
          n = format(round(.data$n), big.mark = ","),
          pct = round(.data$pct, 1)
        ) %>%
        dplyr::select("group", "outcome", "n", "pct")
      if (nrow(d) == 0) return(NULL)
      DT::datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 30),
                    colnames = c("Group", "Outcome", "N", "%"))
    })

    # Delivery mode summary
    delivery_mode_data <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_delivery_mode_from_sr(r) })

    output$delivery_mode_ui <- renderUI({
      db <- chosen_unmatched_db()
      if (is.null(delivery_mode_data()) || is.null(db)) {
        return(div(
          style = "padding: 12px 16px; background: #F3F4F6; border: 1px solid #D1D5DB; border-radius: 6px; margin: 10px 0;",
          "No delivery mode data available. The algorithm output may not include delivery mode flags."
        ))
      }
      d <- delivery_mode_data() %>% dplyr::filter(.data$cdm_name == .env$db)
      if (nrow(d) == 0) {
        return(div(
          style = "padding: 12px 16px; background: #F3F4F6; border: 1px solid #D1D5DB; border-radius: 6px; margin: 10px 0;",
          "No delivery mode data for selected database."
        ))
      }

      # Overall summary table
      overall <- d %>%
        dplyr::filter(.data$period == "overall") %>%
        dplyr::mutate(
          n = format(round(.data$n), big.mark = ","),
          n_cesarean = format(round(.data$n_cesarean), big.mark = ","),
          n_vaginal = format(round(.data$n_vaginal), big.mark = ","),
          n_known = format(round(.data$n_known), big.mark = ","),
          pct_cesarean = paste0(round(.data$pct_cesarean, 1), "%"),
          pct_vaginal = paste0(round(.data$pct_vaginal, 1), "%")
        ) %>%
        dplyr::select("group", "n", "n_cesarean", "pct_cesarean", "n_vaginal", "pct_vaginal", "n_known")

      # By-year table
      by_year <- d %>%
        dplyr::filter(.data$period != "overall") %>%
        dplyr::arrange(.data$group, .data$period) %>%
        dplyr::mutate(
          n = format(round(.data$n), big.mark = ","),
          n_cesarean = format(round(.data$n_cesarean), big.mark = ","),
          n_vaginal = format(round(.data$n_vaginal), big.mark = ","),
          n_known = format(round(.data$n_known), big.mark = ","),
          pct_cesarean = paste0(round(.data$pct_cesarean, 1), "%"),
          pct_vaginal = paste0(round(.data$pct_vaginal, 1), "%")
        ) %>%
        dplyr::select("group", year = "period", "n", "n_cesarean", "pct_cesarean", "n_vaginal", "pct_vaginal", "n_known")

      tagList(
        tags$h5("Overall"),
        DT::renderDT({
          DT::datatable(overall, rownames = FALSE, options = list(dom = "t", pageLength = 10),
                        colnames = c("Group", "N episodes", "Cesarean", "% Cesarean", "Vaginal", "% Vaginal", "N known"))
        }),
        if (nrow(by_year) > 0) tagList(
          tags$h5("By year"),
          DT::renderDT({
            DT::datatable(by_year, rownames = FALSE,
                          options = list(dom = "t", pageLength = 50, scrollX = TRUE),
                          colnames = c("Group", "Year", "N episodes", "Cesarean", "% Cesarean", "Vaginal", "% Vaginal", "N known"))
          })
        )
      )
    })

    # Boundary metrics for unmatched PET episodes
    boundary_metrics_data <- reactive({ r <- result(); if (is.null(r)) return(NULL); extract_pet_only_boundary_metrics_from_sr(r) })

    output$boundary_metrics_ui <- renderUI({
      db <- chosen_unmatched_db()
      if (is.null(boundary_metrics_data()) || is.null(db)) {
        return(div(
          style = "padding: 12px 16px; background: #F3F4F6; border: 1px solid #D1D5DB; border-radius: 6px; margin: 10px 0;",
          "No boundary metrics available. Re-run the PET comparison with ",
          tags$code("startDate"), " and ", tags$code("endDate"), " parameters to generate this data."
        ))
      }
      d <- boundary_metrics_data() %>% dplyr::filter(.data$cdm_name == .env$db)
      if (nrow(d) == 0) {
        return(div(
          style = "padding: 12px 16px; background: #F3F4F6; border: 1px solid #D1D5DB; border-radius: 6px; margin: 10px 0;",
          "No boundary metrics for selected database."
        ))
      }

      first_year <- d %>% dplyr::filter(.data$variable_level == "first_year_ending")
      last_year <- d %>% dplyr::filter(.data$variable_level == "last_year_starting")

      fmtN <- function(x) {
        if (is.na(x) || x == 0) return(tags$span(style = "color: #6b7280;", "0"))
        format(round(x), big.mark = ",")
      }
      fmtPct <- function(x) {
        if (is.na(x)) return("--")
        paste0(round(x, 1), "%")
      }

      metricBox <- function(label, description, n_val, pct_val, boundary_date, color) {
        tags$div(
          style = "text-align:center; flex:1; min-width:200px; padding: 16px; background: #f9fafb; border-radius: 8px; border: 1px solid #e5e7eb;",
          tags$p(style = "color:#6b7280; margin:0; font-size:0.85em; font-weight:600;", label),
          tags$p(style = "color:#9ca3af; margin:2px 0 8px; font-size:0.8em;", description),
          tags$h4(style = paste0("margin:4px 0; color:", color, ";"), fmtN(n_val)),
          tags$p(style = "margin:0; font-size:0.9em;", fmtPct(pct_val)),
          if (!is.na(boundary_date)) tags$p(style = "color:#9ca3af; margin:4px 0 0; font-size:0.75em;",
                                            paste0("Boundary: ", boundary_date))
        )
      }

      first_n <- if (nrow(first_year) > 0) first_year$n[1] else NA_real_
      first_pct <- if (nrow(first_year) > 0) first_year$pct[1] else NA_real_
      first_bd <- if (nrow(first_year) > 0 && "boundary_date" %in% names(first_year)) first_year$boundary_date[1] else NA_character_
      last_n <- if (nrow(last_year) > 0) last_year$n[1] else NA_real_
      last_pct <- if (nrow(last_year) > 0) last_year$pct[1] else NA_real_
      last_bd <- if (nrow(last_year) > 0 && "boundary_date" %in% names(last_year)) last_year$boundary_date[1] else NA_character_

      tagList(
        div(
          style = "display:flex; flex-wrap:wrap; gap:16px; margin: 12px 0;",
          metricBox(
            "Ending in first year",
            "Episodes ending within 1 year of study start",
            first_n, first_pct, first_bd,
            if (isTRUE(first_pct > 20)) "#dc2626" else if (isTRUE(first_pct > 10)) "#d97706" else "#16a34a"
          ),
          metricBox(
            "Starting in last year",
            "Episodes starting within 1 year of study end",
            last_n, last_pct, last_bd,
            if (isTRUE(last_pct > 20)) "#dc2626" else if (isTRUE(last_pct > 10)) "#d97706" else "#16a34a"
          )
        ),
        div(
          style = "padding: 8px 12px; background: #EFF6FF; border: 1px solid #BFDBFE; border-radius: 6px; margin: 8px 0; font-size: 0.85em; color: #1E40AF;",
          "High proportions near study boundaries may indicate that unmatched PET episodes lack ",
          "algorithm coverage due to data truncation rather than true algorithm misses."
        )
      )
    })

    # Concept coverage in PET-only episodes
    output$concept_coverage_ui <- renderUI({
      db <- chosen_unmatched_db()
      if (is.null(concept_coverage_data()) || is.null(db)) {
        return(div(
          style = "padding: 12px 16px; background: #FEF9C3; border: 1px solid #FDE68A; border-radius: 6px; margin: 10px 0;",
          tags$strong("No HIP/PPS concept coverage data available."),
          " Re-run the PET comparison to generate this data."
        ))
      }
      d <- concept_coverage_data() %>% dplyr::filter(.data$cdm_name == .env$db)
      if (nrow(d) == 0) {
        return(div(
          style = "padding: 12px 16px; background: #FEF9C3; border: 1px solid #FDE68A; border-radius: 6px; margin: 10px 0;",
          "No concept coverage data for selected database."
        ))
      }

      # Extract values
      get_val <- function(var_name, est_name) {
        row <- d %>% dplyr::filter(.data$variable_name == var_name, .data$estimate_name == est_name)
        if (nrow(row) == 0) return(NA_real_)
        row$estimate_value[1]
      }
      n_total     <- get_val("pet_only_hip_coverage", "n_total")
      n_with_hip  <- get_val("pet_only_hip_coverage", "n_with_hip")
      pct_hip     <- get_val("pet_only_hip_coverage", "pct_with_hip")
      n_with_pps  <- get_val("pet_only_pps_coverage", "n_with_pps")
      pct_pps     <- get_val("pet_only_pps_coverage", "pct_with_pps")
      n_with_any  <- get_val("pet_only_any_record_coverage", "n_with_any")
      pct_any     <- get_val("pet_only_any_record_coverage", "pct_with_any")

      fmtN <- function(x) {
        if (is.na(x) || x == 0) return(tags$span(style = "color: #6b7280;", "0"))
        format(round(x), big.mark = ",")
      }
      fmtPct <- function(x) {
        if (is.na(x)) return("--")
        paste0(round(x, 1), "%")
      }

      no_concepts <- isTRUE(!is.na(pct_any) && pct_any == 0)

      if (no_concepts) {
        return(div(
          style = "padding: 16px 20px; background: #FEF3C7; border: 1px solid #F59E0B; border-radius: 8px; margin: 10px 0;",
          tags$h5(style = "margin-top: 0; color: #92400E;", "No HIP or PPS concepts found"),
          p(style = "margin-bottom: 0; color: #78350F;",
            "None of the ", tags$strong(fmtN(n_total)), " unmatched PET-only episodes contained any ",
            "HIP (pregnancy record) or PPS (pregnancy-related condition) concepts within their date range. ",
            "This means these PET episodes are derived from data sources or concept domains that the ",
            "PregnancyIdentifier algorithm does not currently use for episode detection.")
        ))
      }

      metricBox <- function(label, n_val, pct_val, color) {
        tags$div(
          style = "text-align:center; flex:1; min-width:150px; padding: 12px; background: #f9fafb; border-radius: 8px; border: 1px solid #e5e7eb;",
          tags$p(style = "color:#6b7280; margin:0; font-size:0.85em;", label),
          tags$h4(style = paste0("margin:4px 0; color:", color, ";"), fmtN(n_val)),
          tags$p(style = "margin:0; font-size:0.9em;", fmtPct(pct_val))
        )
      }

      tagList(
        p(tags$strong(fmtN(n_total)), " total unmatched PET-only episodes examined."),
        div(
          style = "display:flex; flex-wrap:wrap; gap:12px; margin: 12px 0;",
          metricBox("With HIP records", n_with_hip, pct_hip, if (isTRUE(pct_hip > 0)) "#16a34a" else "#6b7280"),
          metricBox("With PPS records", n_with_pps, pct_pps, if (isTRUE(pct_pps > 0)) "#16a34a" else "#6b7280"),
          metricBox("With any HIP/PPS", n_with_any, pct_any, if (isTRUE(pct_any > 0)) "#16a34a" else "#6b7280")
        )
      )
    })

    # HIP category breakdown
    output$hip_category_ui <- renderUI({
      db <- chosen_unmatched_db()

      # Check coverage first - if no concepts, show message
      if (!is.null(concept_coverage_data())) {
        cd <- concept_coverage_data() %>% dplyr::filter(.data$cdm_name == .env$db)
        pct_any_row <- cd %>% dplyr::filter(.data$variable_name == "pet_only_any_record_coverage",
                                             .data$estimate_name == "pct_with_any")
        if (nrow(pct_any_row) > 0 && isTRUE(pct_any_row$estimate_value[1] == 0)) {
          return(div(
            style = "padding: 12px 16px; background: #F3F4F6; border: 1px solid #D1D5DB; border-radius: 6px; margin: 10px 0;",
            "No HIP categories to display - no HIP/PPS concepts were found in unmatched PET episodes."
          ))
        }
      }

      if (is.null(hip_category_data())) {
        return(div(
          style = "padding: 12px 16px; background: #F3F4F6; border: 1px solid #D1D5DB; border-radius: 6px; margin: 10px 0;",
          "No HIP category breakdown data available."
        ))
      }
      d <- hip_category_data() %>% dplyr::filter(.data$cdm_name == .env$db)
      if (nrow(d) == 0) {
        return(div(
          style = "padding: 12px 16px; background: #F3F4F6; border: 1px solid #D1D5DB; border-radius: 6px; margin: 10px 0;",
          "No HIP category data for selected database."
        ))
      }

      d <- d %>%
        dplyr::arrange(dplyr::desc(.data$n)) %>%
        dplyr::mutate(
          n_fmt = format(round(.data$n), big.mark = ","),
          pct_fmt = paste0(round(.data$pct, 1), "%")
        )

      tagList(
        DT::renderDT({
          DT::datatable(
            d %>% dplyr::select("category", n = "n_fmt", pct = "pct_fmt"),
            rownames = FALSE, options = list(dom = "t", pageLength = 30),
            colnames = c("HIP Category", "N episodes", "% of PET-only")
          )
        })
      )
    })

    # Summarised result: display with renamed labels, download with original labels
    output$rawTable <- DT::renderDT({
      renderPrettyDT(as.data.frame(display_result))
    })
    output$download_raw_csv <- downloadHandler(
      filename = function() { "pet_comparison_summarised_result.csv" },
      content = function(file) {
        d <- as.data.frame(result())
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )

    # ---- Unmatched LSC tab ----
    lsc_data <- reactive({
      lsc <- rv$petUnmatchedLsc
      if (is.null(lsc) || !is.data.frame(lsc) || nrow(lsc) == 0) return(NULL)
      as.data.frame(lsc)
    })

    lsc_filtered <- reactive({
      ld <- lsc_data()
      if (is.null(ld)) return(NULL)
      lsc_cdm_names <- unique(ld$cdm_name)
      db <- if (length(lsc_cdm_names) > 1 && !is.null(input$lsc_database)) input$lsc_database else lsc_cdm_names[1]
      d <- ld %>%
          dplyr::filter(.data$cdm_name == db) %>%
          dplyr::filter(.data$estimate_value != "0") %>%
          dplyr::select(
            variable_name  = "variable_name",
            variable_level = "variable_level",
            estimate_name  = "estimate_name",
            estimate_value = "estimate_value"
          ) %>%
          dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) %>%
          dplyr::arrange(dplyr::desc(.data$estimate_value))
        d
      })

      output$lsc_table <- DT::renderDT({
        d <- lsc_filtered()
        DT::datatable(
          d,
          filter   = "top",
          rownames = FALSE,
          options  = list(
            pageLength = 25,
            scrollX    = TRUE,
            order      = list(list(3, "desc"))
          )
        ) %>%
          DT::formatRound("estimate_value", digits = 4)
      })

      output$download_lsc_csv <- downloadHandler(
        filename = function() { "pet_unmatched_lsc.csv" },
        content = function(file) {
          d <- lsc_filtered()
          if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
        }
      )
  })
}

# ---- Legacy: simple table module for multi-CSV PET comparison ----
petComparisonLegacyUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("table")) %>% withSpinner(),
    downloadButton(ns("download_table_csv"), "Download table (.csv)")
  )
}

petComparisonLegacyServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT({
      renderPrettyDT(data)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "pet_comparison.csv" },
      content = function(file) {
        if (!is.null(data) && nrow(data) > 0) readr::write_csv(data, file)
      }
    )
  })
}

# ---- Legacy container: wraps multiple PET comparison tables into tabs ----
petComparisonLegacyContainerUI <- function(id) {
  ns <- NS(id)
  tabs <- list()
  for (varName in names(petComparisonSpec)) {
    if (exists(varName, envir = .GlobalEnv)) {
      data <- get(varName, envir = .GlobalEnv)
      if (!is.null(data) && nrow(data) > 0) {
        displayName <- petComparisonSpec[[varName]]
        tabId <- gsub("[^a-zA-Z0-9]", "_", varName)
        tabs[[displayName]] <- tabPanel(displayName, petComparisonLegacyUI(ns(tabId)))
      }
    }
  }
  if (length(tabs) == 0) return(p("No PET comparison data available."))
  tagList(
    div(class = "tab-help-text", "Compare PregnancyIdentifier episodes with the OMOP Pregnancy Extension Table (PET)."),
    do.call(tabsetPanel, tabs)
  )
}

petComparisonLegacyContainerServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    for (varName in names(petComparisonSpec)) {
      if (exists(varName, envir = .GlobalEnv)) {
        data <- get(varName, envir = .GlobalEnv)
        if (!is.null(data) && nrow(data) > 0) {
          tabId <- gsub("[^a-zA-Z0-9]", "_", varName)
          local({
            d <- data
            petComparisonLegacyServer(tabId, d)
          })
        }
      }
    }
  })
}
