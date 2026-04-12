# 30-pet-summary-table.R
# ============================================================================
# PET Comparison Summary Table - Data extraction & formatting
# ============================================================================
# Builds a cross-database summary table from the PET comparison summarised_result.
# Each row = one database; columns cover episode counts, matching metrics,
# date alignment, and outcome agreement.

#' Extract the cross-database PET comparison summary table.
#'
#' @param sr A summarised_result from compareWithPET.
#' @return A data.frame with one row per cdm_name and columns matching the
#'   publication-style summary table.
extract_pet_summary_table <- function(sr) {
  if (is.null(sr) || !is.data.frame(sr) || nrow(sr) == 0) return(NULL)
  tbl <- as.data.frame(sr)

  cdm_names <- unique(tbl$cdm_name)
  if (length(cdm_names) == 0) return(NULL)

  rows <- lapply(cdm_names, function(db) {
    d <- tbl[tbl$cdm_name == db, , drop = FALSE]

    # Helper: get single estimate value
    get_val <- function(var_name, var_level, est_name) {
      r <- d[d$variable_name == var_name &
               d$variable_level == var_level &
               d$estimate_name == est_name, "estimate_value", drop = TRUE]
      if (length(r) == 0) return(NA_real_)
      suppressWarnings(as.numeric(r[1]))
    }

    # Helper: get integer estimate
    get_int <- function(var_name, var_level, est_name) {
      v <- get_val(var_name, var_level, est_name)
      if (is.na(v)) NA_integer_ else as.integer(round(v))
    }

    # --- Episode counts ---
    total_alg <- get_int("protocol_summary", "overall", "total_algorithm_episodes")
    total_pet <- get_int("protocol_summary", "overall", "total_pet_episodes")
    total_matched <- get_int("protocol_summary", "overall", "total_matched_episodes")

    # Venn counts (fallback if protocol_summary is missing)
    both <- get_int("venn_counts", "both", "n_episodes")
    pet_only <- get_int("venn_counts", "pet_only", "n_episodes")
    alg_only <- get_int("venn_counts", "algorithm_only", "n_episodes")

    if (is.na(total_matched) && !is.na(both)) total_matched <- both
    if (is.na(total_alg) && !is.na(both) && !is.na(alg_only)) total_alg <- both + alg_only
    if (is.na(total_pet) && !is.na(both) && !is.na(pet_only)) total_pet <- both + pet_only

    if (is.na(alg_only) && !is.na(total_alg) && !is.na(total_matched)) alg_only <- total_alg - total_matched
    if (is.na(pet_only) && !is.na(total_pet) && !is.na(total_matched)) pet_only <- total_pet - total_matched

    # --- Sensitivity & PPV ---
    sensitivity <- get_val("ppv_sensitivity", "sensitivity", "value")
    # Normalise to percentage (may be stored as 0-1 or 0-100)
    if (!is.na(sensitivity) && sensitivity <= 1) sensitivity <- sensitivity * 100
    ppv <- get_val("ppv_sensitivity", "ppv", "value")
    if (!is.na(ppv) && ppv <= 1) ppv <- ppv * 100

    # --- Date difference summary (start & end) ---
    # Try new descriptive labels first, fall back to legacy short labels
    start_median <- get_val("date_difference_summary",
                            "Start date difference (PET - Algorithm, days)", "median")
    if (is.na(start_median)) {
      start_median <- get_val("date_difference_summary", "start_diff_days", "median")
    }

    start_q25 <- get_val("date_difference_summary",
                         "Start date difference (PET - Algorithm, days)", "q25")
    if (is.na(start_q25)) {
      start_q25 <- get_val("date_difference_summary", "start_diff_days", "q25")
    }

    start_q75 <- get_val("date_difference_summary",
                         "Start date difference (PET - Algorithm, days)", "q75")
    if (is.na(start_q75)) {
      start_q75 <- get_val("date_difference_summary", "start_diff_days", "q75")
    }

    end_median <- get_val("date_difference_summary",
                          "End date difference (PET - Algorithm, days)", "median")
    if (is.na(end_median)) {
      end_median <- get_val("date_difference_summary", "end_diff_days", "median")
    }

    end_q25 <- get_val("date_difference_summary",
                       "End date difference (PET - Algorithm, days)", "q25")
    if (is.na(end_q25)) {
      end_q25 <- get_val("date_difference_summary", "end_diff_days", "q25")
    }

    end_q75 <- get_val("date_difference_summary",
                       "End date difference (PET - Algorithm, days)", "q75")
    if (is.na(end_q75)) {
      end_q75 <- get_val("date_difference_summary", "end_diff_days", "q75")
    }

    # --- Percentage of start/end diffs == 0 days ---
    # From date_difference_distribution: count in "0" bin / total
    dist_rows <- d[d$variable_name == "date_difference_distribution" &
                     d$estimate_name == "n", , drop = FALSE]
    pct_start_zero <- NA_real_
    pct_end_zero <- NA_real_
    if (nrow(dist_rows) > 0) {
      # Parse "measure::bin" from variable_level
      dist_rows$measure <- sub("^(.*?)::(.*?)$", "\\1", dist_rows$variable_level)
      dist_rows$bin <- sub("^(.*?)::(.*?)$", "\\2", dist_rows$variable_level)
      dist_rows$n <- suppressWarnings(as.integer(dist_rows$estimate_value))

      for (m in c("start_diff", "end_diff")) {
        m_rows <- dist_rows[dist_rows$measure == m, , drop = FALSE]
        if (nrow(m_rows) > 0) {
          total <- sum(m_rows$n, na.rm = TRUE)
          zero_n <- sum(m_rows$n[m_rows$bin == "0"], na.rm = TRUE)
          if (total > 0) {
            pct_val <- 100 * zero_n / total
            if (m == "start_diff") pct_start_zero <- pct_val
            if (m == "end_diff") pct_end_zero <- pct_val
          }
        }
      }
    }

    # --- Outcome agreement (accuracy) ---
    outcome_accuracy <- get_val("outcome_accuracy", "overall", "accuracy")
    # Normalise to percentage (may be stored as 0-1 or 0-100)
    if (!is.na(outcome_accuracy) && outcome_accuracy <= 1) outcome_accuracy <- outcome_accuracy * 100

    tibble::tibble(
      cdm_name = db,
      episodes_algorithm = total_alg,
      episodes_pet = total_pet,
      matched_episodes = total_matched,
      algorithm_only = alg_only,
      pet_only = pet_only,
      sensitivity = sensitivity,
      ppv = ppv,
      start_diff_median_iqr = format_median_iqr(start_median, start_q25, start_q75),
      pct_start_diff_zero = pct_start_zero,
      end_diff_median_iqr = format_median_iqr(end_median, end_q25, end_q75),
      pct_end_diff_zero = pct_end_zero,
      outcome_agreement = outcome_accuracy
    )
  })

  dplyr::bind_rows(rows)
}

#' Format median (IQR) string for display.
format_median_iqr <- function(median, q25, q75) {
  if (is.na(median)) return(NA_character_)
  med <- round(median, 1)
  if (is.na(q25) || is.na(q75)) return(as.character(med))
  paste0(med, " (", round(q25, 1), " to ", round(q75, 1), ")")
}

#' Format the summary table for display with pretty column names and number formatting.
format_pet_summary_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  fmt_int <- function(x) {
    ifelse(is.na(x), NA_character_, format(x, big.mark = ","))
  }
  fmt_pct <- function(x) {
    ifelse(is.na(x), NA_character_, paste0(round(x, 1), "%"))
  }

  df %>%
    dplyr::transmute(
      `Data source` = .data$cdm_name,
      `Episodes (Algorithm)` = fmt_int(.data$episodes_algorithm),
      `Episodes (PET)` = fmt_int(.data$episodes_pet),
      `Matched episodes` = fmt_int(.data$matched_episodes),
      `Algorithm-only` = fmt_int(.data$algorithm_only),
      `PET-only` = fmt_int(.data$pet_only),
      `Sensitivity vs PET` = fmt_pct(.data$sensitivity),
      `PPV vs PET` = fmt_pct(.data$ppv),
      `Start date diff (days) median (IQR)` = .data$start_diff_median_iqr,
      `% start diff = 0d` = fmt_pct(.data$pct_start_diff_zero),
      `End date diff (days) median (IQR)` = .data$end_diff_median_iqr,
      `% end diff = 0d` = fmt_pct(.data$pct_end_diff_zero),
      `Outcome agreement (accuracy %)` = fmt_pct(.data$outcome_agreement)
    )
}
