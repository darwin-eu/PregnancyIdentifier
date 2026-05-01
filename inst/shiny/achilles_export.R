# Extract pregnancy aggregates from v3 results (per-database CSVs under
# inst/shiny/data/<DB>_results_<DATE>/) and reshape into an Achilles-style
# results table plus an analysis dictionary.
#
# Outputs (written next to this script):
#   achilles_results.csv
#   achilles_analysis.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(stringr)
})

here <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(file_arg) > 0) normalizePath(dirname(file_arg))
  else if (!is.null(sys.frame(1)$ofile)) dirname(sys.frame(1)$ofile)
  else getwd()
}, error = function(e) getwd())
data_root <- file.path(here, "data")

site_dirs <- list.dirs(data_root, recursive = FALSE)
site_dirs <- site_dirs[grepl("_results_", basename(site_dirs))]
stopifnot(length(site_dirs) > 0)

read_site_csv <- function(dir, name) {
  f <- file.path(dir, name)
  if (!file.exists(f)) return(tibble())
  suppressWarnings(suppressMessages(
    readr::read_csv(f, show_col_types = FALSE, guess_max = 50000, na = c("", "NA"))
  ))
}

# Numeric coercion that turns "<5" (suppressed cells) into NA.
to_num <- function(x) suppressWarnings(as.numeric(x))

# Empty result-row template
result_row <- function(cdm_name, analysis_id,
                       stratum_1 = NA_character_, stratum_2 = NA_character_,
                       stratum_3 = NA_character_, stratum_4 = NA_character_,
                       stratum_5 = NA_character_,
                       count_value = NA_real_,
                       min_value = NA_real_, max_value = NA_real_,
                       avg_value = NA_real_, stdev_value = NA_real_,
                       median_value = NA_real_,
                       p10_value = NA_real_, p25_value = NA_real_,
                       p75_value = NA_real_, p90_value = NA_real_) {
  tibble(
    cdm_name = cdm_name, analysis_id = as.integer(analysis_id),
    stratum_1 = stratum_1, stratum_2 = stratum_2, stratum_3 = stratum_3,
    stratum_4 = stratum_4, stratum_5 = stratum_5,
    count_value = count_value,
    min_value = min_value, max_value = max_value,
    avg_value = avg_value, stdev_value = stdev_value,
    median_value = median_value,
    p10_value = p10_value, p25_value = p25_value,
    p75_value = p75_value, p90_value = p90_value
  )
}

# 10-year bin label for an integer age
age_band <- function(age) {
  age <- as.integer(age)
  lower <- (age %/% 10) * 10L
  paste0(lower, "-", lower + 9L)
}

# Quantile distribution from a discrete histogram (value -> count).
# Suppressed counts (NA) are treated as 3 (midpoint of <5).
hist_distribution <- function(values, counts) {
  counts[is.na(counts)] <- 3
  ord <- order(values)
  values <- values[ord]; counts <- counts[ord]
  total <- sum(counts)
  if (total == 0) return(NULL)
  q <- function(p) {
    target <- p * total
    cum <- cumsum(counts)
    values[which(cum >= target)[1]]
  }
  mean_v <- sum(values * counts) / total
  var_v  <- sum(counts * (values - mean_v)^2) / max(total - 1, 1)
  list(
    min    = min(values), max = max(values),
    mean   = mean_v, sd = sqrt(var_v),
    median = q(0.5),
    p10    = q(0.10), p25 = q(0.25), p75 = q(0.75), p90 = q(0.90)
  )
}

extract_site <- function(dir) {
  cdm <- {
    src <- read_site_csv(dir, "cdm_source.csv")
    if (nrow(src) > 0 && "cdm_name" %in% names(src)) src$cdm_name[1] else basename(dir)
  }
  message(" - ", cdm)
  out <- list()

  # ----- 100/101 Pregnancy record/person count -----
  ef <- read_site_csv(dir, "episode_frequency.csv")
  if (nrow(ef) > 0) {
    out[[length(out) + 1]] <- result_row(cdm, 100, count_value = to_num(ef$total_episodes[1]))
    out[[length(out) + 1]] <- result_row(cdm, 101, count_value = to_num(ef$total_individuals[1]))
  }

  # ----- 200 Age at start of pregnancy: 10-year-bin record count -----
  asg <- read_site_csv(dir, "age_summary_groups.csv")
  if (nrow(asg) > 0 && all(c("colName", "age_pregnancy_start", "n") %in% names(asg))) {
    asg <- asg %>%
      dplyr::filter(.data$colName == "age_pregnancy_start") %>%
      dplyr::mutate(
        age = suppressWarnings(as.integer(.data$age_pregnancy_start)),
        n   = to_num(.data$n)
      ) %>%
      dplyr::filter(!is.na(.data$age))
    if (nrow(asg) > 0) {
      asg <- asg %>%
        dplyr::mutate(band = age_band(.data$age)) %>%
        dplyr::group_by(.data$band) %>%
        dplyr::summarise(count_value = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(as.integer(stringr::str_extract(.data$band, "^\\d+")))
      out[[length(out) + 1]] <- purrr::map2_dfr(
        asg$band, asg$count_value,
        ~ result_row(cdm, 200, stratum_1 = .x, count_value = .y)
      )
    }
  }

  # ----- 201 Age at start of pregnancy: distribution (overall) -----
  asum <- read_site_csv(dir, "age_summary.csv")
  if (nrow(asum) > 0 && "final_outcome_category" %in% names(asum)) {
    row <- asum %>% dplyr::filter(.data$final_outcome_category == "overall") %>% dplyr::slice(1)
    if (nrow(row) == 1) {
      out[[length(out) + 1]] <- result_row(
        cdm, 201,
        count_value  = to_num(row$n),
        min_value    = to_num(row$min),
        max_value    = to_num(row$max),
        avg_value    = to_num(row$mean),
        stdev_value  = to_num(if ("sd" %in% names(row)) row$sd else NA),
        median_value = to_num(row$median),
        p25_value    = to_num(row$Q25),
        p75_value    = to_num(row$Q75)
      )
    }
  }

  # ----- 300 Pregnancies by calendar year: record count -----
  yt <- read_site_csv(dir, "yearly_trend.csv")
  if (nrow(yt) > 0 && all(c("column", "year", "count") %in% names(yt))) {
    yt <- yt %>%
      dplyr::filter(.data$column == "merge_pregnancy_start") %>%
      dplyr::mutate(year = suppressWarnings(as.integer(.data$year)),
                    count = to_num(.data$count)) %>%
      dplyr::filter(!is.na(.data$year)) %>%
      dplyr::arrange(.data$year)
    if (nrow(yt) > 0) {
      out[[length(out) + 1]] <- purrr::map2_dfr(
        yt$year, yt$count,
        ~ result_row(cdm, 300, stratum_1 = as.character(.x), count_value = .y)
      )
    }
  }

  # ----- 400 Length of pregnancy in weeks: distribution -----
  gad <- read_site_csv(dir, "gestational_age_days_summary.csv")
  if (nrow(gad) > 0) {
    row <- gad %>% dplyr::slice(1)
    out[[length(out) + 1]] <- result_row(
      cdm, 400,
      min_value    = to_num(row$min)    / 7,
      max_value    = to_num(row$max)    / 7,
      avg_value    = to_num(row$mean)   / 7,
      stdev_value  = to_num(row$sd)     / 7,
      median_value = to_num(row$median) / 7,
      p25_value    = to_num(row$Q25)    / 7,
      p75_value    = to_num(row$Q75)    / 7
    )
  }

  # ----- 500 Pregnancy outcomes: record count (algorithm == "hipps") -----
  oc <- read_site_csv(dir, "outcome_categories_count.csv")
  if (nrow(oc) > 0 && "algorithm" %in% names(oc)) {
    oc <- oc %>% dplyr::filter(.data$algorithm == "hipps")
    if (nrow(oc) > 0) {
      oc <- oc %>%
        dplyr::mutate(outcome_category = ifelse(is.na(.data$outcome_category), "NA",
                                                as.character(.data$outcome_category)),
                      n = to_num(.data$n))
      out[[length(out) + 1]] <- purrr::map2_dfr(
        oc$outcome_category, oc$n,
        ~ result_row(cdm, 500, stratum_1 = .x, count_value = .y)
      )
    }
  }

  # ----- 501 Pregnancy outcomes: person count -----
  gpc <- read_site_csv(dir, "gestational_age_days_per_category_summary.csv")
  if (nrow(gpc) > 0 && all(c("final_outcome_category", "person_count") %in% names(gpc))) {
    gpc <- gpc %>%
      dplyr::mutate(person_count = to_num(.data$person_count))
    out[[length(out) + 1]] <- purrr::map2_dfr(
      gpc$final_outcome_category, gpc$person_count,
      ~ result_row(cdm, 501, stratum_1 = as.character(.x), count_value = .y)
    )
  }

  # ----- 600 / 601 Method of delivery -----
  dms <- read_site_csv(dir, "delivery_mode_summary.csv")
  if (nrow(dms) > 0 && all(c("final_outcome_category", "cesarean", "vaginal",
                             "cesarean_count", "vaginal_count") %in% names(dms))) {
    rows_record <- dms %>%
      dplyr::transmute(
        outcome = .data$final_outcome_category,
        vaginal = to_num(.data$vaginal_count),
        cesarean = to_num(.data$cesarean_count)
      ) %>%
      tidyr::pivot_longer(cols = c("vaginal", "cesarean"),
                          names_to = "mode", values_to = "count_value")
    out[[length(out) + 1]] <- purrr::pmap_dfr(
      list(rows_record$mode, rows_record$outcome, rows_record$count_value),
      function(mode, outcome, n) result_row(cdm, 600,
                                            stratum_1 = mode,
                                            stratum_2 = as.character(outcome),
                                            count_value = n)
    )

    rows_person <- dms %>%
      dplyr::transmute(
        outcome = .data$final_outcome_category,
        vaginal = to_num(.data$vaginal),
        cesarean = to_num(.data$cesarean)
      ) %>%
      tidyr::pivot_longer(cols = c("vaginal", "cesarean"),
                          names_to = "mode", values_to = "count_value")
    out[[length(out) + 1]] <- purrr::pmap_dfr(
      list(rows_person$mode, rows_person$outcome, rows_person$count_value),
      function(mode, outcome, n) result_row(cdm, 601,
                                            stratum_1 = mode,
                                            stratum_2 = as.character(outcome),
                                            count_value = n)
    )
  }

  # ----- 700 Number of pregnancies per person: distribution -----
  pf <- read_site_csv(dir, "pregnancy_frequency.csv")
  if (nrow(pf) > 0 && all(c("freq", "number_individuals") %in% names(pf))) {
    pf <- pf %>%
      dplyr::mutate(
        freq = suppressWarnings(as.integer(.data$freq)),
        n    = to_num(.data$number_individuals)
      ) %>%
      dplyr::filter(!is.na(.data$freq))
    if (nrow(pf) > 0) {
      d <- hist_distribution(pf$freq, pf$n)
      if (!is.null(d)) {
        out[[length(out) + 1]] <- result_row(
          cdm, 700,
          count_value  = sum(ifelse(is.na(pf$n), 3, pf$n)),
          min_value    = d$min, max_value = d$max,
          avg_value    = d$mean, stdev_value = d$sd,
          median_value = d$median,
          p10_value    = d$p10, p25_value = d$p25,
          p75_value    = d$p75, p90_value = d$p90
        )
      }
    }
  }

  dplyr::bind_rows(out)
}

message("Extracting from ", length(site_dirs), " sites:")
results <- purrr::map_dfr(site_dirs, extract_site)
results <- results %>% dplyr::arrange(.data$cdm_name, .data$analysis_id,
                                      .data$stratum_1, .data$stratum_2)

analysis <- tibble::tribble(
  ~analysis_id, ~analysis_name,                                       ~stratum_1_name,    ~stratum_2_name,    ~stratum_3_name, ~stratum_4_name, ~stratum_5_name,
  100L,         "Pregnancy record count",                             NA_character_,      NA_character_,      NA_character_,   NA_character_,   NA_character_,
  101L,         "Pregnancy person count",                             NA_character_,      NA_character_,      NA_character_,   NA_character_,   NA_character_,
  200L,         "Pregnancies by age at start (record count)",         "age_band_10y",     NA_character_,      NA_character_,   NA_character_,   NA_character_,
  201L,         "Age at start of pregnancy (distribution)",           NA_character_,      NA_character_,      NA_character_,   NA_character_,   NA_character_,
  300L,         "Pregnancies by calendar year (record count)",        "year",             NA_character_,      NA_character_,   NA_character_,   NA_character_,
  400L,         "Length of pregnancy in weeks (distribution)",        NA_character_,      NA_character_,      NA_character_,   NA_character_,   NA_character_,
  500L,         "Pregnancy outcomes (record count)",                  "outcome_category", NA_character_,      NA_character_,   NA_character_,   NA_character_,
  501L,         "Pregnancy outcomes (person count)",                  "outcome_category", NA_character_,      NA_character_,   NA_character_,   NA_character_,
  600L,         "Pregnancy method of delivery (record count)",        "delivery_mode",    "outcome_category", NA_character_,   NA_character_,   NA_character_,
  601L,         "Pregnancy method of delivery (person count)",        "delivery_mode",    "outcome_category", NA_character_,   NA_character_,   NA_character_,
  700L,         "Number of pregnancies per person (distribution)",    NA_character_,      NA_character_,      NA_character_,   NA_character_,   NA_character_
)

readr::write_csv(results,  file.path(here, "achilles_results.csv"))
readr::write_csv(analysis, file.path(here, "achilles_analysis.csv"))

message("Wrote ", nrow(results), " result rows to achilles_results.csv")
message("Wrote ", nrow(analysis), " analysis rows to achilles_analysis.csv")
