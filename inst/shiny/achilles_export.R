# Extract pregnancy aggregates from v3 results (per-database CSVs under
# inst/shiny/data/<DB>_results_<DATE>/) and reshape into an Achilles-style
# results table plus an analysis dictionary, using the official pregnancy
# analysis IDs from required_analysis_ids.csv.
#
# Outputs (written next to this script):
#   achilles_results.csv
#   achilles_analysis.csv
#
# Stratum values for outcome and delivery-mode analyses are stored as OMOP
# concept_ids (per https://data.darwin-eu.org/PeriNetMapping/implementation-guide.html
# and the canonical mapping in R/compareWithPET.R). Categories with no
# corresponding OMOP concept (PREG, DELIV, ECT, NA) collapse to concept_id 0.
#
# Analysis IDs covered (from required_analysis_ids.csv):
#   3100  Number of persons by pregnancy outcome
#   3101  Number of records by pregnancy outcome
#   3106  Distribution of age of first pregnancy
#   3111  Distribution of length of pregnancy in days
#   3120  Number of records by start year (yyyy; see note in body — v3
#         outputs do not contain a year x month aggregate, so true yyyymm
#         requires an upstream change to the package's monthlyTrends export)
#   3142  Number of records by age decile (stratum is the integer decile,
#         e.g. 1 for ages 10-19, 2 for 20-29)
#   3150  Number of persons by pregnancy mode delivery
#   3151  Number of records by pregnancy mode delivery
#   3156  Number of pregnancies per person (distribution)
#
# Not produced (no source data in v3 outputs):
#   3152, 3153 — Number of persons / records by pregnancy_single (singleton vs multiple)
#   3154       — Number of pregnancies with linked children
#   3155       — Distribution of OP length among children linked to pregnancy

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

# Integer decile label: 14 -> 1 (10-19), 25 -> 2 (20-29), ...
age_decile <- function(age) {
  as.integer(age) %/% 10L
}

# OMOP concept-id maps. Outcome map matches R/compareWithPET.R; delivery-mode
# map follows https://data.darwin-eu.org/PeriNetMapping/implementation-guide.html
# Unmapped categories use concept_id 0 (per OMOP convention for unknown).
outcome_concept_id <- function(category) {
  category <- toupper(as.character(category))
  dplyr::case_when(
    category == "LB"    ~ 4092289L,
    category == "SB"    ~ 443213L,
    category == "AB"    ~ 4081422L,
    category == "SA"    ~ 4067106L,
    category %in% c("PREG", "DELIV", "ECT", "NA") ~ 0L,
    is.na(category)     ~ 0L,
    TRUE                ~ 0L
  )
}

delivery_concept_id <- function(mode) {
  mode <- tolower(as.character(mode))
  dplyr::case_when(
    mode == "vaginal"  ~ 4125611L,
    mode == "cesarean" ~ 4015701L,
    TRUE               ~ 0L
  )
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
    total  = total,
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

  # Pregnancy totals (used as count_value for distribution analyses).
  ef <- read_site_csv(dir, "episode_frequency.csv")
  total_pregnancies <- if (nrow(ef) > 0) to_num(ef$total_episodes[1])     else NA_real_
  total_persons     <- if (nrow(ef) > 0) to_num(ef$total_individuals[1])  else NA_real_

  # ----- 3101 Number of records by pregnancy outcome -----
  oc <- read_site_csv(dir, "outcome_categories_count.csv")
  if (nrow(oc) > 0 && "algorithm" %in% names(oc)) {
    oc <- oc %>% dplyr::filter(.data$algorithm == "hipps")
    if (nrow(oc) > 0) {
      oc <- oc %>%
        dplyr::mutate(
          concept_id = outcome_concept_id(.data$outcome_category),
          n = to_num(.data$n)
        ) %>%
        dplyr::group_by(.data$concept_id) %>%
        dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop")
      out[[length(out) + 1]] <- purrr::map2_dfr(
        oc$concept_id, oc$n,
        ~ result_row(cdm, 3101, stratum_1 = as.character(.x), count_value = .y)
      )
    }
  }

  # ----- 3100 Number of persons by pregnancy outcome -----
  gpc <- read_site_csv(dir, "gestational_age_days_per_category_summary.csv")
  if (nrow(gpc) > 0 && all(c("final_outcome_category", "person_count") %in% names(gpc))) {
    gpc <- gpc %>%
      dplyr::mutate(
        concept_id = outcome_concept_id(.data$final_outcome_category),
        person_count = to_num(.data$person_count)
      ) %>%
      dplyr::group_by(.data$concept_id) %>%
      dplyr::summarise(person_count = sum(.data$person_count, na.rm = TRUE),
                       .groups = "drop")
    out[[length(out) + 1]] <- purrr::map2_dfr(
      gpc$concept_id, gpc$person_count,
      ~ result_row(cdm, 3100, stratum_1 = as.character(.x), count_value = .y)
    )
  }

  # ----- 3106 Distribution of age of first pregnancy -----
  asfp <- read_site_csv(dir, "age_summary_first_pregnancy.csv")
  if (nrow(asfp) > 0 && all(c("final_outcome_category", "year") %in% names(asfp))) {
    row <- asfp %>%
      dplyr::filter(.data$final_outcome_category == "overall",
                    .data$year == "overall") %>%
      dplyr::slice(1)
    if (nrow(row) == 1) {
      # First pregnancy = one per person, so denominator is total persons.
      out[[length(out) + 1]] <- result_row(
        cdm, 3106,
        count_value  = total_persons,
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

  # ----- 3111 Distribution of length of pregnancy in days -----
  gad <- read_site_csv(dir, "gestational_age_days_summary.csv")
  if (nrow(gad) > 0) {
    row <- gad %>% dplyr::slice(1)
    # Distribution is over all pregnancy episodes.
    out[[length(out) + 1]] <- result_row(
      cdm, 3111,
      count_value  = total_pregnancies,
      min_value    = to_num(row$min),
      max_value    = to_num(row$max),
      avg_value    = to_num(row$mean),
      stdev_value  = to_num(row$sd),
      median_value = to_num(row$median),
      p25_value    = to_num(row$Q25),
      p75_value    = to_num(row$Q75)
    )
  }

  # ----- 3120 Number of records by start year (intended: yyyymm) -----
  # NOTE: v3 outputs lack a year x month aggregate. monthly_trends.csv pools
  # across years and yearly_trend.csv pools across months, so a true yyyymm
  # strat cannot be derived without a package-side change to monthlyTrends().
  # For now we emit 4-digit YYYY here and flag this clearly.
  yt <- read_site_csv(dir, "yearly_trend.csv")
  if (nrow(yt) > 0 && all(c("column", "year", "count") %in% names(yt))) {
    yt <- yt %>%
      dplyr::filter(.data$column == "merge_pregnancy_start") %>%
      dplyr::mutate(year  = suppressWarnings(as.integer(.data$year)),
                    count = to_num(.data$count)) %>%
      dplyr::filter(!is.na(.data$year)) %>%
      dplyr::arrange(.data$year)
    if (nrow(yt) > 0) {
      out[[length(out) + 1]] <- purrr::map2_dfr(
        yt$year, yt$count,
        ~ result_row(cdm, 3120,
                     stratum_1 = sprintf("%04d", as.integer(.x)),
                     count_value = .y)
      )
    }
  }

  # ----- 3142 Number of records by age decile -----
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
        dplyr::mutate(decile = age_decile(.data$age)) %>%
        dplyr::group_by(.data$decile) %>%
        dplyr::summarise(count_value = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(.data$decile)
      out[[length(out) + 1]] <- purrr::map2_dfr(
        asg$decile, asg$count_value,
        ~ result_row(cdm, 3142,
                     stratum_1 = as.character(as.integer(.x)),
                     count_value = .y)
      )
    }
  }

  # ----- 3150 / 3151 Number of persons / records by pregnancy mode of delivery -----
  dms <- read_site_csv(dir, "delivery_mode_summary.csv")
  if (nrow(dms) > 0 && all(c("cesarean", "vaginal",
                             "cesarean_count", "vaginal_count") %in% names(dms))) {
    dms_n <- dms %>%
      dplyr::summarise(
        vaginal_persons  = sum(to_num(.data$vaginal),        na.rm = TRUE),
        cesarean_persons = sum(to_num(.data$cesarean),       na.rm = TRUE),
        vaginal_records  = sum(to_num(.data$vaginal_count),  na.rm = TRUE),
        cesarean_records = sum(to_num(.data$cesarean_count), na.rm = TRUE)
      )
    cid_vag <- as.character(delivery_concept_id("vaginal"))
    cid_cs  <- as.character(delivery_concept_id("cesarean"))
    out[[length(out) + 1]] <- dplyr::bind_rows(
      result_row(cdm, 3150, stratum_1 = cid_vag, count_value = dms_n$vaginal_persons),
      result_row(cdm, 3150, stratum_1 = cid_cs,  count_value = dms_n$cesarean_persons),
      result_row(cdm, 3151, stratum_1 = cid_vag, count_value = dms_n$vaginal_records),
      result_row(cdm, 3151, stratum_1 = cid_cs,  count_value = dms_n$cesarean_records)
    )
  }

  # ----- 3156 Number of pregnancies per person (distribution) -----
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
          cdm, 3156,
          count_value  = d$total,
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

# Build analysis dictionary by subsetting required_analysis_ids.csv to the
# pregnancy IDs we actually populated.
required <- readr::read_csv(file.path(here, "required_analysis_ids.csv"),
                            show_col_types = FALSE)
produced_ids <- sort(unique(results$analysis_id))
analysis <- required %>%
  dplyr::filter(.data$analysis_id %in% produced_ids) %>%
  dplyr::transmute(
    analysis_id   = as.integer(.data$analysis_id),
    analysis_name = .data$description,
    stratum_1_name, stratum_2_name, stratum_3_name, stratum_4_name, stratum_5_name
  ) %>%
  dplyr::arrange(.data$analysis_id)

readr::write_csv(results,  file.path(here, "achilles_results.csv"))
readr::write_csv(analysis, file.path(here, "achilles_analysis.csv"))

message("Wrote ", nrow(results), " result rows to achilles_results.csv")
message("Wrote ", nrow(analysis), " analysis rows to achilles_analysis.csv")
