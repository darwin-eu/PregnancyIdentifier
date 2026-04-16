# national-stats-data.R
# ============================================================================
# National Statistics Reference Data - Import & Preparation
# ============================================================================
# This script loads and prepares the external (national) reference dataset
# used to validate pregnancy algorithm results against published statistics.
#
# Data source: National_Statistics_Obj2_v2.csv
# Contains published statistics from 12 European countries covering:
#
#   Metric                          | Source / Notes
#   --------------------------------|-----------------------------------------------
#   Crude birth rate (per 1000)     | Eurostat: demo_gind (crude birth rate)
#   Mean age at first child         | Eurostat: demo_find (mean age of women at
#                                   |   birth of first child)
#   Number of live births per year  | Eurostat: demo_gind (live births)
#   Foetal mortality rate           | Euro-Peristat: foetal deaths at or after
#                                   |   24-week threshold, per 1000 total births
#   Gestational duration distrib.   | Euro-Peristat: live births by gestational
#                                   |   age bins (<32, 32-36, 37-38, 39-41, >=42 wk)
#   Mode of delivery (% vaginal,   | Euro-Peristat: vaginal and cesarean delivery
#     % C-section)                  |   rates among live births
#
# Countries covered: Croatia, Denmark, Finland, France, Germany, Hungary,
#                    Netherlands, Norway, Portugal, Spain, Sweden, UK
# Years: varies by indicator (2014-2023 for Eurostat; 2015, 2019 for Peristat)
# ============================================================================

# ---- Database-to-country mapping ----
# Maps database CDM names (lowercase, as used in allDP) to national statistics
# country names. Only population-level databases should be mapped here.
.DB_COUNTRY_MAP <- list(
  "najs"              = "Croatia",
  "dk-dhr"            = "Denmark",
  "dkdhr"             = "Denmark",

  "finomop-tauh"      = "Finland",
  "finomop-thl"       = "Finland",
  "cdw bordeaux"      = "France",
  "ingef"             = "Germany",
  "sucd"              = "Hungary",
  "ipci"              = "Netherlands",
  "nlhr"              = "Norway",
  "emdb"              = "Portugal",
  "bifap"             = "Spain",
  "imasis"            = "Spain",
  "sidiap"            = "Spain",
  "hi-speed"          = "Sweden",
  "cprd-gold"         = "UK",
  "cprd"              = "UK"
)

# Expected percentage of national population covered by regional data sources.
# Used in the comparison table to contextualize live birth count differences.
.DB_EXPECTED_POP_COVERAGE <- list(
  "bifap"      = 40.0,
  "sidiap"     = 15.0,
  "cprd-gold"  = 4.3,
  "cprd"       = 4.3,
  "ipci"       = 7.0,
  "ingef"      = 9.0,
  "najs"       = 100.0,
  "dk-dhr"     = 100.0,
  "nlhr"       = 100.0,
  "hi-speed"   = 100.0,
  "finomop-thl" = 100.0
)

# Resolve a database cdm_name to its expected population coverage (%).
# Returns NA if not a regional data source.
.resolve_db_coverage <- function(db_name) {
  if (is.null(db_name) || length(db_name) == 0) return(NA_real_)
  for (prefix in names(.DB_EXPECTED_POP_COVERAGE)) {
    if (grepl(paste0("^", prefix), tolower(db_name))) {
      return(.DB_EXPECTED_POP_COVERAGE[[prefix]])
    }
  }
  NA_real_
}

# Databases that should NOT have their live-birth counts compared to national
# statistics (patient cohort / hospital databases, not population-level).
.DB_SKIP_LB_COUNT <- c(
  "finomop-tauh",
  "cdw bordeaux",
  "sucd",
  "emdb",
  "imasis"
)

# Resolve a database cdm_name to its mapped national stats country.
# Matches on prefix (ignores version suffix like "_v3").
.resolve_db_country <- function(db_name) {
  if (is.null(db_name) || length(db_name) == 0) return(NA_character_)
  if (length(db_name) > 1L) {
    return(vapply(db_name, function(x) .resolve_db_country(x), FUN.VALUE = character(1)))
  }
  for (prefix in names(.DB_COUNTRY_MAP)) {
    if (grepl(paste0("^", prefix), tolower(db_name))) return(.DB_COUNTRY_MAP[[prefix]])
  }
  NA_character_
}

# Check if a database should skip live-birth count comparison
.skip_lb_count <- function(db_name) {
  any(vapply(.DB_SKIP_LB_COUNT, function(prefix) {
    grepl(paste0("^", prefix), tolower(db_name))
  }, logical(1)))
}

# Get all mapped DB names from the current allDP
.get_mapped_databases <- function(allDP = NULL) {
  if (is.null(allDP)) {
    if (!exists("allDP", envir = .GlobalEnv)) return(character(0))
    allDP <- get("allDP", envir = .GlobalEnv)
  }
  dbs <- allDP
  mapped <- vapply(dbs, function(db) !is.na(.resolve_db_country(db)), logical(1))
  dbs[mapped]
}

# ---- Parse European-style numbers ----
# Handles comma-as-decimal and dot-as-thousands separator conventions.
.parse_eu_number <- function(x) {
  vapply(x, function(val) {
    if (is.na(val) || !nzchar(trimws(val))) return(NA_real_)
    val <- trimws(val)
    val <- gsub(",", ".", val)
    dots <- gregexpr("\\.", val)[[1]]
    n_dots <- sum(dots > 0)
    if (n_dots == 1L) {
      parts <- strsplit(val, "\\.")[[1]]
      if (nchar(parts[2]) == 3L && grepl("^[0-9]+$", parts[2]))
        val <- gsub("\\.", "", val)
    } else if (n_dots > 1L) {
      val <- gsub("\\.", "", val)
    }
    suppressWarnings(as.numeric(val))
  }, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
}

# ---- Load and parse national statistics CSV ----
# Returns a long-format tibble: indicator, variable, level, year, country, value
.load_national_stats <- function() {
  csvPath <- file.path(getwd(), "National_Statistics_Obj2_v2.csv")
  if (!file.exists(csvPath)) {
    csvPath <- system.file("shiny", "National_Statistics_Obj2_v2.csv",
                           package = "PregnancyIdentifier")
  }
  if (!file.exists(csvPath) || !nzchar(csvPath)) return(NULL)

  raw <- readr::read_csv(csvPath, col_types = readr::cols(.default = "c"),
                          show_col_types = FALSE)
  colnames(raw) <- trimws(colnames(raw))
  country_cols <- setdiff(colnames(raw), c("Indicators", "variable", "level", "Year"))

  raw %>%
    tidyr::pivot_longer(cols = dplyr::all_of(country_cols),
                         names_to = "country", values_to = "value_raw") %>%
    dplyr::rename(indicator = "Indicators", year = "Year") %>%
    dplyr::mutate(
      year = suppressWarnings(as.integer(.data$year)),
      value = .parse_eu_number(.data$value_raw)
    ) %>%
    dplyr::select("indicator", "variable", "level", "year", "country", "value")
}


# ============================================================================
# Internal data extraction helpers
# ============================================================================
# These functions extract metric values from the app's global datasets to
# compare against the national reference.

# ---- Extract yearly birth counts + population from prevalence results ----
# Source: prevalence summarised_result (outcome_count for _lb / _deliv cohorts)
# Filters for: denominator_age_group = "0 to 150", denominator_sex = "Both",
#              analysis_interval = "years"
# Note: outcome_cohort_name lives in group_level (splitGroup), and
#       analysis_interval + prevalence_start_date live in additional_level (splitAdditional).
# Returns tibble: cdm_name, year, numerator, denominator_count
.extract_counts_from_prevalence <- function(prev, db_names, include_deliv = FALSE) {
  empty <- tibble::tibble(cdm_name = character(0), year = integer(0),
                          numerator = numeric(0), denominator_count = numeric(0))
  if (!is.data.frame(prev) || nrow(prev) == 0) return(empty)

  # Step 1: filter by settings (denominator_age_group, denominator_sex)
  s <- tryCatch(omopgenerics::settings(prev), error = function(e) NULL)
  if (is.null(s)) return(empty)

  keep_ids <- s
  if ("denominator_age_group" %in% colnames(keep_ids)) {
    keep_ids <- keep_ids %>% dplyr::filter(.data$denominator_age_group == "0 to 150")
  }
  if ("denominator_sex" %in% colnames(keep_ids)) {
    keep_ids <- keep_ids %>% dplyr::filter(.data$denominator_sex == "Both")
  }
  if (nrow(keep_ids) == 0) return(empty)

  result_id_col <- if ("result_id" %in% colnames(keep_ids)) "result_id" else "cohort_definition_id"
  valid_ids <- keep_ids[[result_id_col]]

  # Step 2: filter rows by result_id and cdm_name
  p <- prev %>%
    dplyr::filter(.data[[result_id_col]] %in% valid_ids) %>%
    dplyr::filter(tolower(.data$cdm_name) %in% tolower(db_names))
  if (nrow(p) == 0) return(empty)

  # Step 3: split group columns to get outcome_cohort_name
  p <- tryCatch(p %>% visOmopResults::splitGroup(), error = function(e) p)
  if (!"outcome_cohort_name" %in% colnames(p)) return(empty)

  # Filter by outcome pattern: hipps_lb (and hipps_deliv if include_deliv)
  outcome_pattern <- if (include_deliv) "^hipps_(lb|deliv)$" else "^hipps_lb$"
  p <- p %>% dplyr::filter(grepl(outcome_pattern, .data$outcome_cohort_name))
  if (nrow(p) == 0) return(empty)

  # Step 4: split additional columns to get prevalence_start_date + analysis_interval
  p <- tryCatch(p %>% visOmopResults::splitAdditional(), error = function(e) p)
  if (!"prevalence_start_date" %in% colnames(p)) return(empty)

  # Filter for yearly interval only
  if ("analysis_interval" %in% colnames(p)) {
    p <- p %>% dplyr::filter(.data$analysis_interval == "years")
  }

  p <- p %>%
    dplyr::mutate(year = suppressWarnings(as.integer(
      stringr::str_extract(.data$prevalence_start_date, "^[0-9]+")
    ))) %>%
    dplyr::filter(!is.na(.data$year))

  # Step 5: extract outcome_count and denominator_count
  outcome_rows <- p %>%
    dplyr::filter(.data$estimate_name == "outcome_count") %>%
    dplyr::mutate(numerator = suppressWarnings(as.numeric(.data$estimate_value))) %>%
    dplyr::filter(!is.na(.data$numerator))

  denom_rows <- p %>%
    dplyr::filter(.data$estimate_name == "denominator_count") %>%
    dplyr::mutate(denominator_count = suppressWarnings(as.numeric(.data$estimate_value))) %>%
    dplyr::filter(!is.na(.data$denominator_count))

  # Sum numerator across LB + DELIV per (cdm_name, year)
  num <- outcome_rows %>%
    dplyr::group_by(.data$cdm_name, .data$year) %>%
    dplyr::summarise(numerator = sum(.data$numerator, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(.data$numerator > 0)

  # Denominator: take max per (cdm_name, year) — same across outcome cohorts
  den <- denom_rows %>%
    dplyr::group_by(.data$cdm_name, .data$year) %>%
    dplyr::summarise(denominator_count = max(.data$denominator_count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(is.finite(.data$denominator_count), .data$denominator_count > 0)

  result <- num %>%
    dplyr::inner_join(den, by = c("cdm_name", "year"))

  result
}

# Extract counts from prevalence for a specific outcome cohort pattern.
# Similar to .extract_counts_from_prevalence but allows arbitrary cohort pattern.
.extract_counts_from_prevalence_by_cohort <- function(prev, db_names, cohort_pattern) {
  empty <- tibble::tibble(cdm_name = character(0), year = integer(0),
                          numerator = numeric(0), denominator_count = numeric(0))
  if (!is.data.frame(prev) || nrow(prev) == 0) return(empty)

  s <- tryCatch(omopgenerics::settings(prev), error = function(e) NULL)
  if (is.null(s)) return(empty)

  keep_ids <- s
  if ("denominator_age_group" %in% colnames(keep_ids)) {
    keep_ids <- keep_ids %>% dplyr::filter(.data$denominator_age_group == "0 to 150")
  }
  if ("denominator_sex" %in% colnames(keep_ids)) {
    keep_ids <- keep_ids %>% dplyr::filter(.data$denominator_sex == "Both")
  }
  if (nrow(keep_ids) == 0) return(empty)

  result_id_col <- if ("result_id" %in% colnames(keep_ids)) "result_id" else "cohort_definition_id"
  valid_ids <- keep_ids[[result_id_col]]

  p <- prev %>%
    dplyr::filter(.data[[result_id_col]] %in% valid_ids) %>%
    dplyr::filter(tolower(.data$cdm_name) %in% tolower(db_names))
  if (nrow(p) == 0) return(empty)

  p <- tryCatch(p %>% visOmopResults::splitGroup(), error = function(e) p)
  if (!"outcome_cohort_name" %in% colnames(p)) return(empty)

  p <- p %>% dplyr::filter(grepl(cohort_pattern, .data$outcome_cohort_name))
  if (nrow(p) == 0) return(empty)

  p <- tryCatch(p %>% visOmopResults::splitAdditional(), error = function(e) p)
  if (!"prevalence_start_date" %in% colnames(p)) return(empty)

  if ("analysis_interval" %in% colnames(p)) {
    p <- p %>% dplyr::filter(.data$analysis_interval == "years")
  }

  p <- p %>%
    dplyr::mutate(year = suppressWarnings(as.integer(
      stringr::str_extract(.data$prevalence_start_date, "^[0-9]+")
    ))) %>%
    dplyr::filter(!is.na(.data$year))

  outcome_rows <- p %>%
    dplyr::filter(.data$estimate_name == "outcome_count") %>%
    dplyr::mutate(numerator = suppressWarnings(as.numeric(.data$estimate_value))) %>%
    dplyr::filter(!is.na(.data$numerator))

  denom_rows <- p %>%
    dplyr::filter(.data$estimate_name == "denominator_count") %>%
    dplyr::mutate(denominator_count = suppressWarnings(as.numeric(.data$estimate_value))) %>%
    dplyr::filter(!is.na(.data$denominator_count))

  num <- outcome_rows %>%
    dplyr::group_by(.data$cdm_name, .data$year) %>%
    dplyr::summarise(numerator = sum(.data$numerator, na.rm = TRUE), .groups = "drop")

  den <- denom_rows %>%
    dplyr::group_by(.data$cdm_name, .data$year) %>%
    dplyr::summarise(denominator_count = max(.data$denominator_count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(is.finite(.data$denominator_count), .data$denominator_count > 0)

  num %>% dplyr::inner_join(den, by = c("cdm_name", "year"))
}

# ---- Extract yearly LB counts from incidence results ----
# Source: incidence summarised_result (outcome_count for _lb cohorts)
# Returns tibble: cdm_name, year, numerator
.extract_lb_counts_from_incidence <- function(inc, db_names, include_deliv = FALSE) {
  empty <- tibble::tibble(cdm_name = character(0), year = integer(0), numerator = numeric(0))
  if (!is.data.frame(inc) || nrow(inc) == 0 ||
      !all(c("estimate_name", "estimate_value", "group_level") %in% colnames(inc))) {
    return(empty)
  }

  gl_pattern <- if (include_deliv) "_(lb|deliv)$" else "_lb$"
  inc_lb <- inc %>%
    dplyr::filter(
      .data$estimate_name == "outcome_count",
      grepl(gl_pattern, .data$group_level)
    )
  if ("strata_name" %in% colnames(inc_lb)) {
    inc_lb <- inc_lb %>% dplyr::filter(.data$strata_name == "overall")
  }
  if (nrow(inc_lb) == 0) return(empty)

  if ("additional_level" %in% colnames(inc_lb) && "additional_name" %in% colnames(inc_lb)) {
    inc_lb <- inc_lb %>%
      dplyr::filter(grepl("incidence_start_date", .data$additional_name)) %>%
      dplyr::mutate(
        year = suppressWarnings(as.integer(stringr::str_extract(
          trimws(sub("\\s*&&&.*", "", .data$additional_level)), "^[0-9]+"
        )))
      )
  } else if ("incidence_start_date" %in% colnames(inc_lb)) {
    inc_lb <- inc_lb %>%
      dplyr::mutate(
        year = suppressWarnings(as.integer(stringr::str_extract(.data$incidence_start_date, "^[0-9]+")))
      )
  } else {
    return(empty)
  }

  inc_lb <- inc_lb %>%
    dplyr::filter(
      !is.na(.data$year),
      tolower(.data$cdm_name) %in% tolower(db_names)
    ) %>%
    dplyr::mutate(numerator = suppressWarnings(as.numeric(.data$estimate_value))) %>%
    dplyr::filter(!is.na(.data$numerator), .data$numerator > 0)

  if (include_deliv) {
    # When combining LB+DELIV, take max per group_level (as before) then sum across categories
    inc_lb <- inc_lb %>%
      dplyr::group_by(.data$cdm_name, .data$year, .data$group_level) %>%
      dplyr::summarise(numerator = suppressWarnings(max(.data$numerator, na.rm = TRUE)), .groups = "drop") %>%
      dplyr::group_by(.data$cdm_name, .data$year) %>%
      dplyr::summarise(numerator = sum(.data$numerator, na.rm = TRUE), .groups = "drop")
  } else {
    inc_lb <- inc_lb %>%
      dplyr::group_by(.data$cdm_name, .data$year) %>%
      dplyr::summarise(numerator = suppressWarnings(max(.data$numerator, na.rm = TRUE)), .groups = "drop")
  }

  inc_lb <- inc_lb %>%
    dplyr::filter(is.finite(.data$numerator))

  inc_lb
}

# ---- Build crude birth rates for mapped databases ----
# Source: prevalence (denominator_count = population; outcome_count = births)
# Metric: (birth count / population) * 1000
# Returns tibble: cdm_name, country, year, numerator, denominator_count, crude_rate_per_1000
.build_crude_rates_internal <- function(include_deliv = FALSE) {
  mapped_dbs <- .get_mapped_databases()
  if (length(mapped_dbs) == 0) {
    return(tibble::tibble(
      cdm_name = character(0), country = character(0), year = integer(0),
      numerator = numeric(0), denominator_count = numeric(0), crude_rate_per_1000 = numeric(0)
    ))
  }

  counts <- tibble::tibble(cdm_name = character(0), year = integer(0),
                           numerator = numeric(0), denominator_count = numeric(0))
  if (exists("prevalence", envir = .GlobalEnv)) {
    prev <- get("prevalence", envir = .GlobalEnv)
    counts <- .extract_counts_from_prevalence(prev, mapped_dbs, include_deliv = include_deliv)
  }

  if (nrow(counts) == 0) return(tibble::tibble(
    cdm_name = character(0), country = character(0), year = integer(0),
    numerator = numeric(0), denominator_count = numeric(0), crude_rate_per_1000 = numeric(0)
  ))

  out <- counts %>%
    dplyr::mutate(
      country = .resolve_db_country(.data$cdm_name),
      crude_rate_per_1000 = dplyr::if_else(
        .data$denominator_count > 0,
        round(1000 * .data$numerator / .data$denominator_count, 1),
        NA_real_
      )
    ) %>%
    dplyr::filter(!is.na(.data$country)) %>%
    dplyr::select("cdm_name", "country", "year", "numerator", "denominator_count", "crude_rate_per_1000")
  out
}


# ============================================================================
# Single-database comparison builder
# ============================================================================
# Builds a row-matched comparison for one DB <-> Country pair.
# For each reference row, extracts the matching internal value.
#
# Metrics matched:
#   - Birth rate (crude): LB count / incidence denominator * 1000
#   - Maternal age: mean age at first pregnancy start (year-matched)
#   - Live births per year: from incidence outcome_count for _lb cohorts
#   - Foetal mortality rate: hipps_sb / hipps_lb (or + hipps_deliv) * 1000 from prevalence
#   - Gestational duration: LB distribution across bins (% comparison)
#   - Mode of delivery: vaginal vs cesarean % among LB
#
# Returns tibble: indicator, variable, level, year, external_value,
#   internal_value, internal_note

.build_comparison <- function(natl, db_name, country_name, lb_categories = "LB") {

  ext <- natl %>%
    dplyr::filter(.data$country == country_name) %>%
    dplyr::select("indicator", "variable", "level", "year", "value") %>%
    dplyr::rename(external_value = "value")

  if (nrow(ext) == 0) return(tibble::tibble())

  # Pre-compute reference gestational percentages per year
  ext_gest_pct <- ext %>%
    dplyr::filter(.data$indicator == "Gestational duration distribution") %>%
    dplyr::group_by(.data$year) %>%
    dplyr::mutate(
      total = sum(.data$external_value, na.rm = TRUE),
      external_pct = dplyr::if_else(.data$total > 0,
        round(100 * .data$external_value / .data$total, 1), NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("level", "year", "external_pct")

  # ---- Gather internal result data sources ----

  # 1. Maternal age (year-matched from ageSummaryFirstPregnancy)
  int_age <- tibble::tibble()
  if (exists("ageSummaryFirstPregnancy")) {
    afp <- ageSummaryFirstPregnancy
    if (!is.null(afp) && nrow(afp) > 0 && "year" %in% colnames(afp)) {
      int_age <- afp %>%
        dplyr::filter(
          tolower(.data$cdm_name) == tolower(db_name) |
            grepl(paste0("^", tolower(sub("_v[0-9]+$", "", db_name))),
                  tolower(.data$cdm_name)),
          .data$final_outcome_category == "overall",
          .data$year != "overall"
        ) %>%
        dplyr::mutate(
          year = suppressWarnings(as.integer(.data$year)),
          mean = round(suppressWarnings(as.numeric(.data$mean)), 1)
        ) %>%
        dplyr::filter(!is.na(.data$year)) %>%
        dplyr::select("year", "mean")
    }
  }

  # 2. Yearly live birth counts + population from prevalence results
  #    Skip for non-population databases (hospital/patient cohorts)
  skip_lb <- .skip_lb_count(db_name)
  include_deliv <- "DELIV" %in% lb_categories
  int_prev_counts <- tibble::tibble()
  if (!skip_lb && exists("prevalence", envir = .GlobalEnv)) {
    prev <- get("prevalence", envir = .GlobalEnv)
    int_prev_counts <- .extract_counts_from_prevalence(prev, db_name, include_deliv = include_deliv)
  }

  int_trend <- tibble::tibble()
  if (nrow(int_prev_counts) > 0) {
    int_trend <- int_prev_counts %>%
      dplyr::transmute(year = .data$year, count = .data$numerator,
                       denominator_count = .data$denominator_count)
  }

  # 2b. Crude birth rate (from prevalence: births / population * 1000)
  #     Uses LB or LB+DELIV based on lb_categories selection
  int_crude <- tibble::tibble()
  crude_rates_all <- .build_crude_rates_internal(include_deliv = include_deliv)
  if (nrow(crude_rates_all) > 0) {
    int_crude <- crude_rates_all %>%
      dplyr::filter(
        (tolower(.data$cdm_name) == tolower(db_name) |
           grepl(paste0("^", tolower(sub("_v[0-9]+$", "", db_name))),
                 tolower(.data$cdm_name))),
        .data$country == country_name
      ) %>%
      dplyr::select("year", "crude_rate_per_1000", "numerator", "denominator_count")
  }

  # 2c. Get individual LB and DELIV counts for birth rate numerator display
  #     When include_deliv is TRUE, get LB-only counts separately for breakdown
  int_lb_only <- tibble::tibble()
  int_combined <- tibble::tibble()
  if (include_deliv && exists("prevalence", envir = .GlobalEnv)) {
    prev <- get("prevalence", envir = .GlobalEnv)
    lb_only_counts <- .extract_counts_from_prevalence(prev, db_name, include_deliv = FALSE)
    combined_counts <- .extract_counts_from_prevalence(prev, db_name, include_deliv = TRUE)
    if (nrow(lb_only_counts) > 0) {
      int_lb_only <- lb_only_counts %>%
        dplyr::transmute(year = .data$year, lb_count = .data$numerator)
    }
    if (nrow(combined_counts) > 0) {
      int_combined <- combined_counts %>%
        dplyr::transmute(year = .data$year, combined_count = .data$numerator)
    }
  }

  # 3. Foetal mortality rate: hipps_sb / (hipps_sb + hipps_lb [+ hipps_deliv]) * 1000
  #    from prevalence data. Denominator determined by lb_categories dropdown.
  int_fm <- tibble::tibble()
  if (exists("prevalence", envir = .GlobalEnv)) {
    prev <- get("prevalence", envir = .GlobalEnv)
    # Extract SB counts from prevalence (hipps_sb cohort)
    int_fm_sb <- .extract_counts_from_prevalence_by_cohort(prev, db_name, "^hipps_sb$")
    # Extract denominator counts: hipps_lb or hipps_lb + hipps_deliv
    int_fm_denom <- .extract_counts_from_prevalence(prev, db_name, include_deliv = include_deliv)
    if (nrow(int_fm_sb) > 0 && nrow(int_fm_denom) > 0) {
      int_fm <- int_fm_sb %>%
        dplyr::select("year", sb_count = "numerator") %>%
        dplyr::inner_join(
          int_fm_denom %>% dplyr::select("year", lb_count = "numerator"),
          by = "year"
        ) %>%
        dplyr::mutate(total = .data$sb_count + .data$lb_count) %>%
        dplyr::filter(.data$total > 0) %>%
        dplyr::mutate(fm_rate = round(1000 * .data$sb_count / .data$total, 1))
    }
  }

  # 4. Gestational duration distribution (% per bin)
  #    Maps app bins to national stats bins for comparison
  #    App bins: <12, 12-27, 28-31, 32-36, 37-38, 39-41, 42-43, 45-49, >=50
  #    National bins: <32, 32-36, 37-38, 39-41, >=42
  int_gest <- tibble::tibble()
  if (exists("gestationalWeeksBinned")) {
    gwb <- gestationalWeeksBinned
    if (!is.null(gwb) && nrow(gwb) > 0) {
      app_bin_map <- dplyr::tribble(
        ~app_bin,  ~natl_bin,
        "<12",     "<32",
        "12-27",   "<32",
        "28-31",   "<32",
        "32-36",   "32-36",
        "37-38",   "37-38",
        "39-41",   "39-41",
        "42-43",   ">=42",
        "45-49",   ">=42",
        ">=50",    ">=42"
      )

      gwb_filtered <- gwb %>%
        dplyr::filter(
          tolower(.data$cdm_name) == tolower(db_name) |
            grepl(paste0("^", tolower(sub("_v[0-9]+$", "", db_name))),
                  tolower(.data$cdm_name))
        )
      if ("final_outcome_category" %in% colnames(gwb_filtered)) {
        gwb_filtered <- gwb_filtered %>%
          dplyr::filter(.data$final_outcome_category %in% lb_categories)
      }

      int_gest <- gwb_filtered %>%
        dplyr::mutate(gestational_weeks = as.character(.data$gestational_weeks)) %>%
        dplyr::left_join(app_bin_map, by = c("gestational_weeks" = "app_bin")) %>%
        dplyr::group_by(.data$natl_bin) %>%
        dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop")

      total_n <- sum(int_gest$n, na.rm = TRUE)
      if (total_n > 0) {
        int_gest <- int_gest %>%
          dplyr::mutate(
            pct = round(100 * .data$n / total_n, 1),
            n = round(.data$n)
          ) %>%
          dplyr::rename(bin = "natl_bin")
      }
    }
  }

  # 5. Delivery mode (% by mode)
  #    Source: deliveryModeSummary aggregated by mode
  int_dm <- tibble::tibble()
  if (exists("deliveryModeSummary")) {
    dms <- deliveryModeSummary
    if (!is.null(dms) && nrow(dms) > 0) {
      dm_db <- dms %>%
        dplyr::filter(
          tolower(.data$cdm_name) == tolower(db_name) |
            grepl(paste0("^", tolower(sub("_v[0-9]+$", "", db_name))),
                  tolower(.data$cdm_name))
        )
      if ("final_outcome_category" %in% colnames(dm_db)) {
        dm_db <- dm_db %>%
          dplyr::filter(.data$final_outcome_category %in% lb_categories)
      }
      dm_db <- dm_db %>%
        dplyr::mutate(
          n = suppressWarnings(as.numeric(.data$n)),
          pct = suppressWarnings(as.numeric(.data$pct))
        )

      if ("n_known" %in% colnames(dm_db)) {
        dm_db <- dm_db %>%
          dplyr::mutate(n_known = suppressWarnings(as.numeric(.data$n_known))) %>%
          dplyr::group_by(.data$mode) %>%
          dplyr::summarise(n = sum(.data$n, na.rm = TRUE),
                            n_known = sum(.data$n_known, na.rm = TRUE),
                            .groups = "drop") %>%
          dplyr::mutate(pct = round(100 * .data$n / .data$n_known, 1))
      } else {
        dm_db <- dm_db %>%
          dplyr::group_by(.data$mode) %>%
          dplyr::summarise(pct = mean(.data$pct, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(pct = round(.data$pct, 1))
      }
      int_dm <- dm_db %>%
        dplyr::mutate(mode = dplyr::case_when(
          tolower(.data$mode) == "vaginal"   ~ "Vaginal",
          tolower(.data$mode) == "cesarean"  ~ "Cesarean",
          TRUE ~ .data$mode
        )) %>%
        dplyr::select("mode", "pct")
    }
  }

  # ---- Match reference rows with internal values ----
  result <- ext %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      internal_value = {
        ind <- .data$indicator
        var <- .data$variable
        lvl <- .data$level
        yr <- .data$year

        if (ind == "Gestational duration distribution") {
          if (nrow(int_gest) > 0) {
            natl_bin <- dplyr::case_when(
              lvl == "\u226542" ~ ">=42",
              TRUE ~ lvl
            )
            match_row <- int_gest %>% dplyr::filter(.data$bin == natl_bin)
            if (nrow(match_row) > 0) match_row$pct[1] else NA_real_
          } else NA_real_

        } else if (ind == "Mode of delivery") {
          if (nrow(int_dm) > 0) {
            mode_match <- dplyr::case_when(
              tolower(lvl) == "vaginal"   ~ "Vaginal",
              tolower(lvl) == "c-section" ~ "Cesarean",
              TRUE ~ lvl
            )
            match_row <- int_dm %>% dplyr::filter(.data$mode == mode_match)
            if (nrow(match_row) > 0) match_row$pct[1] else NA_real_
          } else NA_real_

        } else if (grepl("Mean age", var)) {
          if (nrow(int_age) > 0) {
            match_row <- int_age %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) match_row$mean[1] else NA_real_
          } else NA_real_

        } else if (grepl("Crude birth rate", var)) {
          if (nrow(int_crude) > 0) {
            match_row <- int_crude %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) match_row$crude_rate_per_1000[1] else NA_real_
          } else NA_real_

        } else if (grepl("Number of live births", var)) {
          if (nrow(int_trend) > 0) {
            match_row <- int_trend %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) match_row$count[1] else NA_real_
          } else NA_real_

        } else if (grepl("Foetal mortality", var)) {
          if (nrow(int_fm) > 0) {
            match_row <- int_fm %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) match_row$fm_rate[1] else NA_real_
          } else NA_real_

        } else {
          NA_real_
        }
      },
      internal_numerator = {
        ind <- .data$indicator
        var <- .data$variable
        yr <- .data$year

        if (grepl("Crude birth rate", var)) {
          # Birth rate uses LB or LB+DELIV based on lb_categories
          if (nrow(int_crude) > 0) {
            match_row <- int_crude %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) match_row$numerator[1] else NA_real_
          } else NA_real_
        } else if (grepl("Number of live births", var)) {
          NA_real_
        } else if (ind == "Gestational duration distribution") {
          if (nrow(int_gest) > 0) {
            natl_bin <- dplyr::case_when(.data$level == "\u226542" ~ ">=42", TRUE ~ .data$level)
            match_row <- int_gest %>% dplyr::filter(.data$bin == natl_bin)
            if (nrow(match_row) > 0) match_row$n[1] else NA_real_
          } else NA_real_
        } else if (grepl("Foetal mortality", var)) {
          if (nrow(int_fm) > 0) {
            match_row <- int_fm %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) match_row$sb_count[1] else NA_real_
          } else NA_real_
        } else {
          NA_real_
        }
      },
      internal_denominator = {
        ind <- .data$indicator
        var <- .data$variable
        yr <- .data$year

        if (grepl("Crude birth rate", var)) {
          if (nrow(int_crude) > 0) {
            match_row <- int_crude %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) match_row$denominator_count[1] else NA_real_
          } else NA_real_
        } else if (grepl("Number of live births", var)) {
          NA_real_
        } else if (ind == "Gestational duration distribution") {
          if (nrow(int_gest) > 0) sum(int_gest$n, na.rm = TRUE) else NA_real_
        } else if (grepl("Foetal mortality", var)) {
          if (nrow(int_fm) > 0) {
            match_row <- int_fm %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) match_row$total[1] else NA_real_
          } else NA_real_
        } else {
          NA_real_
        }
      },
      internal_note = {
        ind <- .data$indicator
        var <- .data$variable
        yr <- .data$year

        if (ind == "Gestational duration distribution") {
          if (nrow(int_gest) > 0) {
            lvl_check <- .data$level
            natl_bin_check <- dplyr::case_when(
              lvl_check == "\u226542" ~ ">=42",
              TRUE ~ lvl_check
            )
            dplyr::case_when(
              natl_bin_check == "<32"   ~ "% of LB; algorithm result sums <12+12-27+28-31 wk",
              natl_bin_check == "32-36" ~ "% of LB; 32-36 wk",
              natl_bin_check == "37-38" ~ "% of LB; 37-38 wk",
              natl_bin_check == "39-41" ~ "% of LB; 39-41 wk",
              natl_bin_check == ">=42"  ~ "% of LB; algorithm result sums 42-43+45-49+>=50 wk",
              TRUE ~ "% of LB (all years)"
            )
          } else "No gestational data"

        } else if (ind == "Mode of delivery") {
          if (nrow(int_dm) > 0) "Overall (all years, LB)" else "No delivery mode data"

        } else if (grepl("Mean age", var)) {
          if (nrow(int_age) > 0) {
            match_row <- int_age %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) paste0("Year-matched (", yr, ")") else "No data for this year"
          } else "No age data available"

        } else if (grepl("Crude birth rate", var)) {
          if (nrow(int_crude) > 0) {
            match_row <- int_crude %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) {
              paste0("(", paste(lb_categories, collapse = " + "), ") / population (0-150, Both), per 1000")
            } else "No data for this year"
          } else "N/A (prevalence data not available)"

        } else if (grepl("Number of live births", var)) {
          if (nrow(int_trend) > 0) {
            match_row <- int_trend %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) {
              paste0("Birth count in ", yr, " (from prevalence results)")
            } else "No data for this year"
          } else "No prevalence data available"

        } else if (grepl("Foetal mortality", var)) {
          if (nrow(int_fm) > 0) {
            match_row <- int_fm %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) {
              paste0("SB / (SB + ", paste(lb_categories, collapse = " + "), ") * 1000, year ", yr)
            } else "No data for this year"
          } else "No prevalence data"

        } else {
          ""
        }
      }
    ) %>%
    dplyr::ungroup()

  # Add numerator breakdown for birth rate rows (LB + DELIV = total)
  result <- result %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      numerator_breakdown = {
        var <- .data$variable
        yr <- .data$year
        if (grepl("Crude birth rate", var) && nrow(int_lb_only) > 0 && nrow(int_combined) > 0) {
          lb_row <- int_lb_only %>% dplyr::filter(.data$year == yr)
          comb_row <- int_combined %>% dplyr::filter(.data$year == yr)
          if (nrow(lb_row) > 0 && nrow(comb_row) > 0) {
            lb_val <- lb_row$lb_count[1]
            comb_val <- comb_row$combined_count[1]
            deliv_val <- comb_val - lb_val
            paste0(
              format(lb_val, big.mark = ",", scientific = FALSE, trim = TRUE),
              " + ",
              format(deliv_val, big.mark = ",", scientific = FALSE, trim = TRUE),
              " = ",
              format(comb_val, big.mark = ",", scientific = FALSE, trim = TRUE)
            )
          } else NA_character_
        } else NA_character_
      }
    ) %>%
    dplyr::ungroup()

  # Replace gestational reference counts with percentages for fair comparison
  result <- result %>%
    dplyr::left_join(ext_gest_pct, by = c("level", "year")) %>%
    dplyr::mutate(
      external_value = dplyr::if_else(
        .data$indicator == "Gestational duration distribution" & !is.na(.data$external_pct),
        .data$external_pct,
        .data$external_value
      ),
      variable = dplyr::if_else(
        .data$indicator == "Gestational duration distribution",
        "Distribution of live births by gestational age (%)",
        .data$variable
      )
    ) %>%
    dplyr::select(-"external_pct")

  # Adjust live birth count comparison:
  # Scale reference value by population coverage when known.
  # Always show both reference and algorithm values regardless of database type.
  coverage_pct <- .resolve_db_coverage(db_name)
  is_lb_row <- grepl("Number of live births", result$variable)

  if (!is.na(coverage_pct)) {
    # Known coverage: expected = national_total * coverage%
    result <- result %>%
      dplyr::mutate(
        external_value = dplyr::if_else(
          is_lb_row,
          round(.data$external_value * coverage_pct / 100),
          .data$external_value
        ),
        variable = dplyr::if_else(
          is_lb_row,
          "Number of live births per year",
          .data$variable
        )
      )
  }

  result
}


# ============================================================================
# Plot data builders
# ============================================================================

# ---- Gestational % comparison for plots ----
# Returns combined long-format tibble with reference and algorithm result gestational
# duration distributions, suitable for grouped bar charts.
.build_gest_pct_comparison <- function(natl, db_name, country_name, lb_categories = "LB") {
  natl_gest <- natl %>%
    dplyr::filter(.data$indicator == "Gestational duration distribution",
                  .data$country == country_name,
                  !is.na(.data$value)) %>%
    dplyr::select("level", "year", "value") %>%
    dplyr::rename(bin = "level") %>%
    dplyr::mutate(bin = dplyr::case_when(
      .data$bin %in% c("37-38", "39-41") ~ "37-41",
      .data$bin == "\u226542"             ~ ">=42",
      TRUE                               ~ .data$bin
    )) %>%
    dplyr::group_by(.data$year, .data$bin) %>%
    dplyr::summarise(n = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(.data$year) %>%
    dplyr::mutate(pct = round(100 * .data$n / sum(.data$n, na.rm = TRUE), 1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      source = "Reference (National Statistics)",
      label = paste0(country_name, " (Reference, ", .data$year, ")")
    )

  int_gest <- tibble::tibble()
  if (exists("gestationalWeeksBinned")) {
    gwb <- gestationalWeeksBinned
    if (!is.null(gwb) && nrow(gwb) > 0) {
      app_bin_map <- dplyr::tribble(
        ~app_bin,  ~natl_bin,
        "<12",     "<32",
        "12-27",   "<32",
        "28-31",   "<32",
        "32-36",   "32-36",
        "37-38",   "37-41",
        "39-41",   "37-41",
        "37-41",   "37-41",
        "42-43",   ">=42",
        "45-49",   ">=42",
        ">=50",    ">=42"
      )
      gwb_filtered <- gwb %>%
        dplyr::filter(
          tolower(.data$cdm_name) == tolower(db_name) |
            grepl(paste0("^", tolower(sub("_v[0-9]+$", "", db_name))),
                  tolower(.data$cdm_name))
        )
      if ("final_outcome_category" %in% colnames(gwb_filtered)) {
        gwb_filtered <- gwb_filtered %>%
          dplyr::filter(.data$final_outcome_category %in% lb_categories)
      }

      int_gest <- gwb_filtered %>%
        dplyr::mutate(gestational_weeks = as.character(.data$gestational_weeks)) %>%
        dplyr::left_join(app_bin_map, by = c("gestational_weeks" = "app_bin")) %>%
        dplyr::group_by(.data$natl_bin) %>%
        dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop")

      total_n <- sum(int_gest$n, na.rm = TRUE)
      lb_label <- paste(lb_categories, collapse = "+")
      if (total_n > 0) {
        int_gest <- int_gest %>%
          dplyr::mutate(
            pct = round(100 * .data$n / total_n, 1),
            bin = .data$natl_bin,
            year = NA_integer_,
            source = "Algorithm",
            label = paste0(db_name, " (Algorithm, ", lb_label, ")")
          ) %>%
          dplyr::select("year", "bin", "n", "pct", "source", "label")
      }
    }
  }

  dplyr::bind_rows(natl_gest, int_gest)
}

# ---- Delivery mode comparison for plots ----
.build_dm_pct_comparison <- function(natl, db_name, country_name, lb_categories = "LB") {
  natl_dm <- natl %>%
    dplyr::filter(.data$indicator == "Mode of delivery",
                  .data$country == country_name,
                  !is.na(.data$value)) %>%
    dplyr::select("level", "year", "value") %>%
    dplyr::rename(mode = "level", pct = "value") %>%
    dplyr::mutate(
      mode = dplyr::case_when(
        tolower(.data$mode) == "vaginal"   ~ "Vaginal",
        tolower(.data$mode) == "c-section" ~ "Cesarean",
        TRUE                               ~ .data$mode
      ),
      source = "Reference (National Statistics)",
      label = paste0(country_name, " (Reference, ", .data$year, ")")
    )

  int_dm <- tibble::tibble()
  if (exists("deliveryModeSummary")) {
    dms <- deliveryModeSummary
    if (!is.null(dms) && nrow(dms) > 0) {
      dm_db <- dms %>%
        dplyr::filter(
          tolower(.data$cdm_name) == tolower(db_name) |
            grepl(paste0("^", tolower(sub("_v[0-9]+$", "", db_name))),
                  tolower(.data$cdm_name))
        )
      if ("final_outcome_category" %in% colnames(dm_db)) {
        dm_db <- dm_db %>%
          dplyr::filter(.data$final_outcome_category %in% lb_categories)
      }
      dm_db <- dm_db %>%
        dplyr::mutate(
          n = suppressWarnings(as.numeric(.data$n)),
          pct = suppressWarnings(as.numeric(.data$pct))
        )

      if ("n_known" %in% colnames(dm_db)) {
        dm_db <- dm_db %>%
          dplyr::mutate(n_known = suppressWarnings(as.numeric(.data$n_known))) %>%
          dplyr::group_by(.data$mode) %>%
          dplyr::summarise(n = sum(.data$n, na.rm = TRUE),
                            n_known = sum(.data$n_known, na.rm = TRUE),
                            .groups = "drop") %>%
          dplyr::mutate(pct = round(100 * .data$n / .data$n_known, 1))
      } else {
        dm_db <- dm_db %>%
          dplyr::group_by(.data$mode) %>%
          dplyr::summarise(pct = mean(.data$pct, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(pct = round(.data$pct, 1))
      }
      lb_label <- paste(lb_categories, collapse = "+")
      int_dm <- dm_db %>%
        dplyr::mutate(
          mode = dplyr::case_when(
            tolower(.data$mode) == "vaginal"   ~ "Vaginal",
            tolower(.data$mode) == "cesarean"  ~ "Cesarean",
            TRUE ~ .data$mode
          ),
          year = NA_integer_,
          source = "Algorithm",
          label = paste0(db_name, " (Algorithm, ", lb_label, ")")
        ) %>%
        dplyr::select("mode", "year", "pct", "source", "label")
    }
  }

  dplyr::bind_rows(natl_dm, int_dm)
}


# ============================================================================
# All-database comparison builder
# ============================================================================
# Computes the comparison for every mapped database and stacks the results.
# Returns tibble: cdm_name, country, indicator, variable, level, year,
#   external_value, internal_value, diff_abs, diff_pct

.build_all_db_comparison <- function(natl, lb_categories = "LB") {
  mapped_dbs <- .get_mapped_databases()
  if (length(mapped_dbs) == 0) return(tibble::tibble())

  all_results <- purrr::map_dfr(mapped_dbs, function(db) {
    country <- .resolve_db_country(db)
    if (is.na(country)) return(tibble::tibble())
    comp <- tryCatch(.build_comparison(natl, db, country, lb_categories = lb_categories), error = function(e) tibble::tibble())
    if (nrow(comp) == 0) return(tibble::tibble())
    comp %>%
      dplyr::mutate(
        cdm_name = db,
        country = country,
        diff_abs = dplyr::if_else(
          is.na(.data$external_value) | is.na(.data$internal_value),
          NA_real_,
          .data$internal_value - .data$external_value
        ),
        diff_pct = dplyr::case_when(
          is.na(.data$external_value) | is.na(.data$internal_value) ~ NA_real_,
          .data$external_value != 0 ~ round(100 * (.data$internal_value - .data$external_value) / .data$external_value, 1),
          TRUE ~ NA_real_
        )
      )
  })

  all_results
}

# ---- Color coding for difference percentage ----
.diff_color <- function(pct) {
  dplyr::case_when(
    is.na(pct) ~ "#f5f5f5",       # grey: missing
    abs(pct) < 5   ~ "#d4edda",   # green: <5% difference
    abs(pct) < 10  ~ "#fff3cd",   # yellow: 5-10%
    abs(pct) < 50  ~ "#ffe0b2",   # orange: 10-50%
    TRUE           ~ "#f8d7da"    # red: >50%
  )
}
