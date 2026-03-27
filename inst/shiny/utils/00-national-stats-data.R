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
  "cprd gold"         = "UK",
  "cprd"              = "UK"
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

# ---- Extract yearly LB counts from incidence results ----
# Source: incidence summarised_result (outcome_count for _lb cohorts)
# Returns tibble: cdm_name, year, numerator
.extract_lb_counts_from_incidence <- function(inc, db_names) {
  empty <- tibble::tibble(cdm_name = character(0), year = integer(0), numerator = numeric(0))
  if (!is.data.frame(inc) || nrow(inc) == 0 ||
      !all(c("estimate_name", "estimate_value", "group_level") %in% colnames(inc))) {
    return(empty)
  }

  inc_lb <- inc %>%
    dplyr::filter(
      .data$estimate_name == "outcome_count",
      grepl("_lb$", .data$group_level)
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
    dplyr::filter(!is.na(.data$numerator), .data$numerator > 0) %>%
    dplyr::group_by(.data$cdm_name, .data$year) %>%
    dplyr::summarise(numerator = suppressWarnings(max(.data$numerator, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::filter(is.finite(.data$numerator))

  inc_lb
}

# ---- Build crude birth rates for mapped databases ----
# Source: incidence (denominator_count = population; outcome_count for _lb = numerator)
# Metric: (LB count / incidence denominator) * 1000
# Returns tibble: cdm_name, country, year, numerator, denominator_count, crude_rate_per_1000
.build_crude_rates_internal <- function() {
  mapped_dbs <- .get_mapped_databases()
  if (length(mapped_dbs) == 0) {
    return(tibble::tibble(
      cdm_name = character(0), country = character(0), year = integer(0),
      numerator = numeric(0), denominator_count = numeric(0), crude_rate_per_1000 = numeric(0)
    ))
  }

  # Denominator: max denominator_count per (cdm_name, year) from incidence results
  denom <- tibble::tibble(cdm_name = character(0), year = integer(0), denominator_count = numeric(0))
  if (exists("incidence", envir = .GlobalEnv)) {
    inc <- get("incidence", envir = .GlobalEnv)
    if (is.data.frame(inc) && nrow(inc) > 0 &&
        "estimate_name" %in% colnames(inc) && "estimate_value" %in% colnames(inc)) {

      inc_sub <- inc %>%
        dplyr::filter(.data$estimate_name == "denominator_count")
      if ("strata_name" %in% colnames(inc_sub)) {
        inc_sub <- inc_sub %>% dplyr::filter(.data$strata_name == "overall")
      }

      if (nrow(inc_sub) > 0) {
        if ("incidence_start_date" %in% colnames(inc_sub)) {
          inc_sub <- inc_sub %>%
            dplyr::mutate(
              year = suppressWarnings(as.integer(stringr::str_extract(.data$incidence_start_date, "^[0-9]+")))
            ) %>%
            dplyr::filter(!is.na(.data$year))
        } else if ("additional_name" %in% colnames(inc_sub) && "additional_level" %in% colnames(inc_sub)) {
          inc_sub <- inc_sub %>%
            dplyr::filter(grepl("incidence_start_date", .data$additional_name)) %>%
            dplyr::mutate(
              year = suppressWarnings(as.integer(stringr::str_extract(
                trimws(sub("\\s*&&&.*", "", .data$additional_level)), "^[0-9]+"
              )))
            ) %>%
            dplyr::filter(!is.na(.data$year))
        } else {
          inc_sub <- tibble::tibble()
        }
      }

      if (nrow(inc_sub) > 0) {
        denom <- inc_sub %>%
          dplyr::mutate(denominator_count = suppressWarnings(as.numeric(.data$estimate_value))) %>%
          dplyr::filter(!is.na(.data$denominator_count), .data$denominator_count > 0) %>%
          dplyr::group_by(.data$cdm_name, .data$year) %>%
          dplyr::summarise(denominator_count = suppressWarnings(max(.data$denominator_count, na.rm = TRUE)), .groups = "drop") %>%
          dplyr::filter(is.finite(.data$denominator_count))
      }
    }
  }

  # Numerator: yearly live birth counts
  num <- tibble::tibble(cdm_name = character(0), year = integer(0), numerator = numeric(0))
  if (exists("incidence", envir = .GlobalEnv)) {
    inc <- get("incidence", envir = .GlobalEnv)
    num <- .extract_lb_counts_from_incidence(inc, mapped_dbs)
  }

  out <- num %>%
    dplyr::inner_join(denom, by = c("cdm_name", "year")) %>%
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
#   - Foetal mortality rate: SB / (SB + LB) * 1000 (overall, all years)
#   - Gestational duration: LB distribution across bins (% comparison)
#   - Mode of delivery: vaginal vs cesarean % among LB
#
# Returns tibble: indicator, variable, level, year, external_value,
#   internal_value, internal_note

.build_comparison <- function(natl, db_name, country_name) {

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

  # 2. Yearly live birth counts (from incidence results, outcome_count for _lb)
  int_trend <- tibble::tibble()
  if (exists("incidence")) {
    lb_counts <- .extract_lb_counts_from_incidence(incidence, db_name)
    if (nrow(lb_counts) > 0) {
      int_trend <- lb_counts %>%
        dplyr::transmute(year = .data$year, count = .data$numerator)
    }
  }

  # 2b. Crude birth rate
  int_crude <- tibble::tibble()
  crude_rates_all <- .build_crude_rates_internal()
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

  # 3. Foetal mortality rate: SB / (SB + LB) * 1000 from outcomeCategoriesCount
  int_fm_rate <- NA_real_
  if (exists("outcomeCategoriesCount")) {
    oc <- outcomeCategoriesCount
    if (!is.null(oc) && nrow(oc) > 0 && "outcome_category" %in% colnames(oc)) {
      oc_db <- oc %>%
        dplyr::filter(
          tolower(.data$cdm_name) == tolower(db_name) |
            grepl(paste0("^", tolower(sub("_v[0-9]+$", "", db_name))),
                  tolower(.data$cdm_name)),
          .data$outcome_category %in% c("SB", "LB")
        ) %>%
        dplyr::mutate(n = suppressWarnings(as.numeric(.data$n)))
      sb <- sum(oc_db$n[oc_db$outcome_category == "SB"], na.rm = TRUE)
      total <- sum(oc_db$n, na.rm = TRUE)
      if (total > 0) int_fm_rate <- round(1000 * sb / total, 1)
    }
  }

  # 4. Gestational duration distribution (LB only, % per bin)
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
          dplyr::filter(.data$final_outcome_category %in% c("LB"))
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

  # 5. Delivery mode (LB only, %)
  #    Source: deliveryModeSummary filtered to LB, aggregated by mode
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
          dplyr::filter(.data$final_outcome_category %in% c("LB"))
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
          int_fm_rate

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
              natl_bin_check == "<32"   ~ "% of LB; our result sums <12+12-27+28-31 wk",
              natl_bin_check == "32-36" ~ "% of LB; 32-36 wk",
              natl_bin_check == "37-38" ~ "% of LB; 37-38 wk",
              natl_bin_check == "39-41" ~ "% of LB; 39-41 wk",
              natl_bin_check == ">=42"  ~ "% of LB; our result sums 42-43+45-49+>=50 wk",
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
              "LB count / incidence denominator (overall, both sexes), per 1000"
            } else "No data for this year"
          } else "N/A (incidence denominator or trend data not available)"

        } else if (grepl("Number of live births", var)) {
          if (nrow(int_trend) > 0) {
            match_row <- int_trend %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) {
              paste0("Live birth episodes in ", yr, " (from incidence results)")
            } else "No data for this year"
          } else "No incidence data available"

        } else if (grepl("Foetal mortality", var)) {
          if (!is.na(int_fm_rate)) "Overall rate (all years)" else "No outcome data"

        } else {
          ""
        }
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

  result
}


# ============================================================================
# Plot data builders
# ============================================================================

# ---- Gestational % comparison for plots ----
# Returns combined long-format tibble with reference and our result gestational
# duration distributions, suitable for grouped bar charts.
.build_gest_pct_comparison <- function(natl, db_name, country_name) {
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
          dplyr::filter(.data$final_outcome_category %in% c("LB"))
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
            bin = .data$natl_bin,
            year = NA_integer_,
            source = "Our Result",
            label = paste0(db_name, " (Our Result, LB)")
          ) %>%
          dplyr::select("year", "bin", "n", "pct", "source", "label")
      }
    }
  }

  dplyr::bind_rows(natl_gest, int_gest)
}

# ---- Delivery mode comparison for plots ----
.build_dm_pct_comparison <- function(natl, db_name, country_name) {
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
          dplyr::filter(.data$final_outcome_category %in% c("LB"))
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
        dplyr::mutate(
          mode = dplyr::case_when(
            tolower(.data$mode) == "vaginal"   ~ "Vaginal",
            tolower(.data$mode) == "cesarean"  ~ "Cesarean",
            TRUE ~ .data$mode
          ),
          year = NA_integer_,
          source = "Our Result",
          label = paste0(db_name, " (Our Result, LB)")
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

.build_all_db_comparison <- function(natl) {
  mapped_dbs <- .get_mapped_databases()
  if (length(mapped_dbs) == 0) return(tibble::tibble())

  all_results <- purrr::map_dfr(mapped_dbs, function(db) {
    country <- .resolve_db_country(db)
    if (is.na(country)) return(tibble::tibble())
    comp <- tryCatch(.build_comparison(natl, db, country), error = function(e) tibble::tibble())
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
