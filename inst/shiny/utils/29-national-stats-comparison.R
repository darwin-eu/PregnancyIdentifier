# 29-national-stats-comparison.R - National statistics comparison module
# Compares our results with reference (national statistics) values as validation.
# Compares databases from 14 countries across Europe.

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
  # dplyr may call this on 0-row dataframes, resulting in a 0-length vector.
  # In that case there is no meaningful mapping, so return NA.
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
.get_mapped_databases <- function() {
  if (!exists("allDP")) return(character(0))
  dbs <- allDP
  mapped <- vapply(dbs, function(db) !is.na(.resolve_db_country(db)), logical(1))
  dbs[mapped]
}

# ---- Our crude rates (pregnancy count / incidence denominator, per 1000) ----
# Builds a dataframe of crude rates for DBs that match a country: uses pregnancy counts
# (numerators) from trendData and population denominators from the incidence
# summarised result (overall, both sexes, by year). Only for mapped databases.
# Returns tibble: cdm_name, country, year, numerator, denominator_count, crude_rate_per_1000
.build_crude_rates_internal <- function() {
  mapped_dbs <- .get_mapped_databases()
  if (length(mapped_dbs) == 0) {
    return(tibble::tibble(
      cdm_name = character(0), country = character(0), year = integer(0),
      numerator = numeric(0), denominator_count = numeric(0), crude_rate_per_1000 = numeric(0)
    ))
  }
  # Denominator: from incidence (overall, both sexes, by year)
  # Uses maximum denominator_count per (cdm_name, year) as the total population count
  denom <- tibble::tibble(cdm_name = character(0), year = integer(0), denominator_count = numeric(0))
  if (exists("incidence", envir = .GlobalEnv)) {
    inc <- get("incidence", envir = .GlobalEnv)
    if (is.data.frame(inc) && nrow(inc) > 0 &&
        "estimate_name" %in% colnames(inc) && "estimate_value" %in% colnames(inc)) {

      # Filter to denominator_count rows, overall strata
      inc_sub <- inc %>%
        dplyr::filter(.data$estimate_name == "denominator_count")
      if ("strata_name" %in% colnames(inc_sub)) {
        inc_sub <- inc_sub %>% dplyr::filter(.data$strata_name == "overall")
      }

      # Extract year from the data
      if (nrow(inc_sub) > 0) {
        if ("incidence_start_date" %in% colnames(inc_sub)) {
          # Direct column
          inc_sub <- inc_sub %>%
            dplyr::mutate(
              year = suppressWarnings(as.integer(stringr::str_extract(.data$incidence_start_date, "^[0-9]+")))
            ) %>%
            dplyr::filter(!is.na(.data$year))
        } else if ("additional_name" %in% colnames(inc_sub) && "additional_level" %in% colnames(inc_sub)) {
          # &&&-delimited format: additional_name = "incidence_start_date &&& ..."
          # additional_level = "2015-01-01 &&& 2015-12-31 &&& years"
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
        # Take max denominator per (cdm_name, year) = total population
        denom <- inc_sub %>%
          dplyr::mutate(denominator_count = suppressWarnings(as.numeric(.data$estimate_value))) %>%
          dplyr::filter(!is.na(.data$denominator_count), .data$denominator_count > 0) %>%
          dplyr::group_by(.data$cdm_name, .data$year) %>%
          dplyr::summarise(denominator_count = max(.data$denominator_count, na.rm = TRUE), .groups = "drop")
      }
    }
  }
  # Numerator: yearly pregnancy (episode) counts from trendData
  num <- tibble::tibble(cdm_name = character(0), year = integer(0), numerator = numeric(0))
  if (exists("trendData", envir = .GlobalEnv)) {
    td <- get("trendData", envir = .GlobalEnv)
    if (!is.null(td) && nrow(td) > 0 &&
        "period" %in% colnames(td) && "column" %in% colnames(td) && "value" %in% colnames(td) && "count" %in% colnames(td)) {
      num <- td %>%
        dplyr::filter(
          .data$period == "year",
          .data$column == "final_episode_end_date",
          tolower(.data$cdm_name) %in% tolower(mapped_dbs)
        ) %>%
        dplyr::mutate(
          year = as.integer(.data$value),
          numerator = as.numeric(.data$count)
        ) %>%
        dplyr::filter(!is.na(.data$year)) %>%
        dplyr::select("cdm_name", "year", "numerator") %>%
        dplyr::distinct()
    }
  }
  # Join and compute rate per 1000; only for mapped (cdm_name, country)
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

# ---- Load and parse national statistics ----
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


# ---- Build row-matched comparison for a single DB <-> Country pair ----
# Returns a tibble with columns: indicator, variable, level, year,
#   external_value, internal_value, internal_note
.build_comparison <- function(natl, db_name, country_name) {

  # ---- Reference data for this country ----
  ext <- natl %>%
    dplyr::filter(.data$country == country_name) %>%
    dplyr::select("indicator", "variable", "level", "year", "value") %>%
    dplyr::rename(external_value = "value")

  if (nrow(ext) == 0) return(tibble::tibble())

  # Pre-compute reference gestational percentages per year (for fair comparison)
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

  # ---- Gather all our result data sources ----

  # 1. Maternal age (year-matched)
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

  # 2. Yearly episode counts (year-matched)
  int_trend <- tibble::tibble()
  if (exists("trendData")) {
    td <- trendData
    if (!is.null(td) && nrow(td) > 0) {
      int_trend <- td %>%
        dplyr::filter(
          .data$period == "year",
          .data$column == "final_episode_end_date",
          tolower(.data$cdm_name) == tolower(db_name) |
            grepl(paste0("^", tolower(sub("_v[0-9]+$", "", db_name))),
                  tolower(.data$cdm_name))
        ) %>%
        dplyr::mutate(
          year = as.integer(.data$value),
          count = as.numeric(.data$count)
        ) %>%
        dplyr::select("year", "count")
    }
  }

  # 2b. Crude birth rate (pregnancy count / incidence denominator, overall both sexes, by year)
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

  # 3. Foetal mortality rate (overall, no year breakdown)
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

  # 4. Gestational duration distribution (LB only, percentage per bin)
  # National stats report live births by gestational age, so we filter to LB.
  # Our data spans all years, so we compare distributions (%) not counts.
  int_gest <- tibble::tibble()
  if (exists("gestationalWeeksBinned")) {
    gwb <- gestationalWeeksBinned
    if (!is.null(gwb) && nrow(gwb) > 0) {
      # Map our app bins to national stats bins.
      # The national stats CSV has individual bins: <32, 32-36, 37-38, 39-41, >=42
      # Our data has: <12, 12-27, 28-31, 32-36, 37-38, 39-41, 42-43, 44-49, >=50
      app_bin_map <- dplyr::tribble(
        ~app_bin,  ~natl_bin,
        "<12",     "<32",
        "12-27",   "<32",
        "28-31",   "<32",
        "32-36",   "32-36",
        "37-38",   "37-38",
        "39-41",   "39-41",
        "42-43",   ">=42",
        "44-49",   ">=42",
        ">=50",    ">=42"
      )

      gwb_filtered <- gwb %>%
        dplyr::filter(
          tolower(.data$cdm_name) == tolower(db_name) |
            grepl(paste0("^", tolower(sub("_v[0-9]+$", "", db_name))),
                  tolower(.data$cdm_name))
        )
      # Filter to live births only if outcome column is present
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

  # 5. Delivery mode (LB only, percentage)
  # National stats report delivery mode for live births, so filter to LB.
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
      # Filter to LB only to match national stats (delivery mode for live births)
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

  # ---- Match reference rows with our result values ----
  result <- ext %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      internal_value = {
        ind <- .data$indicator
        var <- .data$variable
        lvl <- .data$level
        yr <- .data$year

        # Check indicator-based matches FIRST (before variable-based)
        # because gestational variable text contains "live births" which
        # would incorrectly match the "Number of live births per year" check.
        if (ind == "Gestational duration distribution") {
          # Gestational bins: compare percentages (distributions)
          # Reference data is in counts; our data spans all years so only % is comparable
          if (nrow(int_gest) > 0) {
            natl_bin <- dplyr::case_when(
              lvl == "\u226542" ~ ">=42",
              TRUE ~ lvl
            )
            match_row <- int_gest %>% dplyr::filter(.data$bin == natl_bin)
            if (nrow(match_row) > 0) match_row$pct[1] else NA_real_
          } else NA_real_

        } else if (ind == "Mode of delivery") {
          # Delivery mode: match by level (Vaginal/C-section)
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
          # Maternal age: year-matched
          if (nrow(int_age) > 0) {
            match_row <- int_age %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) match_row$mean[1] else NA_real_
          } else NA_real_

        } else if (grepl("Crude birth rate", var)) {
          # Birth rate: pregnancy count / incidence denominator (overall, both sexes), per 1000
          if (nrow(int_crude) > 0) {
            match_row <- int_crude %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) match_row$crude_rate_per_1000[1] else NA_real_
          } else NA_real_

        } else if (grepl("Number of live births", var)) {
          # Live births: match with yearly total episodes
          if (nrow(int_trend) > 0) {
            match_row <- int_trend %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) match_row$count[1] else NA_real_
          } else NA_real_

        } else if (grepl("Foetal mortality", var)) {
          # Foetal mortality: overall rate (no yearly breakdown)
          int_fm_rate

        } else {
          NA_real_
        }
      },
      internal_note = {
        ind <- .data$indicator
        var <- .data$variable
        yr <- .data$year

        # Check indicator-based matches FIRST (same order as internal_value)
        if (ind == "Gestational duration distribution") {
          if (nrow(int_gest) > 0) {
            lvl_check <- .data$level
            natl_bin_check <- dplyr::case_when(
              lvl_check == "\u226542" ~ ">=42",
              TRUE ~ lvl_check
            )
            # Show which of our bins were summed; both values are %
            bin_sources <- dplyr::case_when(
              natl_bin_check == "<32"   ~ "% of LB; our result sums <12+12-27+28-31 wk",
              natl_bin_check == "32-36" ~ "% of LB; 32-36 wk",
              natl_bin_check == "37-38" ~ "% of LB; 37-38 wk",
              natl_bin_check == "39-41" ~ "% of LB; 39-41 wk",
              natl_bin_check == ">=42"  ~ "% of LB; our result sums 42-43+44-49+>=50 wk",
              TRUE ~ "% of LB (all years)"
            )
            bin_sources
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
              "Pregnancy count / incidence denominator (overall, both sexes), per 1000"
            } else "No data for this year"
          } else "N/A (incidence denominator or trend data not available)"

        } else if (grepl("Number of live births", var)) {
          if (nrow(int_trend) > 0) {
            match_row <- int_trend %>% dplyr::filter(.data$year == yr)
            if (nrow(match_row) > 0) {
              paste0("Total pregnancy episodes in ", yr, " (all outcomes)")
            } else "No data for this year"
          } else "No trend data available"

        } else if (grepl("Foetal mortality", var)) {
          if (!is.na(int_fm_rate)) "Overall rate (all years)" else "No outcome data"

        } else {
          ""
        }
      }
    ) %>%
    dplyr::ungroup()

  # For gestational duration rows, replace reference counts with percentages
  # so both columns are comparable (our result is already %)
  result <- result %>%
    dplyr::left_join(ext_gest_pct, by = c("level", "year")) %>%
    dplyr::mutate(
      external_value = dplyr::if_else(
        .data$indicator == "Gestational duration distribution" & !is.na(.data$external_pct),
        .data$external_pct,
        .data$external_value
      ),
      # Update variable text to indicate both values are percentages
      variable = dplyr::if_else(
        .data$indicator == "Gestational duration distribution",
        "Distribution of live births by gestational age (%)",
        .data$variable
      )
    ) %>%
    dplyr::select(-"external_pct")

  # Delivery mode: reference is already in % so no change needed

  result
}


# ---- Compute gestational pct comparison for plots ----
.build_gest_pct_comparison <- function(natl, db_name, country_name) {
  # Reference: gestational bins as percentages
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

  # Our result: gestational bins as percentages
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
        "44-49",   ">=42",
        ">=50",    ">=42"
      )
      gwb_filtered <- gwb %>%
        dplyr::filter(
          tolower(.data$cdm_name) == tolower(db_name) |
            grepl(paste0("^", tolower(sub("_v[0-9]+$", "", db_name))),
                  tolower(.data$cdm_name))
        )
      # Filter to live births only for fair comparison with national stats
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

# ---- Compute delivery mode comparison for plots ----
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
      # Filter to LB only to match national stats
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


# ---- Build comparison for ALL mapped databases ----
# Returns a tibble with columns: cdm_name, country, indicator, variable, level,
#   year, external_value, internal_value, diff_abs, diff_pct
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

# ---- Color coding helper for difference percentage ----
# Returns a background color based on absolute percentage difference
.diff_color <- function(pct) {
  dplyr::case_when(
    is.na(pct) ~ "#f5f5f5",       # grey for missing
    abs(pct) < 5   ~ "#d4edda",   # green: no meaningful diff
    abs(pct) < 10  ~ "#fff3cd",   # yellow: small diff
    abs(pct) < 50  ~ "#ffe0b2",   # orange: moderate diff
    TRUE           ~ "#f8d7da"    # red: extreme diff >50%
  )
}


# ---- UI ----
nationalStatsComparisonUI <- function(id) {
  ns <- NS(id)

  tagList(
    h3("National Statistics Comparison"),
    p("Compare our results with reference values from national statistics as validation.",
      "Each reference value is matched with its counterpart from our results."),
    tabsetPanel(
      id = ns("tabs"),
      type = "tabs",

      # -- Tab 1: Matched comparison table --
      tabPanel(
        "Comparison Table",
        br(),
        p("Row-by-row comparison of reference values (national statistics) with our results.",
          "Years and levels are matched where possible."),
        fluidRow(
          column(4, pickerInput(ns("comp_db"), "Database (our result)",
                                choices = character(0), selected = character(0),
                                multiple = FALSE, options = opt)),
          column(4, uiOutput(ns("comp_country_display")))
        ),
        gt::gt_output(ns("comparison_table")) %>% withSpinner(type = 6),
        downloadButton(ns("download_comparison"), "Download table (.docx)")
      ),

      # -- Tab 2: Gestational duration comparison --
      tabPanel(
        "Gestational Duration",
        br(),
        p("Distribution of births by gestational age bin: our results vs. reference values (national statistics), shown as percentages."),
        tabsetPanel(
          tabPanel(
            "Table",
            gt::gt_output(ns("gest_table")) %>% withSpinner(type = 6),
            downloadButton(ns("download_gest_table"), "Download table (.docx)")
          ),
          tabPanel(
            "Plot",
            fluidRow(
              column(3, textInput(ns("gest_plot_height"), "Height (cm)", value = "12")),
              column(3, textInput(ns("gest_plot_width"), "Width (cm)", value = "24")),
              column(3, textInput(ns("gest_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            plotlyOutput(ns("gest_plot"), height = "600px") %>% withSpinner(type = 6),
            downloadButton(ns("download_gest_plot"), "Download plot (PNG)")
          )
        )
      ),

      # -- Tab 3: Delivery mode comparison --
      tabPanel(
        "Delivery Mode",
        br(),
        p("Cesarean vs. vaginal delivery rates: our results vs. reference values (national statistics)."),
        tabsetPanel(
          tabPanel(
            "Table",
            gt::gt_output(ns("dm_table")) %>% withSpinner(type = 6),
            downloadButton(ns("download_dm_table"), "Download table (.docx)")
          ),
          tabPanel(
            "Plot",
            fluidRow(
              column(3, textInput(ns("dm_plot_height"), "Height (cm)", value = "10")),
              column(3, textInput(ns("dm_plot_width"), "Width (cm)", value = "20")),
              column(3, textInput(ns("dm_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            plotlyOutput(ns("dm_plot"), height = "500px") %>% withSpinner(type = 6),
            downloadButton(ns("download_dm_plot"), "Download plot (PNG)")
          )
        )
      ),

      # -- Tab 4: Year-level metrics --
      tabPanel(
        "Year-Level Metrics",
        br(),
        p("Yearly comparison of our episode counts, maternal age, and foetal mortality against reference values (national statistics)."),
        tabsetPanel(
          tabPanel(
            "Live Births / Episodes",
            plotlyOutput(ns("live_births_plot"), height = "500px") %>% withSpinner(type = 6),
            fluidRow(
              column(3, textInput(ns("lb_plot_height"), "Height (cm)", value = "10")),
              column(3, textInput(ns("lb_plot_width"), "Width (cm)", value = "20")),
              column(3, textInput(ns("lb_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            downloadButton(ns("download_lb_plot"), "Download plot (PNG)")
          ),
          tabPanel(
            "Maternal Age",
            plotlyOutput(ns("maternal_age_plot"), height = "500px") %>% withSpinner(type = 6),
            fluidRow(
              column(3, textInput(ns("ma_plot_height"), "Height (cm)", value = "10")),
              column(3, textInput(ns("ma_plot_width"), "Width (cm)", value = "20")),
              column(3, textInput(ns("ma_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            downloadButton(ns("download_ma_plot"), "Download plot (PNG)")
          ),
          tabPanel(
            "Foetal Mortality",
            plotlyOutput(ns("foetal_mort_plot"), height = "500px") %>% withSpinner(type = 6),
            fluidRow(
              column(3, textInput(ns("fm_plot_height"), "Height (cm)", value = "10")),
              column(3, textInput(ns("fm_plot_width"), "Width (cm)", value = "20")),
              column(3, textInput(ns("fm_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            downloadButton(ns("download_fm_plot"), "Download plot (PNG)")
          )
        )
      ),

      # -- Tab 5: All Databases comparison --
      tabPanel(
        "All Databases",
        br(),
        p("Comparison table across all mapped databases. Use the metric filter to focus on specific indicators."),
        fluidRow(
          column(6, pickerInput(ns("alldb_metrics"), "Filter by metric",
                                choices = character(0), selected = character(0),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE,
                                               `selected-text-format` = "count > 2",
                                               `count-selected-text` = "{0} metrics selected",
                                               `live-search` = TRUE)))
        ),
        gt::gt_output(ns("alldb_table")) %>% withSpinner(type = 6),
        downloadButton(ns("download_alldb"), "Download table (.docx)")
      ),

      # -- Tab 6: Difference Overview across DBs --
      tabPanel(
        "Difference Overview",
        br(),
        p("Shows the percentage difference for each indicator across all databases.",
          "Color coding: ",
          tags$span("< 5%", style = "background-color:#d4edda; padding:2px 6px; border-radius:3px;"), " ",
          tags$span("5-10%", style = "background-color:#fff3cd; padding:2px 6px; border-radius:3px;"), " ",
          tags$span("10-50%", style = "background-color:#ffe0b2; padding:2px 6px; border-radius:3px;"), " ",
          tags$span("> 50%", style = "background-color:#f8d7da; padding:2px 6px; border-radius:3px;")),
        fluidRow(
          column(6, pickerInput(ns("diffov_metrics"), "Filter by metric",
                                choices = character(0), selected = character(0),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE,
                                               `selected-text-format` = "count > 2",
                                               `count-selected-text` = "{0} metrics selected",
                                               `live-search` = TRUE)))
        ),
        gt::gt_output(ns("diffov_table")) %>% withSpinner(type = 6),
        downloadButton(ns("download_diffov"), "Download table (.docx)")
      ),

      # -- Tab 7: Raw data --
      tabPanel(
        "Raw Data",
        br(),
        DT::DTOutput(ns("raw_table")) %>% withSpinner(type = 6),
        downloadButton(ns("download_raw"), "Download raw data (.csv)")
      )
    )
  )
}


# ---- Server ----
nationalStatsComparisonServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: load national stats once
    natl_data <- reactive({ .load_national_stats() })

    # ---- Update picker inputs ----
    observe({
      natl <- natl_data()
      if (is.null(natl)) return()
      mapped_dbs <- .get_mapped_databases()
      databases <- if (exists("allDP")) allDP else character(0)

      # Only show mapped databases in the comparison picker
      if (length(mapped_dbs) > 0) {
        updatePickerInput(session, "comp_db",
                          choices = mapped_dbs,
                          selected = mapped_dbs[1])
      } else {
        updatePickerInput(session, "comp_db",
                          choices = databases,
                          selected = if (length(databases) > 0) databases[1] else character(0))
      }
    })

    # Show which country is mapped
    output$comp_country_display <- renderUI({
      db <- input$comp_db
      if (is.null(db) || !nzchar(db)) return(NULL)
      country <- .resolve_db_country(db)
      if (is.na(country)) {
        tags$div(class = "alert alert-warning",
                 paste0("No country mapping found for '", db, "'."))
      } else {
        tags$div(
          tags$strong("Mapped country (reference): "),
          tags$span(country, style = "font-size: 1.1em; color: #337ab7;")
        )
      }
    })

    # Reactive: selected DB and country
    selected_pair <- reactive({
      db <- req(input$comp_db)
      country <- .resolve_db_country(db)
      req(!is.na(country))
      list(db = db, country = country)
    })

    # ========== TAB 1: Comparison Table ==========
    comparison_data <- reactive({
      natl <- natl_data()
      if (is.null(natl)) return(tibble::tibble())
      pair <- selected_pair()
      .build_comparison(natl, pair$db, pair$country)
    })

    comparison_gt <- reactive({
      d <- comparison_data()
      validate(need(nrow(d) > 0, "No comparison data available."))
      pair <- selected_pair()

      d_display <- d %>%
        dplyr::mutate(
          external_fmt = dplyr::if_else(
            is.na(.data$external_value), "-",
            format(.data$external_value, big.mark = ",", scientific = FALSE, trim = TRUE)
          ),
          internal_fmt = dplyr::if_else(
            is.na(.data$internal_value), "-",
            format(.data$internal_value, big.mark = ",", scientific = FALSE, trim = TRUE)
          ),
          difference = dplyr::case_when(
            is.na(.data$external_value) | is.na(.data$internal_value) ~ "-",
            .data$external_value != 0 ~ paste0(
              format(round(.data$internal_value - .data$external_value, 1),
                     big.mark = ",", trim = TRUE),
              " (",
              ifelse(.data$internal_value >= .data$external_value, "+", ""),
              round(100 * (.data$internal_value - .data$external_value) / .data$external_value, 1),
              "%)"
            ),
            TRUE ~ format(round(.data$internal_value - .data$external_value, 1),
                          big.mark = ",", trim = TRUE)
          ),
          year = dplyr::if_else(is.na(.data$year), "-", as.character(.data$year))
        ) %>%
        dplyr::select(
          "indicator", "variable", "level", "year",
          "external_fmt", "internal_fmt", "difference", "internal_note"
        )

      gt::gt(d_display) %>%
        gt::tab_header(
          title = paste0("National Statistics Comparison: ",
                         pair$db, " (Our Result) vs. ", pair$country, " (Reference)"),
          subtitle = "Row-by-row matching of each reference value with our results"
        ) %>%
        gt::cols_label(
          indicator = "Indicator",
          variable = "Variable",
          level = "Level",
          year = "Year",
          external_fmt = paste0(pair$country, "\n(Reference)"),
          internal_fmt = paste0(pair$db, "\n(Our Result)"),
          difference = "Difference",
          internal_note = "Notes"
        ) %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#f0f7ff"),
          locations = gt::cells_body(columns = "external_fmt")
        ) %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#fff0f0"),
          locations = gt::cells_body(columns = "internal_fmt")
        ) %>%
        gt::sub_missing(missing_text = "-") %>%
        gt::tab_options(table.font.size = "small") %>%
        gt::cols_width(
          "internal_note" ~ gt::px(200),
          "variable" ~ gt::px(250)
        )
    })

    output$comparison_table <- gt::render_gt({ comparison_gt() })

    output$download_comparison <- downloadHandler(
      filename = function() "national_stats_comparison.docx",
      content = function(file) {
        tryCatch(gt::gtsave(comparison_gt(), file), error = function(e) NULL)
      }
    )


    # ========== TAB 2: Gestational Duration ==========
    gest_data <- reactive({
      natl <- natl_data()
      if (is.null(natl)) return(tibble::tibble())
      pair <- selected_pair()
      .build_gest_pct_comparison(natl, pair$db, pair$country)
    })

    gest_gt_reactive <- reactive({
      d <- gest_data()
      validate(need(nrow(d) > 0, "No gestational duration data available."))

      d_wide <- d %>%
        dplyr::select("label", "bin", "pct") %>%
        tidyr::pivot_wider(names_from = "bin", values_from = "pct") %>%
        dplyr::arrange(.data$label)

      bin_order <- c("<32", "32-36", "37-41", ">=42")
      present_bins <- intersect(bin_order, colnames(d_wide))

      gt::gt(d_wide) %>%
        gt::tab_header(title = "Gestational Duration Distribution (%)",
                       subtitle = "Our results vs. reference values (national statistics)") %>%
        gt::cols_label(label = "Source") %>%
        gt::fmt_number(columns = dplyr::all_of(present_bins), decimals = 1) %>%
        gt::sub_missing(missing_text = "-") %>%
        gt::tab_options(table.font.size = "small") %>%
        gt::tab_spanner(label = "Gestational Age (weeks)", columns = dplyr::all_of(present_bins))
    })

    output$gest_table <- gt::render_gt({ gest_gt_reactive() })

    output$download_gest_table <- downloadHandler(
      filename = function() "gestational_duration_comparison.docx",
      content = function(file) {
        tryCatch(gt::gtsave(gest_gt_reactive(), file), error = function(e) NULL)
      }
    )

    gest_ggplot <- reactive({
      d <- gest_data()
      validate(need(nrow(d) > 0, "No gestational duration data to plot."))

      d <- d %>%
        dplyr::mutate(bin = factor(.data$bin, levels = c("<32", "32-36", "37-41", ">=42")))

      ggplot2::ggplot(d, ggplot2::aes(x = .data$label, y = .data$pct, fill = .data$bin)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(x = NULL, y = "Percentage (%)", fill = "Gestational\nAge (weeks)",
                      title = "Gestational Duration: Our Results vs. Reference") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_brewer(palette = "Set2")
    })

    output$gest_plot <- plotly::renderPlotly({
      p <- gest_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_gest_plot <- downloadHandler(
      filename = function() "gestational_duration_comparison.png",
      content = function(file) {
        p <- gest_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p,
                          width = as.numeric(input$gest_plot_width),
                          height = as.numeric(input$gest_plot_height),
                          dpi = as.numeric(input$gest_plot_dpi), units = "cm")
        }
      }
    )


    # ========== TAB 3: Delivery Mode ==========
    dm_data <- reactive({
      natl <- natl_data()
      if (is.null(natl)) return(tibble::tibble())
      pair <- selected_pair()
      .build_dm_pct_comparison(natl, pair$db, pair$country)
    })

    dm_gt_reactive <- reactive({
      d <- dm_data()
      validate(need(nrow(d) > 0, "No delivery mode data available."))

      d_wide <- d %>%
        dplyr::select("label", "mode", "pct") %>%
        tidyr::pivot_wider(names_from = "mode", values_from = "pct") %>%
        dplyr::arrange(.data$label)

      gt::gt(d_wide) %>%
        gt::tab_header(title = "Delivery Mode Distribution (%)",
                       subtitle = "Our results vs. reference values (national statistics)") %>%
        gt::cols_label(label = "Source") %>%
        gt::fmt_number(columns = dplyr::any_of(c("Vaginal", "Cesarean")), decimals = 1) %>%
        gt::sub_missing(missing_text = "-") %>%
        gt::tab_options(table.font.size = "small")
    })

    output$dm_table <- gt::render_gt({ dm_gt_reactive() })

    output$download_dm_table <- downloadHandler(
      filename = function() "delivery_mode_comparison.docx",
      content = function(file) {
        tryCatch(gt::gtsave(dm_gt_reactive(), file), error = function(e) NULL)
      }
    )

    dm_ggplot <- reactive({
      d <- dm_data()
      validate(need(nrow(d) > 0, "No delivery mode data to plot."))

      ggplot2::ggplot(d, ggplot2::aes(x = .data$label, y = .data$pct, fill = .data$mode)) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::labs(x = NULL, y = "Percentage (%)", fill = "Mode",
                      title = "Delivery Mode: Our Results vs. Reference") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_manual(values = c("Vaginal" = "#4DAF4A", "Cesarean" = "#E41A1C"))
    })

    output$dm_plot <- plotly::renderPlotly({
      p <- dm_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_dm_plot <- downloadHandler(
      filename = function() "delivery_mode_comparison.png",
      content = function(file) {
        p <- dm_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p,
                          width = as.numeric(input$dm_plot_width),
                          height = as.numeric(input$dm_plot_height),
                          dpi = as.numeric(input$dm_plot_dpi), units = "cm")
        }
      }
    )


    # ========== TAB 4: Year-Level Metrics ==========

    # ---- Live births / episodes per year ----
    lb_data <- reactive({
      natl <- natl_data()
      if (is.null(natl)) return(tibble::tibble())
      pair <- selected_pair()

      natl_lb <- natl %>%
        dplyr::filter(
          grepl("Number of live births", .data$variable),
          .data$country == pair$country,
          !is.na(.data$value)
        ) %>%
        dplyr::transmute(
          source_label = paste0(pair$country, " (Reference)"),
          year = .data$year,
          count = .data$value,
          source = "Reference"
        )

      int_lb <- tibble::tibble()
      if (exists("trendData")) {
        int_lb <- trendData %>%
          dplyr::filter(
            .data$period == "year",
            .data$column == "final_episode_end_date",
            tolower(.data$cdm_name) == tolower(pair$db) |
              grepl(paste0("^", tolower(sub("_v[0-9]+$", "", pair$db))),
                    tolower(.data$cdm_name))
          ) %>%
          dplyr::mutate(year = as.integer(.data$value), count = as.numeric(.data$count)) %>%
          # Only show years that overlap with external data
          dplyr::filter(.data$year %in% natl_lb$year) %>%
          dplyr::transmute(
            source_label = paste0(pair$db, " (Our Result)"),
            year = .data$year,
            count = .data$count,
            source = "Our Result"
          )
      }

      dplyr::bind_rows(natl_lb, int_lb)
    })

    lb_ggplot <- reactive({
      d <- lb_data()
      validate(need(nrow(d) > 0, "No live births data."))
      pair <- selected_pair()

      ggplot2::ggplot(d, ggplot2::aes(x = as.factor(.data$year), y = .data$count,
                                       fill = .data$source_label)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(x = "Year", y = "Count", fill = "Source",
                      title = paste0("Live Births (", pair$country, ") vs. Total Episodes (", pair$db, ")"),
                      caption = "Note: Our result count includes all pregnancy episodes (LB, AB, SA, SB, ECT, etc.)") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       plot.title = ggplot2::element_text(hjust = 0.5, size = 11)) +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_fill_manual(values = c(
          setNames("#377EB8", paste0(pair$country, " (Reference)")),
          setNames("#E41A1C", paste0(pair$db, " (Our Result)"))
        ))
    })

    output$live_births_plot <- plotly::renderPlotly({
      p <- lb_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_lb_plot <- downloadHandler(
      filename = function() "live_births_episodes_comparison.png",
      content = function(file) {
        p <- lb_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p,
                          width = as.numeric(input$lb_plot_width),
                          height = as.numeric(input$lb_plot_height),
                          dpi = as.numeric(input$lb_plot_dpi), units = "cm")
        }
      }
    )

    # ---- Maternal Age ----
    ma_data <- reactive({
      natl <- natl_data()
      if (is.null(natl)) return(tibble::tibble())
      pair <- selected_pair()

      natl_ma <- natl %>%
        dplyr::filter(
          grepl("Mean age", .data$variable),
          .data$country == pair$country,
          !is.na(.data$value)
        ) %>%
        dplyr::transmute(
          source_label = paste0(pair$country, " (Reference)"),
          year = .data$year,
          value = .data$value,
          source = "Reference"
        )

      int_ma <- tibble::tibble()
      if (exists("ageSummaryFirstPregnancy")) {
        afp <- ageSummaryFirstPregnancy
        if (!is.null(afp) && nrow(afp) > 0 && "year" %in% colnames(afp)) {
          int_ma <- afp %>%
            dplyr::filter(
              tolower(.data$cdm_name) == tolower(pair$db) |
                grepl(paste0("^", tolower(sub("_v[0-9]+$", "", pair$db))),
                      tolower(.data$cdm_name)),
              .data$final_outcome_category == "overall",
              .data$year != "overall"
            ) %>%
            dplyr::mutate(
              year = suppressWarnings(as.integer(.data$year)),
              value = round(suppressWarnings(as.numeric(.data$mean)), 1)
            ) %>%
            dplyr::filter(!is.na(.data$year),
                          .data$year %in% natl_ma$year) %>%
            dplyr::transmute(
              source_label = paste0(pair$db, " (Our Result)"),
              year = .data$year,
              value = .data$value,
              source = "Our Result"
            )
        }
      }

      dplyr::bind_rows(natl_ma, int_ma)
    })

    ma_ggplot <- reactive({
      d <- ma_data()
      validate(need(nrow(d) > 0, "No maternal age data."))
      pair <- selected_pair()

      ggplot2::ggplot(d, ggplot2::aes(x = as.factor(.data$year), y = .data$value,
                                       fill = .data$source_label)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::geom_text(ggplot2::aes(label = .data$value),
                           position = ggplot2::position_dodge(width = 0.9),
                           vjust = -0.5, size = 3.5) +
        ggplot2::labs(x = "Year", y = "Mean age (years)", fill = "Source",
                      title = paste0("Mean Maternal Age at First Child: ", pair$db, " vs. ", pair$country),
                      caption = paste0("Reference: Mean age at birth of first child\n",
                                       "Our result: Mean age at pregnancy start (first pregnancy)")) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       plot.title = ggplot2::element_text(hjust = 0.5, size = 11)) +
        ggplot2::scale_fill_manual(values = c(
          setNames("#377EB8", paste0(pair$country, " (Reference)")),
          setNames("#E41A1C", paste0(pair$db, " (Our Result)"))
        )) +
        ggplot2::coord_cartesian(ylim = c(25, 35))
    })

    output$maternal_age_plot <- plotly::renderPlotly({
      p <- ma_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_ma_plot <- downloadHandler(
      filename = function() "maternal_age_comparison.png",
      content = function(file) {
        p <- ma_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p,
                          width = as.numeric(input$ma_plot_width),
                          height = as.numeric(input$ma_plot_height),
                          dpi = as.numeric(input$ma_plot_dpi), units = "cm")
        }
      }
    )

    # ---- Foetal Mortality ----
    fm_data <- reactive({
      natl <- natl_data()
      if (is.null(natl)) return(tibble::tibble())
      pair <- selected_pair()

      natl_fm <- natl %>%
        dplyr::filter(
          grepl("Foetal mortality", .data$variable),
          .data$country == pair$country,
          !is.na(.data$value)
        ) %>%
        dplyr::transmute(
          source_label = paste0(pair$country, " (Reference, ", .data$year, ")"),
          year = .data$year,
          value = .data$value,
          source = "Reference"
        )

      # Our result: overall rate (no yearly breakdown available)
      int_fm <- tibble::tibble()
      if (exists("outcomeCategoriesCount")) {
        oc <- outcomeCategoriesCount
        if (!is.null(oc) && nrow(oc) > 0 && "outcome_category" %in% colnames(oc)) {
          oc_db <- oc %>%
            dplyr::filter(
              tolower(.data$cdm_name) == tolower(pair$db) |
                grepl(paste0("^", tolower(sub("_v[0-9]+$", "", pair$db))),
                      tolower(.data$cdm_name)),
              .data$outcome_category %in% c("SB", "LB")
            ) %>%
            dplyr::mutate(n = suppressWarnings(as.numeric(.data$n)))
          sb <- sum(oc_db$n[oc_db$outcome_category == "SB"], na.rm = TRUE)
          total <- sum(oc_db$n, na.rm = TRUE)
          if (total > 0) {
            rate <- round(1000 * sb / total, 1)
            int_fm <- tibble::tibble(
              source_label = paste0(pair$db, " (Our Result, overall)"),
              year = NA_integer_,
              value = rate,
              source = "Our Result"
            )
          }
        }
      }

      dplyr::bind_rows(natl_fm, int_fm)
    })

    fm_ggplot <- reactive({
      d <- fm_data()
      validate(need(nrow(d) > 0, "No foetal mortality data."))
      pair <- selected_pair()

      ggplot2::ggplot(d, ggplot2::aes(x = .data$source_label, y = .data$value,
                                       fill = .data$source)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_text(ggplot2::aes(label = .data$value),
                           vjust = -0.5, size = 3.5) +
        ggplot2::labs(x = NULL, y = "Rate (per 1,000 total births)", fill = "Source",
                      title = paste0("Foetal Mortality Rate: ", pair$db, " vs. ", pair$country),
                      caption = "Our result rate is computed as SB/(SB+LB)*1000 across all years") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       plot.title = ggplot2::element_text(hjust = 0.5, size = 11)) +
        ggplot2::scale_fill_manual(values = c("Reference" = "#377EB8", "Our Result" = "#E41A1C"))
    })

    output$foetal_mort_plot <- plotly::renderPlotly({
      p <- fm_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_fm_plot <- downloadHandler(
      filename = function() "foetal_mortality_comparison.png",
      content = function(file) {
        p <- fm_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p,
                          width = as.numeric(input$fm_plot_width),
                          height = as.numeric(input$fm_plot_height),
                          dpi = as.numeric(input$fm_plot_dpi), units = "cm")
        }
      }
    )


    # ========== TAB 5: All Databases ==========

    # Reactive: compute comparison across all mapped DBs (cached)
    all_db_data <- reactive({
      natl <- natl_data()
      if (is.null(natl)) return(tibble::tibble())
      .build_all_db_comparison(natl)
    })

    # Update metric picker when data is available
    observe({
      d <- all_db_data()
      if (is.null(d) || nrow(d) == 0) return()
      # Build labels from indicator + variable + level
      d <- d %>%
        dplyr::mutate(
          metric_label = dplyr::if_else(
            is.na(.data$level) | !nzchar(.data$level),
            paste0(.data$indicator, " | ", .data$variable),
            paste0(.data$indicator, " | ", .data$variable, " [", .data$level, "]")
          )
        )
      metrics <- sort(unique(d$metric_label))
      updatePickerInput(session, "alldb_metrics",
                        choices = metrics, selected = metrics)
      updatePickerInput(session, "diffov_metrics",
                        choices = metrics, selected = metrics)
    })

    # Filtered data for All Databases tab
    alldb_filtered <- reactive({
      d <- all_db_data()
      if (is.null(d) || nrow(d) == 0) return(tibble::tibble())
      sel <- input$alldb_metrics
      if (is.null(sel) || length(sel) == 0) return(tibble::tibble())
      d %>%
        dplyr::mutate(
          metric_label = dplyr::if_else(
            is.na(.data$level) | !nzchar(.data$level),
            paste0(.data$indicator, " | ", .data$variable),
            paste0(.data$indicator, " | ", .data$variable, " [", .data$level, "]")
          )
        ) %>%
        dplyr::filter(.data$metric_label %in% sel)
    })

    alldb_gt <- reactive({
      d <- alldb_filtered()
      validate(need(nrow(d) > 0, "No data available. Check that mapped databases exist."))

      d_display <- d %>%
        dplyr::mutate(
          external_fmt = dplyr::if_else(
            is.na(.data$external_value), "-",
            format(.data$external_value, big.mark = ",", scientific = FALSE, trim = TRUE)
          ),
          internal_fmt = dplyr::if_else(
            is.na(.data$internal_value), "-",
            format(.data$internal_value, big.mark = ",", scientific = FALSE, trim = TRUE)
          ),
          difference = dplyr::case_when(
            is.na(.data$external_value) | is.na(.data$internal_value) ~ "-",
            .data$external_value != 0 ~ paste0(
              format(round(.data$diff_abs, 1), big.mark = ",", trim = TRUE),
              " (", ifelse(.data$diff_abs >= 0, "+", ""),
              round(.data$diff_pct, 1), "%)"
            ),
            TRUE ~ format(round(.data$diff_abs, 1), big.mark = ",", trim = TRUE)
          ),
          diff_pct_val = .data$diff_pct,
          year = dplyr::if_else(is.na(.data$year), "-", as.character(.data$year))
        ) %>%
        dplyr::select(
          "cdm_name", "country", "indicator", "variable", "level", "year",
          "external_fmt", "internal_fmt", "difference", "diff_pct_val"
        ) %>%
        dplyr::arrange(.data$indicator, .data$variable, .data$level, .data$year, .data$cdm_name)

      tbl <- gt::gt(d_display) %>%
        gt::tab_header(
          title = "National Statistics Comparison: All Databases",
          subtitle = "Stacked comparison across all mapped databases"
        ) %>%
        gt::cols_label(
          cdm_name = "Database",
          country = "Country",
          indicator = "Indicator",
          variable = "Variable",
          level = "Level",
          year = "Year",
          external_fmt = "Reference",
          internal_fmt = "Our Result",
          difference = "Difference"
        ) %>%
        gt::cols_hide("diff_pct_val") %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#f0f7ff"),
          locations = gt::cells_body(columns = "external_fmt")
        ) %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#fff0f0"),
          locations = gt::cells_body(columns = "internal_fmt")
        ) %>%
        gt::sub_missing(missing_text = "-") %>%
        gt::tab_options(table.font.size = "small") %>%
        gt::cols_width(
          "variable" ~ gt::px(220),
          "cdm_name" ~ gt::px(120)
        )

      # Apply color coding to difference column row by row
      for (i in seq_len(nrow(d_display))) {
        bg <- .diff_color(d_display$diff_pct_val[i])
        tbl <- tbl %>%
          gt::tab_style(
            style = gt::cell_fill(color = bg),
            locations = gt::cells_body(columns = "difference", rows = i)
          )
      }

      tbl
    })

    output$alldb_table <- gt::render_gt({ alldb_gt() })

    output$download_alldb <- downloadHandler(
      filename = function() "national_stats_all_databases.docx",
      content = function(file) {
        tryCatch(gt::gtsave(alldb_gt(), file), error = function(e) NULL)
      }
    )


    # ========== TAB 6: Difference Overview ==========

    diffov_filtered <- reactive({
      d <- all_db_data()
      if (is.null(d) || nrow(d) == 0) return(tibble::tibble())
      sel <- input$diffov_metrics
      if (is.null(sel) || length(sel) == 0) return(tibble::tibble())
      d %>%
        dplyr::mutate(
          metric_label = dplyr::if_else(
            is.na(.data$level) | !nzchar(.data$level),
            paste0(.data$indicator, " | ", .data$variable),
            paste0(.data$indicator, " | ", .data$variable, " [", .data$level, "]")
          )
        ) %>%
        dplyr::filter(.data$metric_label %in% sel)
    })

    diffov_gt <- reactive({
      d <- diffov_filtered()
      validate(need(nrow(d) > 0, "No data available."))

      # Create a row label combining indicator/variable/level/year
      d_pivot <- d %>%
        dplyr::mutate(
          row_label = dplyr::case_when(
            is.na(.data$level) | !nzchar(.data$level) ~
              paste0(.data$variable, " (", dplyr::if_else(is.na(.data$year), "-", as.character(.data$year)), ")"),
            TRUE ~
              paste0(.data$variable, " [", .data$level, "] (", dplyr::if_else(is.na(.data$year), "-", as.character(.data$year)), ")")
          ),
          diff_label = dplyr::case_when(
            is.na(.data$diff_pct) ~ "-",
            TRUE ~ paste0(ifelse(.data$diff_pct >= 0, "+", ""), round(.data$diff_pct, 1), "%")
          )
        )

      # Pivot wider: one column per database showing diff_pct
      wide_label <- d_pivot %>%
        dplyr::select("indicator", "row_label", "cdm_name", "diff_label") %>%
        tidyr::pivot_wider(names_from = "cdm_name", values_from = "diff_label") %>%
        dplyr::arrange(.data$indicator, .data$row_label)

      # Also get numeric values for color coding
      wide_pct <- d_pivot %>%
        dplyr::select("indicator", "row_label", "cdm_name", "diff_pct") %>%
        tidyr::pivot_wider(names_from = "cdm_name", values_from = "diff_pct",
                           names_prefix = "pct_") %>%
        dplyr::arrange(.data$indicator, .data$row_label)

      db_cols <- setdiff(colnames(wide_label), c("indicator", "row_label"))
      pct_cols <- paste0("pct_", db_cols)

      # Join for color coding
      display <- wide_label %>%
        dplyr::left_join(wide_pct, by = c("indicator", "row_label"))

      tbl <- gt::gt(display, groupname_col = "indicator") %>%
        gt::tab_header(
          title = "Difference Overview Across Databases",
          subtitle = "Percentage difference (Our Result vs Reference) per indicator and database"
        ) %>%
        gt::cols_label(row_label = "Metric") %>%
        gt::cols_hide(dplyr::all_of(pct_cols)) %>%
        gt::sub_missing(missing_text = "-") %>%
        gt::tab_options(
          table.font.size = "small",
          row_group.font.weight = "bold"
        ) %>%
        gt::cols_width(
          "row_label" ~ gt::px(300)
        )

      # Color code each DB column cell based on pct value
      for (db in db_cols) {
        pct_col <- paste0("pct_", db)
        if (pct_col %in% colnames(display)) {
          for (i in seq_len(nrow(display))) {
            pct_val <- display[[pct_col]][i]
            bg <- .diff_color(pct_val)
            tbl <- tbl %>%
              gt::tab_style(
                style = gt::cell_fill(color = bg),
                locations = gt::cells_body(columns = db, rows = i)
              )
          }
        }
      }

      tbl
    })

    output$diffov_table <- gt::render_gt({ diffov_gt() })

    output$download_diffov <- downloadHandler(
      filename = function() "difference_overview_all_databases.docx",
      content = function(file) {
        tryCatch(gt::gtsave(diffov_gt(), file), error = function(e) NULL)
      }
    )


    # ========== TAB 7: Raw Data ==========
    output$raw_table <- DT::renderDT({
      natl <- natl_data()
      validate(need(!is.null(natl) && nrow(natl) > 0, "No national statistics data available."))
      DT::datatable(
        natl %>% dplyr::mutate(value = round(.data$value, 2)),
        filter = "top",
        options = list(scrollX = TRUE, pageLength = 25),
        colnames = c("Indicator", "Variable", "Level", "Year", "Country", "Value")
      )
    })

    output$download_raw <- downloadHandler(
      filename = function() "national_statistics_raw.csv",
      content = function(file) {
        natl <- natl_data()
        if (!is.null(natl) && nrow(natl) > 0) readr::write_csv(natl, file)
      }
    )

  })
}
