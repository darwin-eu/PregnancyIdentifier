#' Compare PregnancyIdentifier results with national statistics
#'
#' Reads the bundled National_Statistics_Obj2_v2.csv and compares key indicators
#' (birth rate, maternal age, gestational duration distribution, delivery mode)
#' with the corresponding summaries produced by the pipeline.
#'
#' @param incidence A \code{summarised_result} incidence object (may be NULL).
#' @param gestationalWeeksBinned Data frame of binned gestational-age counts
#'   produced by global.R (columns: cdm_name, gestational_weeks, n, pct, and
#'   optionally final_outcome_category).
#' @param deliveryModeSummary Data frame of delivery-mode percentages (long
#'   format with columns: cdm_name, final_outcome_category, mode, pct).
#' @param outcomeCategoriesCount Data frame of outcome-category counts (columns:
#'   cdm_name, outcome_category / final_outcome_category, n, pct).
#' @param yearlyTrend Data frame with yearly episode counts (columns: cdm_name,
#'   column, value [year], count).
#' @param ageSummary Data frame with age summary statistics (columns: cdm_name,
#'   final_outcome_category, mean, median, etc.).
#' @param nationalStatsPath Path to the national statistics CSV. Defaults to the
#'   bundled copy inside the installed package.
#'
#' @return A list with components:
#' \describe{
#'   \item{gestational_duration}{Long data frame comparing gestational-age bin
#'     percentages (source = "Database" or "National Statistics").}
#'   \item{delivery_mode}{Long data frame comparing delivery-mode percentages.}
#'   \item{national_stats_raw}{The parsed national statistics data frame.}
#' }
#'
#' @export
compareWithNationalStats <- function(
    incidence = NULL,
    gestationalWeeksBinned = NULL,
    deliveryModeSummary = NULL,
    outcomeCategoriesCount = NULL,
    yearlyTrend = NULL,
    ageSummary = NULL,
    nationalStatsPath = NULL
) {
  if (is.null(nationalStatsPath)) {
    nationalStatsPath <- system.file(
      "shiny", "National_Statistics_Obj2_v2.csv",
      package = "PregnancyIdentifier"
    )
  }
  if (!file.exists(nationalStatsPath)) {
    stop("National statistics CSV not found at: ", nationalStatsPath)
  }

  natl <- parseNationalStats(nationalStatsPath)

  list(
    gestational_duration = compareGestationalDuration(gestationalWeeksBinned, natl),
    delivery_mode        = compareDeliveryMode(deliveryModeSummary, natl),
    national_stats_raw   = natl
  )
}


#' Parse the national statistics CSV into a tidy long-format data frame.
#'
#' The CSV uses European number formatting (dots as thousands separators) for
#' some numeric fields. This function normalises those to standard numerics and
#' pivots country columns into long format.
#'
#' @param path Path to the CSV file.
#' @return A tibble with columns: indicator, variable, level, year, country, value.
#' @keywords internal
parseNationalStats <- function(path) {
  raw <- readr::read_csv(
    path,
    col_types = readr::cols(.default = "c"),
    show_col_types = FALSE
  )
  colnames(raw) <- trimws(colnames(raw))

  country_cols <- setdiff(colnames(raw), c("Indicators", "variable", "level", "Year"))

  long <- raw %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(country_cols),
      names_to = "country",
      values_to = "value_raw"
    ) %>%
    dplyr::rename(indicator = "Indicators", year = "Year") %>%
    dplyr::mutate(
      year = suppressWarnings(as.integer(.data$year)),
      value = parseEuropeanNumber(.data$value_raw)
    ) %>%
    dplyr::select("indicator", "variable", "level", "year", "country", "value")

  long
}


#' Parse a character vector of numbers that may use European formatting
#' (dots as thousands separators, commas as decimal separators).
#'
#' @param x Character vector.
#' @return Numeric vector.
#' @keywords internal
parseEuropeanNumber <- function(x) {
  # The CSV uses dots as thousands separators (e.g. "2.101" = 2101)
  # and commas as decimal separators (e.g. "591.072" in UK 2023 might actually

  # be 591072 or 591.072). We detect context: if the value contains only one dot
  # and the part after the dot has exactly 3 digits, treat as thousands separator.
  vapply(x, function(val) {
    if (is.na(val) || !nzchar(trimws(val))) return(NA_real_)
    val <- trimws(val)
    # Replace comma decimal separator with dot
    val <- gsub(",", ".", val)
    # Count dots
    dots <- gregexpr("\\.", val)[[1]]
    n_dots <- sum(dots > 0)
    if (n_dots == 1L) {
      parts <- strsplit(val, "\\.")[[1]]
      # If the part after the dot has exactly 3 digits, treat as thousands sep
      if (nchar(parts[2]) == 3L && grepl("^[0-9]+$", parts[2])) {
        val <- gsub("\\.", "", val)
      }
    } else if (n_dots > 1L) {
      # Multiple dots = definitely thousands separators
      val <- gsub("\\.", "", val)
    }
    suppressWarnings(as.numeric(val))
  }, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
}


#' Compare gestational-duration bin distributions
#'
#' Maps the app's gestational-week bins to the national-statistics bins and
#' returns a combined long-format data frame.
#'
#' @param gestationalWeeksBinned App data (may be NULL).
#' @param natl Parsed national statistics.
#' @return A tibble.
#' @keywords internal
compareGestationalDuration <- function(gestationalWeeksBinned, natl) {
  # National stats bins: <32, 32-36, 37-38, 39-41, >=42
  natl_gest <- natl %>%
    dplyr::filter(.data$indicator == "Gestational duration distribution") %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::select("level", "year", "country", "value") %>%
    dplyr::rename(bin = "level")

  if (nrow(natl_gest) == 0) return(tibble::tibble())

  # Compute percentages within each country-year
  natl_gest <- natl_gest %>%
    dplyr::group_by(.data$country, .data$year) %>%
    dplyr::mutate(
      total = sum(.data$value, na.rm = TRUE),
      pct = dplyr::if_else(.data$total > 0, round(100 * .data$value / .data$total, 1), NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(source = "National Statistics") %>%
    dplyr::select("country", "year", "bin", "n" = "value", "pct", "source")

  if (is.null(gestationalWeeksBinned) || nrow(gestationalWeeksBinned) == 0) {
    return(natl_gest)
  }

  # Map app bins to national-stats bins
  bin_map <- c(
    "<12"   = "<32",
    "12-27" = "<32",
    "28-31" = "<32",
    "32-36" = "32-36",
    "37-41" = "37-38",  # Will be split below
    "42-43" = "\u226542",
    "44-49" = "\u226542",
    ">=50"  = "\u226542"
  )

  # The app uses finer bins; we need to re-aggregate to match national stats bins
  # National: <32, 32-36, 37-38, 39-41, >=42
  # App:      <12, 12-27, 28-31, 32-36, 37-41, 42-43, 44-49, >=50
  # Problem: app has 37-41 combined but national has 37-38 and 39-41 separate.
  # We can only compare at the level where both have data.
  # Best approach: re-map app bins to coarser bins that can be compared.

  app_bin_map <- dplyr::tribble(
    ~app_bin,  ~natl_bin,
    "<12",     "<32",
    "12-27",   "<32",
    "28-31",   "<32",
    "32-36",   "32-36",
    "37-41",   "37-41",
    "42-43",   "\u226542",
    "44-49",   "\u226542",
    ">=50",    "\u226542"
  )

  # Also need to merge national 37-38 + 39-41 into 37-41
  natl_gest_remapped <- natl_gest %>%
    dplyr::mutate(bin = dplyr::case_when(
      .data$bin %in% c("37-38", "39-41") ~ "37-41",
      .data$bin == "\u226542"             ~ "\u226542",
      TRUE                               ~ .data$bin
    )) %>%
    dplyr::group_by(.data$country, .data$year, .data$bin, .data$source) %>%
    dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(.data$country, .data$year) %>%
    dplyr::mutate(pct = round(100 * .data$n / sum(.data$n, na.rm = TRUE), 1)) %>%
    dplyr::ungroup()

  # Aggregate app data
  app_data <- gestationalWeeksBinned %>%
    dplyr::mutate(gestational_weeks = as.character(.data$gestational_weeks)) %>%
    dplyr::left_join(app_bin_map, by = c("gestational_weeks" = "app_bin"))

  # Aggregate across outcome categories if present
  if ("final_outcome_category" %in% colnames(app_data)) {
    app_data <- app_data %>%
      dplyr::group_by(.data$cdm_name, .data$natl_bin) %>%
      dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop")
  } else {
    app_data <- app_data %>%
      dplyr::group_by(.data$cdm_name, .data$natl_bin) %>%
      dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop")
  }

  app_data <- app_data %>%
    dplyr::group_by(.data$cdm_name) %>%
    dplyr::mutate(pct = round(100 * .data$n / sum(.data$n, na.rm = TRUE), 1)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(bin = "natl_bin", country = "cdm_name") %>%
    dplyr::mutate(source = "Database", year = NA_integer_) %>%
    dplyr::select("country", "year", "bin", "n", "pct", "source")

  dplyr::bind_rows(natl_gest_remapped, app_data)
}


#' Compare delivery-mode distributions
#'
#' @param deliveryModeSummary App data (long format with mode, pct columns).
#' @param natl Parsed national statistics.
#' @return A tibble.
#' @keywords internal
compareDeliveryMode <- function(deliveryModeSummary, natl) {
  natl_dm <- natl %>%
    dplyr::filter(.data$indicator == "Mode of delivery") %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::select("level", "year", "country", "value") %>%
    dplyr::rename(mode = "level", pct = "value") %>%
    dplyr::mutate(
      mode = dplyr::case_when(
        tolower(.data$mode) == "vaginal"   ~ "vaginal",
        tolower(.data$mode) == "c-section" ~ "cesarean",
        TRUE                               ~ tolower(.data$mode)
      ),
      source = "National Statistics"
    )

  if (is.null(deliveryModeSummary) || nrow(deliveryModeSummary) == 0) {
    return(natl_dm)
  }

  # Aggregate across outcome categories (LB + DELIV) to get overall %
  app_dm <- deliveryModeSummary %>%
    dplyr::mutate(
      n = suppressWarnings(as.numeric(.data$n)),
      pct = suppressWarnings(as.numeric(.data$pct))
    )

  # Weighted average of pct across outcome categories by n_known
  if ("n_known" %in% colnames(app_dm)) {
    app_dm <- app_dm %>%
      dplyr::mutate(n_known = suppressWarnings(as.numeric(.data$n_known))) %>%
      dplyr::group_by(.data$cdm_name, .data$mode) %>%
      dplyr::summarise(
        n = sum(.data$n, na.rm = TRUE),
        n_known = sum(.data$n_known, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(pct = round(100 * .data$n / .data$n_known, 1))
  } else {
    app_dm <- app_dm %>%
      dplyr::group_by(.data$cdm_name, .data$mode) %>%
      dplyr::summarise(pct = mean(.data$pct, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(pct = round(.data$pct, 1))
  }

  app_dm <- app_dm %>%
    dplyr::rename(country = "cdm_name") %>%
    dplyr::mutate(source = "Database", year = NA_integer_) %>%
    dplyr::select("country", "year", "mode", "pct", "source")

  dplyr::bind_rows(natl_dm, app_dm)
}
