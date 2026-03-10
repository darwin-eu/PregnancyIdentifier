# helpers.R - Shared utility constants and functions used across modules

# Picker options used throughout the app
opt <- list("actions-box" = TRUE, size = 10, "selected-text-format" = "count > 3")

#' Fixed colour palette for pregnancy outcome categories.
OUTCOME_COLOURS <- c(
  ECT  = "#E41A1C",
  AB   = "#377EB8",
  SA   = "#4DAF4A",
  SB   = "#984EA3",
  DELIV = "#FF7F00",
  LB   = "#A65628",
  PREG = "#999999"
)

#' Human-readable column name lookup.
PRETTY_NAMES <- c(
  # --- Identifiers & metadata ---
  cdm_name                              = "Database",
  cdm_source_name                       = "Source Name",
  cdm_description                       = "Description",
  cdm_documentation_reference           = "Documentation",
  cdm_version                           = "CDM Version",
  cdm_holder                            = "CDM Holder",
  cdm_release_date                      = "Release Date",
  vocabulary_version                    = "Vocabulary Version",
  snapshot_date                         = "Snapshot Date",
  date_run                              = "Run Date",
  date_export                           = "Export Date",
  pkg_version                           = "Package Version",

  # --- Counts & percentages ---
  n                                     = "N",
  pct                                   = "%",
  total                                 = "Total",
  count                                 = "Count",
  freq                                  = "Frequency",
  number_individuals                    = "Individuals",
  total_episodes                        = "Total Episodes",
  total_individuals                     = "Total Individuals",
  record_count                          = "Record Count",
  person_count                          = "Person Count",
  episode_count                         = "Episode Count",
  observation_period_count              = "Observation Periods",
  earliest_observation_period_start_date = "Earliest Obs Start",
  latest_observation_period_end_date    = "Latest Obs End",

  # --- Summary statistics ---
  min                                   = "Min",
  Q25                                   = "Q1 (25th)",
  median                                = "Median",
  Q75                                   = "Q3 (75th)",
  max                                   = "Max",
  mean                                  = "Mean",
  sd                                    = "Std Dev",
  density                               = "Density",

  # --- Outcome & algorithm ---
  final_outcome_category                = "Outcome",
  outcome_category                      = "Outcome",
  algorithm                             = "Algorithm",
  colName                               = "Category",

  # --- Gestational age ---
  gestational_weeks                     = "Gestational Weeks",
  esd_precision_days                    = "ESD Precision (days)",
  esd_gestational_age_days_calculated   = "Gestational Age (days)",
  less_1day                             = "< 1 Day",
  over_308days                          = "> 308 Days",

  # --- Delivery mode ---
  mode                                  = "Delivery Mode",
  cesarean                              = "Cesarean",
  vaginal                               = "Vaginal",
  cesarean_pct                          = "Cesarean %",
  vaginal_pct                           = "Vaginal %",
  n_known                               = "N with known mode",
  cesarean_count                        = "Cesarean Count",
  vaginal_count                         = "Vaginal Count",

  # --- Overlap & swapped dates ---
  overlap                               = "Overlap",
  overlap_status                        = "Overlap Status",
  n_swapped                             = "N Swapped",
  n_rev_hip                             = "HIP Reversed",
  n_rev_pps                             = "PPS Reversed",
  metric                                = "Metric",
  source                                = "Source",

  # --- Missing dates ---
  n_missing                             = "N Missing",
  percent_missing                       = "% Missing",

  # --- Concept check ---
  concept_id                            = "Concept ID",
  concept_name                          = "Concept Name",
  esd_concept_id                        = "ESD Concept ID",
  esd_concept_name                      = "ESD Concept Name",
  pps_concept_id                        = "PPS Concept ID",
  pps_concept_name                      = "PPS Concept Name",
  n_after_min                           = "N After Min Date",
  n_prior_max                           = "N Before Max Date",
  n_on_or_before_min                    = "Number of occurrences in episode / in pre-window",
  n_on_or_after_max                     = "Number of occurrences in episode / in post-window",
  n_in_span                             = "Number of occurrences in episode / in window",
  n_at_midpoint                         = "Number of occurrences at midpoint",
  n_occurrences_in_episodes             = "Number of occurrences in pregnancy episodes",
  p_after_min                           = "% After Min Date",
  p_prior_max                           = "% Before Max Date",
  p_on_or_before_min                    = "% in pre-window (of row total)",
  p_on_or_after_max                     = "% in post-window (of row total)",
  p_in_span                             = "% in window (of row total)",
  p_at_midpoint                         = "% at midpoint (of row total)",
  p_concept                             = "% of pregnancies with ≥1 occurrence",

  # --- Attrition ---
  step                                  = "Step",
  step_number                           = "#",
  table                                 = "Table",
  outcome                               = "Outcome",
  prior_records                         = "Prior Records",
  prior_persons                         = "Prior Persons",
  dropped_records                       = "Dropped Records",
  dropped_persons                       = "Dropped Persons",
  post_records                          = "Post Records",
  post_persons                          = "Post Persons",

  # --- Quality check cleanup ---
  n_records_overlapping                 = "Overlapping Records",
  n_persons_overlapping                 = "Overlapping Persons",
  n_records_too_long                    = "Too Long Records",
  n_persons_too_long                    = "Too Long Persons",
  n_records_after_cleanup               = "Records After Cleanup",
  n_persons_after_cleanup               = "Persons After Cleanup",
  max_episode_days                      = "Max Episode (days)",

  # --- Temporal & observation ---
  column                                = "Date Column",
  month                                 = "Month",
  year                                  = "Year",
  period                                = "Period",
  value                                 = "Value",
  min_obs                               = "Earliest Year",
  max_obs                               = "Latest Year",

  # --- Age ---
  age_pregnancy_start                   = "Age at Pregnancy Start",
  age_pregnancy_end                     = "Age at Pregnancy End"
)

#' Apply human-readable column names to a data.frame for DT display.
prettyColNames <- function(df) {
  nms <- colnames(df)
  matched <- nms %in% names(PRETTY_NAMES)
  nms[matched] <- PRETTY_NAMES[nms[matched]]
  colnames(df) <- nms
  df
}

#' Bind rows from data.frames with potentially different columns.
#' Coerces columns to a common type when the same column has different types across frames
#' (e.g. logical vs character from CSV reads) so bind_rows does not fail.
bindRowsAligned <- function(...) {
  dfs <- list(...)
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  if (length(dfs) == 0) return(tibble::tibble())
  allCols <- Reduce(union, lapply(dfs, colnames))
  aligned <- lapply(dfs, function(df) {
    for (col in setdiff(allCols, colnames(df))) df[[col]] <- NA
    df[, allCols, drop = FALSE]
  })
  # Unify column types so bind_rows does not error on mixed logical/character/etc.
  for (col in allCols) {
    types <- vapply(aligned, function(d) typeof(d[[col]]), character(1L))
    if (length(unique(types)) > 1L) {
      aligned <- lapply(aligned, function(d) {
        d[[col]] <- as.character(d[[col]])
        d
      })
    }
  }
  dplyr::bind_rows(aligned)
}

#' Normalise cdm_name to match the format used by allDP.
normaliseCdmName <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0 || !"cdm_name" %in% colnames(df)) return(df)
  version <- ""
  if ("pkg_version" %in% colnames(df)) {
    version <- paste0("_v", as.numeric(sub("^([0-9]+).*", "\\1", as.character(df$pkg_version))))
    df <- df %>% dplyr::select(-"pkg_version")
  }
  df %>%
    dplyr::select(-dplyr::any_of(c("date_run", "date_export"))) %>%
    dplyr::mutate(cdm_name = dplyr::if_else(.data$cdm_name == "cdm", "EMBD-ULSGE", .data$cdm_name)) %>%
    dplyr::mutate(cdm_name = tolower(paste0(.data$cdm_name, version)))
}

#' Render a DT datatable with PRETTY_NAMES, standard formatting, and optional overlap highlighting.
#' @param numDigits Optional number of decimal places for numeric (non-pct) columns; if NULL, uses 0.
renderPrettyDT <- function(data, filter = "top", pageLength = 25, scrollX = TRUE, numDigits = NULL) {
  rowsToColor <- NULL
  if ("overlap" %in% colnames(data)) {
    data$rowId <- as.numeric(rownames(data))
    rowsToColor <- data %>% dplyr::filter(overlap == TRUE) %>% dplyr::pull(rowId)
    data <- data %>% dplyr::select(-rowId)
  }
  data <- data %>% dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

  pctCols <- grep("pct$|percent|Percent|^%$", colnames(data), value = TRUE)
  numCols <- colnames(data)[vapply(data, is.numeric, logical(1))]
  numCols <- setdiff(numCols, pctCols)

  displayNames <- colnames(data)
  matched <- displayNames %in% names(PRETTY_NAMES)
  displayNames[matched] <- PRETTY_NAMES[displayNames[matched]]

  result <- DT::datatable(
    data = data,
    colnames = displayNames,
    filter = filter,
    options = list(scrollX = scrollX, pageLength = pageLength)
  )

  if (length(pctCols) > 0) result <- result %>% DT::formatRound(pctCols, digits = 2)
  numDig <- if (is.null(numDigits)) 0 else numDigits
  if (length(numCols) > 0) result <- result %>% DT::formatRound(numCols, digits = numDig, mark = ",")

  if (!is.null(rowsToColor)) {
    result <- result %>%
      DT::formatStyle(0, target = "row", backgroundColor = DT::styleEqual(rowsToColor, rep("red", length(rowsToColor))))
  }
  result
}

#' Darwin footer
customDarwinFooter <- function() {
  shiny::tags$footer(
    class = "darwin-footer",
    shiny::h6(
      sprintf(
        "PregnancyIdentifier | Deployed on: %s | %s European Medicines Agency. All rights reserved.",
        Sys.Date(),
        substr(Sys.Date(), start = 1, stop = 4)
      )
    )
  )
}

#' Helper to filter data by selected databases; used in all modules.
filterByCdm <- function(data, selected, allChoices) {
  if (is.null(selected) || length(selected) == 0) selected <- allChoices
  if (!"cdm_name" %in% colnames(data)) return(data)
  data %>% dplyr::filter(.data$cdm_name %in% selected)
}
