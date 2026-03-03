# helpers.R - Shared utility functions to reduce boilerplate across modules
# These extract patterns that were repeated 15+ times across the codebase.

#' Fixed colour palette for pregnancy outcome categories.
#' Used by barPlot() and modules to ensure consistent colours across all tabs.
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
#' Applied to DT tables so users see "Database" instead of "cdm_name".
#' Covers all standard columns across all CSVs produced by PregnancyIdentifier.
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
  n_in_span                             = "N In Expected Window",
  n_at_midpoint                         = "N At Midpoint",
  p_after_min                           = "% After Min Date",
  p_prior_max                           = "% Before Max Date",
  p_in_span                             = "% In Expected Window",
  p_at_midpoint                         = "% At Midpoint",
  p_concept                             = "% Concept Coverage",

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
#' Only renames columns that exist in the lookup; leaves others unchanged.
#' @param df A data.frame
#' @return A data.frame with prettier column names
prettyColNames <- function(df) {
  nms <- colnames(df)
  matched <- nms %in% names(PRETTY_NAMES)
  nms[matched] <- PRETTY_NAMES[nms[matched]]
  colnames(df) <- nms
  df
}

#' Create a standard database picker InputPanel.
#' Extracts the 10-line boilerplate that was identical across 15+ modules.
#' @param dp Character vector of database names (choices).
#' @param parentNamespace The parent module's namespace.
#' @param multiple Allow multiple selection (default TRUE).
#' @return An InputPanel object ready to use.
createDatabasePicker <- function(dp, parentNamespace, multiple = TRUE) {
  opts <- if (multiple) {
    list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
  } else {
    list(size = 10)
  }
  panel <- InputPanel$new(
    fun = list(cdm_name = shinyWidgets::pickerInput),
    args = list(cdm_name = list(
      inputId = "cdm_name",
      label = "Database",
      choices = dp,
      selected = if (multiple) dp else dp[1],
      multiple = multiple,
      options = opts
    )),
    growDirection = "horizontal"
  )
  panel$parentNamespace <- parentNamespace
  panel
}

#' Get selected databases from a picker, falling back to all choices.
#' @param picker An InputPanel with a cdm_name picker.
#' @param allChoices Character vector of all available choices.
#' @return Character vector of selected database names.
getSelectedCdm <- function(picker, allChoices) {
  sel <- picker$inputValues$cdm_name
  if (is.null(sel) || length(sel) == 0) allChoices else sel
}

#' Bind rows from data.frames with potentially different columns.
#' Adds missing columns as NA so rbind works across schema versions.
#' Replaces 4+ copies of the same loop pattern.
#' @param ... Data frames to bind.
#' @return A combined data.frame.
bindRowsAligned <- function(...) {
  dfs <- list(...)
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  if (length(dfs) == 0) return(tibble::tibble())
  allCols <- Reduce(union, lapply(dfs, colnames))
  aligned <- lapply(dfs, function(df) {
    for (col in setdiff(allCols, colnames(df))) df[[col]] <- NA
    df[, allCols, drop = FALSE]
  })
  dplyr::bind_rows(aligned)
}

#' Normalise cdm_name to match the format used by allDP.
#' Mirrors the transformation in loadFile(): version suffix + lowercase.
#' @param df A data.frame with cdm_name column.
#' @return The data.frame with normalised cdm_name (and date/version columns removed).
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
