#' Export pregnancy results into shareable summary CSVs
#'
#' Reads the patient-level pregnancy episode results produced by the pipeline (from
#' `outputDir`; see `runPregnancyIdentifier()`), generates a set of de-identified
#' summary tables (counts, age summaries, timing distributions, outcome counts,
#' and date completeness checks), writes them to `exportDir`, and creates a ZIP
#' archive of all exported files.
#'
#' The export is intended for lightweight QA and sharing across sites. Small cell
#' counts can be suppressed via `minCellCount`.
#'
#' @param cdm (`cdm_reference`) CDM reference used to compute exports that require
#'   database tables (e.g., `person`, `observation_period`).
#' @param outputDir (`character(1)`) Directory containing pipeline outputs
#'   (e.g., `final_pregnancy_episodes.rds`, logs, `pps_concept_counts.csv`).
#' @param exportDir (`character(1)`) Directory where shareable CSVs (and ZIP) will
#'   be written.
#' @param minCellCount (`integer(1)`) Minimum count threshold for suppression of
#'   small cells (default 5). Values in (0, minCellCount) are replaced with `NA`.
#'
#' @return Invisibly returns `NULL`. Writes CSVs and a ZIP file to `exportDir`.
#' @export
exportPregnancies <- function(cdm, outputDir, exportDir, minCellCount = 5) {
  runStart <- utils::read.csv(file.path(outputDir, "runStart.csv"))$start

  dir.create(exportDir, showWarnings = FALSE, recursive = TRUE)
  snap <- CDMConnector::snapshot(cdm)
  utils::write.csv(snap, file.path(exportDir, "cdm_source.csv"), row.names = FALSE)

  res <- readRDS(file.path(outputDir, "final_pregnancy_episodes.rds"))
  names(res) <- tolower(names(res)) # standardize: episode result column names are snake_case
  if (!"merge_pregnancy_start" %in% names(res) && "final_episode_start_date" %in% names(res)) {
    res$merge_pregnancy_start <- res$final_episode_start_date
  }

  # Copy key raw artifacts (if present)
  for (f in c("pps_concept_counts.csv", "log.txt")) {
    src <- file.path(outputDir, f)
    if (file.exists(src)) file.copy(src, file.path(exportDir, f), overwrite = TRUE)
  }

  pkgVersion <- as.character(utils::packageVersion("PregnancyIdentifier"))

  # Standard metadata appended to every export
  meta <- list(
    snap = snap,
    runStart = runStart,
    pkgVersion = pkgVersion,
    minCellCount = minCellCount
  )

  exportAgeSummary(res, cdm, exportDir, meta$snap, meta$runStart, meta$pkgVersion, meta$minCellCount)
  exportPrecisionDays(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion)
  exportEpisodeFrequency(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion, meta$minCellCount)
  exportPregnancyFrequency(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion, meta$minCellCount)
  exportEpisodeFrequencySummary(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion)
  exportGestationalAgeSummary(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion)
  exportGestationalAgeCounts(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion)
  exportGestationalWeeksCounts(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion, meta$minCellCount)
  exportGestationalDurationCounts(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion)
  exportTimeTrends(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion)
  exportObservationPeriodRange(res, cdm, exportDir, meta$snap, meta$runStart, meta$pkgVersion)
  exportPregnancyOverlapCounts(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion)
  exportDateConsistency(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion)
  exportReversedDatesCounts(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion)
  exportOutcomeCategoriesCounts(res, exportDir, meta$snap, meta$runStart, meta$pkgVersion)
  exportConceptTimingCheck(cdm, res, exportDir, meta$snap, meta$runStart, meta$pkgVersion)

  zipName <- sprintf("%s-%s-%s-results.zip", snap$snapshot_date, pkgVersion, snap$cdm_name)
  utils::zip(
    zipfile = file.path(exportDir, zipName),
    files = list.files(path = exportDir, full.names = TRUE),
    flags = "-j"
  )

  message(sprintf("Files have been written to: %s", exportDir))
  invisible(NULL)
}

#' @noRd
exportConceptTimingCheck <- function(cdm, res, resPath, snap, runStart, pkgVersion) {
  concepts <- utils::read.csv(system.file("concepts/check_concepts.csv", package = "PregnancyIdentifier", mustWork = TRUE))
  totalEpisodes <- nrow(res)

  # Get concepts from multiple OMOP domains into one schema and combine with episodes
  conceptsPerEpisode <- dplyr::union_all(
    cdm$condition_occurrence %>%
      dplyr::select(
        "person_id",
        concept_id = "condition_concept_id",
        concept_start = "condition_start_date",
        concept_end = "condition_end_date"
      ),
    cdm$procedure_occurrence %>%
      dplyr::select(
        "person_id",
        concept_id = "procedure_concept_id",
        concept_start = "procedure_date",
        concept_end = "procedure_end_date"
      )
  ) %>%
    dplyr::union_all(
      cdm$observation %>%
        dplyr::select(
          "person_id",
          concept_id = "observation_concept_id",
          concept_start = "observation_date",
          concept_end = "observation_date"
        )
    ) %>%
    dplyr::right_join(res, by = "person_id", copy = TRUE) %>%
    dplyr::filter(.data$concept_id %in% concepts$concept_id) %>%
    dplyr::collect() %>%
    dplyr::left_join(concepts, by = "concept_id")

  # select and add columns
  conceptsPerEpisode <- conceptsPerEpisode %>%
    dplyr::rename(episode_num = .data$merge_episode_number) %>%
    dplyr::select(
      "person_id", "episode_num", "merge_pregnancy_start", "hip_end_date", "pps_end_date",
      "concept_id", "concept_name", "concept_start", "concept_end",
      "min_month", "max_month", "span", "midpoint"
    ) %>%
    dplyr::mutate(
      concept_end = dplyr::coalesce(.data$concept_end, .data$concept_start),
      min_date = .data$merge_pregnancy_start + (.data$min_month * 30),
      max_date = .data$merge_pregnancy_start + (.data$max_month * 30),
      after_min = .data$min_date >= .data$concept_start,
      prior_max = .data$max_date <= .data$concept_end,
      in_span = .data$concept_start >= .data$min_date & .data$concept_start <= .data$max_date,
      # NOTE: keeping original behavior (even though this condition looks suspicious)
      at_midpoint = .data$concept_start >= (.data$merge_pregnancy_start + .data$midpoint) &
        .data$concept_start <= (.data$merge_pregnancy_start - .data$midpoint * 30)
    )

  # Group by concept and summarize counts
  conceptsPerEpisode %>%
    dplyr::group_by(.data$concept_id, .data$concept_name) %>%
    dplyr::summarise(
      n_after_min = sum(.data$after_min, na.rm = TRUE),
      n_prior_max = sum(.data$prior_max, na.rm = TRUE),
      n_in_span = sum(.data$in_span, na.rm = TRUE),
      n_at_midpoint = sum(.data$at_midpoint, na.rm = TRUE),
      total = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_after_min = .data$n_after_min / totalEpisodes * 100,
      p_prior_max = .data$n_prior_max / totalEpisodes * 100,
      p_in_span = .data$n_in_span / totalEpisodes * 100,
      p_at_midpoint = .data$n_at_midpoint / totalEpisodes * 100,
      p_concept = .data$total / totalEpisodes * 100,
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "concept_check.csv"), row.names = FALSE)
}

# ---- Shared helpers (internal) ------------------------------------------------

#' @noRd
addAge <- function(cdm, res) {
  cdm$person %>%
    dplyr::select("person_id", "gender_concept_id", "birth_datetime", "year_of_birth") %>%
    dplyr::mutate(
      birth_datetime = dplyr::if_else(
        is.na(.data$birth_datetime),
        as.Date(paste0(as.character(.data$year_of_birth), "-01-01")),
        as.Date(.data$birth_datetime)
      )
    ) %>%
    dplyr::right_join(res, by = "person_id", copy = TRUE) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      age_pregnancy_start = as.numeric(as.Date(.data$final_episode_start_date) - .data$birth_datetime)
    )
}

#' @noRd
exportAgeSummary <- function(res, cdm, resPath, snap, runStart, pkgVersion, minCellCount) {
  resAge <- addAge(cdm, res) %>%
    dplyr::mutate(age_pregnancy_start = age_pregnancy_start / 365.25)

  resAge %>%
    dplyr::select(age_pregnancy_start) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    write.csv(file.path(resPath, "age.csv"), row.names = FALSE)

  resAge %>%
    summariseColumn("age_pregnancy_start") %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "age_summary.csv"), row.names = FALSE)

  resAgeRound <- resAge %>%
    dplyr::filter(!is.na(.data$age_pregnancy_start)) %>%
    dplyr::mutate(age_pregnancy_start = round(.data$age_pregnancy_start))

  perYear <- summariseCategory(resAgeRound, "age_pregnancy_start")

  groups <- resAgeRound %>%
    dplyr::mutate(
      age_pregnancy_start_group = dplyr::case_when(
        .data$age_pregnancy_start < 12 ~ "<12",
        .data$age_pregnancy_start > 55 ~ ">55",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(.data$age_pregnancy_start_group)) %>%
    summariseCategory("age_pregnancy_start_group") %>%
    dplyr::rename(age_pregnancy_start = "age_pregnancy_start_group")

  # dplyr::bind_rows(perYear, groups) %>% type mismatch chr vs numeric in age_pregnancy_start
  rbind(perYear, groups) %>%
    suppressCounts("n", minCellCount) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "age_summary_groups.csv"), row.names = FALSE)
}

#' @noRd
exportPrecisionDays <- function(res, resPath, snap, runStart, pkgVersion) {
  d <- res %>%
    dplyr::filter(!is.na(.data$esd_precision_days)) %>%
    dplyr::pull(.data$esd_precision_days) %>%
    stats::density()

  data.frame(esd_precision_days = d$x, density = d$y) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "precision_days.csv"), row.names = FALSE)
}

#' @noRd
exportEpisodeFrequency <- function(res, resPath, snap, runStart, pkgVersion, minCellCount) {
  res %>%
    dplyr::summarise(
      total_episodes = dplyr::n(),
      total_individuals = dplyr::n_distinct(.data$person_id)
    ) %>%
    suppressCounts("total_individuals", minCellCount) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "episode_frequency.csv"), row.names = FALSE)
}

#' @noRd
exportPregnancyFrequency <- function(res, resPath, snap, runStart, pkgVersion, minCellCount) {
  res %>%
    dplyr::count(.data$person_id, name = "freq") %>%
    dplyr::count(.data$freq, name = "number_individuals") %>%
    suppressCounts("number_individuals", minCellCount) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "pregnancy_frequency.csv"), row.names = FALSE)
}

#' @noRd
exportEpisodeFrequencySummary <- function(res, resPath, snap, runStart, pkgVersion) {
  res %>%
    dplyr::count(.data$person_id, name = "freq") %>%
    summariseColumn("freq") %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "episode_frequency_summary.csv"), row.names = FALSE)
}

#' @noRd
exportGestationalAgeSummary <- function(res, resPath, snap, runStart, pkgVersion) {
  res %>%
    summariseColumn("esd_gestational_age_days_calculated") %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "gestational_age_days_summary.csv"), row.names = FALSE)
}

#' @noRd
exportGestationalAgeCounts <- function(res, resPath, snap, runStart, pkgVersion) {
  res %>%
    dplyr::summarise(
      less_1day = sum(.data$esd_gestational_age_days_calculated < 1, na.rm = TRUE),
      over_308days = sum(.data$esd_gestational_age_days_calculated > 308, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "gestational_age_days_counts.csv"), row.names = FALSE)
}

#' @noRd
exportGestationalWeeksCounts <- function(res, resPath, snap, runStart, pkgVersion, minCellCount) {
  res %>%
    dplyr::mutate(gestational_weeks = floor(.data$esd_gestational_age_days_calculated / 7)) %>%
    dplyr::count(.data$gestational_weeks, name = "n") %>%
    dplyr::mutate(
      pct = .data$n / sum(.data$n) * 100
    ) %>%
    suppressCounts(colNames = c("n"), minCellCount = minCellCount) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "gestational_weeks.csv"), row.names = FALSE)
}

#' @noRd
exportGestationalDurationCounts <- function(res, resPath, snap, runStart, pkgVersion) {
  res %>%
    dplyr::group_by(.data$final_outcome_category) %>%
    summariseColumn("esd_gestational_age_days_calculated") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(
      file.path(resPath, "gestational_age_days_per_category_summary.csv"),
      row.names = FALSE
    )
}

#' @noRd
exportTimeTrends <- function(res, resPath, snap, runStart, pkgVersion) {
  dateCols <- c(
    "merge_pregnancy_start", "hip_end_date", "pps_end_date", "pps_episode_min_date", "pps_episode_max_date",
    "merge_episode_start", "merge_episode_end", "final_episode_start_date", "final_episode_end_date"
  )

  resLong <- res %>%
    tidyr::pivot_longer(cols = dplyr::any_of(dateCols), names_to = "column", values_to = "date") %>%
    dplyr::mutate(
      date = as.Date(.data$date),
      month = as.integer(format(.data$date, "%m")),
      year = as.integer(format(.data$date, "%Y"))
    )

  writeTrend <- function(df, fileName) {
    df %>%
      dplyr::mutate(
        cdm_name = snap$cdm_name,
        date_run = runStart,
        date_export = snap$snapshot_date,
        pkg_version = pkgVersion
      ) %>%
      utils::write.csv(file.path(resPath, fileName), row.names = FALSE)
  }

  yearly <- resLong %>%
    dplyr::count(.data$column, .data$year, name = "count")

  writeTrend(dplyr::filter(yearly, is.na(.data$year)), "yearly_trend_missing.csv")
  writeTrend(dplyr::filter(yearly, !is.na(.data$year)), "yearly_trend.csv")

  monthly <- resLong %>%
    dplyr::count(.data$column, .data$month, name = "count") %>%
    dplyr::mutate(month = factor(base::month.name[.data$month], levels = base::month.name))

  writeTrend(dplyr::filter(monthly, is.na(.data$month)), "monthly_trend_missing.csv")
  writeTrend(dplyr::filter(monthly, !is.na(.data$month)), "monthly_trends.csv")
}

#' @noRd
exportObservationPeriodRange <- function(res, cdm, resPath, snap, runStart, pkgVersion) {
  cdm$observation_period %>%
    dplyr::summarise(
      min_obs = min(!!CDMConnector::datepart("observation_period_start_date", interval = "year"), na.rm = TRUE),
      max_obs = max(!!CDMConnector::datepart("observation_period_end_date", interval = "year"), na.rm = TRUE)
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "observation_period_range.csv"), row.names = FALSE)
}

#' @noRd
exportPregnancyOverlapCounts <- function(res, resPath, snap, runStart, pkgVersion) {
  res %>%
    dplyr::add_count(.data$person_id) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::arrange(.data$person_id, .data$final_episode_start_date, .data$final_episode_end_date) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::mutate(
      prev_end = dplyr::lag(.data$final_episode_end_date),
      overlap = as.Date(.data$final_episode_start_date) <= as.Date(.data$prev_end)
    ) %>%
    dplyr::filter(!is.na(.data$prev_end)) %>%
    summariseColumn("overlap") %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "pregnancy_overlap_counts.csv"), row.names = FALSE)
}

#' @noRd
exportDateConsistency <- function(res, resPath, snap, runStart, pkgVersion) {
  dateCols <- c(
    "merge_pregnancy_start", "hip_end_date", "pps_end_date", "pps_episode_min_date", "pps_episode_max_date",
    "merge_episode_start", "merge_episode_end", "final_episode_start_date", "final_episode_end_date"
  )

  res %>%
    dplyr::summarise(dplyr::across(dplyr::any_of(dateCols), ~ mean(is.na(.)))) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "date_consistency.csv"), row.names = FALSE)
}

#' @noRd
exportReversedDatesCounts <- function(res, resPath, snap, runStart, pkgVersion) {
  res %>%
    dplyr::mutate(
      rev_hip = .data$merge_pregnancy_start > .data$hip_end_date,
      rev_pps = .data$merge_pregnancy_start > .data$pps_end_date
    ) %>%
    dplyr::summarise(
      n_rev_hip = sum(.data$rev_hip, na.rm = TRUE),
      n_rev_pps = sum(.data$rev_pps, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "swapped_dates.csv"), row.names = FALSE)
}

#' @noRd
exportOutcomeCategoriesCounts <- function(res, resPath, snap, runStart, pkgVersion) {
  bindCounts <- function(df, col, algo) {
    df %>%
      dplyr::count(.data[[col]], name = "n") %>%
      dplyr::mutate(algorithm = algo) %>%
      dplyr::rename(outcome_category = dplyr::all_of(col))
  }

  dplyr::bind_rows(
    bindCounts(res, "hip_outcome_category", "hip"),
    bindCounts(res, "pps_outcome_category", "pps"),
    bindCounts(res, "final_outcome_category", "hipps")
  ) %>%
    dplyr::group_by(.data$algorithm) %>%
    dplyr::mutate(pct = .data$n / sum(.data$n) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "outcome_categories_count.csv"), row.names = FALSE)
}

# ---- Small utility functions --------------------------------------------------

#' @noRd
suppressCounts <- function(df, colNames, minCellCount) {
  suppressOne <- function(x) {
    x[x > 0 & x < minCellCount] <- paste0("<", minCellCount)
    x
  }
  df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(colNames), suppressOne))
}

#' @noRd
summariseCategory <- function(df, colName) {
  df %>%
    dplyr::count(.data[[colName]], name = "n") %>%
    dplyr::mutate(
      colName = colName,
      total = sum(.data$n),
      pct = .data$n / .data$total * 100
    ) %>%
    dplyr::relocate("colName")
}

#' @noRd
summariseNumeric <- function(df, colName) {
  df %>%
    dplyr::summarise(
      colName = colName,
      min = min(.data[[colName]], na.rm = TRUE),
      Q25 = stats::quantile(.data[[colName]], 0.25, na.rm = TRUE),
      median = stats::median(.data[[colName]], na.rm = TRUE),
      Q75 = stats::quantile(.data[[colName]], 0.75, na.rm = TRUE),
      max = max(.data[[colName]], na.rm = TRUE),
      mean = mean(.data[[colName]], na.rm = TRUE),
      sd = stats::sd(.data[[colName]], na.rm = TRUE)
    ) %>%
    dplyr::tibble()
}

#' @noRd
summariseColumn <- function(df, colName) {
  suppressWarnings({
    sampleClass <- df %>%
      utils::head(1000) %>%
      dplyr::pull(.data[[colName]]) %>%
      class()

    switch(
      sampleClass,
      numeric  = summariseNumeric(df, colName),
      integer  = summariseNumeric(df, colName),
      character = summariseCategory(df, colName),
      logical  = summariseCategory(df, colName),
      rlang::abort("class not supported by summariseColumn!")
    )
  })
}
