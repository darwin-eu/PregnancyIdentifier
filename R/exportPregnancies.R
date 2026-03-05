#' Export pregnancy results into shareable summary CSVs
#'
#' Reads the patient-level pregnancy episode results produced by the pipeline (from
#' \code{outputFolder}; see \code{runPregnancyIdentifier()}), generates a set of
#' de-identified summary tables (counts, age summaries, timing distributions,
#' outcome counts, and date completeness checks), and writes them to \code{exportFolder}.
#' \code{runPregnancyIdentifier()} runs this step automatically and writes to
#' \code{exportFolder} (default \code{file.path(outputDir, "export")}); use
#' \code{exportPregnancies()} when you need to re-export or write to a different
#' directory. Does not create a ZIP file; use \code{zipExportFolder()} after export
#' (and optionally after writing PET comparison tables to the same folder) to create
#' an archive.
#'
#' The export is intended for lightweight QA and sharing across sites. Small cell
#' counts can be suppressed via \code{minCellCount}.
#'
#' @param cdm (`cdm_reference`) CDM reference used to compute exports that require
#'   database tables (e.g., `person`, `observation_period`).
#' @param outputFolder (`character(1)`) Directory containing pipeline outputs
#'   (e.g., `final_pregnancy_episodes.rds`, logs, `pps_concept_counts.csv`).
#' @param exportFolder (`character(1)`) Directory where shareable CSVs will be written.
#' @param minCellCount (`integer(1)`) Minimum count threshold for suppression of
#'   small cells (default 5). Values in (0, minCellCount) are replaced with `NA`.
#' @param res Optional data frame of pregnancy episodes. If provided, used instead
#'   of reading \code{final_pregnancy_episodes.rds} from \code{outputFolder}. Used when
#'   exporting a conformed copy (e.g. \code{conformToValidation = "both"}).
#'
#' @return Invisibly returns `NULL`. Writes CSVs to `exportFolder`.
#' @export
exportPregnancies <- function(cdm, outputFolder, exportFolder, minCellCount = 5, res = NULL) {
  runStart <- utils::read.csv(file.path(outputFolder, "runStart.csv"))$start

  dir.create(exportFolder, showWarnings = FALSE, recursive = TRUE)
  snap <- CDMConnector::snapshot(cdm)
  utils::write.csv(snap, file.path(exportFolder, "cdm_source.csv"), row.names = FALSE)

  if (is.null(res)) {
    res <- readRDS(file.path(outputFolder, "final_pregnancy_episodes.rds"))
  }
  names(res) <- tolower(names(res)) # standardize: episode result column names are snake_case
  if (!"merge_pregnancy_start" %in% names(res) && "final_episode_start_date" %in% names(res)) {
    res$merge_pregnancy_start <- res$final_episode_start_date
  }

  # Copy key raw artifacts (if present)
  for (f in c("hip_concept_counts.csv", "pps_concept_counts.csv", "esd_concept_counts.csv", "log.txt", "attrition.csv")) {
    src <- file.path(outputFolder, f)
    if (file.exists(src)) file.copy(src, file.path(exportFolder, f), overwrite = TRUE)
  }

  pkgVersion <- as.character(utils::packageVersion("PregnancyIdentifier"))

  # Standard metadata appended to every export
  meta <- list(
    snap = snap,
    runStart = runStart,
    pkgVersion = pkgVersion,
    minCellCount = minCellCount
  )

  exportAgeSummary(res, cdm, exportFolder, meta$snap, meta$runStart, meta$pkgVersion, meta$minCellCount)
  exportPrecisionDays(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportPrecisionDaysDenominators(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportEpisodeFrequency(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion, meta$minCellCount)
  exportPregnancyFrequency(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion, meta$minCellCount)
  exportEpisodeFrequencySummary(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportGestationalAgeSummary(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportGestationalAgeCounts(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportGestationalWeeksCounts(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion, meta$minCellCount)
  exportGestationalDurationCounts(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportTimeTrends(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportObservationPeriodRange(res, cdm, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportPregnancyOverlapCounts(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportMissingDates(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportReversedDatesCounts(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportOutcomeCategoriesCounts(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportDeliveryModeSummary(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportConceptTimingCheck(cdm, res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)
  exportCleanupQualityCheck(res, exportFolder, meta$snap, meta$runStart, meta$pkgVersion)

  message(sprintf("Files have been written to: %s", exportFolder))
  invisible(NULL)
}

#' Create a ZIP archive of an export folder
#'
#' Zips all files in the given export directory into a single ZIP file. Use this
#' after \code{exportPregnancies()} and, if applicable, after writing PET
#' comparison tables to the same folder, so the archive includes both shareable
#' CSVs and PET comparison outputs.
#'
#' @param exportFolder (\code{character(1)}) Path to the export folder (contents will
#'   be zipped).
#' @param zipPath (\code{character(1)} or \code{NULL}) Full path for the output ZIP
#'   file. If \code{NULL}, the ZIP is created inside \code{exportFolder} with a
#'   name like \code{YYYY-MM-DD-version-results.zip} (using today's date and
#'   package version). Supply \code{zipPath} for a custom name (e.g. including
#'   your CDM name).
#' @return Invisibly returns the path to the created ZIP file.
#' @export
zipExportFolder <- function(exportFolder, zipPath = NULL) {
  checkmate::assertCharacter(exportFolder, len = 1L)
  checkmate::assertDirectoryExists(exportFolder)
  if (is.null(zipPath)) {
    pkgVersion <- as.character(utils::packageVersion("PregnancyIdentifier"))
    zipName <- sprintf("%s-%s-results.zip", format(Sys.Date(), "%Y-%m-%d"), pkgVersion)
    zipPath <- file.path(exportFolder, zipName)
  }
  checkmate::assertCharacter(zipPath, len = 1L)
  files <- list.files(path = exportFolder, full.names = TRUE)
  if (length(files) == 0) {
    warning("Export folder is empty; no ZIP created.")
    return(invisible(NULL))
  }
  utils::zip(zipfile = zipPath, files = files, flags = "-j -q")
  message(sprintf("ZIP created: %s", zipPath))
  invisible(zipPath)
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
        concept_end = "procedure_date"
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
    dplyr::rename(episode_num = "merge_episode_number") %>%
    dplyr::select(
      "person_id", "episode_num", "merge_pregnancy_start", "hip_end_date", "pps_end_date",
      "concept_id", "concept_name", "concept_start", "concept_end",
      "min_month", "max_month", "span", "midpoint"
    ) %>%
    dplyr::mutate(
      concept_end = dplyr::coalesce(.data$concept_end, .data$concept_start),
      min_date = .data$merge_pregnancy_start + (.data$min_month * 30),
      max_date = .data$merge_pregnancy_start + (.data$max_month * 30),
      on_or_before_min = .data$min_date >= .data$concept_start,
      on_or_after_max = .data$max_date <= .data$concept_end,
      in_span = .data$concept_start >= .data$min_date & .data$concept_start <= .data$max_date,
      # NOTE: keeping original behavior (even though this condition looks suspicious)
      at_midpoint = .data$concept_start >= (.data$merge_pregnancy_start + .data$midpoint) &
        .data$concept_start <= (.data$merge_pregnancy_start - .data$midpoint * 30)
    )

  # Group by concept and summarize counts
  conceptsPerEpisode %>%
    dplyr::group_by(.data$concept_id, .data$concept_name) %>%
    dplyr::summarise(
      n_on_or_before_min = sum(.data$on_or_before_min, na.rm = TRUE),
      n_on_or_after_max = sum(.data$on_or_after_max, na.rm = TRUE),
      n_in_span = sum(.data$in_span, na.rm = TRUE),
      n_at_midpoint = sum(.data$at_midpoint, na.rm = TRUE),
      total = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Timing %: denominator = row total (occurrences in pregnancy episodes for this concept)
      p_on_or_before_min = dplyr::if_else(.data$total > 0, .data$n_on_or_before_min / .data$total * 100, NA_real_),
      p_on_or_after_max = dplyr::if_else(.data$total > 0, .data$n_on_or_after_max / .data$total * 100, NA_real_),
      p_in_span = dplyr::if_else(.data$total > 0, .data$n_in_span / .data$total * 100, NA_real_),
      p_at_midpoint = dplyr::if_else(.data$total > 0, .data$n_at_midpoint / .data$total * 100, NA_real_),
      # % of pregnancies with at least one occurrence of this concept (denominator = total episodes)
      p_concept = .data$total / totalEpisodes * 100,
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    dplyr::rename(n_occurrences_in_episodes = "total") %>%
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
      age_pregnancy_start = (as.numeric(as.Date(.data$final_episode_start_date) - .data$birth_datetime) / 365.25),
      age_pregnancy_end = (as.numeric(as.Date(.data$final_episode_end_date) - .data$birth_datetime) / 365.25)
    )
}

#' @noRd
exportAgeSummary <- function(res, cdm, resPath, snap, runStart, pkgVersion, minCellCount) {
  resAge <- addAge(cdm, res)

  # 5-number summary (min, Q25, median, Q75, max, mean) of age_pregnancy_start overall and by outcome
  resAgeFiltered <- resAge %>%
    dplyr::filter(!is.na(.data$age_pregnancy_start))
  overall <- resAgeFiltered %>%
    dplyr::summarise(
      final_outcome_category = "overall",
      n = dplyr::n(),
      min = min(.data$age_pregnancy_start, na.rm = TRUE),
      Q25 = stats::quantile(.data$age_pregnancy_start, 0.25, na.rm = TRUE),
      median = stats::median(.data$age_pregnancy_start, na.rm = TRUE),
      Q75 = stats::quantile(.data$age_pregnancy_start, 0.75, na.rm = TRUE),
      max = max(.data$age_pregnancy_start, na.rm = TRUE),
      mean = mean(.data$age_pregnancy_start, na.rm = TRUE),
      .groups = "drop"
    )
  by_outcome <- resAgeFiltered %>%
    dplyr::group_by(.data$final_outcome_category) %>%
    dplyr::summarise(
      n = dplyr::n(),
      min = min(.data$age_pregnancy_start, na.rm = TRUE),
      Q25 = stats::quantile(.data$age_pregnancy_start, 0.25, na.rm = TRUE),
      median = stats::median(.data$age_pregnancy_start, na.rm = TRUE),
      Q75 = stats::quantile(.data$age_pregnancy_start, 0.75, na.rm = TRUE),
      max = max(.data$age_pregnancy_start, na.rm = TRUE),
      mean = mean(.data$age_pregnancy_start, na.rm = TRUE),
      .groups = "drop"
    )
  dplyr::bind_rows(overall, by_outcome) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "age_summary.csv"), row.names = FALSE)

  # summarise age at first pregnancy start (first LB by episode start date)
  resAge %>%
    dplyr::filter(.data$final_outcome_category == "LB") %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::slice_min(order_by = .data$final_episode_start_date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    summariseColumn("age_pregnancy_start") %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    ) %>%
    utils::write.csv(file.path(resPath, "age_summary_first_pregnancy.csv"), row.names = FALSE)

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
  x <- res %>%
    dplyr::filter(!is.na(.data$esd_precision_days)) %>%
    dplyr::pull(.data$esd_precision_days)

  if (length(x) < 2L) {
    if (length(x) == 0L) {
      out <- data.frame(
        esd_precision_days = numeric(0),
        density = numeric(0),
        cdm_name = character(0),
        date_run = character(0),
        date_export = character(0),
        pkg_version = character(0)
      )
    } else {
      out <- data.frame(
        esd_precision_days = x,
        density = 1,
        cdm_name = as.character(snap$cdm_name)[1],
        date_run = as.character(runStart)[1],
        date_export = as.character(snap$snapshot_date)[1],
        pkg_version = as.character(pkgVersion)[1]
      )
    }
  } else {
    d <- stats::density(x)
    out <- data.frame(esd_precision_days = d$x, density = d$y) %>%
      dplyr::mutate(
        cdm_name = snap$cdm_name,
        date_run = runStart,
        date_export = snap$snapshot_date,
        pkg_version = pkgVersion
      )
  }
  utils::write.csv(out, file.path(resPath, "precision_days.csv"), row.names = FALSE)
}

#' @noRd
exportPrecisionDaysDenominators <- function(res, resPath, snap, runStart, pkgVersion) {
  total_episodes <- nrow(res)
  with_precision <- sum(!is.na(suppressWarnings(as.numeric(res$esd_precision_days))))
  pct_precision <- if (total_episodes > 0) round(100 * with_precision / total_episodes, 1) else NA_real_
  out <- data.frame(
    cdm_name = as.character(snap$cdm_name)[1],
    total_episodes = total_episodes,
    episodes_with_precision_days = with_precision,
    pct_with_precision_days = pct_precision,
    date_run = as.character(runStart)[1],
    date_export = as.character(snap$snapshot_date)[1],
    pkg_version = as.character(pkgVersion)[1]
  )
  if ("esd_gw_flag" %in% names(res)) {
    gw_ok <- suppressWarnings(as.numeric(res$esd_gw_flag))
    with_gw <- sum(!is.na(gw_ok) & gw_ok > 0)
    out$episodes_with_gw_timing <- with_gw
    out$pct_with_gw_timing <- if (total_episodes > 0) round(100 * with_gw / total_episodes, 1) else NA_real_
  }
  utils::write.csv(out, file.path(resPath, "precision_days_denominators.csv"), row.names = FALSE)
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
    dplyr::group_by(.data$final_outcome_category) %>%
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
  counts <- res %>%
    dplyr::group_by(.data$final_outcome_category) %>%
    dplyr::summarise(
      episode_count = dplyr::n(),
      person_count = dplyr::n_distinct(.data$person_id),
      .groups = "drop"
    )
  res %>%
    dplyr::group_by(.data$final_outcome_category) %>%
    summariseColumn("esd_gestational_age_days_calculated") %>%
    dplyr::ungroup() %>%
    dplyr::left_join(counts, by = "final_outcome_category") %>%
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

#' Export overlap counts in long format: one row per overlap category (FALSE, TRUE).
#' FALSE = episode does not overlap another (including no previous episode); TRUE = episode overlaps another.
#' Output has colName, overlap, n, total, pct, and metadata (cdm_name, date_run, date_export, pkg_version).
#' @noRd
exportPregnancyOverlapCounts <- function(res, resPath, snap, runStart, pkgVersion) {
  total_n <- nrow(res)
  if (total_n == 0) {
    summary_overlap <- tibble::tibble(
      colName = character(0),
      overlap = logical(0),
      n = integer(0),
      total = integer(0),
      pct = numeric(0),
      cdm_name = character(0),
      date_run = character(0),
      date_export = character(0),
      pkg_version = character(0)
    )
  } else {
    overlap_by_record <- res %>%
      dplyr::arrange(.data$person_id, .data$final_episode_start_date, .data$final_episode_end_date) %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(
        prev_end = dplyr::lag(.data$final_episode_end_date),
        overlap = dplyr::if_else(
          is.na(.data$prev_end),
          FALSE,
          as.Date(.data$final_episode_start_date) <= as.Date(.data$prev_end)
        )
      ) %>%
      dplyr::ungroup()

    n_false <- as.integer(sum(overlap_by_record$overlap == FALSE))
    n_true <- as.integer(sum(overlap_by_record$overlap == TRUE))
    counts <- tibble::tibble(
      colName = "overlap",
      overlap = c(FALSE, TRUE),
      n = c(n_false, n_true),
      total = total_n,
      pct = as.numeric(c(n_false, n_true)) / total_n * 100
    )

    summary_overlap <- counts %>%
      dplyr::mutate(
        cdm_name = snap$cdm_name,
        date_run = as.character(runStart),
        date_export = as.character(snap$snapshot_date),
        pkg_version = as.character(pkgVersion)
      )
  }

  utils::write.csv(summary_overlap, file.path(resPath, "pregnancy_overlap_counts.csv"), row.names = FALSE)
}

# Maximum episode length in days (same as ESD/utils cleanup). Episodes longer are dropped.
.cleanupMaxDays <- 308L

#' Count records and persons with overlapping episodes (start_a < end_b and end_a > start_b within person)
#' @noRd
countOverlappingRecordsAndPersons <- function(res, personIdCol = "person_id",
                                              startDateCol = "final_episode_start_date",
                                              endDateCol = "final_episode_end_date") {
  complete <- res %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      !is.na(.data[[startDateCol]]) & !is.na(.data[[endDateCol]])
    )
  if (nrow(complete) == 0) {
    return(list(records = 0L, persons = 0L))
  }
  complete <- complete %>%
    dplyr::mutate(.row = dplyr::row_number())
  other <- complete %>%
    dplyr::select(
      dplyr::all_of(personIdCol),
      ".row2" = ".row",
      "start2" = dplyr::all_of(startDateCol),
      "end2" = dplyr::all_of(endDateCol)
    )
  overlaps <- complete %>%
    dplyr::inner_join(other, by = personIdCol, relationship = "many-to-many") %>%
    dplyr::filter(
      .data$.row != .data$.row2,
      as.Date(.data[[startDateCol]]) < as.Date(.data$end2),
      as.Date(.data[[endDateCol]]) > as.Date(.data$start2)
    )
  overlapping_rows <- unique(overlaps$.row)
  n_records <- length(overlapping_rows)
  n_persons <- if (n_records == 0) 0L else as.integer(complete %>%
    dplyr::filter(.data$.row %in% .env$overlapping_rows) %>%
    dplyr::distinct(.data[[personIdCol]]) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::pull("n"))
  list(records = as.integer(n_records), persons = as.integer(n_persons))
}

#' Count records and persons with episode length > maxDays
#' @noRd
countTooLongRecordsAndPersons <- function(res, maxDays = .cleanupMaxDays,
                                         startDateCol = "final_episode_start_date",
                                         endDateCol = "final_episode_end_date") {
  with_days <- res %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .days = as.numeric(as.Date(.data[[endDateCol]]) - as.Date(.data[[startDateCol]]))
    )
  too_long <- with_days %>%
    dplyr::filter(!is.na(.data$.days), .data$.days > .env$maxDays)
  list(
    records = as.integer(nrow(too_long)),
    persons = as.integer(dplyr::n_distinct(too_long$person_id))
  )
}

#' Count records with start date >= end date (both non-NA)
#' @noRd
countStartGteEnd <- function(res, startDateCol = "final_episode_start_date",
                             endDateCol = "final_episode_end_date") {
  res %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      !is.na(.data[[startDateCol]]),
      !is.na(.data[[endDateCol]]),
      as.Date(.data[[startDateCol]]) >= as.Date(.data[[endDateCol]])
    ) %>%
    nrow()
}

#' Apply cleanup: fix start < end, then remove episodes with length > maxDays, then removeOverlaps
#' @param res Data frame of final pregnancy episodes.
#' @param maxDays Maximum episode length in days (default from .cleanupMaxDays).
#' @param return_steps If TRUE, return list with \code{df}, \code{steps}, \code{n_fix_corrected}; otherwise return cleaned data frame only.
#' @noRd
applyCleanup <- function(res, maxDays = .cleanupMaxDays, return_steps = FALSE) {
  termMaxMin <- readxl::read_excel(
    system.file("concepts", "Matcho_term_durations.xlsx", package = "PregnancyIdentifier", mustWork = TRUE)
  )
  fixResult <- fixStartBeforeEnd(
    res,
    termMaxMin,
    startDateCol = "final_episode_start_date",
    endDateCol = "final_episode_end_date",
    outcomeCol = "final_outcome_category"
  )
  after_fix <- fixResult$df
  after_long <- after_fix %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .days = as.numeric(as.Date(.data$final_episode_end_date) - as.Date(.data$final_episode_start_date))
    ) %>%
    dplyr::filter(is.na(.data$.days) | .data$.days <= .env$maxDays) %>%
    dplyr::select(-".days")
  after_overlaps <- removeOverlaps(after_long,
                 personIdCol = "person_id",
                 startDateCol = "final_episode_start_date",
                 endDateCol = "final_episode_end_date")
  if (return_steps) {
    list(
      df = after_overlaps,
      steps = list(after_fix = after_fix, after_long = after_long, after_overlaps = after_overlaps),
      n_fix_corrected = fixResult$n_corrected
    )
  } else {
    after_overlaps
  }
}

#' Export final quality check: overlapping / too-long counts and post-cleanup counts, plus attrition-if-cleanup
#' Runs the cleanup pipeline once and reports on what was actually done.
#' @noRd
exportCleanupQualityCheck <- function(res, resPath, snap, runStart, pkgVersion) {
  startDateCol <- "final_episode_start_date"
  endDateCol <- "final_episode_end_date"
  maxDays <- .cleanupMaxDays

  n_before_records <- nrow(res)
  n_before_persons <- as.integer(dplyr::n_distinct(res$person_id))
  n_start_geq_end_before <- countStartGteEnd(res, startDateCol, endDateCol)

  # Run cleanup once; get final result and intermediate steps for reporting
  cleanup <- applyCleanup(res, maxDays, return_steps = TRUE)
  after_cleanup <- cleanup$df
  steps <- cleanup$steps
  n_fix_corrected <- cleanup$n_fix_corrected

  res_after_fix <- steps$after_fix
  after_long_only <- steps$after_long

  n_start_geq_end_after <- countStartGteEnd(res_after_fix, startDateCol, endDateCol)
  message(sprintf("fix_start_before_end: episodes with start >= end before fix: %d", n_fix_corrected))
  message(sprintf("fix_start_before_end: episodes with start >= end after fix: %d", n_start_geq_end_after))

  n_after_records <- nrow(after_cleanup)
  n_after_persons <- as.integer(dplyr::n_distinct(after_cleanup$person_id))
  n_after_fix_records <- nrow(res_after_fix)
  n_after_fix_persons <- as.integer(dplyr::n_distinct(res_after_fix$person_id))
  n_after_long_records <- nrow(after_long_only)
  n_after_long_persons <- as.integer(dplyr::n_distinct(after_long_only$person_id))

  overlap_counts <- countOverlappingRecordsAndPersons(res_after_fix, "person_id", startDateCol, endDateCol)
  too_long_counts <- countTooLongRecordsAndPersons(res_after_fix, maxDays, startDateCol, endDateCol)
  dropped_long_records <- n_after_fix_records - n_after_long_records
  dropped_long_persons <- n_after_fix_persons - n_after_long_persons
  dropped_overlap_records <- n_after_long_records - n_after_records
  dropped_overlap_persons <- n_after_long_persons - n_after_persons

  # ---- Per-record quality flag counts (from ESD quality flags) ----
  # These flags are computed by runEsd() and characterize each episode.
  # When conformToValidation=FALSE (default), flagged records are retained and

  # counted here so users can see what cleanup would affect.
  .countFlag <- function(df, flagCol) {
    if (!flagCol %in% names(df)) return(list(records = NA_integer_, persons = NA_integer_))
    flagged <- df[!is.na(df[[flagCol]]) & df[[flagCol]] == TRUE, , drop = FALSE]
    list(
      records = as.integer(nrow(flagged)),
      persons = as.integer(dplyr::n_distinct(flagged$person_id))
    )
  }
  flag_start_obs  <- .countFlag(res, "is_start_outside_observation_period")
  flag_end_obs    <- .countFlag(res, "is_end_outside_observation_period")
  flag_start_gte  <- .countFlag(res, "is_start_gte_end")
  flag_zero       <- .countFlag(res, "is_zero_length")
  flag_too_long   <- .countFlag(res, "is_too_long")
  flag_overlap    <- .countFlag(res, "is_overlapping")

  # Quality check summary CSV (report what was actually done)
  quality <- tibble::tibble(
    n_records_start_geq_end_before_fix = n_start_geq_end_before,
    n_records_start_geq_end_after_fix = n_start_geq_end_after,
    n_records_overlapping = overlap_counts$records,
    n_persons_overlapping = overlap_counts$persons,
    n_records_too_long = too_long_counts$records,
    n_persons_too_long = too_long_counts$persons,
    n_records_after_cleanup = n_after_records,
    n_persons_after_cleanup = n_after_persons,
    # Quality flag counts (from ESD per-record flags)
    n_flagged_start_outside_obs_records = flag_start_obs$records,
    n_flagged_start_outside_obs_persons = flag_start_obs$persons,
    n_flagged_end_outside_obs_records = flag_end_obs$records,
    n_flagged_end_outside_obs_persons = flag_end_obs$persons,
    n_flagged_start_gte_end_records = flag_start_gte$records,
    n_flagged_start_gte_end_persons = flag_start_gte$persons,
    n_flagged_zero_length_records = flag_zero$records,
    n_flagged_zero_length_persons = flag_zero$persons,
    n_flagged_too_long_records = flag_too_long$records,
    n_flagged_too_long_persons = flag_too_long$persons,
    n_flagged_overlapping_records = flag_overlap$records,
    n_flagged_overlapping_persons = flag_overlap$persons,
    max_episode_days = maxDays,
    cdm_name = snap$cdm_name,
    date_run = runStart,
    date_export = snap$snapshot_date,
    pkg_version = pkgVersion
  )
  utils::write.csv(quality, file.path(resPath, "quality_check_cleanup.csv"), row.names = FALSE)

  # Attrition from the single pipeline run (fix does not drop rows; long and overlap steps do)
  # Also includes obs-period and zero-length steps when flag columns exist.
  has_flags <- "is_start_outside_observation_period" %in% names(res)
  if (has_flags) {
    # Build a comprehensive attrition table showing all cleanup steps
    # Obs period and zero-length steps show how many records WOULD be affected
    attrition_if_cleanup <- tibble::tibble(
      step = c(
        "final_episodes_before_cleanup",
        "remove_start_outside_obs_period",
        "remove_end_outside_obs_period",
        "fix_start_before_end",
        "remove_start_gte_end",
        "remove_long_episodes",
        "remove_zero_length_episodes",
        "remove_overlaps"
      ),
      table = rep("final_pregnancy_episodes", 8L),
      outcome = rep(NA_character_, 8L),
      flagged_records = c(
        NA_integer_,
        flag_start_obs$records,
        flag_end_obs$records,
        as.integer(n_fix_corrected),
        flag_start_gte$records,
        flag_too_long$records,
        flag_zero$records,
        flag_overlap$records
      ),
      flagged_persons = c(
        NA_integer_,
        flag_start_obs$persons,
        flag_end_obs$persons,
        NA_integer_,
        flag_start_gte$persons,
        flag_too_long$persons,
        flag_zero$persons,
        flag_overlap$persons
      ),
      prior_records = c(n_before_records, rep(NA_integer_, 7L)),
      prior_persons = c(n_before_persons, rep(NA_integer_, 7L)),
      dropped_records = c(NA_integer_, rep(NA_integer_, 2L), 0L, NA_integer_, dropped_long_records, NA_integer_, dropped_overlap_records),
      dropped_persons = c(NA_integer_, rep(NA_integer_, 2L), 0L, NA_integer_, dropped_long_persons, NA_integer_, dropped_overlap_persons),
      post_records = c(n_before_records, rep(NA_integer_, 2L), n_after_fix_records, NA_integer_, n_after_long_records, NA_integer_, n_after_records),
      post_persons = c(n_before_persons, rep(NA_integer_, 2L), n_after_fix_persons, NA_integer_, n_after_long_persons, NA_integer_, n_after_persons)
    )
  } else {
    attrition_if_cleanup <- tibble::tibble(
      step = c("final_episodes_before_cleanup", "fix_start_before_end", "remove_long_episodes", "remove_overlaps"),
      table = rep("final_pregnancy_episodes", 4L),
      outcome = rep(NA_character_, 4L),
      flagged_records = c(NA_integer_, as.integer(n_fix_corrected), too_long_counts$records, overlap_counts$records),
      flagged_persons = c(NA_integer_, NA_integer_, too_long_counts$persons, overlap_counts$persons),
      prior_records = c(n_before_records, n_before_records, n_after_fix_records, n_after_long_records),
      prior_persons = c(n_before_persons, n_before_persons, n_after_fix_persons, n_after_long_persons),
      dropped_records = c(NA_integer_, 0L, dropped_long_records, dropped_overlap_records),
      dropped_persons = c(NA_integer_, 0L, dropped_long_persons, dropped_overlap_persons),
      post_records = c(n_before_records, n_after_fix_records, n_after_long_records, n_after_records),
      post_persons = c(n_before_persons, n_after_fix_persons, n_after_long_persons, n_after_persons)
    )
  }
  utils::write.csv(attrition_if_cleanup, file.path(resPath, "attrition_if_cleanup.csv"), row.names = FALSE)
}

#' Export number and percentage of missing start and end dates for HIP, PPS, and ESD (final) episodes.
#' @noRd
exportMissingDates <- function(res, resPath, snap, runStart, pkgVersion) {
  n <- nrow(res)
  summariseMissing <- function(col) {
    if (!col %in% names(res)) return(list(n = NA_integer_, pct = NA_real_))
    n_miss <- sum(is.na(res[[col]]))
    list(n = as.integer(n_miss), pct = if (n > 0) round(100 * n_miss / n, 2) else NA_real_)
  }
  hip_start <- summariseMissing("merge_pregnancy_start")
  hip_end <- summariseMissing("hip_end_date")
  pps_start <- summariseMissing("pps_episode_min_date")
  pps_end <- summariseMissing("pps_episode_max_date")
  esd_start <- summariseMissing("final_episode_start_date")
  esd_end <- summariseMissing("final_episode_end_date")

  out <- tibble::tibble(
    hip_start_n = hip_start$n,
    hip_start_pct = hip_start$pct,
    hip_end_n = hip_end$n,
    hip_end_pct = hip_end$pct,
    pps_start_n = pps_start$n,
    pps_start_pct = pps_start$pct,
    pps_end_n = pps_end$n,
    pps_end_pct = pps_end$pct,
    esd_start_n = esd_start$n,
    esd_start_pct = esd_start$pct,
    esd_end_n = esd_end$n,
    esd_end_pct = esd_end$pct,
    cdm_name = snap$cdm_name,
    date_run = runStart,
    date_export = snap$snapshot_date,
    pkg_version = pkgVersion
  )
  utils::write.csv(out, file.path(resPath, "missing_dates.csv"), row.names = FALSE)
}

#' Export swapped (reversed) date counts: HIP, PPS, and ESD (final episode).
#' One row per source (hip, pps, esd) with n_swapped, total, pct, and metadata.
#' @noRd
exportReversedDatesCounts <- function(res, resPath, snap, runStart, pkgVersion) {
  if (nrow(res) == 0) {
    out <- tibble::tibble(
      source = character(0),
      n_swapped = integer(0),
      total = integer(0),
      pct = numeric(0),
      cdm_name = character(0),
      date_run = character(0),
      date_export = character(0),
      pkg_version = character(0)
    )
  } else {
    rev_hip <- as.Date(res$merge_pregnancy_start) > as.Date(res$hip_end_date)
    rev_pps <- as.Date(res$merge_pregnancy_start) > as.Date(res$pps_end_date)
    rev_esd <- as.Date(res$final_episode_start_date) > as.Date(res$final_episode_end_date)
    n_hip <- sum(!is.na(res$merge_pregnancy_start) & !is.na(res$hip_end_date))
    n_pps <- sum(!is.na(res$merge_pregnancy_start) & !is.na(res$pps_end_date))
    n_esd <- sum(!is.na(res$final_episode_start_date) & !is.na(res$final_episode_end_date))
    n_rev_hip <- as.integer(sum(rev_hip, na.rm = TRUE))
    n_rev_pps <- as.integer(sum(rev_pps, na.rm = TRUE))
    n_rev_esd <- as.integer(sum(rev_esd, na.rm = TRUE))
    pct_hip <- if (n_hip > 0) as.numeric(n_rev_hip) / n_hip * 100 else NA_real_
    pct_pps <- if (n_pps > 0) as.numeric(n_rev_pps) / n_pps * 100 else NA_real_
    pct_esd <- if (n_esd > 0) as.numeric(n_rev_esd) / n_esd * 100 else NA_real_
    out <- tibble::tibble(
      source = c("hip", "pps", "esd"),
      n_swapped = c(n_rev_hip, n_rev_pps, n_rev_esd),
      total = c(n_hip, n_pps, n_esd),
      pct = c(pct_hip, pct_pps, pct_esd),
      cdm_name = snap$cdm_name,
      date_run = as.character(runStart),
      date_export = as.character(snap$snapshot_date),
      pkg_version = as.character(pkgVersion)
    )
  }
  utils::write.csv(out, file.path(resPath, "swapped_dates.csv"), row.names = FALSE)
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

exportDeliveryModeSummary <- function(res, resPath, snap, runStart, pkgVersion) {
  deliveryModeSummary <- res %>%
    dplyr::select(c("final_outcome_category", dplyr::starts_with("cesarean"), dplyr::starts_with("vaginal"))) %>%
    dplyr::group_by(.data$final_outcome_category) %>%
    dplyr::summarise(
      n = dplyr::n(),
      cesarean = sum(.data$cesarean_m30_to_30),
      cesarean_count = sum(.data$cesarean_m30_to_30_count),
      vaginal = sum(.data$vaginal_m30_to_30),
      vaginal_count = sum(.data$vaginal_m30_to_30_count)
    ) %>%
    dplyr::mutate(
      n_known = .data$cesarean + .data$vaginal,
      cesarean_pct = dplyr::if_else(.data$n_known > 0, 100 * .data$cesarean / .data$n_known, NA_real_),
      vaginal_pct = dplyr::if_else(.data$n_known > 0, 100 * .data$vaginal / .data$n_known, NA_real_)
    ) %>%
    dplyr::mutate(
      cdm_name = snap$cdm_name,
      date_run = runStart,
      date_export = snap$snapshot_date,
      pkg_version = pkgVersion
    )

  utils::write.csv(deliveryModeSummary, file.path(resPath, "delivery_mode_summary.csv"), row.names = FALSE)
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

