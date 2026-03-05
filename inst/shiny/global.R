# global.R - Libraries, data loading, and feature flags

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(glue)
library(rlang)
library(gt)
library(IncidencePrevalence)
library(CohortCharacteristics)
library(visOmopResults)
library(omopgenerics)
library(scales)
library(amVennDiagram5)

# Load utility functions and module files
invisible(lapply(list.files("utils", full.names = TRUE, pattern = "\\.R$"), source))

############################ Load data ############################

if (!exists("shinySettings")) {
  opt_data <- getOption("shiny.data.folder", "")
  env_data <- Sys.getenv("SHINY_DATA_FOLDER", "")
  if (nzchar(opt_data)) {
    shinySettings <- list(dataFolder = opt_data)
  } else if (nzchar(env_data)) {
    shinySettings <- list(dataFolder = env_data)
  } else {
    dataFolder <- "data"
    if (file.exists(dataFolder)) {
      shinySettings <- list(dataFolder = dataFolder)
    } else {
      shinySettings <- list(dataFolder = paste0("./", dataFolder))
    }
  }
}

dataFolder <- shinySettings$dataFolder
if (!dir.exists(dataFolder) && !grepl("^[/~]", dataFolder)) {
  for (prefix in c("inst/shiny", ".", "..")) {
    candidate <- file.path(prefix, dataFolder)
    if (dir.exists(candidate)) {
      dataFolder <- normalizePath(candidate, mustWork = FALSE)
      break
    }
  }
}
if (dir.exists(dataFolder)) {
  dataFolder <- normalizePath(dataFolder, mustWork = FALSE)
}

# Data sources: zip files first, then folders that contain CSVs
zipFiles <- list.files(dataFolder, pattern = "\\.zip$", full.names = TRUE)
zipFiles <- zipFiles[!is.na(zipFiles) & nzchar(zipFiles)]
if (length(zipFiles) > 0) {
  info <- file.info(zipFiles)
  zipFiles <- zipFiles[file.exists(zipFiles) & !is.na(info$size) & info$size > 0]
}

listCsvFolders <- function(root) {
  if (!dir.exists(root)) return(character(0))
  csvs <- list.files(root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(csvs) == 0) return(character(0))
  out <- unique(dirname(csvs))
  out[order(nchar(out))]
}

subfolders <- character(0)
csvFolders <- listCsvFolders(dataFolder)
if (length(zipFiles) == 0) {
  subfolders <- csvFolders
}

hasData <- length(zipFiles) > 0 || length(subfolders) > 0

if (hasData) {
  # Ensure concept check / precision days / quality check cleanup exist (empty) so loadFile can fill them
  if (!exists("conceptCheck", envir = .GlobalEnv)) {
    assign("conceptCheck", tibble::tibble(cdm_name = character(0)), envir = .GlobalEnv)
  }
  if (!exists("precisionDays", envir = .GlobalEnv)) {
    assign("precisionDays", tibble::tibble(cdm_name = character(0)), envir = .GlobalEnv)
  }
  if (!exists("precisionDaysDenominators", envir = .GlobalEnv)) {
    assign("precisionDaysDenominators", tibble::tibble(cdm_name = character(0)), envir = .GlobalEnv)
  }
  if (!exists("qualityCheckCleanup", envir = .GlobalEnv)) {
    assign("qualityCheckCleanup", tibble::tibble(cdm_name = character(0)), envir = .GlobalEnv)
  }

  if (length(zipFiles) > 0) {
    td <- tempdir()
    if (!dir.exists(td)) dir.create(td, recursive = TRUE, showWarnings = FALSE)
    for (i in seq_along(zipFiles)) {
      writeLines(paste("Processing zip", zipFiles[i]))
      tempFolder <- tempfile(tmpdir = td)
      dir.create(tempFolder, recursive = TRUE, showWarnings = FALSE)
      unzip(zipFiles[i], exdir = tempFolder, junkpaths = TRUE)
      csvFiles <- list.files(tempFolder, pattern = "\\.csv$")
      zipName <- gsub("\\.zip$", "", basename(zipFiles[i]))
      zipNameParts <- unlist(strsplit(zipName, "-"))
      versionOrDBName <- zipNameParts[4]
      if (startsWith(versionOrDBName, "1") || startsWith(versionOrDBName, "2") || startsWith(versionOrDBName, "3")) {
        version <- versionOrDBName
        dbName <- zipNameParts[5]
      } else {
        version <- NULL
        dbName <- versionOrDBName
      }
      runDate <- gsub(paste0("-", dbName, ".*"), "", zipName)
      lapply(csvFiles, loadFile, dbName = dbName, runDate = runDate, zipVersion = version, folder = tempFolder, overwrite = (i == 1))
      unlink(tempFolder, recursive = TRUE)
    }
  } else {
    for (i in seq_along(subfolders)) {
      d <- subfolders[i]
      dbName <- basename(d)

      # Try to get the canonical database name from cdm_source.csv
      cdmSourcePath <- file.path(d, "cdm_source.csv")
      if (file.exists(cdmSourcePath)) {
        .cdmSrc <- tryCatch(
          readr::read_csv(cdmSourcePath, col_types = readr::cols(.default = "c"), show_col_types = FALSE),
          error = function(e) NULL
        )
        if (!is.null(.cdmSrc) && "cdm_name" %in% colnames(.cdmSrc) && nrow(.cdmSrc) > 0) {
          dbName <- .cdmSrc$cdm_name[1]
        }
        rm(.cdmSrc)
      }

      # Try to extract a run date from the folder name (e.g. "IPCI_results_04_03_2026" -> "2026-03-04")
      folderRunDate <- ""
      folderBaseName <- basename(d)
      dateMatch <- regmatches(folderBaseName, regexpr("[0-9]{2}_[0-9]{2}_[0-9]{4}", folderBaseName))
      if (length(dateMatch) > 0 && nzchar(dateMatch)) {
        dateParts <- unlist(strsplit(dateMatch, "_"))
        folderRunDate <- paste0(dateParts[3], "-", dateParts[2], "-", dateParts[1])
      }

      writeLines(paste("Processing database folder", basename(d), "-> dbName:", dbName, "runDate:", folderRunDate))
      csvFiles <- list.files(d, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
      for (csvPath in csvFiles) {
        loadFile(file = basename(csvPath), dbName = dbName, runDate = folderRunDate, zipVersion = NULL, folder = dirname(csvPath), overwrite = (i == 1))
      }
    }
  }

}

############################ Data transformations ############################

if (hasData && exists("cdmSource") && !is.null(cdmSource) && nrow(cdmSource) > 0) {
  dbinfo <- cdmSource %>%
    dplyr::select_if(~ !all(is.na(.))) %>%
    dplyr::arrange(cdm_name)

  allDP <- unique(dbinfo$cdm_name)
  allDP <- allDP[order(allDP)]

  # Ensure tables exist (empty if not loaded)
  if (!exists("gestationalAgeDaysCounts", envir = .GlobalEnv)) {
    assign("gestationalAgeDaysCounts",
           tibble::tibble(cdm_name = character(0), less_1day = character(0), over_308days = character(0)),
           envir = .GlobalEnv)
  }
  if (!exists("gestationalAgeDaysSummary", envir = .GlobalEnv)) {
    assign("gestationalAgeDaysSummary",
           tibble::tibble(colName = character(0), cdm_name = character(0), min = numeric(0), Q25 = numeric(0), median = numeric(0), Q75 = numeric(0), max = numeric(0), mean = numeric(0), sd = numeric(0)),
           envir = .GlobalEnv)
  }
  if (!exists("gestationalAgeDaysPerCategorySummary", envir = .GlobalEnv)) {
    assign("gestationalAgeDaysPerCategorySummary",
           tibble::tibble(cdm_name = character(0), final_outcome_category = character(0), colName = character(0), min = numeric(0), Q25 = numeric(0), median = numeric(0), Q75 = numeric(0), max = numeric(0), mean = numeric(0), sd = numeric(0), person_count = integer(0), episode_count = integer(0)),
           envir = .GlobalEnv)
  }
  if (!exists("gestationalWeeks", envir = .GlobalEnv)) {
    assign("gestationalWeeks",
           tibble::tibble(cdm_name = character(0), final_outcome_category = character(0), gestational_weeks = numeric(0), n = numeric(0), pct = numeric(0)),
           envir = .GlobalEnv)
  }
  if (!exists("outcomeCategoriesCount", envir = .GlobalEnv) || !is.data.frame(get("outcomeCategoriesCount", envir = .GlobalEnv)) || nrow(get("outcomeCategoriesCount", envir = .GlobalEnv)) == 0) {
    assign("outcomeCategoriesCount",
           tibble::tibble(cdm_name = character(0), outcome_category = character(0), algorithm = character(0), n = numeric(0), pct = numeric(0)),
           envir = .GlobalEnv)
  }
  if (!exists("deliveryModeSummary", envir = .GlobalEnv) || !is.data.frame(get("deliveryModeSummary", envir = .GlobalEnv)) || nrow(get("deliveryModeSummary", envir = .GlobalEnv)) == 0) {
    assign("deliveryModeSummary",
           tibble::tibble(cdm_name = character(0), final_outcome_category = character(0), mode = character(0), total = numeric(0), n = numeric(0), pct = numeric(0)),
           envir = .GlobalEnv)
  }
  if (!exists("missingDates", envir = .GlobalEnv) || !is.data.frame(get("missingDates", envir = .GlobalEnv)) || nrow(get("missingDates", envir = .GlobalEnv)) == 0) {
    assign("missingDates",
           tibble::tibble(cdm_name = character(0)),
           envir = .GlobalEnv)
  }

  if (nrow(gestationalAgeDaysCounts) > 0) {
    gestationalAgeDaysCounts <- dataToLong(gestationalAgeDaysCounts)
  } else {
    gestationalAgeDaysCounts <- tibble::tibble(name = character(0))
  }

  # Swapped dates transformation
  if ("source" %in% colnames(swappedDates) && "n_swapped" %in% colnames(swappedDates)) {
    swappedDatesDisplay <- swappedDates %>%
      dplyr::mutate(
        n_swapped = suppressWarnings(as.integer(.data$n_swapped)),
        total = suppressWarnings(as.integer(.data$total)),
        pct = suppressWarnings(as.numeric(.data$pct)),
        metric = dplyr::case_when(
          .data$source == "hip" ~ "HIP reversed (start after end)",
          .data$source == "pps" ~ "PPS reversed (start after end)",
          .data$source == "esd" ~ "ESD (final episode) reversed (start after end)",
          TRUE ~ .data$source
        )
      ) %>%
      dplyr::select("cdm_name", "metric", "n_swapped", "total", "pct") %>%
      dplyr::mutate(pct = round(.data$pct, 2))
  } else {
    swappedDatesLong <- dataToLong(swappedDates)
    swappedDatesDbCols <- setdiff(colnames(swappedDatesLong), "name")
    swappedDatesWide <- swappedDatesLong %>%
      dplyr::mutate(across(!.data$name, ~ suppressWarnings(as.numeric(.))))

    episodeCount <- pregnancyOverlapCounts %>%
      dplyr::mutate(total = as.numeric(.data$total)) %>%
      dplyr::group_by(.data$cdm_name, .data$total) %>%
      dplyr::summarise(total = mean(.data$total), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = "cdm_name", values_from = "total", values_fn = mean) %>%
      dplyr::mutate(name = "total") %>%
      dplyr::select(dplyr::any_of(c("name", swappedDatesDbCols)))
    swappedDatesWide <- rbind(swappedDatesWide, episodeCount)
    percDF <- rbind(
      cbind(name = "rev_hip_perc", round(100 * swappedDatesWide[1, -1] / swappedDatesWide[3, -1], 2)),
      cbind(name = "rev_pps_perc", round(100 * swappedDatesWide[2, -1] / swappedDatesWide[3, -1], 2))
    )
    colnames(percDF) <- colnames(swappedDatesWide)
    swappedDatesWide <- rbind(swappedDatesWide, percDF) %>%
      dplyr::arrange(match(.data$name, c("rev_hip_perc", "rev_pps_perc", "n_rev_hip", "n_rev_pps", "total")))

    swappedDatesDisplay <- swappedDatesWide %>%
      dplyr::filter(.data$name %in% c("n_rev_hip", "n_rev_pps", "total", "rev_hip_perc", "rev_pps_perc")) %>%
      tidyr::pivot_longer(cols = -.data$name, names_to = "cdm_name", values_to = "value") %>%
      dplyr::mutate(value = suppressWarnings(as.numeric(.data$value))) %>%
      tidyr::pivot_wider(names_from = "name", values_from = "value")
    if (nrow(swappedDatesDisplay) > 0) {
      swappedDatesDisplay <- dplyr::bind_rows(
        swappedDatesDisplay %>%
          dplyr::transmute(
            cdm_name = .data$cdm_name,
            metric = "HIP reversed (start after end)",
            n_swapped = as.integer(.data$n_rev_hip),
            total = as.integer(.data$total),
            pct = round(.data$rev_hip_perc, 2)
          ),
        swappedDatesDisplay %>%
          dplyr::transmute(
            cdm_name = .data$cdm_name,
            metric = "PPS reversed (start after end)",
            n_swapped = as.integer(.data$n_rev_pps),
            total = as.integer(.data$total),
            pct = round(.data$rev_pps_perc, 2)
          )
      )
    } else {
      swappedDatesDisplay <- tibble::tibble(cdm_name = character(0), metric = character(0), n_swapped = integer(0), total = integer(0), pct = numeric(0))
    }
  }

  # Age summary
  if (!exists("ageSummary", envir = .GlobalEnv) || is.null(get("ageSummary", envir = .GlobalEnv)) || nrow(get("ageSummary", envir = .GlobalEnv)) == 0) {
    assign("ageSummary",
           tibble::tibble(cdm_name = character(0), colName = character(0)),
           envir = .GlobalEnv)
  }
  ageSummary <- get("ageSummary", envir = .GlobalEnv)
  ageSummaryRaw <- ageSummary

  # Missing dates transformation
  missingDates <- get("missingDates", envir = .GlobalEnv)
  if (nrow(missingDates) == 0) {
    missingDates <- tibble::tibble(
      cdm_name = character(0),
      "Date field" = character(0),
      "N missing" = numeric(0),
      "Percent missing" = numeric(0)
    )
  } else {
    metaCols <- c("cdm_name", "date_run", "date_export", "pkg_version")
    hasNewMissingDatesFormat <- any(grepl("_n$", colnames(missingDates))) && any(grepl("_pct$", colnames(missingDates)))
    if (hasNewMissingDatesFormat) {
      dateCols <- setdiff(colnames(missingDates), metaCols)
      dateCols <- dateCols[grepl("_n$|_pct$", dateCols)]
      missingDates <- missingDates %>%
        dplyr::select(dplyr::all_of(c("cdm_name", dateCols))) %>%
        tidyr::pivot_longer(-.data$cdm_name, names_to = "key", values_to = "value") %>%
        dplyr::mutate(
          value = suppressWarnings(as.numeric(.data$value)),
          name = gsub("_n$|_pct$", "", .data$key),
          metric = ifelse(grepl("_n$", .data$key), "n_missing", "percent_missing")
        ) %>%
        dplyr::select(-.data$key) %>%
        tidyr::pivot_wider(names_from = .data$metric, values_from = .data$value) %>%
        dplyr::mutate(percent_missing = round(.data$percent_missing, 2))
      missingDates <- missingDates %>%
        dplyr::mutate(
          name = dplyr::case_when(
            .data$name == "hip_start" ~ "HIP start",
            .data$name == "hip_end" ~ "HIP end",
            .data$name == "pps_start" ~ "PPS start",
            .data$name == "pps_end" ~ "PPS end",
            .data$name == "esd_start" ~ "ESD (final) start",
            .data$name == "esd_end" ~ "ESD (final) end",
            TRUE ~ .data$name
          )
        )
    } else {
      skipCols <- intersect(metaCols, colnames(missingDates))
      missingDates <- dataToLong(missingDates, skipCols = skipCols) %>%
        tidyr::pivot_longer(-.data$name, names_to = "cdm_name", values_to = "percent_missing") %>%
        dplyr::mutate(
          percent_missing = round(100 * suppressWarnings(as.numeric(.data$percent_missing)), 2),
          n_missing = NA_integer_
        )
    }
    missingDates <- missingDates %>%
      dplyr::rename(
        "Date field" = .data$name,
        "N missing" = .data$n_missing,
        "Percent missing" = .data$percent_missing
      ) %>%
      dplyr::select(dplyr::any_of("cdm_name"), "Date field", "N missing", "Percent missing")
  }

  observationPeriodRange <- dataToLong(observationPeriodRange)

  # Ensure pregnancy frequency and episode frequency exist
  if (!exists("pregnancyFrequency", envir = .GlobalEnv) || !is.data.frame(get("pregnancyFrequency", envir = .GlobalEnv))) {
    assign("pregnancyFrequency", tibble::tibble(freq = integer(0), number_individuals = numeric(0), cdm_name = character(0)), envir = .GlobalEnv)
  }
  if (!exists("episodeFrequencySummary", envir = .GlobalEnv) || !is.data.frame(get("episodeFrequencySummary", envir = .GlobalEnv))) {
    assign("episodeFrequencySummary", tibble::tibble(cdm_name = character(0), colName = character(0)), envir = .GlobalEnv)
  }
  if (!exists("episodeFrequency", envir = .GlobalEnv) || !is.data.frame(get("episodeFrequency", envir = .GlobalEnv))) {
    assign("episodeFrequency", tibble::tibble(cdm_name = character(0)), envir = .GlobalEnv)
  }
  pregnancyFrequency <- get("pregnancyFrequency", envir = .GlobalEnv)
  episodeFrequencySummary <- get("episodeFrequencySummary", envir = .GlobalEnv)
  episodeFrequency <- get("episodeFrequency", envir = .GlobalEnv)

  if (nrow(pregnancyFrequency) > 0 && !"freq" %in% colnames(pregnancyFrequency)) {
    firstCol <- setdiff(colnames(pregnancyFrequency), c("cdm_name", "number_individuals", "date_run", "date_export", "pkg_version"))[1]
    if (!is.na(firstCol)) pregnancyFrequency <- pregnancyFrequency %>% dplyr::rename(freq = !!rlang::sym(firstCol))
  }
  pregnancyFrequency <- pregnancyFrequency %>%
    dplyr::mutate(
      freq = factor(.data$freq, levels = unique(.data$freq)),
      number_individuals = suppressWarnings(as.numeric(.data$number_individuals))
    ) %>%
    dplyr::group_by(.data$cdm_name) %>%
    dplyr::mutate(number_individuals = dplyr::if_else(
      !is.na(.data$number_individuals) & .data$number_individuals > 0 & .data$number_individuals < 5,
      NA_real_,
      .data$number_individuals
    )) %>%
    dplyr::ungroup()

  episodeFrequency <- dataToLong(episodeFrequency %>% dplyr::left_join(episodeFrequencySummary), skipCols = c("cdm_name", "colName"))

  if (nrow(gestationalAgeDaysSummary) > 0) {
    gestationalAgeDaysSummary <- dataToLong(gestationalAgeDaysSummary, skipCols = c("colName", "cdm_name"))
  } else {
    gestationalAgeDaysSummary <- tibble::tibble(name = character(0))
  }
  gestationalAgeDaysPerCategorySummary <- gestationalAgeDaysPerCategorySummary %>%
    dplyr::mutate_at(vars(-(c("cdm_name", "final_outcome_category", "colName"))), as.numeric) %>%
    dplyr::mutate(final_outcome_category = factor(final_outcome_category, levels = c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG")))
  # Ensure total records (episodes) and person count per cdm–outcome exist for table display
  if (!"episode_count" %in% colnames(gestationalAgeDaysPerCategorySummary)) {
    gestationalAgeDaysPerCategorySummary$episode_count <- NA_integer_
  }
  if (!"person_count" %in% colnames(gestationalAgeDaysPerCategorySummary)) {
    gestationalAgeDaysPerCategorySummary$person_count <- NA_integer_
  }
  gestationalAgeDaysPerCategorySummary <- gestationalAgeDaysPerCategorySummary %>%
    dplyr::mutate(
      episode_count = suppressWarnings(as.integer(.data$episode_count)),
      person_count = suppressWarnings(as.integer(.data$person_count))
    )
  # When CSV lacks episode_count/person_count (e.g. old export), fill episode_count from gestational_weeks (sum n per cdm–outcome)
  if (nrow(gestationalAgeDaysPerCategorySummary) > 0 && nrow(gestationalWeeks) > 0 &&
      "final_outcome_category" %in% colnames(gestationalWeeks) &&
      "n" %in% colnames(gestationalWeeks)) {
    episodeFromWeeks <- gestationalWeeks %>%
      dplyr::mutate(n = suppressWarnings(as.numeric(.data$n))) %>%
      dplyr::group_by(.data$cdm_name, .data$final_outcome_category) %>%
      dplyr::summarise(episode_count_weeks = sum(.data$n, na.rm = TRUE), .groups = "drop")
    gestationalAgeDaysPerCategorySummary <- gestationalAgeDaysPerCategorySummary %>%
      dplyr::left_join(episodeFromWeeks, by = c("cdm_name", "final_outcome_category")) %>%
      dplyr::mutate(
        episode_count = dplyr::if_else(
          is.na(.data$episode_count) | .data$episode_count == 0L,
          as.integer(.data$episode_count_weeks),
          .data$episode_count
        )
      ) %>%
      dplyr::select(-dplyr::any_of("episode_count_weeks"))
  }

  minObservationPeriod <- observationPeriodRange %>%
    dplyr::filter(name == "min_obs") %>%
    dplyr::select(-c("name")) %>%
    as.numeric() %>%
    min()

  pregnancyOverlapCounts <- pregnancyOverlapCounts %>%
    dplyr::select(-"colName") %>%
    dplyr::mutate(
      n = suppressWarnings(as.numeric(n)),
      total = suppressWarnings(as.numeric(total)),
      pct = suppressWarnings(as.numeric(pct))
    )

  hasOutcomeInWeeks <- "final_outcome_category" %in% colnames(gestationalWeeks)

  summariseGestationalWeeks <- function(data, lowerBoundary, upperBoundary, label = NULL) {
    if (is.null(label)) {
      label <- glue::glue("{lowerBoundary}-{upperBoundary}")
    }
    out <- data %>%
      dplyr::filter(gestational_weeks >= lowerBoundary & gestational_weeks < upperBoundary) %>%
      dplyr::select(-"gestational_weeks")
    if (hasOutcomeInWeeks) {
      out <- out %>%
        dplyr::group_by(cdm_name, final_outcome_category) %>%
        dplyr::summarise(n = sum(n), .groups = "drop") %>%
        dplyr::group_by(cdm_name) %>%
        dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
        dplyr::ungroup()
    } else {
      out <- out %>%
        dplyr::group_by(cdm_name) %>%
        dplyr::summarise(n = sum(n), pct = sum(pct), .groups = "drop")
    }
    out %>%
      dplyr::mutate(gestational_weeks = label, .after = if (hasOutcomeInWeeks) "final_outcome_category" else "cdm_name")
  }

  gestationalWeeks <- gestationalWeeks %>%
    dplyr::mutate(
      gestational_weeks = suppressWarnings(as.numeric(gestational_weeks)),
      n = suppressWarnings(as.numeric(n)),
      pct = suppressWarnings(as.numeric(pct))
    )
  maxWeeks <- if (nrow(gestationalWeeks) > 0) max(50, ceiling(max(gestationalWeeks$gestational_weeks, na.rm = TRUE))) else 50

  gestationalWeeksSummary <- gestationalWeeks %>%
    dplyr::mutate(pct = round(pct, 1))

  gestationalWeeksBinned <- rbind(
    summariseGestationalWeeks(gestationalWeeks, 0, 12, "<12"),
    summariseGestationalWeeks(gestationalWeeks, 12, 28),
    summariseGestationalWeeks(gestationalWeeks, 28, 32),
    summariseGestationalWeeks(gestationalWeeks, 32, 37),
    summariseGestationalWeeks(gestationalWeeks, 37, 42),
    summariseGestationalWeeks(gestationalWeeks, 42, maxWeeks, ">42")
  )

  gestationalWeeksBinned <- gestationalWeeksBinned %>%
    dplyr::mutate(
      gestational_weeks = factor(gestational_weeks, levels = c("<12", "12-28", "28-32", "32-37", "37-42", ">42")),
      pct = round(pct, 1)
    )

  # Trend data
  yearlyTrend <- yearlyTrend %>%
    dplyr::mutate(
      count = as.numeric(count),
      year = as.numeric(year),
      period = "year"
    ) %>%
    dplyr::rename(value = year)

  yearlyTrendMissing <- yearlyTrendMissing %>%
    dplyr::mutate(
      count = as.numeric(count),
      year = as.numeric(year),
      period = "year"
    ) %>%
    dplyr::rename(value = year)

  monthlyTrends <- monthlyTrends %>%
    dplyr::mutate(
      count = as.numeric(count),
      period = "month"
    ) %>%
    dplyr::rename(value = month)
  monthlyTrendMissing <- monthlyTrendMissing %>%
    dplyr::mutate(
      count = as.numeric(count),
      period = "month"
    ) %>%
    dplyr::rename(value = month)

  trendData <- rbind(yearlyTrend, monthlyTrends)
  trendDataMissing <- rbind(yearlyTrendMissing, monthlyTrendMissing)

  # Fallback: if outcome/delivery still empty, try reading from zip files
  .oc <- if (exists("outcomeCategoriesCount", envir = .GlobalEnv)) get("outcomeCategoriesCount", envir = .GlobalEnv) else tibble::tibble()
  .dm <- if (exists("deliveryModeSummary", envir = .GlobalEnv)) get("deliveryModeSummary", envir = .GlobalEnv) else tibble::tibble()
  if ((!is.data.frame(.oc) || nrow(.oc) == 0) || (!is.data.frame(.dm) || nrow(.dm) == 0)) {
    zipFilesForOutcome <- list.files(dataFolder, pattern = "\\.zip$", full.names = TRUE)
    zipFilesForOutcome <- zipFilesForOutcome[file.exists(zipFilesForOutcome)]
    for (zf in zipFilesForOutcome) {
      contents <- utils::unzip(zf, list = TRUE)$Name
      if (!is.character(contents)) next
      baseNames <- basename(contents)
      if ("outcome_categories_count.csv" %in% baseNames && (!is.data.frame(.oc) || nrow(.oc) == 0)) {
        idx <- which(baseNames == "outcome_categories_count.csv")[1L]
        conn <- utils::unz(zf, contents[idx], open = "rb")
        x <- tryCatch(
          readr::read_csv(conn, col_types = readr::cols(.default = "c"), guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE),
          error = function(e) NULL,
          finally = close(conn)
        )
        if (!is.null(x) && nrow(x) >= 0) {
          colnames(x) <- gsub("^['\"]*|['\"]*$", "", colnames(x))
          if ("...1" %in% colnames(x)) x <- x %>% dplyr::select(-dplyr::any_of("...1"))
          if (!"cdm_name" %in% colnames(x)) x <- x %>% dplyr::mutate(cdm_name = "export")
          if (!exists("outcomeCategoriesCount", envir = .GlobalEnv) || nrow(get("outcomeCategoriesCount", envir = .GlobalEnv)) == 0) {
            assign("outcomeCategoriesCount", x, envir = .GlobalEnv)
          } else {
            assign("outcomeCategoriesCount", dplyr::bind_rows(get("outcomeCategoriesCount", envir = .GlobalEnv), x), envir = .GlobalEnv)
          }
          .oc <- get("outcomeCategoriesCount", envir = .GlobalEnv)
        }
      }
      if ("delivery_mode_summary.csv" %in% baseNames && (!is.data.frame(.dm) || nrow(.dm) == 0)) {
        idx <- which(baseNames == "delivery_mode_summary.csv")[1L]
        conn <- utils::unz(zf, contents[idx], open = "rb")
        x <- tryCatch(
          readr::read_csv(conn, col_types = readr::cols(.default = "c"), guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE),
          error = function(e) NULL,
          finally = close(conn)
        )
        if (!is.null(x) && nrow(x) >= 0) {
          colnames(x) <- gsub("^['\"]*|['\"]*$", "", colnames(x))
          if ("...1" %in% colnames(x)) x <- x %>% dplyr::select(-dplyr::any_of("...1"))
          if (!"cdm_name" %in% colnames(x)) x <- x %>% dplyr::mutate(cdm_name = "export")
          if (!exists("deliveryModeSummary", envir = .GlobalEnv) || nrow(get("deliveryModeSummary", envir = .GlobalEnv)) == 0) {
            assign("deliveryModeSummary", x, envir = .GlobalEnv)
          } else {
            assign("deliveryModeSummary", dplyr::bind_rows(get("deliveryModeSummary", envir = .GlobalEnv), x), envir = .GlobalEnv)
          }
          .dm <- get("deliveryModeSummary", envir = .GlobalEnv)
        }
      }
    }
  }

  # Transform outcome categories (already loaded by main loop)
  outcomeCategoriesCount <- get("outcomeCategoriesCount", envir = .GlobalEnv)
  if (nrow(outcomeCategoriesCount) > 0) {
    if ("n" %in% colnames(outcomeCategoriesCount)) outcomeCategoriesCount <- outcomeCategoriesCount %>% dplyr::mutate(n = suppressWarnings(as.numeric(.data$n)))
    if ("pct" %in% colnames(outcomeCategoriesCount)) outcomeCategoriesCount <- outcomeCategoriesCount %>% dplyr::mutate(pct = round(suppressWarnings(as.numeric(.data$pct)), 4))
    if ("outcome_category" %in% colnames(outcomeCategoriesCount)) {
      outcomeCategoriesCount <- outcomeCategoriesCount %>% dplyr::mutate(outcome_category = factor(.data$outcome_category, levels = c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG", "NA")))
    }
  }

  # Transform delivery mode (already loaded by main loop)
  deliveryModeSummary <- get("deliveryModeSummary", envir = .GlobalEnv)
  deliveryModeRequired <- c("final_outcome_category", "n", "cesarean", "vaginal", "cesarean_pct", "vaginal_pct")
  if (nrow(deliveryModeSummary) > 0 && all(deliveryModeRequired %in% colnames(deliveryModeSummary))) {
    deliveryModeSummary <- deliveryModeSummary %>%
      dplyr::filter(final_outcome_category %in% c("DELIV", "LB")) %>%
      dplyr::select(-dplyr::any_of(c("cesarean_count", "vaginal_count"))) %>%
      dplyr::rename(total = n)
    if ("n_known" %in% colnames(deliveryModeSummary)) {
      deliveryModeSummary <- deliveryModeSummary %>% dplyr::mutate(n_known = suppressWarnings(as.numeric(.data$n_known)))
    } else {
      deliveryModeSummary <- deliveryModeSummary %>% dplyr::mutate(n_known = .data$cesarean + .data$vaginal)
    }
    deliveryModeSummary <- dplyr::left_join(
      deliveryModeSummary %>%
        tidyr::pivot_longer(cols = c("cesarean", "vaginal"), names_to = "mode", values_to = "n"),
      deliveryModeSummary %>%
        dplyr::select(-dplyr::any_of(c("cesarean", "vaginal"))) %>%
        dplyr::rename(
          vaginal = vaginal_pct,
          cesarean = cesarean_pct
        ) %>%
        tidyr::pivot_longer(cols = c("cesarean", "vaginal"), names_to = "mode", values_to = "pct")
    ) %>%
      dplyr::mutate_at(vars(-(c("cdm_name", "final_outcome_category", "mode"))), ~ suppressWarnings(as.numeric(.))) %>%
      dplyr::mutate(pct = round(pct, 2)) %>%
      dplyr::select(c("cdm_name", "final_outcome_category", "mode", "total", "n_known", "n", "pct"))
  }

  # Get incidence/prevalence/characteristics (already proper summarised_result objects)
  if (exists("incidence", envir = .GlobalEnv)) {
    incidence <- get("incidence", envir = .GlobalEnv)
  }
  if (exists("prevalence", envir = .GlobalEnv)) {
    prevalence <- get("prevalence", envir = .GlobalEnv)
  }
  if (exists("characteristics", envir = .GlobalEnv)) {
    characteristics <- get("characteristics", envir = .GlobalEnv)
  }

  # Quality check cleanup
  qualityCheckCleanup <- get("qualityCheckCleanup", envir = .GlobalEnv)
  if (nrow(qualityCheckCleanup) > 0 && "cdm_name" %in% colnames(qualityCheckCleanup)) {
    qualityCheckCleanup <- qualityCheckCleanup %>% dplyr::select("cdm_name", dplyr::everything())
  }

  # Precision days
  precisionDays <- get("precisionDays", envir = .GlobalEnv)
  if (exists("precisionDaysDenominators", envir = .GlobalEnv)) {
    precisionDaysDenominators <- get("precisionDaysDenominators", envir = .GlobalEnv)
  } else {
    precisionDaysDenominators <- tibble::tibble(cdm_name = character(0))
  }

  # Concept check
  conceptCheck <- get("conceptCheck", envir = .GlobalEnv)

  # Attrition
  if (exists("attrition_episodes", envir = .GlobalEnv)) {
    attritionEpisodes <- get("attrition_episodes", envir = .GlobalEnv)
  }
  if (exists("attritionIfCleanup", envir = .GlobalEnv)) {
    attritionIfCleanup <- get("attritionIfCleanup", envir = .GlobalEnv)
  }

  # Age data
  if (exists("ageSummaryFirstPregnancy", envir = .GlobalEnv)) {
    ageSummaryFirstPregnancy <- get("ageSummaryFirstPregnancy", envir = .GlobalEnv)
  }
  if (exists("ageSummaryGroups", envir = .GlobalEnv)) {
    ageSummaryGroups <- get("ageSummaryGroups", envir = .GlobalEnv)
  }

  # PET comparison
  if (exists("petComparisonSummarisedResult", envir = .GlobalEnv)) {
    petComparisonSummarisedResult <- get("petComparisonSummarisedResult", envir = .GlobalEnv)
  }

  # Concept counts
  if (exists("esdConceptCounts", envir = .GlobalEnv)) {
    esdConceptCounts <- get("esdConceptCounts", envir = .GlobalEnv)
  }
  if (exists("hipConceptCounts", envir = .GlobalEnv)) {
    hipConceptCounts <- get("hipConceptCounts", envir = .GlobalEnv)
  }
  if (exists("ppsConceptCounts", envir = .GlobalEnv)) {
    ppsConceptCounts <- get("ppsConceptCounts", envir = .GlobalEnv)
  }

  ############################ Feature flags ############################

  defaultPlotHeight <- "400px"
  plotHeight <- "600px"

  has_incidence <- exists("incidence") && is.data.frame(incidence) && nrow(incidence) > 0
  has_prevalence <- exists("prevalence") && is.data.frame(prevalence) && nrow(prevalence) > 0
  has_characteristics <- exists("characteristics") && is.data.frame(characteristics) && nrow(characteristics) > 0
  has_attrition <- exists("attritionEpisodes") && is.data.frame(attritionEpisodes) && nrow(attritionEpisodes) > 0
  has_attrition_cleanup <- exists("attritionIfCleanup") && is.data.frame(attritionIfCleanup) && nrow(attritionIfCleanup) > 0
  has_esd_concepts <- exists("esdConceptCounts") && is.data.frame(esdConceptCounts) && nrow(esdConceptCounts) > 0
  has_hip_concepts <- exists("hipConceptCounts") && is.data.frame(hipConceptCounts) && nrow(hipConceptCounts) > 0
  has_pps_concepts <- exists("ppsConceptCounts") && is.data.frame(ppsConceptCounts) && nrow(ppsConceptCounts) > 0
  has_concept_counts <- has_esd_concepts || has_hip_concepts || has_pps_concepts
  has_age_summary <- nrow(ageSummaryRaw) > 0
  has_age_first_pregnancy <- exists("ageSummaryFirstPregnancy") && is.data.frame(ageSummaryFirstPregnancy) && nrow(ageSummaryFirstPregnancy) > 0
  has_age_groups <- exists("ageSummaryGroups") && is.data.frame(ageSummaryGroups) && nrow(ageSummaryGroups) > 0
  has_age <- has_age_summary || has_age_first_pregnancy || has_age_groups
  has_pet_comparison_sr <- exists("petComparisonSummarisedResult") && !is.null(petComparisonSummarisedResult) && nrow(petComparisonSummarisedResult) > 0

  # Legacy PET comparison tables
  petComparisonSpec <- list(
    petComparisonEpisodeCounts = "Episode counts",
    petComparisonPersonOverlap = "Person overlap",
    petComparisonVennCounts = "Venn counts",
    petComparisonProtocolSummary = "Protocol summary",
    petComparisonTimeOverlapSummary = "Time overlap summary",
    petComparisonConfusion2x2 = "Confusion 2x2",
    petComparisonPpvSensitivity = "PPV and sensitivity",
    petComparisonDateDifferenceSummary = "Date difference summary",
    petComparisonDateDifferences = "Date differences",
    petComparisonOutcomeConfusionMatrix = "Outcome confusion matrix",
    petComparisonOutcomeAccuracy = "Outcome accuracy",
    petComparisonOutcomeByYear = "Outcome by year",
    petComparisonDurationSummary = "Duration summary",
    petComparisonDurationMatchedSummary = "Duration matched summary",
    petComparisonDurationDistribution = "Duration distribution"
  )
  has_pet_legacy <- !has_pet_comparison_sr && any(vapply(names(petComparisonSpec), function(v) {
    exists(v, envir = .GlobalEnv) && is.data.frame(get(v, envir = .GlobalEnv)) && nrow(get(v, envir = .GlobalEnv)) > 0
  }, logical(1)))
  has_pet <- has_pet_comparison_sr || has_pet_legacy
  has_ip <- has_incidence || has_prevalence
}
