library(shiny)
library(DarwinShinyModules)
library(IncidencePrevalence)
library(ggplot2)
library(visOmopResults)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(amVennDiagram5)
# cleanup environment variables

rm(list = ls())

############################ Load modules ############################

sapply(list.files("utils", full.names = TRUE), source)

############################ Load data ############################

if (!exists("shinySettings")) {
  # Allow tests to force a data folder via option or env var (e.g. shinytest2)
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

# Data sources: zip files first, then folders that contain CSVs (recursive under data folder)
zipFiles <- list.files(dataFolder, pattern = "\\.zip$", full.names = TRUE)
zipFiles <- zipFiles[!is.na(zipFiles) & nzchar(zipFiles)]
if (length(zipFiles) > 0) {
  info <- file.info(zipFiles)
  zipFiles <- zipFiles[file.exists(zipFiles) & !is.na(info$size) & info$size > 0]
}

# All directories under dataFolder that contain at least one CSV (recursive).
# Handles: dataFolder with CSVs (one DB), or dataFolder/db1, dataFolder/db2, or dataFolder/exports/db1, etc.
listCsvFolders <- function(root) {
  if (!dir.exists(root)) return(character(0))
  csvs <- list.files(root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(csvs) == 0) return(character(0))
  out <- unique(dirname(csvs))
  out[order(nchar(out))]  # parent folders first (e.g. dataFolder before dataFolder/db1)
}

subfolders <- character(0)
csvFolders <- listCsvFolders(dataFolder)  # recursive list of all folders that contain CSVs
if (length(zipFiles) == 0) {
  subfolders <- csvFolders
}

hasData <- length(zipFiles) > 0 || length(subfolders) > 0

if (!hasData) {
  ui <- fluidPage(
    titlePanel("PregnancyIdentifier"),
    mainPanel(
      shiny::p("No data available. Add export zip files or one subfolder per database (with CSV files) inside the data folder to view results.")
    )
  )
  server <- function(input, output, session) {}
  shiny::shinyApp(ui = ui, server = server)
} else {
  # Ensure concept check / precision days / quality check cleanup exist (empty) so loadFile can fill them and tabs always appear
  if (!exists("conceptCheck", envir = .GlobalEnv)) {
    assign("conceptCheck", tibble::tibble(cdm_name = character(0)), envir = .GlobalEnv)
  }
  if (!exists("precisionDays", envir = .GlobalEnv)) {
    assign("precisionDays", tibble::tibble(cdm_name = character(0)), envir = .GlobalEnv)
  }
  if (!exists("qualityCheckCleanup", envir = .GlobalEnv)) {
    assign("qualityCheckCleanup", tibble::tibble(cdm_name = character(0)), envir = .GlobalEnv)
  }

  if (length(zipFiles) > 0) {
    for (i in seq_along(zipFiles)) {
      writeLines(paste("Processing zip", zipFiles[i]))
      tempFolder <- tempfile()
      dir.create(tempFolder)
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
      writeLines(paste("Processing database folder", dbName))
      csvFiles <- list.files(d, pattern = "\\.csv$")
      lapply(csvFiles, loadFile, dbName = dbName, runDate = "", zipVersion = NULL, folder = d, overwrite = (i == 1))
    }
  }

  # Ensure age-related CSVs are loaded from data folder root when present (e.g. single CSV export)
  ageCsvs <- c("age_summary.csv", "age_summary_first_pregnancy.csv", "age_summary_groups.csv")
  for (ageFile in ageCsvs) {
    agePath <- file.path(dataFolder, ageFile)
    if (file.exists(agePath)) {
      tableName <- gsub("\\.csv$", "", ageFile)
      camelCaseName <- snakeCaseToCamelCase(tableName)
      if (!exists(camelCaseName, envir = .GlobalEnv) || nrow(get(camelCaseName, envir = .GlobalEnv)) == 0) {
        loadFile(file = ageFile, dbName = basename(dataFolder), runDate = "", zipVersion = NULL, folder = dataFolder, overwrite = TRUE)
      }
    }
  }

  # Ensure pregnancy_frequency and episode_frequency are loaded from data folder root when present
  freqCsvs <- c("pregnancy_frequency.csv", "episode_frequency.csv", "episode_frequency_summary.csv")
  for (freqFile in freqCsvs) {
    freqPath <- file.path(dataFolder, freqFile)
    if (file.exists(freqPath)) {
      tableName <- gsub("\\.csv$", "", freqFile)
      camelCaseName <- snakeCaseToCamelCase(tableName)
      if (!exists(camelCaseName, envir = .GlobalEnv) || nrow(get(camelCaseName, envir = .GlobalEnv)) == 0) {
        loadFile(file = freqFile, dbName = basename(dataFolder), runDate = "", zipVersion = NULL, folder = dataFolder, overwrite = TRUE)
      }
    }
  }

  # Load concept_check, precision_days, quality_check_cleanup from data folder root when present and still empty (same as age CSVs)
  qaCsvs <- c("concept_check.csv", "precision_days.csv", "quality_check_cleanup.csv")
  qaVars <- c("conceptCheck", "precisionDays", "qualityCheckCleanup")
  for (j in seq_along(qaCsvs)) {
    path <- file.path(dataFolder, qaCsvs[j])
    if (file.exists(path)) {
      v <- qaVars[j]
      if (!exists(v, envir = .GlobalEnv) || nrow(get(v, envir = .GlobalEnv)) == 0) {
        loadFile(file = qaCsvs[j], dbName = basename(dataFolder), runDate = "", zipVersion = NULL, folder = dataFolder, overwrite = TRUE)
      }
    }
  }
  # Load from all CSV-containing folders (recursive) so QA tabs get data
  for (j in seq_along(qaCsvs)) {
    firstLoad <- TRUE
    for (d in csvFolders) {
      f <- qaCsvs[j]
      path <- file.path(d, f)
      if (file.exists(path)) {
        loadFile(file = f, dbName = basename(d), runDate = "", zipVersion = NULL, folder = d, overwrite = firstLoad)
        firstLoad <- FALSE
      }
    }
  }

  # Load concept count CSVs from root and subfolders (same pattern as QA) so DT tables get data
  conceptCountCsvs <- c("esd_concept_counts.csv", "hip_concept_counts.csv", "pps_concept_counts.csv")
  for (ccFile in conceptCountCsvs) {
    pathRoot <- file.path(dataFolder, ccFile)
    if (file.exists(pathRoot)) {
      loadFile(file = ccFile, dbName = basename(dataFolder), runDate = "", zipVersion = NULL, folder = dataFolder, overwrite = TRUE)
    }
  }
  for (ccFile in conceptCountCsvs) {
    firstLoad <- TRUE
    for (d in csvFolders) {
      path <- file.path(d, ccFile)
      if (file.exists(path)) {
        loadFile(file = ccFile, dbName = basename(d), runDate = "", zipVersion = NULL, folder = d, overwrite = firstLoad)
        firstLoad <- FALSE
      }
    }
  }

  # Load PET comparison: single SummarisedResult CSV (pet_comparison_summarised_result.csv) or legacy many CSVs
  petCsvsRoot <- list.files(dataFolder, pattern = "^pet_comparison_.*\\.csv$")
  for (petFile in petCsvsRoot) {
    loadFile(file = petFile, dbName = basename(dataFolder), runDate = "", zipVersion = NULL, folder = dataFolder, overwrite = TRUE)
  }
  for (d in csvFolders) {
    petCsvsSub <- list.files(d, pattern = "^pet_comparison_.*\\.csv$", full.names = FALSE)
    firstLoad <- TRUE
    for (petFile in petCsvsSub) {
      loadFile(file = petFile, dbName = basename(d), runDate = "", zipVersion = NULL, folder = d, overwrite = firstLoad)
      firstLoad <- FALSE
    }
  }

  # Formatting (requires cdmSource from at least one source)
  if (!exists("cdmSource") || is.null(cdmSource) || nrow(cdmSource) == 0) {
    ui <- fluidPage(
      titlePanel("PregnancyIdentifier"),
      mainPanel(
        shiny::p("No valid database metadata (cdm_source) found in the data folder.")
      )
    )
    server <- function(input, output, session) {}
    shiny::shinyApp(ui = ui, server = server)
  } else {
    dbinfo <- cdmSource %>%
      dplyr::select_if(~ !all(is.na(.))) %>%
      dplyr::arrange(cdm_name)

    allDP <- unique(dbinfo$cdm_name)
    allDP <- allDP[order(allDP)]

    # Ensure gestational tables exist (empty if not loaded from CSV) so app does not crash
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
             tibble::tibble(cdm_name = character(0), final_outcome_category = character(0), colName = character(0), min = numeric(0), Q25 = numeric(0), median = numeric(0), Q75 = numeric(0), max = numeric(0), mean = numeric(0), sd = numeric(0)),
             envir = .GlobalEnv)
    }
    if (!exists("gestationalWeeks", envir = .GlobalEnv)) {
      assign("gestationalWeeks",
             tibble::tibble(cdm_name = character(0), final_outcome_category = character(0), gestational_weeks = numeric(0), n = numeric(0), pct = numeric(0)),
             envir = .GlobalEnv)
    }

    if (nrow(gestationalAgeDaysCounts) > 0) {
      gestationalAgeDaysCounts <- dataToLong(gestationalAgeDaysCounts)
    } else {
      gestationalAgeDaysCounts <- tibble::tibble(name = character(0))
    }

    swappedDatesLong <- dataToLong(swappedDates)
    swappedDatesDbCols <- setdiff(colnames(swappedDatesLong), "name")
    swappedDates <- swappedDatesLong %>%
      dplyr::mutate(across(!name, ~ suppressWarnings(as.numeric(.))))

    episodeCount <- pregnancyOverlapCounts %>%
      dplyr::mutate(total = as.numeric(total)) %>%
      dplyr::group_by(cdm_name, total) %>%
      dplyr::summarise(total = mean(total), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = "cdm_name", values_from = "total", values_fn = mean) %>%
      dplyr::mutate(name = "total") %>%
      dplyr::select(dplyr::any_of(c("name", swappedDatesDbCols)))
    swappedDates <- rbind(swappedDates, episodeCount)
    percDF <- rbind(
      cbind(name = "rev_hip_perc", round(100 * swappedDates[1, -1] / swappedDates[3, -1], 2)),
      cbind(name = "rev_pps_perc", round(100 * swappedDates[2, -1] / swappedDates[3, -1], 2))
    )
    colnames(percDF) <- colnames(swappedDates)
    swappedDates <- rbind(swappedDates, percDF) %>%
      dplyr::arrange(match(name, c("rev_hip_perc", "rev_pps_perc", "n_rev_hip", "n_rev_pps", "total")))

    # Swapped dates display: one row per metric per database with n and pct in separate columns
    swappedDatesDisplay <- swappedDates %>%
      dplyr::filter(.data$name %in% c("n_rev_hip", "n_rev_pps", "total", "rev_hip_perc", "rev_pps_perc")) %>%
      tidyr::pivot_longer(cols = -"name", names_to = "cdm_name", values_to = "value") %>%
      dplyr::mutate(value = suppressWarnings(as.numeric(.data$value))) %>%
      tidyr::pivot_wider(names_from = "name", values_from = "value")
    if (nrow(swappedDatesDisplay) > 0) {
      swappedDatesDisplay <- dplyr::bind_rows(
        swappedDatesDisplay %>%
          dplyr::transmute(
            cdm_name,
            metric = "HIP reversed (start after end)",
            n = .data$n_rev_hip,
            pct = .data$rev_hip_perc
          ),
        swappedDatesDisplay %>%
          dplyr::transmute(
            cdm_name,
            metric = "PPS reversed (start after end)",
            n = .data$n_rev_pps,
            pct = .data$rev_pps_perc
          ),
        swappedDatesDisplay %>%
          dplyr::transmute(
            cdm_name,
            metric = "Total episodes",
            n = .data$total,
            pct = NA_real_
          )
      ) %>%
        dplyr::mutate(pct = round(.data$pct, 2))
    } else {
      swappedDatesDisplay <- tibble::tibble(cdm_name = character(0), metric = character(0), n = numeric(0), pct = numeric(0))
    }

    # Ensure ageSummary exists (from age_summary.csv) so Age tab (Age summary) does not crash
    if (!exists("ageSummary", envir = .GlobalEnv) || is.null(get("ageSummary", envir = .GlobalEnv)) || nrow(get("ageSummary", envir = .GlobalEnv)) == 0) {
      assign("ageSummary",
             tibble::tibble(cdm_name = character(0), colName = character(0)),
             envir = .GlobalEnv)
    }
    ageSummary <- get("ageSummary", envir = .GlobalEnv)
    ageSummaryRaw <- ageSummary  # keep raw copy for Age tab DT table
    if (nrow(ageSummary) > 0) {
      # Long format for Age summary tab: cdm_name, identifier cols, name (metric), value
      skipCols <- intersect(c("cdm_name", "colName"), colnames(ageSummary))
      ageSummary <- ageSummary %>%
        tidyr::pivot_longer(cols = setdiff(colnames(.), skipCols), names_to = "name", values_to = "value")
    } else {
      ageSummary <- tibble::tibble(cdm_name = character(0), name = character(0), value = character(0))
    }
    # Date consistency: table with N missing and Percent missing by date field (support old and new export format)
    metaCols <- c("cdm_name", "date_run", "date_export", "pkg_version")
    hasNewDateConsistencyFormat <- any(grepl("_n$", colnames(dateConsistency))) && any(grepl("_pct$", colnames(dateConsistency)))
    if (hasNewDateConsistencyFormat) {
      dateCols <- setdiff(colnames(dateConsistency), metaCols)
      dateCols <- dateCols[grepl("_n$|_pct$", dateCols)]
      dateConsistency <- dateConsistency %>%
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
    } else {
      skipCols <- intersect(metaCols, colnames(dateConsistency))
      dateConsistency <- dataToLong(dateConsistency, skipCols = skipCols) %>%
        tidyr::pivot_longer(-.data$name, names_to = "cdm_name", values_to = "percent_missing") %>%
        dplyr::mutate(
          percent_missing = round(100 * suppressWarnings(as.numeric(.data$percent_missing)), 2),
          n_missing = NA_integer_
        )
    }
    dateConsistency <- dateConsistency %>%
      dplyr::rename(
        "Date field" = .data$name,
        "N missing" = .data$n_missing,
        "Percent missing" = .data$percent_missing
      ) %>%
      dplyr::select(dplyr::any_of("cdm_name"), "Date field", "N missing", "Percent missing")

    observationPeriodRange <- dataToLong(observationPeriodRange)

    # Ensure pregnancy frequency and episode frequency exist (empty placeholder if never loaded)
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

    # Keep long format (freq, cdm_name, number_individuals) so pregnancy frequency is faceted by cdm_name
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

    # trend data
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

    # outcome categories
    outcomeCategoriesCount <- outcomeCategoriesCount %>%
      dplyr::mutate(
        n = suppressWarnings(as.numeric(n)),
        pct = round(suppressWarnings(as.numeric(pct)), 4)
      ) %>%
      dplyr::mutate(outcome_category = factor(outcome_category, levels = c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG")))

    # delivery mode
    deliveryModeSummary <- deliveryModeSummary %>%
      dplyr::filter(final_outcome_category %in% c("DELIV", "LB")) %>%
      dplyr::select(-c("cesarean_count", "vaginal_count")) %>%
      dplyr::rename(total = n)

    deliveryModeSummary <- dplyr::left_join(
      deliveryModeSummary %>%
        tidyr::pivot_longer(cols = c("cesarean", "vaginal"), names_to = "mode", values_to = "n"),
      deliveryModeSummary %>%
        dplyr::select(-c("cesarean", "vaginal")) %>%
        dplyr::rename(
          vaginal = vaginal_pct,
          cesarean = cesarean_pct
        ) %>%
        tidyr::pivot_longer(cols = c("cesarean", "vaginal"), names_to = "mode", values_to = "pct")
    ) %>%
      dplyr::mutate_at(vars(-(c("cdm_name", "final_outcome_category", "mode"))), ~ suppressWarnings(as.numeric(.))) %>%
      dplyr::mutate(pct = round(pct, 2)) %>%
      dplyr::select(c("cdm_name", "final_outcome_category", "mode", "total", "n", "pct"))

    ############################ Shiny app ############################
    allDP <- unique(dbinfo$cdm_name)
    allDP <- allDP[order(allDP)]
    defaultPlotHeight <- "400px"
    plotHeight <- "600px"

    # Episode frequency: include "Incidence rate" only when incidence data exists
    episodeFreqItems <- list(
      "Episode frequency" = tabWithHelpText(
        handleEmptyResult(object = EpisodeFrequencyModule$new(data = episodeFrequency, dp = allDP), result = episodeFrequency),
        "Total pregnancy episodes and total individuals. Used as the main denominator for rates and site-level summaries."
      ),
      "Pregnancy frequency" = tabWithHelpText(
        handleEmptyResult(object = PregnancyFrequencyModule$new(data = pregnancyFrequency, dp = allDP), result = pregnancyFrequency),
        "Distribution of pregnancy count per person (parity-like). Used to describe repeat pregnancies and check for implausible multiplicity."
      )
    )
    if (exists("incidence", envir = .GlobalEnv)) {
      incidence <- get("incidence", envir = .GlobalEnv)
      episodeFreqItems <- c(
        episodeFreqItems[1],
        list("Incidence rate" = tabWithHelpText(
          handleEmptyResult(object = IncidencePlot$new(data = incidence), result = incidence),
          "Incidence estimates over time. Used for temporal trends and rate comparisons across databases."
        )),
        episodeFreqItems[2]
      )
    }

    appStructure <- list(
      "Study background" = list(StudyBackground$new(
        background = "./background.md",
        EUPAS = "EUPAS"
      )),
      "Database information" = list(tabWithHelpText(
        handleEmptyResult(object = FilterTableModule$new(dbinfo), result = dbinfo),
        "CDM and snapshot metadata. Used to track provenance and identify the source database when comparing or aggregating exports."
      )),
      "Episode frequency" = episodeFreqItems,
      "Episode duration" = list(
        "Gestational age weeks" = tabWithHelpText(
          GestationalAgeModule$new(data = gestationalWeeksSummary, daysData = gestationalAgeDaysCounts, height = "420px"),
          "Distribution of gestational age by week. Used for gestational-age histograms, preterm/term summaries, and cross-site comparison."
        ),
        "Gestational age binned" = tabWithHelpText(
          GestationalAgeModule$new(data = gestationalWeeksBinned, maxWeeksFilter = FALSE, height = "420px"),
          "Gestational age grouped into bands (e.g. &lt;12, 12–28, 28–32 weeks). Used for outcome bands and cross-site comparison."
        ),
        "Gestational age days per category" = tabWithHelpText(
          GestationalAgeDaysPerCategoryModule$new(data = gestationalAgeDaysPerCategorySummary),
          "Gestational duration by outcome type. Used to check that outcome-specific durations (e.g. live birth vs miscarriage) are plausible."
        ),
        "Temporal patterns" = tabWithHelpText(
          TemporalPatternsModule$new(data = trendData, missingData = trendDataMissing),
          "Episode counts by year and month for each date type. Used for temporal trends, seasonality, and study-window checks."
        )
      ),
      "Episode construction" = list(
        "Pregnancy overlap" = tabWithHelpText(
          handleEmptyResult(
            object = PregnancyOverlapModule$new(
              data = pregnancyOverlapCounts,
              dp = unique(pregnancyOverlapCounts$cdm_name)
            ),
            result = pregnancyOverlapCounts,
            emptyMessage = "No pregnancy overlap data available."
          ),
          "Count of pregnancy episodes by overlap status (overlapping vs non-overlapping). Used for data quality (overlaps may indicate algorithm or data issues)."
        ),
        "Swapped dates" = tabWithHelpText(
          handleEmptyResult(
            object = FilterTableModule$new(
              data = swappedDatesDisplay,
              dp = unique(swappedDatesDisplay$cdm_name)
            ),
            result = swappedDatesDisplay,
            emptyMessage = "No swapped dates data available."
          ),
          paste(
            "Count of episodes with **reversed start/end** (start date after end date). Used for data quality and algorithm validation.",
            "",
            "**Table columns:**",
            "- **n**: number of episodes",
            "- **pct**: percentage of total episodes",
            "",
            "**Metrics:**",
            "- **HIP reversed (start after end)**: pregnancy start (merge_pregnancy_start) is after HIP end date (hip_end_date)",
            "- **PPS reversed (start after end)**: pregnancy start is after PPS end date (pps_end_date)",
            "- **Total episodes**: denominator used for the percentages",
            sep = "\n\n"
          )
        ),
        "Date consistency" = tabWithHelpText(
          handleEmptyResult(
            object = FilterTableModule$new(data = dateConsistency, dp = allDP),
            result = dateConsistency,
            emptyMessage = "No date consistency data available."
          ),
          paste(
            "**Counts and percentages of missing dates** by date field.",
            "",
            "**N missing** = number of records with a missing date; **Percent missing** = percentage of all records.",
            "Used to assess completeness of key dates and to compare across sites.",
            sep = "\n\n"
          )
        )
      ),
      "Episode outcomes" = list(
        "Mode of delivery" = tabWithHelpText(
          handleEmptyResult(
            object = DeliveryModeModule$new(
              data = deliveryModeSummary,
              dp = allDP,
              yVar = "pct",
              label = "n",
              fillVar = "mode",
              title = "Delivery mode rates"
            ),
            result = deliveryModeSummary,
            emptyMessage = "No delivery mode data available."
          ),
          paste(
            "Cesarean vs vaginal delivery for delivery and live-birth outcomes. Used for mode-of-delivery summaries and cross-site comparison.",
            "",
            "**Table columns:**",
            "- **total**: absolute count of episodes for that database, outcome category, and delivery mode (before any table search or filters).",
            "- **n**: count of episodes that also match any filters or search applied in the table; when no filters are applied, **n** equals **total** for that row.",
            "- **pct**: percentage for that row, calculated as (n / total) × 100.",
            sep = "\n\n"
          )
        ),
        "Outcome categories" = tabWithHelpText(
          handleEmptyResult(
            object = OutcomeCategoriesModule$new(data = outcomeCategoriesCount, dp = allDP),
            result = outcomeCategoriesCount,
            emptyMessage = "No outcome categories data available."
          ),
          "Outcome distribution by algorithm (HIP, PPS, final harmonized). Used to compare algorithm agreement and describe outcome mix across sites."
        )
      )
    )
    if (exists("characteristics", envir = .GlobalEnv)) {
      characteristics <- get("characteristics", envir = .GlobalEnv)
      appStructure[["Cohort Characteristics"]] <- list(tabWithHelpText(
        handleEmptyResult(object = Characteristics$new(characteristics), result = characteristics),
        "Cohort characteristics and stratified summaries. Used for cohort description and covariate balance."
      ))
    }
    incidencePrevalenceItems <- list()
    if (exists("incidence", envir = .GlobalEnv)) {
      incidence <- get("incidence", envir = .GlobalEnv)
      incidencePrevalenceItems[["Incidence"]] <- list(tabWithHelpText(
        handleEmptyResult(object = Incidence$new(incidence), result = incidence),
        "Incidence estimates and confidence intervals. Used for rate comparison and temporal trends."
      ))
    }
    if (exists("prevalence", envir = .GlobalEnv)) {
      prevalence <- get("prevalence", envir = .GlobalEnv)
      incidencePrevalenceItems[["Prevalence"]] <- list(tabWithHelpText(
        handleEmptyResult(object = Prevalence$new(prevalence, defaults = list()), result = prevalence),
        "Prevalence estimates over time. Used for burden of pregnancy outcomes and cross-database comparison."
      ))
    }
    if (length(incidencePrevalenceItems) > 0) {
      appStructure[["IncidencePrevalence"]] <- incidencePrevalenceItems
    }
    appStructure[["Observation Period"]] <- list(
      "Observation period range" = tabWithHelpText(
        handleEmptyResult(object = FilterTableModule$new(data = observationPeriodRange, dp = allDP), result = observationPeriodRange),
        "Range of observation periods in the CDM. Used to interpret temporal coverage and compare study windows across sites."
      )
    )

    # Concept counts tab (ESD, HIP, PPS concept count tables)
    # Use each table's cdm_name for dp so filter matches (concept count CSVs get cdm_name from their load folder)
    conceptCountsTabs <- list()
    if (exists("esdConceptCounts", envir = .GlobalEnv)) {
      esdConceptCounts <- get("esdConceptCounts", envir = .GlobalEnv)
      esdDp <- if (is.data.frame(esdConceptCounts) && "cdm_name" %in% names(esdConceptCounts)) unique(esdConceptCounts$cdm_name) else allDP
      conceptCountsTabs[["ESD concept counts"]] <- tabWithHelpText(
        handleEmptyResult(
          object = FilterTableModule$new(data = esdConceptCounts, dp = esdDp),
          result = esdConceptCounts
        ),
        "Counts of ESD (Estimated Start Date) concept usage. Used to describe concept coverage and compare across sites."
      )
    }
    if (exists("hipConceptCounts", envir = .GlobalEnv)) {
      hipConceptCounts <- get("hipConceptCounts", envir = .GlobalEnv)
      hipDp <- if (is.data.frame(hipConceptCounts) && "cdm_name" %in% names(hipConceptCounts)) unique(hipConceptCounts$cdm_name) else allDP
      conceptCountsTabs[["HIP concept counts"]] <- tabWithHelpText(
        handleEmptyResult(
          object = FilterTableModule$new(data = hipConceptCounts, dp = hipDp),
          result = hipConceptCounts
        ),
        "Counts of HIP (Hierarchical Identification of Pregnancy) concept usage. Used to compare concept coverage across sites."
      )
    }
    if (exists("ppsConceptCounts", envir = .GlobalEnv)) {
      ppsConceptCounts <- get("ppsConceptCounts", envir = .GlobalEnv)
      ppsDp <- if (is.data.frame(ppsConceptCounts) && "cdm_name" %in% names(ppsConceptCounts)) unique(ppsConceptCounts$cdm_name) else allDP
      conceptCountsTabs[["PPS concept counts"]] <- tabWithHelpText(
        handleEmptyResult(
          object = FilterTableModule$new(data = ppsConceptCounts, dp = ppsDp),
          result = ppsConceptCounts
        ),
        "Counts of pregnancy-related concepts used by PPS per concept. Used to check that expected pregnancy concepts are present."
      )
    }
    if (length(conceptCountsTabs) > 0) {
      appStructure[["Concept counts"]] <- conceptCountsTabs
    }

    # Age tab: age_summary.csv, age_summary_first_pregnancy.csv, age_summary_groups.csv
    ageTabs <- list()
    if (nrow(ageSummaryRaw) > 0) {
      ageTabs[["Age Summary by Outcome"]] <- tabWithHelpText(
        handleEmptyResult(
          object = AgeSummaryCsvModule$new(data = ageSummaryRaw, dp = unique(ageSummaryRaw$cdm_name)),
          result = ageSummaryRaw
        ),
        "Distribution of maternal age at pregnancy start. Boxplot: one row per outcome group, faceted by database. Filters for database and outcome group. Used for cohort description and feasibility checks."
      )
    }
    if (exists("ageSummaryFirstPregnancy", envir = .GlobalEnv)) {
      ageSummaryFirstPregnancy <- get("ageSummaryFirstPregnancy", envir = .GlobalEnv)
      ageTabs[["Age at first pregnancy"]] <- tabWithHelpText(
        handleEmptyResult(
          object = AgeSummaryFirstPregnancyModule$new(data = ageSummaryFirstPregnancy, dp = unique(ageSummaryFirstPregnancy$cdm_name)),
          result = ageSummaryFirstPregnancy
        ),
        "Summary of maternal age at first pregnancy. Boxplot: one row per database. Used for cohort description and feasibility checks."
      )
    }
    if (exists("ageSummaryGroups", envir = .GlobalEnv)) {
      ageSummaryGroups <- get("ageSummaryGroups", envir = .GlobalEnv)
      ageTabs[["Age distribution"]] <- tabWithHelpText(
        handleEmptyResult(
          object = AgeSummaryGroupsModule$new(data = ageSummaryGroups, dp = unique(ageSummaryGroups$cdm_name)),
          result = ageSummaryGroups
        ),
        "Counts and percentages of pregnancies by age (by year and boundary groups). Histogram with filters for database and outcome type; Y axis can be count (n) or percent (pct). Used for age-stratified summaries."
      )
    }
    if (length(ageTabs) > 0) {
      appStructure[["Age"]] <- ageTabs
    }

    # Attrition tab (attrition.csv, attrition_if_cleanup.csv)
    attritionTabs <- list()
    if (exists("attrition_episodes", envir = .GlobalEnv)) {
      attritionEpisodes <- get("attrition_episodes", envir = .GlobalEnv)
      attritionTabs[["Attrition"]] <- tabWithHelpText(
        handleEmptyResult(
          object = AttritionTableModule$new(data = attritionEpisodes, dp = allDP),
          result = attritionEpisodes
        ),
        "Number of episodes and persons excluded at each pipeline step. Used to document cohort flow and feasibility."
      )
    }
    if (exists("attritionIfCleanup", envir = .GlobalEnv)) {
      attritionIfCleanup <- get("attritionIfCleanup", envir = .GlobalEnv)
      attritionTabs[["Attrition if cleanup"]] <- tabWithHelpText(
        handleEmptyResult(
          object = AttritionTableModule$new(data = attritionIfCleanup, dp = allDP),
          result = attritionIfCleanup
        ),
        "Attrition that would apply if quality-check cleanup were run. Used to assess impact of cleanup on cohort size."
      )
    }
    if (length(attritionTabs) > 0) {
      appStructure[["Attrition"]] <- attritionTabs
    }

    # Concept check (concept_check.csv) — tab always shown; data from CSV or empty
    # Use union with table cdm_name so data loaded from subfolders (e.g. data/ipci) is shown even when allDP came from zip with different naming
    conceptCheck <- get("conceptCheck", envir = .GlobalEnv)
    conceptCheckDp <- if (nrow(conceptCheck) > 0 && "cdm_name" %in% colnames(conceptCheck)) unique(c(allDP, conceptCheck$cdm_name)) else allDP
    appStructure[["Concept check"]] <- list(
      tabWithHelpText(
        handleEmptyResult(
          object = FilterTableModule$new(data = conceptCheck, dp = conceptCheckDp),
          result = conceptCheck,
          emptyMessage = "No concept check data available."
        ),
        paste(
          "Concept-level checks for pregnancy-related concepts. Validates concept presence and whether concept dates fall within expected timing relative to each pregnancy episode (using concept-specific expected month ranges).",
          "",
          "**Table columns:**",
          "- **concept_id / concept_name:** OMOP concept identifier and name.",
          "- **n_after_min:** Number of episode–concept pairs where the concept date is on or before the expected window start (min_date).",
          "- **n_prior_max:** Number of pairs where the concept end date is on or after the expected window end (max_date).",
          "- **n_in_span:** Number of pairs where the concept start date falls within the expected window [min_date, max_date] for that concept.",
          "- **n_at_midpoint:** Number of pairs where the concept date is near the midpoint of the expected range.",
          "- **total:** Total number of episode–concept pairs for this concept.",
          "- **p_after_min, p_prior_max, p_in_span, p_at_midpoint:** Same counts as percentages of all episodes in the database.",
          "- **p_concept:** Percentage of episodes that have at least one record for this concept (concept coverage).",
          "- **cdm_name, date_run, date_export, pkg_version:** Database and export metadata.",
          sep = "\n\n"
        )
      )
    )

    # Precision days (precision_days.csv)
    # Use allDP only (same as concept counts / age tabs) so we don't show alternate CDM names from the CSV
    precisionDays <- get("precisionDays", envir = .GlobalEnv)
    if (nrow(precisionDays) > 0 && "cdm_name" %in% colnames(precisionDays)) {
      precisionDays <- precisionDays %>% dplyr::filter(.data$cdm_name %in% .env$allDP)
    }
    precisionDaysDp <- allDP
    appStructure[["Precision days"]] <- list(
      tabWithHelpText(
        handleEmptyResult(
          object = PrecisionDaysModule$new(data = precisionDays, dp = precisionDaysDp),
          result = precisionDays,
          emptyMessage = "No precision days data available."
        ),
        paste(
          "**Distribution of ESD start-date precision (in days).**",
          "",
          "**What it is:** ESD (Estimated Start Date) precision is the uncertainty in the inferred pregnancy start date for each episode. Lower values mean a more precise start date (e.g. 0–7 days when gestational-week concepts agree); higher values mean the start date is only known to within a wider window (e.g. when only trimester-level information is available).",
          "",
          "**How it is calculated:** For each episode, the algorithm derives a start date from ESD concepts (gestational week or trimester). Precision in days is the spread of possible start dates: the range in days between the earliest and latest plausible start date implied by those concepts. The plot shows the **density** of these precision values across all episodes (one curve per database).",
          "",
          "**Use:** Compare how precise inferred start dates are within a database and across sites; useful for data quality and for understanding how much timing information is available.",
          sep = "\n\n"
        )
      )
    )

    # Quality check cleanup (quality_check_cleanup.csv)
    qualityCheckCleanup <- get("qualityCheckCleanup", envir = .GlobalEnv)
    if (nrow(qualityCheckCleanup) > 0 && "cdm_name" %in% colnames(qualityCheckCleanup)) {
      qualityCheckCleanup <- qualityCheckCleanup %>% dplyr::select("cdm_name", dplyr::everything())
    }
    qualityCheckCleanupDp <- if (nrow(qualityCheckCleanup) > 0 && "cdm_name" %in% colnames(qualityCheckCleanup)) unique(c(allDP, qualityCheckCleanup$cdm_name)) else allDP
    appStructure[["Quality check cleanup"]] <- list(
      tabWithHelpText(
        handleEmptyResult(
          object = QualityCheckCleanupModule$new(data = qualityCheckCleanup, dp = qualityCheckCleanupDp),
          result = qualityCheckCleanup,
          emptyMessage = "No quality check cleanup data available."
        ),
        "Summary of episodes that would be removed by quality-check cleanup. Used for data quality and algorithm tuning."
      )
    )

    # PET comparison tab: single sidebar tab (no subtabs) with Overview, Plot, Table, Summarised result; or legacy many tables
    petComparisonTabs <- list()
    if (exists("petComparisonSummarisedResult", envir = .GlobalEnv)) {
      petSr <- get("petComparisonSummarisedResult", envir = .GlobalEnv)
      if (!is.null(petSr) && nrow(petSr) > 0) {
        petComparisonTabs <- list(
          tabWithHelpText(
            handleEmptyResult(
              object = PetComparisonModule$new(result = petSr),
              result = petSr,
              emptyMessage = "No PET comparison data available."
            ),
            "Compare PregnancyIdentifier episodes with the OMOP Pregnancy Extension Table (PET): methodology (Overview), Venn diagram (Plot), formatted table with Database filter (Table), and raw summarised result (Summarised result)."
          )
        )
      }
    }
    # Legacy: many PET comparison CSVs (old export format)
    if (length(petComparisonTabs) == 0) {
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
      for (varName in names(petComparisonSpec)) {
        if (exists(varName, envir = .GlobalEnv)) {
          data <- get(varName, envir = .GlobalEnv)
          if (!is.null(data) && nrow(data) > 0) {
            displayName <- petComparisonSpec[[varName]]
            petComparisonTabs[[displayName]] <- tabWithHelpText(
              handleEmptyResult(
                object = PetComparisonTableModule$new(data = data, title = displayName),
                result = data,
                emptyMessage = "No PET comparison data available."
              ),
              sprintf("PET comparison: %s. Compare PregnancyIdentifier output with the OMOP Pregnancy Extension Table (PET).", displayName)
            )
          }
        }
      }
    }
    if (length(petComparisonTabs) > 0) {
      appStructure[["PET comparison"]] <- petComparisonTabs
    }

    app <- DarwinDashboardApp$new(appStructure, title = "PregnancyIdentifier")
    app$launch()
  }
}
