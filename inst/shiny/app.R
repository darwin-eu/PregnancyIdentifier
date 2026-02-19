library(shiny)
library(DarwinShinyModules)
library(IncidencePrevalence)
library(ggplot2)
library(visOmopResults)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
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

# Data sources: zip files first, then subfolders (one per database)
zipFiles <- list.files(dataFolder, pattern = "\\.zip$", full.names = TRUE)
zipFiles <- zipFiles[!is.na(zipFiles) & nzchar(zipFiles)]
if (length(zipFiles) > 0) {
  info <- file.info(zipFiles)
  zipFiles <- zipFiles[file.exists(zipFiles) & !is.na(info$size) & info$size > 0]
}

subfolders <- character(0)
if (length(zipFiles) == 0) {
  subfolders <- list.dirs(dataFolder, full.names = TRUE, recursive = FALSE)
  subfolders <- subfolders[vapply(subfolders, function(d) {
    length(list.files(d, pattern = "\\.csv$")) > 0
  }, logical(1))]
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
  ageCsvs <- c("age.csv", "age_summary.csv", "age_summary_first_pregnancy.csv", "age_summary_groups.csv")
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
  # Load from subfolders (e.g. data/ipci) whenever the CSV exists so QA tabs get data even when main load was from zip only
  subfoldersForQa <- list.dirs(dataFolder, full.names = TRUE, recursive = FALSE)
  for (j in seq_along(qaCsvs)) {
    firstLoad <- TRUE
    for (d in subfoldersForQa) {
      f <- qaCsvs[j]
      path <- file.path(d, f)
      if (file.exists(path)) {
        loadFile(file = f, dbName = basename(d), runDate = "", zipVersion = NULL, folder = d, overwrite = firstLoad)
        firstLoad <- FALSE
      }
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

    # Ensure ageSummary exists (from age_summary.csv) so Episode frequency and Age summary tab do not crash
    if (!exists("ageSummary", envir = .GlobalEnv) || is.null(get("ageSummary", envir = .GlobalEnv)) || nrow(get("ageSummary", envir = .GlobalEnv)) == 0) {
      assign("ageSummary",
             tibble::tibble(cdm_name = character(0), colName = character(0)),
             envir = .GlobalEnv)
    }
    ageSummary <- get("ageSummary", envir = .GlobalEnv)
    ageSummaryRaw <- ageSummary  # keep raw copy for Age tab DT table
    if (nrow(ageSummary) > 0) {
      ageSummary <- dataToLong(ageSummary, skipCols = c("cdm_name", "colName"))
    } else {
      ageSummary <- tibble::tibble(name = character(0))
    }
    numRound <- function(x) {
      round(100 * as.numeric(x), 2)
    }
    dateConsistency <- dataToLong(dateConsistency) %>%
      dplyr::mutate(across(!name, numRound))

    observationPeriodRange <- dataToLong(observationPeriodRange)

    pregnancyFrequencyList <- lapply(unique(pregnancyFrequency$cdm_name), FUN = function(name) {
      pregnancyFrequency %>%
        dplyr::filter(cdm_name == name) %>%
        dplyr::select(-"cdm_name") %>%
        dplyr::rename(!!name := number_individuals)
    })
    pregnancyFrequencyList <- pregnancyFrequencyList[order(sapply(pregnancyFrequencyList, nrow), decreasing = TRUE)]
    pregnancyFrequency <- purrr::reduce(pregnancyFrequencyList, dplyr::left_join, by = "freq")
    pregnancyFrequency <- pregnancyFrequency %>%
      dplyr::mutate(freq = factor(freq, levels = unique(pregnancyFrequency$freq))) %>%
      dplyr::mutate(across(!freq, ~ suppressWarnings(as.numeric(.)))) %>%
      suppressCounts(colNames = allDP)

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
      ),
      "Age summary" = tabWithHelpText(
        handleEmptyResult(object = AgeSummaryModule$new(data = ageSummary, dp = allDP), result = ageSummary),
        "Distribution of maternal age at pregnancy start. Used for cohort description, feasibility checks, and comparing age across sites."
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
        episodeFreqItems[2:3]
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
          GestationalAgeModule$new(data = gestationalWeeksSummary, daysData = gestationalAgeDaysCounts, dp = allDP),
          "Distribution of gestational age by week. Used for gestational-age histograms, preterm/term summaries, and cross-site comparison."
        ),
        "Gestational age binned" = tabWithHelpText(
          GestationalAgeModule$new(data = gestationalWeeksBinned, dp = allDP, maxWeeksFilter = FALSE),
          "Gestational age grouped into bands (e.g. &lt;12, 12–28, 28–32 weeks). Used for outcome bands and cross-site comparison."
        ),
        "Gestational age days per category" = tabWithHelpText(
          GestationalAgeDaysPerCategoryModule$new(data = gestationalAgeDaysPerCategorySummary, dp = allDP),
          "Gestational duration by outcome type. Used to check that outcome-specific durations (e.g. live birth vs miscarriage) are plausible."
        ),
        "Temporal patterns" = tabWithHelpText(
          TemporalPatternsModule$new(data = trendData, missingData = trendDataMissing, dp = allDP),
          "Episode counts by year and month for each date type. Used for temporal trends, seasonality, and study-window checks."
        )
      ),
      "Episode construction" = list(
        "Pregnancy overlap" = tabWithHelpText(
          handleEmptyResult(
            object = EpisodeConstructionModule$new(
              data = pregnancyOverlapCounts,
              dp = unique(pregnancyOverlapCounts$cdm_name),
              yVar = "pct",
              label = "n",
              fillVar = "overlap",
              title = "Pregnancy overlap counts"
            ),
            result = pregnancyOverlapCounts,
            emptyMessage = "No pregnancy overlap data available."
          ),
          "Count of persons with overlapping inferred pregnancy intervals. Used for data quality (overlaps may indicate algorithm or data issues)."
        ),
        "Swapped dates" = tabWithHelpText(
          handleEmptyResult(
            object = EpisodeConstructionModule$new(
              data = swappedDates,
              dp = setdiff(colnames(swappedDates), "name"),
              convertDataForPlot = TRUE,
              yVar = "value",
              fillVar = "name",
              title = "Swapped dates"
            ),
            result = swappedDates,
            emptyMessage = "No swapped dates data available."
          ),
          "Count of episodes with reversed start/end (start after end). Used for data quality and algorithm validation."
        ),
        "Date consistency" = tabWithHelpText(
          handleEmptyResult(
            object = EpisodeConstructionModule$new(
              data = dateConsistency,
              dp = allDP,
              convertDataForPlot = TRUE,
              yVar = "value",
              fillVar = "name",
              position = "dodge",
              title = "Date consistency"
            ),
            result = dateConsistency,
            emptyMessage = "No date consistency data available."
          ),
          "Proportion of missing dates by field. Used to assess completeness of key dates and to compare across sites."
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
          "Cesarean vs vaginal delivery for delivery and live-birth outcomes. Used for mode-of-delivery summaries and cross-site comparison."
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
    conceptCountsTabs <- list()
    if (exists("esdConceptCounts", envir = .GlobalEnv)) {
      esdConceptCounts <- get("esdConceptCounts", envir = .GlobalEnv)
      conceptCountsTabs[["ESD concept counts"]] <- tabWithHelpText(
        handleEmptyResult(
          object = FilterTableModule$new(data = esdConceptCounts, dp = allDP),
          result = esdConceptCounts
        ),
        "Counts of ESD (Estimated Start Date) concept usage. Used to describe concept coverage and compare across sites."
      )
    }
    if (exists("hipConceptCounts", envir = .GlobalEnv)) {
      hipConceptCounts <- get("hipConceptCounts", envir = .GlobalEnv)
      conceptCountsTabs[["HIP concept counts"]] <- tabWithHelpText(
        handleEmptyResult(
          object = FilterTableModule$new(data = hipConceptCounts, dp = allDP),
          result = hipConceptCounts
        ),
        "Counts of HIP (Hierarchical Identification of Pregnancy) concept usage. Used to compare concept coverage across sites."
      )
    }
    if (exists("ppsConceptCounts", envir = .GlobalEnv)) {
      ppsConceptCounts <- get("ppsConceptCounts", envir = .GlobalEnv)
      conceptCountsTabs[["PPS concept counts"]] <- tabWithHelpText(
        handleEmptyResult(
          object = FilterTableModule$new(data = ppsConceptCounts, dp = allDP),
          result = ppsConceptCounts
        ),
        "Counts of pregnancy-related concepts used by PPS per concept. Used to check that expected pregnancy concepts are present."
      )
    }
    if (length(conceptCountsTabs) > 0) {
      appStructure[["Concept counts"]] <- conceptCountsTabs
    }

    # Age tab: load age.csv, age_summary.csv, age_summary_first_pregnancy.csv, age_summary_groups.csv and show in DT tables
    ageTabs <- list()
    if (exists("age", envir = .GlobalEnv)) {
      age <- get("age", envir = .GlobalEnv)
      ageTabs[["age.csv"]] <- tabWithHelpText(
        handleEmptyResult(
          object = Table$new(data = age, title = "age.csv", options = list(scrollX = TRUE, pageLength = 25)),
          result = age
        ),
        "Age-at-pregnancy-start data. Used for cohort description and age-stratified analyses."
      )
    }
    if (nrow(ageSummaryRaw) > 0) {
      ageTabs[["age_summary.csv"]] <- tabWithHelpText(
        handleEmptyResult(
          object = Table$new(data = ageSummaryRaw, title = "age_summary.csv", options = list(scrollX = TRUE, pageLength = 25)),
          result = ageSummaryRaw
        ),
        "Distribution of maternal age at pregnancy start. Used for cohort description and feasibility checks."
      )
    }
    if (exists("ageSummaryFirstPregnancy", envir = .GlobalEnv)) {
      ageSummaryFirstPregnancy <- get("ageSummaryFirstPregnancy", envir = .GlobalEnv)
      ageTabs[["age_summary_first_pregnancy.csv"]] <- tabWithHelpText(
        handleEmptyResult(
          object = Table$new(data = ageSummaryFirstPregnancy, title = "age_summary_first_pregnancy.csv", options = list(scrollX = TRUE, pageLength = 25)),
          result = ageSummaryFirstPregnancy
        ),
        "Summary of maternal age at first pregnancy. Used for cohort description and feasibility checks."
      )
    }
    if (exists("ageSummaryGroups", envir = .GlobalEnv)) {
      ageSummaryGroups <- get("ageSummaryGroups", envir = .GlobalEnv)
      ageTabs[["age_summary_groups.csv"]] <- tabWithHelpText(
        handleEmptyResult(
          object = Table$new(data = ageSummaryGroups, title = "age_summary_groups.csv", options = list(scrollX = TRUE, pageLength = 25)),
          result = ageSummaryGroups
        ),
        "Counts and percentages of pregnancies by age (by year and boundary groups). Used for age-stratified summaries."
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
          object = FilterTableModule$new(data = attritionEpisodes, dp = allDP),
          result = attritionEpisodes
        ),
        "Number of episodes and persons excluded at each pipeline step. Used to document cohort flow and feasibility."
      )
    }
    if (exists("attritionIfCleanup", envir = .GlobalEnv)) {
      attritionIfCleanup <- get("attritionIfCleanup", envir = .GlobalEnv)
      attritionTabs[["Attrition if cleanup"]] <- tabWithHelpText(
        handleEmptyResult(
          object = FilterTableModule$new(data = attritionIfCleanup, dp = allDP),
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
        "Concept-level checks for pregnancy-related concepts. Used to validate concept presence and coverage."
      )
    )

    # Precision days (precision_days.csv)
    precisionDays <- get("precisionDays", envir = .GlobalEnv)
    precisionDaysDp <- if (nrow(precisionDays) > 0 && "cdm_name" %in% colnames(precisionDays)) unique(c(allDP, precisionDays$cdm_name)) else allDP
    appStructure[["Precision days"]] <- list(
      tabWithHelpText(
        handleEmptyResult(
          object = FilterTableModule$new(data = precisionDays, dp = precisionDaysDp),
          result = precisionDays,
          emptyMessage = "No precision days data available."
        ),
        "Distribution of ESD start-date precision (in days). Used to see how precise inferred start dates are and to compare across sites."
      )
    )

    # Quality check cleanup (quality_check_cleanup.csv)
    qualityCheckCleanup <- get("qualityCheckCleanup", envir = .GlobalEnv)
    qualityCheckCleanupDp <- if (nrow(qualityCheckCleanup) > 0 && "cdm_name" %in% colnames(qualityCheckCleanup)) unique(c(allDP, qualityCheckCleanup$cdm_name)) else allDP
    appStructure[["Quality check cleanup"]] <- list(
      tabWithHelpText(
        handleEmptyResult(
          object = FilterTableModule$new(data = qualityCheckCleanup, dp = qualityCheckCleanupDp),
          result = qualityCheckCleanup,
          emptyMessage = "No quality check cleanup data available."
        ),
        "Summary of episodes that would be removed by quality-check cleanup. Used for data quality and algorithm tuning."
      )
    )

    # PET comparison tab (when comparePregnancyIdentifierWithPET outputs are loaded from zip)
    if (exists("petComparisonEpisodeCounts", envir = .GlobalEnv)) {
      petComparisonEpisodeCounts <- get("petComparisonEpisodeCounts", envir = .GlobalEnv)
      appStructure[["PET comparison"]] <- list(
        "Episode counts" = tabWithHelpText(
          handleEmptyResult(object = FilterTableModule$new(data = petComparisonEpisodeCounts), result = petComparisonEpisodeCounts),
          "Episode counts from PregnancyIdentifier vs PET. Used to compare pipeline outputs and assess agreement."
        )
      )
      if (exists("petComparisonVennCounts", envir = .GlobalEnv)) {
        petComparisonVennCounts <- get("petComparisonVennCounts", envir = .GlobalEnv)
        appStructure[["PET comparison"]][["Venn counts"]] <- tabWithHelpText(
          handleEmptyResult(object = FilterTableModule$new(data = petComparisonVennCounts), result = petComparisonVennCounts),
          "Overlap and exclusive counts between PregnancyIdentifier and PET episodes. Used for concordance assessment."
        )
      }
      if (exists("petComparisonPpvSensitivity", envir = .GlobalEnv)) {
        petComparisonPpvSensitivity <- get("petComparisonPpvSensitivity", envir = .GlobalEnv)
        appStructure[["PET comparison"]][["PPV and sensitivity"]] <- tabWithHelpText(
          handleEmptyResult(object = FilterTableModule$new(data = petComparisonPpvSensitivity), result = petComparisonPpvSensitivity),
          "Positive predictive value and sensitivity vs PET. Used to quantify agreement and algorithm performance."
        )
      }
      if (exists("petComparisonDateDifferenceSummary", envir = .GlobalEnv)) {
        petComparisonDateDifferenceSummary <- get("petComparisonDateDifferenceSummary", envir = .GlobalEnv)
        appStructure[["PET comparison"]][["Date difference summary"]] <- tabWithHelpText(
          handleEmptyResult(object = FilterTableModule$new(data = petComparisonDateDifferenceSummary), result = petComparisonDateDifferenceSummary),
          "Summary of differences in episode start/end dates between PregnancyIdentifier and PET. Used for date-level validation."
        )
      }
      if (exists("petComparisonOutcomeConfusionMatrix", envir = .GlobalEnv)) {
        petComparisonOutcomeConfusionMatrix <- get("petComparisonOutcomeConfusionMatrix", envir = .GlobalEnv)
        appStructure[["PET comparison"]][["Outcome confusion matrix"]] <- tabWithHelpText(
          handleEmptyResult(object = FilterTableModule$new(data = petComparisonOutcomeConfusionMatrix), result = petComparisonOutcomeConfusionMatrix),
          "Cross-tabulation of outcome categories: PregnancyIdentifier vs PET. Used to assess outcome agreement."
        )
      }
      if (exists("petComparisonOutcomeAccuracy", envir = .GlobalEnv)) {
        petComparisonOutcomeAccuracy <- get("petComparisonOutcomeAccuracy", envir = .GlobalEnv)
        appStructure[["PET comparison"]][["Outcome accuracy"]] <- tabWithHelpText(
          handleEmptyResult(object = FilterTableModule$new(data = petComparisonOutcomeAccuracy), result = petComparisonOutcomeAccuracy),
          "Outcome-level accuracy metrics vs PET. Used to compare outcome classification performance."
        )
      }
      if (exists("petComparisonDurationSummary", envir = .GlobalEnv)) {
        petComparisonDurationSummary <- get("petComparisonDurationSummary", envir = .GlobalEnv)
        appStructure[["PET comparison"]][["Duration summary"]] <- tabWithHelpText(
          handleEmptyResult(object = FilterTableModule$new(data = petComparisonDurationSummary), result = petComparisonDurationSummary),
          "Summary of gestational duration differences between PregnancyIdentifier and PET. Used for duration validation."
        )
      }
      if (exists("petComparisonDurationDistribution", envir = .GlobalEnv)) {
        petComparisonDurationDistribution <- get("petComparisonDurationDistribution", envir = .GlobalEnv)
        appStructure[["PET comparison"]][["Duration distribution"]] <- tabWithHelpText(
          handleEmptyResult(object = FilterTableModule$new(data = petComparisonDurationDistribution), result = petComparisonDurationDistribution),
          "Distribution of duration (and differences) vs PET. Used to assess gestational-age agreement."
        )
      }
    }

    app <- DarwinDashboardApp$new(appStructure, title = "PregnancyIdentifier")
    app$launch()
  }
}
