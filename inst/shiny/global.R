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
source(file.path("utils", "helpers.R")) # need to source helpers first
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

useCachedData <- if (exists("shinySettings") && "useCachedData" %in% names(shinySettings)) shinySettings$useCachedData else TRUE

# Cache file lives alongside global.R (app working directory)
cachePath <- file.path(getwd(), "data.rds")

# Invalidate cache if it doesn't exist, is disabled, or was built for a different dataFolder
cacheValid <- file.exists(cachePath) && !isFALSE(useCachedData)
if (cacheValid) {
  cachedFolder <- tryCatch(readr::read_rds(cachePath)[["_dataFolder"]], error = function(e) NULL)
  if (is.null(cachedFolder) || !identical(cachedFolder, dataFolder)) {
    message("Cache exists but dataFolder has changed — rebuilding cache.")
    cacheValid <- FALSE
  }
}

if (!cacheValid) {
  message("Building data cache from: ", dataFolder)
  if (!dir.exists(dataFolder)) cli::cli_abort("dataFolder: {dataFolder} does not exist!")

  # Summarised results
  incidence <- readResults(dataFolder, regex = "_incidence\\.csv$")
  prevalence <- readResults(dataFolder, regex = "_prevalence\\.csv$")
  characteristics <- readResults(dataFolder, regex = "_characteristics\\.csv$")
  petComparisonSummarisedResult <- readResults(dataFolder, regex = "pet_comparison_summarised_result")
  petUnmatchedLsc <- readResults(dataFolder, regex = "pet_unmatched_lsc")

  # Plain CSVs
  cdmSource <- readResults(dataFolder, regex = "cdm_source\\.csv$", reader = "csv")
  ageSummary <- readResults(dataFolder, regex = "/age_summary\\.csv$", reader = "csv")
  ageSummaryFirstPregnancy <- readResults(dataFolder, regex = "age_summary_first_pregnancy\\.csv$", reader = "csv")
  ageSummaryFirstPregnancyEnd <- readResults(dataFolder, regex = "age_summary_first_pregnancy_end\\.csv$", reader = "csv")
  ageSummaryGroups <- readResults(dataFolder, regex = "age_summary_groups\\.csv$", reader = "csv")
  attrition <- readResults(dataFolder, regex = "/attrition\\.csv$", reader = "csv")
  attritionIfCleanup <- readResults(dataFolder, regex = "attrition_if_cleanup\\.csv$", reader = "csv")
  conceptCheck <- readResults(dataFolder, regex = "concept_check\\.csv$", reader = "csv")
  deliveryModeSummary <- readResults(dataFolder, regex = "delivery_mode_summary\\.csv$", reader = "csv")
  deliveryModeByYear <- readResults(dataFolder, regex = "delivery_mode_by_year\\.csv$", reader = "csv")
  episodeFrequency <- readResults(dataFolder, regex = "/episode_frequency\\.csv$", reader = "csv")
  episodeFrequencySummary <- readResults(dataFolder, regex = "/episode_frequency_summary\\.csv$", reader = "csv")
  esdConceptCounts <- readResults(dataFolder, regex = "esd_concept_counts\\.csv$", reader = "csv")
  gestationalAgeDaysCounts <- readResults(dataFolder, regex = "gestational_age_days_counts\\.csv$", reader = "csv")
  gestationalAgeDaysPerCategorySummary <- readResults(dataFolder, regex = "gestational_age_days_per_category_summary\\.csv$", reader = "csv")
  gestationalAgeDaysSummary <- readResults(dataFolder, regex = "gestational_age_days_summary\\.csv$", reader = "csv")
  gestationalWeeks <- readResults(dataFolder, regex = "gestational_weeks\\.csv$", reader = "csv")
  hipConceptCounts <- readResults(dataFolder, regex = "hip_concept_counts\\.csv$", reader = "csv")
  missingDates <- readResults(dataFolder, regex = "missing_dates\\.csv$", reader = "csv")
  monthlyTrendMissing <- readResults(dataFolder, regex = "/monthly_trend_missing\\.csv$", reader = "csv")
  monthlyTrends <- readResults(dataFolder, regex = "/monthly_trends\\.csv$", reader = "csv")
  observationPeriodRange <- readResults(dataFolder, regex = "observation_period_range\\.csv$", reader = "csv")
  outcomeCategoriesCount <- readResults(dataFolder, regex = "outcome_categories_count\\.csv$", reader = "csv")
  ppsConceptCounts <- readResults(dataFolder, regex = "pps_concept_counts\\.csv$", reader = "csv")
  precisionDays <- readResults(dataFolder, regex = "/precision_days\\.csv$", reader = "csv")
  precisionDaysDenominators <- readResults(dataFolder, regex = "/precision_days_denominators\\.csv$", reader = "csv")
  pregnancyFrequency <- readResults(dataFolder, regex = "pregnancy_frequency\\.csv$", reader = "csv")
  pregnancyOverlapCounts <- readResults(dataFolder, regex = "pregnancy_overlap_counts\\.csv$", reader = "csv")
  qualityCheckCleanup <- readResults(dataFolder, regex = "quality_check_cleanup\\.csv$", reader = "csv")
  swappedDates <- readResults(dataFolder, regex = "swapped_dates\\.csv$", reader = "csv")
  yearlyTrend <- readResults(dataFolder, regex = "/yearly_trend\\.csv$", reader = "csv")
  yearlyTrendMissing <- readResults(dataFolder, regex = "/yearly_trend_missing\\.csv$", reader = "csv")

  # ---- Tag v3 data and load v1/v2 from data2 folder ----
  tagVersion <- function(df, ver) addVersionColumn(replaceVersionSuffix(df, ver), ver)

  # Tag all v3 data
  incidence <- tagVersion(incidence, "v3")
  prevalence <- tagVersion(prevalence, "v3")
  characteristics <- tagVersion(characteristics, "v3")
  petComparisonSummarisedResult <- tagVersion(petComparisonSummarisedResult, "v3")
  petUnmatchedLsc <- tagVersion(petUnmatchedLsc, "v3")
  cdmSource <- tagVersion(cdmSource, "v3")
  ageSummary <- tagVersion(ageSummary, "v3")
  ageSummaryFirstPregnancy <- tagVersion(ageSummaryFirstPregnancy, "v3")
  ageSummaryFirstPregnancyEnd <- tagVersion(ageSummaryFirstPregnancyEnd, "v3")
  ageSummaryGroups <- tagVersion(ageSummaryGroups, "v3")
  attrition <- tagVersion(attrition, "v3")
  attritionIfCleanup <- tagVersion(attritionIfCleanup, "v3")
  conceptCheck <- tagVersion(conceptCheck, "v3")
  deliveryModeSummary <- tagVersion(deliveryModeSummary, "v3")
  deliveryModeByYear <- tagVersion(deliveryModeByYear, "v3")
  episodeFrequency <- tagVersion(episodeFrequency, "v3")
  episodeFrequencySummary <- tagVersion(episodeFrequencySummary, "v3")
  esdConceptCounts <- tagVersion(esdConceptCounts, "v3")
  gestationalAgeDaysCounts <- tagVersion(gestationalAgeDaysCounts, "v3")
  gestationalAgeDaysPerCategorySummary <- tagVersion(gestationalAgeDaysPerCategorySummary, "v3")
  gestationalAgeDaysSummary <- tagVersion(gestationalAgeDaysSummary, "v3")
  gestationalWeeks <- tagVersion(gestationalWeeks, "v3")
  hipConceptCounts <- tagVersion(hipConceptCounts, "v3")
  missingDates <- tagVersion(missingDates, "v3")
  monthlyTrendMissing <- tagVersion(monthlyTrendMissing, "v3")
  monthlyTrends <- tagVersion(monthlyTrends, "v3")
  observationPeriodRange <- tagVersion(observationPeriodRange, "v3")
  outcomeCategoriesCount <- tagVersion(outcomeCategoriesCount, "v3")
  ppsConceptCounts <- tagVersion(ppsConceptCounts, "v3")
  precisionDays <- tagVersion(precisionDays, "v3")
  precisionDaysDenominators <- tagVersion(precisionDaysDenominators, "v3")
  pregnancyFrequency <- tagVersion(pregnancyFrequency, "v3")
  pregnancyOverlapCounts <- tagVersion(pregnancyOverlapCounts, "v3")
  qualityCheckCleanup <- tagVersion(qualityCheckCleanup, "v3")
  swappedDates <- tagVersion(swappedDates, "v3")
  yearlyTrend <- tagVersion(yearlyTrend, "v3")
  yearlyTrendMissing <- tagVersion(yearlyTrendMissing, "v3")

  # Load v1 and v2 data from data2 folder
  data2Folder <- file.path(dirname(dataFolder), "data2")
  for (.ver in c("v1", "v2")) {
    .verFolder <- file.path(data2Folder, .ver)
    if (!dir.exists(.verFolder)) {
      message("Skipping ", .ver, " — folder not found: ", .verFolder)
      next
    }
    message("Loading ", .ver, " data from: ", .verFolder)

    # Summarised results
    .inc <- tagVersion(readResults(.verFolder, regex = "_incidence\\.csv$"), .ver)
    .prev <- tagVersion(readResults(.verFolder, regex = "_prevalence\\.csv$"), .ver)
    .char <- tagVersion(readResults(.verFolder, regex = "_characteristics\\.csv$"), .ver)

    # Plain CSVs
    .cdmSrc <- tagVersion(readResults(.verFolder, regex = "cdm_source\\.csv$", reader = "csv"), .ver)
    .ageSummary <- tagVersion(readResults(.verFolder, regex = "/age_summary\\.csv$", reader = "csv"), .ver)
    .ageSummaryGroups <- tagVersion(readResults(.verFolder, regex = "age_summary_groups\\.csv$", reader = "csv"), .ver)
    .episodeFreq <- tagVersion(readResults(.verFolder, regex = "/episode_frequency\\.csv$", reader = "csv"), .ver)
    .episodeFreqSummary <- tagVersion(readResults(.verFolder, regex = "/episode_frequency_summary\\.csv$", reader = "csv"), .ver)
    .gestAgeDaysCounts <- tagVersion(readResults(.verFolder, regex = "gestational_age_days_counts\\.csv$", reader = "csv"), .ver)
    .gestAgeDaysPerCat <- tagVersion(readResults(.verFolder, regex = "gestational_age_days_per_category_summary\\.csv$", reader = "csv"), .ver)
    .gestAgeDaysSummary <- tagVersion(readResults(.verFolder, regex = "gestational_age_days_summary\\.csv$", reader = "csv"), .ver)
    .gestWeeks <- tagVersion(readResults(.verFolder, regex = "gestational_weeks\\.csv$", reader = "csv"), .ver)
    .missDates <- tagVersion(readResults(.verFolder, regex = "missing_dates\\.csv$", reader = "csv"), .ver)
    .monthlyTrendMiss <- tagVersion(readResults(.verFolder, regex = "/monthly_trend_missing\\.csv$", reader = "csv"), .ver)
    .monthlyTrends <- tagVersion(readResults(.verFolder, regex = "/monthly_trends\\.csv$", reader = "csv"), .ver)
    .obsPeriod <- tagVersion(readResults(.verFolder, regex = "observation_period_range\\.csv$", reader = "csv"), .ver)
    .outcomeCats <- tagVersion(readResults(.verFolder, regex = "outcome_categories_count\\.csv$", reader = "csv"), .ver)
    .ppsConc <- tagVersion(readResults(.verFolder, regex = "pps_concept_counts\\.csv$", reader = "csv"), .ver)
    .precDays <- tagVersion(readResults(.verFolder, regex = "/precision_days\\.csv$", reader = "csv"), .ver)
    .pregFreq <- tagVersion(readResults(.verFolder, regex = "pregnancy_frequency\\.csv$", reader = "csv"), .ver)
    .pregOverlap <- tagVersion(readResults(.verFolder, regex = "pregnancy_overlap_counts\\.csv$", reader = "csv"), .ver)
    .swapped <- tagVersion(readResults(.verFolder, regex = "swapped_dates\\.csv$", reader = "csv"), .ver)
    .yearlyTrend <- tagVersion(readResults(.verFolder, regex = "/yearly_trend\\.csv$", reader = "csv"), .ver)
    .yearlyTrendMiss <- tagVersion(readResults(.verFolder, regex = "/yearly_trend_missing\\.csv$", reader = "csv"), .ver)

    # Merge with existing data
    incidence <- safeCombine(incidence, .inc)
    prevalence <- safeCombine(prevalence, .prev)
    characteristics <- safeCombine(characteristics, .char)
    cdmSource <- safeCombine(cdmSource, .cdmSrc)
    ageSummary <- safeCombine(ageSummary, .ageSummary)
    ageSummaryGroups <- safeCombine(ageSummaryGroups, .ageSummaryGroups)
    episodeFrequency <- safeCombine(episodeFrequency, .episodeFreq)
    episodeFrequencySummary <- safeCombine(episodeFrequencySummary, .episodeFreqSummary)
    gestationalAgeDaysCounts <- safeCombine(gestationalAgeDaysCounts, .gestAgeDaysCounts)
    gestationalAgeDaysPerCategorySummary <- safeCombine(gestationalAgeDaysPerCategorySummary, .gestAgeDaysPerCat)
    gestationalAgeDaysSummary <- safeCombine(gestationalAgeDaysSummary, .gestAgeDaysSummary)
    gestationalWeeks <- safeCombine(gestationalWeeks, .gestWeeks)
    missingDates <- safeCombine(missingDates, .missDates)
    monthlyTrendMissing <- safeCombine(monthlyTrendMissing, .monthlyTrendMiss)
    monthlyTrends <- safeCombine(monthlyTrends, .monthlyTrends)
    observationPeriodRange <- safeCombine(observationPeriodRange, .obsPeriod)
    outcomeCategoriesCount <- safeCombine(outcomeCategoriesCount, .outcomeCats)
    ppsConceptCounts <- safeCombine(ppsConceptCounts, .ppsConc)
    precisionDays <- safeCombine(precisionDays, .precDays)
    pregnancyFrequency <- safeCombine(pregnancyFrequency, .pregFreq)
    pregnancyOverlapCounts <- safeCombine(pregnancyOverlapCounts, .pregOverlap)
    swappedDates <- safeCombine(swappedDates, .swapped)
    yearlyTrend <- safeCombine(yearlyTrend, .yearlyTrend)
    yearlyTrendMissing <- safeCombine(yearlyTrendMissing, .yearlyTrendMiss)
  }

  # Post-process PET comparison
  if (!is.null(petComparisonSummarisedResult)) {
    # Capture class and settings before dplyr operations strip them
    .pet_sr_class <- class(petComparisonSummarisedResult)
    .pet_sr_settings <- if (inherits(petComparisonSummarisedResult, "summarised_result")) {
      tryCatch(omopgenerics::settings(petComparisonSummarisedResult), error = function(e) NULL)
    } else NULL

    petComparisonSummarisedResult <- flattenListCols(petComparisonSummarisedResult)
    petComparisonSummarisedResult <- deduplicateSummarisedResult(petComparisonSummarisedResult)
    if ("group_name" %in% names(petComparisonSummarisedResult) && "group_level" %in% names(petComparisonSummarisedResult)) {
      petComparisonSummarisedResult <- petComparisonSummarisedResult %>% dplyr::mutate(
        group_level = dplyr::if_else(
          .data$group_name == "pet_comparison" & .data$group_level == "overall",
          "all", .data$group_level
        )
      )
    }

    # Restore summarised_result class stripped by dplyr/flattenListCols
    if ("summarised_result" %in% .pet_sr_class && !inherits(petComparisonSummarisedResult, "summarised_result")) {
      petComparisonSummarisedResult <- tibble::as_tibble(petComparisonSummarisedResult)
      class(petComparisonSummarisedResult) <- .pet_sr_class
      if (!is.null(.pet_sr_settings)) attr(petComparisonSummarisedResult, "settings") <- .pet_sr_settings
    }
  }
  if (!is.null(petUnmatchedLsc)) {
    .lsc_sr_class <- class(petUnmatchedLsc)
    .lsc_sr_settings <- if (inherits(petUnmatchedLsc, "summarised_result")) {
      tryCatch(omopgenerics::settings(petUnmatchedLsc), error = function(e) NULL)
    } else NULL

    petUnmatchedLsc <- flattenListCols(petUnmatchedLsc)
    petUnmatchedLsc <- deduplicateSummarisedResult(petUnmatchedLsc)

    if ("summarised_result" %in% .lsc_sr_class && !inherits(petUnmatchedLsc, "summarised_result")) {
      petUnmatchedLsc <- tibble::as_tibble(petUnmatchedLsc)
      class(petUnmatchedLsc) <- .lsc_sr_class
      if (!is.null(.lsc_sr_settings)) attr(petUnmatchedLsc, "settings") <- .lsc_sr_settings
    }
  }

  # Normalise gestational_age_days_per_category_summary column names
  if (!is.null(gestationalAgeDaysPerCategorySummary)) {
    nc <- colnames(gestationalAgeDaysPerCategorySummary)
    nc_lower <- tolower(trimws(gsub("[^a-z0-9]", "", nc)))
    epCol <- nc[nc_lower == "episodecount" | tolower(nc) == "episode_count"][1L]
    pcCol <- nc[nc_lower == "personcount" | tolower(nc) == "person_count"][1L]
    renames <- character(0)
    if (length(epCol) == 1L && !is.na(epCol) && epCol != "episode_count") renames["episode_count"] <- epCol
    if (length(pcCol) == 1L && !is.na(pcCol) && pcCol != "person_count") renames["person_count"] <- pcCol
    if (length(renames) > 0) gestationalAgeDaysPerCategorySummary <- dplyr::rename(gestationalAgeDaysPerCategorySummary, dplyr::all_of(renames))
    if ("episode_count" %in% colnames(gestationalAgeDaysPerCategorySummary)) gestationalAgeDaysPerCategorySummary$episode_count <- suppressWarnings(as.integer(gestationalAgeDaysPerCategorySummary$episode_count))
    if ("person_count" %in% colnames(gestationalAgeDaysPerCategorySummary)) gestationalAgeDaysPerCategorySummary$person_count <- suppressWarnings(as.integer(gestationalAgeDaysPerCategorySummary$person_count))
  }

  # Remove cdm_data_hash from cdm_source if present
  if (!is.null(cdmSource) && "cdm_data_hash" %in% colnames(cdmSource)) {
    cdmSource <- cdmSource %>% dplyr::select(-"cdm_data_hash")
  }

  # Rename attrition to attrition_episodes for downstream consistency
  attrition_episodes <- attrition
  attrition <- NULL

  # Build data list and cache
  data <- list(
    cdmSource = cdmSource,
    incidence = incidence,
    prevalence = prevalence,
    characteristics = characteristics,
    petComparisonSummarisedResult = petComparisonSummarisedResult,
    petUnmatchedLsc = petUnmatchedLsc,
    ageSummary = ageSummary,
    ageSummaryFirstPregnancy = ageSummaryFirstPregnancy,
    ageSummaryFirstPregnancyEnd = ageSummaryFirstPregnancyEnd,
    ageSummaryGroups = ageSummaryGroups,
    attrition_episodes = attrition_episodes,
    attritionIfCleanup = attritionIfCleanup,
    conceptCheck = conceptCheck,
    deliveryModeSummary = deliveryModeSummary,
    deliveryModeByYear = deliveryModeByYear,
    episodeFrequency = episodeFrequency,
    episodeFrequencySummary = episodeFrequencySummary,
    esdConceptCounts = esdConceptCounts,
    gestationalAgeDaysCounts = gestationalAgeDaysCounts,
    gestationalAgeDaysPerCategorySummary = gestationalAgeDaysPerCategorySummary,
    gestationalAgeDaysSummary = gestationalAgeDaysSummary,
    gestationalWeeks = gestationalWeeks,
    hipConceptCounts = hipConceptCounts,
    missingDates = missingDates,
    monthlyTrendMissing = monthlyTrendMissing,
    monthlyTrends = monthlyTrends,
    observationPeriodRange = observationPeriodRange,
    outcomeCategoriesCount = outcomeCategoriesCount,
    ppsConceptCounts = ppsConceptCounts,
    precisionDays = precisionDays,
    precisionDaysDenominators = precisionDaysDenominators,
    pregnancyFrequency = pregnancyFrequency,
    pregnancyOverlapCounts = pregnancyOverlapCounts,
    qualityCheckCleanup = qualityCheckCleanup,
    swappedDates = swappedDates,
    yearlyTrend = yearlyTrend,
    yearlyTrendMissing = yearlyTrendMissing
  )

  data[["_dataFolder"]] <- dataFolder
  readr::write_rds(data, cachePath)
  message("Cache written to: ", cachePath)
  dataNames <- setdiff(names(data), "_dataFolder")
  purrr::walk(dataNames, ~assign(., data[[.]], envir = .GlobalEnv))
} else {
  message("Loading data from cache: ", cachePath)
  data <- readr::read_rds(cachePath)
  dataNames <- setdiff(names(data), "_dataFolder")
  purrr::walk(dataNames, ~assign(., data[[.]], envir = .GlobalEnv))
}

hasData <- !is.null(cdmSource) && is.data.frame(cdmSource) && nrow(cdmSource) > 0

############################ Data transformations ############################

if (hasData) {
  dbinfo <- cdmSource %>%
    dplyr::select_if(~ !all(is.na(.))) %>%
    dplyr::arrange(cdm_name)

  allDP <- unique(dbinfo$cdm_name)
  allDP <- allDP[order(allDP)]

  # Available versions (derived from data)
  allVersions <- sort(unique(dbinfo$version))
  if (length(allVersions) == 0) allVersions <- "v3"

  # Ensure tables exist (empty if not loaded)
  if (is.null(gestationalAgeDaysCounts)) gestationalAgeDaysCounts <- tibble::tibble(cdm_name = character(0), less_1day = character(0), over_308days = character(0))
  if (is.null(gestationalAgeDaysSummary)) gestationalAgeDaysSummary <- tibble::tibble(colName = character(0), cdm_name = character(0), min = numeric(0), Q25 = numeric(0), median = numeric(0), Q75 = numeric(0), max = numeric(0), mean = numeric(0), sd = numeric(0))
  if (is.null(gestationalAgeDaysPerCategorySummary)) gestationalAgeDaysPerCategorySummary <- tibble::tibble(cdm_name = character(0), final_outcome_category = character(0), colName = character(0), min = numeric(0), Q25 = numeric(0), median = numeric(0), Q75 = numeric(0), max = numeric(0), mean = numeric(0), sd = numeric(0), person_count = integer(0), episode_count = integer(0))
  if (is.null(gestationalWeeks)) gestationalWeeks <- tibble::tibble(cdm_name = character(0), final_outcome_category = character(0), gestational_weeks = numeric(0), n = numeric(0), pct = numeric(0))
  if (is.null(outcomeCategoriesCount) || !is.data.frame(outcomeCategoriesCount) || nrow(outcomeCategoriesCount) == 0) outcomeCategoriesCount <- tibble::tibble(cdm_name = character(0), outcome_category = character(0), algorithm = character(0), n = numeric(0), pct = numeric(0))
  if (is.null(deliveryModeSummary) || !is.data.frame(deliveryModeSummary) || nrow(deliveryModeSummary) == 0) deliveryModeSummary <- tibble::tibble(cdm_name = character(0), final_outcome_category = character(0), mode = character(0), total = numeric(0), n = numeric(0), pct = numeric(0))
  if (is.null(deliveryModeByYear) || !is.data.frame(deliveryModeByYear) || nrow(deliveryModeByYear) == 0) deliveryModeByYear <- tibble::tibble(cdm_name = character(0), year = integer(0), final_outcome_category = character(0), mode = character(0), total = numeric(0), n = numeric(0), pct = numeric(0))
  if (is.null(missingDates) || !is.data.frame(missingDates) || nrow(missingDates) == 0) missingDates <- tibble::tibble(cdm_name = character(0))

  if (nrow(gestationalAgeDaysCounts) > 0) {
    gestationalAgeDaysCounts <- dataToLong(gestationalAgeDaysCounts)
  } else {
    gestationalAgeDaysCounts <- tibble::tibble(name = character(0))
  }

  # Swapped dates transformation
  if (is.null(swappedDates)) swappedDates <- tibble::tibble(cdm_name = character(0))
  if (is.null(pregnancyOverlapCounts)) pregnancyOverlapCounts <- tibble::tibble(cdm_name = character(0), colName = character(0), n = character(0), total = character(0), pct = character(0))
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
  if (is.null(ageSummary) || nrow(ageSummary) == 0) {
    ageSummary <- tibble::tibble(cdm_name = character(0), colName = character(0))
  }
  ageSummaryRaw <- ageSummary

  # Missing dates transformation
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
        tidyr::pivot_longer(-"cdm_name", names_to = "key", values_to = "value") %>%
        dplyr::mutate(
          value = suppressWarnings(as.numeric(.data$value)),
          name = gsub("_n$|_pct$", "", .data$key),
          metric = ifelse(grepl("_n$", .data$key), "n_missing", "percent_missing")
        ) %>%
        dplyr::select(-"key") %>%
        tidyr::pivot_wider(names_from = "metric", values_from = "value", values_fn = dplyr::first) %>%
        dplyr::mutate(percent_missing = round(suppressWarnings(as.numeric(.data$percent_missing)), 2))
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
        tidyr::pivot_longer(-"name", names_to = "cdm_name", values_to = "percent_missing") %>%
        dplyr::mutate(
          percent_missing = round(100 * suppressWarnings(as.numeric(.data$percent_missing)), 2),
          n_missing = NA_integer_
        )
    }
    missingDates <- missingDates %>%
      dplyr::rename(
        "Date field" = "name",
        "N missing" = "n_missing",
        "Percent missing" = "percent_missing"
      ) %>%
      dplyr::select(dplyr::any_of("cdm_name"), "Date field", "N missing", "Percent missing")
  }

  if (is.null(observationPeriodRange)) observationPeriodRange <- tibble::tibble(cdm_name = character(0))
  observationPeriodRange <- dataToLong(observationPeriodRange)

  # Ensure pregnancy frequency and episode frequency exist
  if (is.null(pregnancyFrequency) || !is.data.frame(pregnancyFrequency)) pregnancyFrequency <- tibble::tibble(freq = integer(0), number_individuals = numeric(0), cdm_name = character(0))
  if (is.null(episodeFrequencySummary) || !is.data.frame(episodeFrequencySummary)) episodeFrequencySummary <- tibble::tibble(cdm_name = character(0), colName = character(0))
  if (is.null(episodeFrequency) || !is.data.frame(episodeFrequency)) episodeFrequency <- tibble::tibble(cdm_name = character(0))

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
    dplyr::ungroup() %>%
    dplyr::group_by(.data$freq, .data$cdm_name) %>%
    dplyr::summarise(number_individuals = dplyr::first(.data$number_individuals), .groups = "drop")

  episodeFrequency <- dataToLong(
    episodeFrequency %>% dplyr::left_join(episodeFrequencySummary, relationship = "many-to-many"),
    skipCols = c("cdm_name", "colName")
  )

  if (nrow(gestationalAgeDaysSummary) > 0) {
    gestationalAgeDaysSummary <- dataToLong(gestationalAgeDaysSummary, skipCols = c("colName", "cdm_name"))
  } else {
    gestationalAgeDaysSummary <- tibble::tibble(name = character(0))
  }
  gestationalAgeDaysPerCategorySummary <- gestationalAgeDaysPerCategorySummary %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of(c("cdm_name", "final_outcome_category", "colName", "version", "date_export", "pkg_version")), as.numeric)) %>%
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
    dplyr::slice(1L) %>%
    unlist(use.names = FALSE) %>%
    as.numeric() %>%
    min(na.rm = TRUE)

  pregnancyOverlapCounts <- pregnancyOverlapCounts %>%
    dplyr::select(-dplyr::any_of("colName")) %>%
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
    # Suppressed counts (e.g. <= minCellCount exported as "<5") become NA when read; treat as 0 when summing bins
    out <- data %>%
      dplyr::filter(gestational_weeks >= lowerBoundary & gestational_weeks < upperBoundary) %>%
      dplyr::select(-"gestational_weeks")
    if (hasOutcomeInWeeks) {
      out <- out %>%
        dplyr::group_by(cdm_name, final_outcome_category) %>%
        dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
        dplyr::group_by(cdm_name) %>%
        dplyr::mutate(pct = round(100 * .data$n / sum(.data$n, na.rm = TRUE), 1)) %>%
        dplyr::ungroup()
    } else {
      out <- out %>%
        dplyr::group_by(cdm_name) %>%
        dplyr::summarise(n = sum(.data$n, na.rm = TRUE), pct = sum(.data$pct, na.rm = TRUE), .groups = "drop")
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


  # Round gestational age to integer weeks and aggregate counts for non-overlapping bins
  # Convention: 308 days (exactly 44.0 weeks) is counted as 43 weeks
  gestationalWeeksForBins <- gestationalWeeks %>%
    dplyr::mutate(
      gestational_weeks = round(.data$gestational_weeks),
      gestational_weeks = dplyr::if_else(.data$gestational_weeks == 44, 43, .data$gestational_weeks)
    )
  if (hasOutcomeInWeeks) {
    gestationalWeeksForBins <- gestationalWeeksForBins %>%
      dplyr::group_by(.data$cdm_name, .data$final_outcome_category, .data$gestational_weeks) %>%
      dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop")
  } else {
    gestationalWeeksForBins <- gestationalWeeksForBins %>%
      dplyr::group_by(.data$cdm_name, .data$gestational_weeks) %>%
      dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(.data$cdm_name) %>%
      dplyr::mutate(pct = round(100 * .data$n / sum(.data$n, na.rm = TRUE), 1)) %>%
      dplyr::ungroup()
  }

  # Non-overlapping bins: [lower, upper) in integer weeks
  # Note: week 44 (308 days) has been remapped to 43 above, so 42-43 includes 308d
  binnedLevels <- c("<12", "12-27", "28-31", "32-36", "37-38", "39-41", "42-43", "45-49", ">=50")
  gestationalWeeksBinned <- rbind(
    summariseGestationalWeeks(gestationalWeeksForBins, 0, 12, "<12"),
    summariseGestationalWeeks(gestationalWeeksForBins, 12, 28, "12-27"),
    summariseGestationalWeeks(gestationalWeeksForBins, 28, 32, "28-31"),
    summariseGestationalWeeks(gestationalWeeksForBins, 32, 37, "32-36"),
    summariseGestationalWeeks(gestationalWeeksForBins, 37, 39, "37-38"),
    summariseGestationalWeeks(gestationalWeeksForBins, 39, 42, "39-41"),
    summariseGestationalWeeks(gestationalWeeksForBins, 42, 44, "42-43"),
    summariseGestationalWeeks(gestationalWeeksForBins, 45, 50, "45-49"),
    summariseGestationalWeeks(gestationalWeeksForBins, 50, maxWeeks, ">=50")
  )

  gestationalWeeksBinned <- gestationalWeeksBinned %>%
    dplyr::mutate(
      gestational_weeks = factor(gestational_weeks, levels = binnedLevels),
      pct = round(pct, 1)
    )

  # Binary plausibility: ≤44 weeks = plausible, >44 weeks = implausible
  plausibilityLevels <- c("Plausible", "Implausible")
  gestationalWeeksPlausibility <- rbind(
    summariseGestationalWeeks(gestationalWeeksForBins, -Inf, 45, plausibilityLevels[1]),
    summariseGestationalWeeks(gestationalWeeksForBins, 45, maxWeeks, plausibilityLevels[2])
  ) %>%
    dplyr::rename(plausibility = gestational_weeks) %>%
    dplyr::mutate(plausibility = factor(plausibility, levels = plausibilityLevels))

  # Recalculate pct within plausibility groups (per cdm, optionally per outcome)
  if (hasOutcomeInWeeks) {
    gestationalWeeksPlausibility <- gestationalWeeksPlausibility %>%
      dplyr::group_by(cdm_name, final_outcome_category) %>%
      dplyr::mutate(pct = round(100 * n / sum(n, na.rm = TRUE), 1)) %>%
      dplyr::ungroup()
  } else {
    gestationalWeeksPlausibility <- gestationalWeeksPlausibility %>%
      dplyr::group_by(cdm_name) %>%
      dplyr::mutate(pct = round(100 * n / sum(n, na.rm = TRUE), 1)) %>%
      dplyr::ungroup()
  }

  # Trend data
  emptyTrend <- tibble::tibble(cdm_name = character(0), column = character(0), count = numeric(0), value = numeric(0), period = character(0))
  if (is.null(yearlyTrend)) yearlyTrend <- tibble::tibble(cdm_name = character(0), column = character(0), count = character(0), year = character(0))
  if (is.null(yearlyTrendMissing)) yearlyTrendMissing <- tibble::tibble(cdm_name = character(0), column = character(0), count = character(0), year = character(0))
  if (is.null(monthlyTrends)) monthlyTrends <- tibble::tibble(cdm_name = character(0), column = character(0), count = character(0), month = character(0))
  if (is.null(monthlyTrendMissing)) monthlyTrendMissing <- tibble::tibble(cdm_name = character(0), column = character(0), count = character(0), month = character(0))

  if (nrow(yearlyTrend) > 0) {
    yearlyTrend <- yearlyTrend %>%
      dplyr::mutate(count = as.numeric(count), year = as.numeric(year), period = "year") %>%
      dplyr::rename(value = year)
  } else {
    yearlyTrend <- emptyTrend
  }
  if (nrow(yearlyTrendMissing) > 0) {
    yearlyTrendMissing <- yearlyTrendMissing %>%
      dplyr::mutate(count = as.numeric(count), year = as.numeric(year), period = "year") %>%
      dplyr::rename(value = year)
  } else {
    yearlyTrendMissing <- emptyTrend
  }
  if (nrow(monthlyTrends) > 0) {
    monthlyTrends <- monthlyTrends %>%
      dplyr::mutate(count = as.numeric(count), period = "month") %>%
      dplyr::rename(value = month)
  } else {
    monthlyTrends <- emptyTrend
  }
  if (nrow(monthlyTrendMissing) > 0) {
    monthlyTrendMissing <- monthlyTrendMissing %>%
      dplyr::mutate(count = as.numeric(count), period = "month") %>%
      dplyr::rename(value = month)
  } else {
    monthlyTrendMissing <- emptyTrend
  }

  trendData <- rbind(yearlyTrend, monthlyTrends)
  trendDataMissing <- rbind(yearlyTrendMissing, monthlyTrendMissing)

  # Transform outcome categories
  if (nrow(outcomeCategoriesCount) > 0) {
    if ("n" %in% colnames(outcomeCategoriesCount)) outcomeCategoriesCount <- outcomeCategoriesCount %>% dplyr::mutate(n = suppressWarnings(as.numeric(.data$n)))
    if ("pct" %in% colnames(outcomeCategoriesCount)) outcomeCategoriesCount <- outcomeCategoriesCount %>% dplyr::mutate(pct = round(suppressWarnings(as.numeric(.data$pct)), 4))
    if ("outcome_category" %in% colnames(outcomeCategoriesCount)) {
      outcomeCategoriesCount <- outcomeCategoriesCount %>% dplyr::mutate(outcome_category = factor(.data$outcome_category, levels = c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG", "NA")))
    }
  }

  # Transform delivery mode
  deliveryModeRequired <- c("final_outcome_category", "n", "cesarean", "vaginal", "cesarean_pct", "vaginal_pct")
  if (nrow(deliveryModeSummary) > 0 && all(deliveryModeRequired %in% colnames(deliveryModeSummary))) {
    deliveryModeSummary <- deliveryModeSummary %>%
      dplyr::filter(final_outcome_category %in% c("DELIV", "LB")) %>%
      dplyr::select(-dplyr::any_of(c("cesarean_count", "vaginal_count"))) %>%
      dplyr::rename(total = n)
    if ("n_known" %in% colnames(deliveryModeSummary)) {
      deliveryModeSummary <- deliveryModeSummary %>% dplyr::mutate(n_known = suppressWarnings(as.numeric(.data$n_known)))
    } else {
      deliveryModeSummary <- deliveryModeSummary %>%
        dplyr::mutate(
          cesarean = suppressWarnings(as.numeric(.data$cesarean)),
          vaginal = suppressWarnings(as.numeric(.data$vaginal)),
          n_known = .data$cesarean + .data$vaginal
        )
    }
    # One row per (cdm_name, final_outcome_category) so pivot + join is one-to-one (avoids many-to-many from bind_rows duplicates)
    deliveryModeSummary <- deliveryModeSummary %>%
      dplyr::distinct(.data$cdm_name, .data$final_outcome_category, .keep_all = TRUE)
    dmJoinCols <- c("cdm_name", "final_outcome_category", "total", "n_known", "mode")
    deliveryModeSummary <- dplyr::left_join(
      deliveryModeSummary %>%
        tidyr::pivot_longer(cols = c("cesarean", "vaginal"), names_to = "mode", values_to = "n"),
      deliveryModeSummary %>%
        dplyr::select(-dplyr::any_of(c("cesarean", "vaginal"))) %>%
        dplyr::rename(
          vaginal = vaginal_pct,
          cesarean = cesarean_pct
        ) %>%
        tidyr::pivot_longer(cols = c("cesarean", "vaginal"), names_to = "mode", values_to = "pct"),
      by = dmJoinCols
    ) %>%
      dplyr::mutate(dplyr::across(
        -dplyr::all_of(c("cdm_name", "final_outcome_category", "mode")),
        ~ suppressWarnings(as.numeric(.))
      )) %>%
      dplyr::mutate(pct = round(pct, 2)) %>%
      dplyr::select(c("cdm_name", "final_outcome_category", "mode", "total", "n_known", "n", "pct"))
  }

  # Transform delivery mode by year
  deliveryModeByYearRequired <- c("year", "final_outcome_category", "n", "cesarean", "vaginal", "cesarean_pct", "vaginal_pct")
  if (nrow(deliveryModeByYear) > 0 && all(deliveryModeByYearRequired %in% colnames(deliveryModeByYear))) {
    deliveryModeByYear <- deliveryModeByYear %>%
      dplyr::filter(final_outcome_category %in% c("DELIV", "LB")) %>%
      dplyr::select(-dplyr::any_of(c("cesarean_count", "vaginal_count"))) %>%
      dplyr::rename(total = n) %>%
      dplyr::mutate(year = suppressWarnings(as.integer(.data$year)))
    if ("n_known" %in% colnames(deliveryModeByYear)) {
      deliveryModeByYear <- deliveryModeByYear %>% dplyr::mutate(n_known = suppressWarnings(as.numeric(.data$n_known)))
    } else {
      deliveryModeByYear <- deliveryModeByYear %>%
        dplyr::mutate(
          cesarean = suppressWarnings(as.numeric(.data$cesarean)),
          vaginal = suppressWarnings(as.numeric(.data$vaginal)),
          n_known = .data$cesarean + .data$vaginal
        )
    }
    deliveryModeByYear <- deliveryModeByYear %>%
      dplyr::distinct(.data$cdm_name, .data$year, .data$final_outcome_category, .keep_all = TRUE)
    joinCols <- c("cdm_name", "year", "final_outcome_category", "total", "n_known", "mode")
    deliveryModeByYear <- dplyr::left_join(
      deliveryModeByYear %>%
        tidyr::pivot_longer(cols = c("cesarean", "vaginal"), names_to = "mode", values_to = "n"),
      deliveryModeByYear %>%
        dplyr::select(-dplyr::any_of(c("cesarean", "vaginal"))) %>%
        dplyr::rename(
          vaginal = vaginal_pct,
          cesarean = cesarean_pct
        ) %>%
        tidyr::pivot_longer(cols = c("cesarean", "vaginal"), names_to = "mode", values_to = "pct"),
      by = joinCols
    ) %>%
      dplyr::mutate(dplyr::across(
        -dplyr::all_of(c("cdm_name", "year", "final_outcome_category", "mode")),
        ~ suppressWarnings(as.numeric(.))
      )) %>%
      dplyr::mutate(pct = round(pct, 2)) %>%
      dplyr::select(c("cdm_name", "year", "final_outcome_category", "mode", "total", "n_known", "n", "pct"))
  }

  # Quality check cleanup
  if (!is.null(qualityCheckCleanup) && nrow(qualityCheckCleanup) > 0 && "cdm_name" %in% colnames(qualityCheckCleanup)) {
    qualityCheckCleanup <- qualityCheckCleanup %>% dplyr::select("cdm_name", dplyr::everything())
  }
  if (is.null(precisionDaysDenominators)) precisionDaysDenominators <- tibble::tibble(cdm_name = character(0))

  # Rename attrition_episodes for downstream
  attritionEpisodes <- attrition_episodes

  ############################ Feature flags ############################

  defaultPlotHeight <- "400px"
  plotHeight <- "600px"

  hasRows <- function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0

  has_incidence <- hasRows(incidence)
  has_prevalence <- hasRows(prevalence)
  has_characteristics <- hasRows(characteristics)
  has_attrition <- hasRows(attritionEpisodes)
  has_attrition_cleanup <- hasRows(attritionIfCleanup)
  has_esd_concepts <- hasRows(esdConceptCounts)
  has_hip_concepts <- hasRows(hipConceptCounts)
  has_pps_concepts <- hasRows(ppsConceptCounts)
  has_concept_counts <- has_esd_concepts || has_hip_concepts || has_pps_concepts
  has_age_summary <- nrow(ageSummaryRaw) > 0
  has_age_first_pregnancy <- hasRows(ageSummaryFirstPregnancy)
  has_age_first_pregnancy_end <- hasRows(ageSummaryFirstPregnancyEnd)
  has_age_groups <- hasRows(ageSummaryGroups)
  has_age <- has_age_summary || has_age_first_pregnancy || has_age_first_pregnancy_end || has_age_groups
  has_pet_comparison_sr <- hasRows(petComparisonSummarisedResult)
  has_pet_unmatched_lsc <- hasRows(petUnmatchedLsc)

  # Version differences (static reference CSV)
  versionDiffPath <- file.path(getwd(), "data", "version_differences.csv")
  if (file.exists(versionDiffPath)) {
    versionDifferences <- readr::read_csv(versionDiffPath, col_types = "ccc",
                                           show_col_types = FALSE)
    has_version_diff <- nrow(versionDifferences) > 0
  } else {
    versionDifferences <- tibble::tibble(old_version = character(0),
                                          new_version = character(0),
                                          difference_explanation = character(0))
    has_version_diff <- FALSE
  }

  # National statistics comparison
  natlStatsPath <- file.path(getwd(), "National_Statistics_Obj2_v2.csv")
  if (!file.exists(natlStatsPath)) {
    natlStatsPath <- system.file("shiny", "National_Statistics_Obj2_v2.csv",
                                  package = "PregnancyIdentifier")
  }
  has_national_stats <- file.exists(natlStatsPath) && nzchar(natlStatsPath)

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
    obj <- tryCatch(get(v, envir = .GlobalEnv), error = function(e) NULL)
    hasRows(obj)
  }, logical(1)))
  has_pet <- has_pet_comparison_sr || has_pet_legacy
  has_ip <- has_incidence || has_prevalence
}
