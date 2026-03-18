# utils.R - Shared utility functions

#' Plotly placeholder when there is no data to display
emptyPlotlyMessage <- function(message = "No data to display.") {
  plotly::plot_ly() %>%
    plotly::add_annotations(
      text = message,
      x = 0.5, y = 0.5, xref = "paper", yref = "paper",
      showarrow = FALSE, font = list(size = 16)
    ) %>%
    plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
}

# copied from SQLRender, so we don't need to include this dependency (and rJava)
snakeCaseToCamelCase <- function(string) {
  string <- tolower(string)
  for (letter in letters) {
    string <- gsub(paste("_", letter, sep = ""), toupper(letter), string)
  }
  string <- gsub("_([0-9])", "\\1", string)
  return(string)
}

#' Flatten list columns in a data frame by taking the first element of each list.
flattenListCols <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0) return(data)
  list_cols <- names(data)[vapply(data, is.list, logical(1))]
  for (col in list_cols) {
    data[[col]] <- vapply(data[[col]], function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      x <- unlist(x, use.names = FALSE)
      if (length(x) == 0) return(NA_character_)
      as.character(x[1])
    }, FUN.VALUE = NA_character_)
  }
  data
}

#' Deduplicate a long-format summarised_result so pivotEstimates does not create list-cols.
deduplicateSummarisedResult <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0) return(data)
  if (!"estimate_name" %in% names(data) || !"estimate_value" %in% names(data)) return(data)
  key_cols <- setdiff(names(data), "estimate_value")
  key_cols <- key_cols[!vapply(data[key_cols], is.list, logical(1))]
  if (length(key_cols) == 0) return(data)
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()
}

loadFile <- function(file, dbName, runDate, zipVersion, folder, overwrite, envir = .GlobalEnv) {
  if (endsWith(file, ".csv")) {
    # Skip static metadata files that are not database results
    if (file == "version_differences.csv") return(invisible(NULL))
    message("Loading: ", file)
    tableName <- gsub(".csv$", "", file)
    camelCaseName <- snakeCaseToCamelCase(tableName)
    if (camelCaseName == "attrition") {
      camelCaseName <- "attrition_episodes"
    }
    if (file == "precision-days.csv") {
      camelCaseName <- "precisionDays"
    }
    if (file == "precision_days_denominators.csv") {
      camelCaseName <- "precisionDaysDenominators"
    }
    if (file == "date_consistency.csv") {
      camelCaseName <- "missingDates"
    }
    if (file == "outcome_categories_count.csv") {
      camelCaseName <- "outcomeCategoriesCount"
    }
    if (file == "delivery_mode_summary.csv") {
      camelCaseName <- "deliveryModeSummary"
    }
    if (file == "delivery_mode_by_year.csv") {
      camelCaseName <- "deliveryModeByYear"
    }

    if (file == "pet_comparison_summarised_result.csv") {
      data <- omopgenerics::importSummarisedResult(file.path(folder, file))
      data <- flattenListCols(data)
      data <- deduplicateSummarisedResult(data)
      if (is.data.frame(data) && "group_name" %in% names(data) && "group_level" %in% names(data)) {
        data <- data %>% dplyr::mutate(
          group_level = dplyr::if_else(
            .data$group_name == "pet_comparison" & .data$group_level == "overall",
            "all",
            .data$group_level
          )
        )
      }
      camelCaseName <- "petComparisonSummarisedResult"
    } else if (file == "pet_unmatched_lsc.csv") {
      data <- omopgenerics::importSummarisedResult(file.path(folder, file))
      data <- flattenListCols(data)
      data <- deduplicateSummarisedResult(data)
      camelCaseName <- "petUnmatchedLsc"
    } else if (grepl("incidence|prevalence|characteristics", tolower(file))) {
      parts <- unlist(strsplit(tableName, "_"))
      tableName <- parts[length(parts)]
      camelCaseName <- snakeCaseToCamelCase(tableName)
      data <- omopgenerics::importSummarisedResult(file.path(folder, file))
      # Preserve summarised_result class — don't flatten or deduplicate
    } else {
      data <- readr::read_csv(file.path(folder, file), col_types = readr::cols(.default = "c"), guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"))
      colnames(data) <- gsub("^['\"]*|['\"]*$", "", colnames(data))
      if ("...1" %in% colnames(data)) {
        if (file == "pregnancy_frequency.csv" && !"freq" %in% colnames(data)) {
          data <- data %>% dplyr::rename(freq = "...1")
        } else if (file == "episode_frequency.csv" && !"freq" %in% colnames(data)) {
          data <- data %>% dplyr::rename(freq = "...1")
        } else {
          data <- data %>% dplyr::select(-"...1")
        }
      }
      if (!"cdm_name" %in% colnames(data)) {
        data <- data %>% dplyr::mutate(cdm_name = dbName)
      }
      if (file == "cdm_source.csv" && "cdm_data_hash" %in% colnames(data)) {
        data <- data %>% dplyr::select(-"cdm_data_hash")
      }
      # Normalise count column names for gestational_age_days_per_category_summary (preserve episode_count, person_count)
      if (file == "gestational_age_days_per_category_summary.csv") {
        nc <- colnames(data)
        nc_lower <- tolower(trimws(gsub("[^a-z0-9]", "", nc)))
        epCol <- nc[nc_lower == "episodecount" | tolower(nc) == "episode_count"][1L]
        pcCol <- nc[nc_lower == "personcount" | tolower(nc) == "person_count"][1L]
        renames <- character(0)
        if (length(epCol) == 1L && !is.na(epCol) && epCol != "episode_count") renames["episode_count"] <- epCol
        if (length(pcCol) == 1L && !is.na(pcCol) && pcCol != "person_count") renames["person_count"] <- pcCol
        if (length(renames) > 0) data <- dplyr::rename(data, dplyr::all_of(renames))
      }
    }

    {
      isSR <- inherits(data, "summarised_result")

      version <- NULL
      if (!is.null(zipVersion)) {
        version <- paste0("_v", as.numeric(substr(zipVersion, 1, 1)))
        if (!isSR && "pkg_version" %in% colnames(data)) data <- data %>% dplyr::select(-"pkg_version")
      } else if ("pkg_version" %in% colnames(data)) {
        version <- data %>% dplyr::pull("pkg_version") %>% unique()
        version <- paste0("_v", as.numeric(substr(version, 1, 1)))
        if (!isSR) data <- data %>% dplyr::select(-"pkg_version")
      } else if (nzchar(trimws(runDate))) {
        runDateParsed <- suppressWarnings(as.Date(runDate))
        if (!is.na(runDateParsed)) {
          if (dplyr::between(runDateParsed, as.Date("2025-11-17"), as.Date("2025-11-30"))) {
            version <- "_v1"
          } else if (dplyr::between(runDateParsed, as.Date("2025-12-07"), as.Date("2026-02-16"))) {
            version <- "_v2"
          } else if (dplyr::between(runDateParsed, as.Date("2026-02-17"), as.Date("2026-03-31"))) {
            version <- "_v3"
          }
        }
      }

      if (isSR) {
        # Use base R assignment to preserve summarised_result class
        data$cdm_name <- tolower(paste0(
          ifelse(data$cdm_name == "cdm", "EMBD-ULSGE", data$cdm_name),
          version
        ))
      } else {
        data <- data %>%
          dplyr::select(-dplyr::any_of(c("date_run", "date_export"))) %>%
          dplyr::mutate(cdm_name = dplyr::if_else(cdm_name == "cdm", "EMBD-ULSGE", cdm_name)) %>%
          dplyr::mutate(cdm_name = sub("_v[0-9]+$", "", .data$cdm_name)) %>%
          dplyr::mutate(cdm_name = paste0(.data$cdm_name, version)) %>%
          dplyr::mutate(cdm_name = tolower(.data$cdm_name))
      }
      # Ensure episode_count and person_count are integer for gestational_age_days_per_category_summary
      if (file == "gestational_age_days_per_category_summary.csv" && !isSR) {
        if ("episode_count" %in% colnames(data)) data$episode_count <- suppressWarnings(as.integer(data$episode_count))
        if ("person_count" %in% colnames(data)) data$person_count <- suppressWarnings(as.integer(data$person_count))
      }
    }

    if (!overwrite && exists(camelCaseName, envir = envir)) {
      existingData <- get(camelCaseName, envir = envir)
      if (isSR && inherits(existingData, "summarised_result")) {
        data <- omopgenerics::bind(existingData, data)
      } else {
        data <- bindRowsAligned(existingData, data)
      }
    }
    assign(camelCaseName, data, envir = envir)
    invisible(NULL)
  }
}

dataToLong <- function(data, skipCols = c("cdm_name")) {
  skipCols <- intersect(skipCols, colnames(data))
  do.call(cbind, lapply(unique(data$cdm_name), FUN = function(db) {
    dbData <- data %>% dplyr::filter(cdm_name == db)
    valColName <- as.character(db)[1L]
    return(dbData %>%
             tidyr::pivot_longer(cols = setdiff(colnames(.), skipCols), names_to = "name", values_to = valColName) %>%
             dplyr::select(-dplyr::any_of(skipCols)))
  })) %>% dplyr::select(unique(colnames(.)))
}

trendsPlot <- function(data, xVar, xLabel, facetVar = NULL, xIntercept = NULL, return_ggplot = FALSE) {
  if (length(xLabel) > 1) xLabel <- xLabel[1]
  xLabel <- as.character(xLabel)
  if (!nzchar(xLabel)) xLabel <- "Period"
  p <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = .data[[xVar]], y = .data[["count"]], color = .data[["column"]], group = .data[["column"]])) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = xLabel, y = "Count (N)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 6))

  if (!is.null(facetVar)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facetVar)))
  }

  if (!is.null(xIntercept)) {
    p <- p + ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = xIntercept), linetype = "dashed")
  }
  if (return_ggplot) return(p)
  plotly::ggplotly(p)
}

barPlot <- function(data, xVar, yVar, fillVar = NULL, facetVar = NULL, labelFunction = NULL,
                    label = NULL, position = "dodge", xLabel = NULL, yLabel = NULL, title = NULL,
                    rotateAxisText = FALSE, flipCoordinates = FALSE, facetTextSize = 8, verticalLinesPos = NULL,
                    yLim = NULL, return_ggplot = FALSE) {
  if (!is.null(label)) {
    p <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = .data[[xVar]], y = .data[[yVar]], label = .data[[label]]))
  } else {
    p <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = .data[[xVar]], y = .data[[yVar]]))
  }

  if (is.null(fillVar)) {
    p <- p + ggplot2::geom_bar(stat = "identity", position = position)
  } else {
    p <- p + ggplot2::geom_bar(stat = "identity", position = position,
                                mapping = ggplot2::aes(fill = .data[[fillVar]]))
    if (exists("OUTCOME_COLOURS") && fillVar %in% c("final_outcome_category", "outcome_category")) {
      availCats <- intersect(names(OUTCOME_COLOURS), unique(as.character(data[[fillVar]])))
      if (length(availCats) > 0) {
        p <- p + ggplot2::scale_fill_manual(values = OUTCOME_COLOURS[availCats], na.value = "#CCCCCC")
      }
    }
  }
  if (!is.null(facetVar)) {
    if (is.null(labelFunction)) {
      p <- p + ggplot2::facet_wrap(as.formula(paste("~", facetVar)))
    } else {
      p <- p + ggplot2::facet_wrap(as.formula(paste("~", facetVar)), labeller = labelFunction)
    }
  }
  if (!is.null(xLabel) && !is.null(yLabel)) {
    p <- p + ggplot2::labs(x = xLabel, y = yLabel)
  }
  if (rotateAxisText) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
                             plot.title = ggplot2::element_text(hjust = 0.5),
                             strip.text = ggplot2::element_text(size = facetTextSize))
  }
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }
  p <- p + ggplot2::theme_minimal()

  if (flipCoordinates) {
    if (!is.null(yLim) && length(yLim) == 2L) {
      p <- p + ggplot2::coord_flip(ylim = yLim)
    } else {
      p <- p + ggplot2::coord_flip()
    }
  } else if (!is.null(yLim) && length(yLim) == 2L) {
    p <- p + ggplot2::coord_cartesian(ylim = yLim)
  }
  if (!is.null(verticalLinesPos)) {
    for (pos in verticalLinesPos) {
      p <- p + ggplot2::geom_vline(xintercept = pos, colour = "red")
    }
  }
  if (return_ggplot) return(p)
  plotly::ggplotly(p)
}

boxPlot <- function(data, facetVar = NULL, colorVar = NULL, transform = FALSE, gg_plot = FALSE, horizontal = FALSE) {
  plotData <- data
  if (transform) {
    nameColumn <- colnames(data)[1]
    dpCols <- colnames(data)[-1]
    plotData <- do.call(rbind, lapply(dpCols, FUN = function(dp) {
      dbData <- data %>% dplyr::select(dplyr::all_of(c(nameColumn, dp)))
      dbData %>%
        tidyr::pivot_wider(names_from = nameColumn, values_from = dp) %>%
        dplyr::mutate(cdm_name = dp)
      }))
  }

  if (gg_plot) {
    plotData <- plotData %>%
      dplyr::mutate(min = as.numeric(min),
                    Q25 = as.numeric(Q25),
                    mean = as.numeric(mean),
                    Q75 = as.numeric(Q75),
                    max = as.numeric(max),
                    x = 1)
    ggplot2::ggplot(plotData, ggplot2::aes(x)) +
      ggplot2::geom_boxplot(ggplot2::aes(ymin = min, lower = Q25, middle = mean, upper = Q75, ymax = max),
                   stat = "identity") +
      ggplot2::facet_wrap(~ cdm_name)
  } else {
    p <- NULL
    if (horizontal) {
      p <- plotly::plot_ly(data = plotData, y = ~ cdm_name) %>%
        plotly::add_boxplot(
          lowerfence = ~ min,
          q1 = ~ Q25,
          median = ~ median,
          mean = ~ mean,
          sd = ~ sd,
          q3 = ~ Q75,
          upperfence = ~ max)
    } else {
      if (is.null(colorVar)) {
        p <- plotly::plot_ly(data = plotData, x = ~ cdm_name)
      } else {
        p <- plotly::plot_ly(data = plotData, x = ~ cdm_name, color = ~ get(colorVar))
      }
      p <- p %>%
        plotly::add_boxplot(
          lowerfence = ~ min,
          q1 = ~ Q25,
          median = ~ median,
          mean = ~ mean,
          sd = ~ sd,
          q3 = ~ Q75,
          upperfence = ~ max)
    }
    p
  }
}

suppressCounts <- function(result, colNames, minCellCount = 5) {
  suppressCountCol <- function(values) {
    values[values > 0 & values < minCellCount] <- NA
    return(values)
  }
  resultNames <- colnames(result)
  colNamesPresent <- resultNames[tolower(resultNames) %in% tolower(colNames)]
  if (length(colNamesPresent) == 0) {
    return(result)
  }
  result %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(colNamesPresent), suppressCountCol))
}
