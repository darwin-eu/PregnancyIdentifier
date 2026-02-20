handleEmptyResult <- function(object, result, emptyMessage = "No data available") {
  if (is.null(result)) {
    return(DarwinShinyModules::Text$new(markdown = emptyMessage))
  }
  if (nrow(result) == 0) {
    return(DarwinShinyModules::Text$new(markdown = emptyMessage))
  } else {
    return(object)
  }
}

#' Plotly placeholder when there is no data to display
#' @param message Text to show (e.g. "Results files are empty." or "No data for selected filters.")
#' @return A plotly object with the message as annotation
emptyPlotlyMessage <- function(message = "No data to display.") {
  plotly::plot_ly() %>%
    plotly::add_annotations(
      text = message,
      x = 0.5, y = 0.5, xref = "paper", yref = "paper",
      showarrow = FALSE, font = list(size = 16)
    ) %>%
    plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
}

#' Wrap a Shiny module with short helper text shown at the top of the tab.
#' @param module Module object with UI() and server() (e.g. from handleEmptyResult).
#' @param text One or two sentences explaining what the tab shows.
#' @return An object that can be used in appStructure in place of the module.
tabWithHelpText <- function(module, text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(module)
  }
  inner <- module
  desc <- trimws(text)
  helpUi <- if (requireNamespace("markdown", quietly = TRUE)) {
    shiny::div(
      class = "tab-help-text",
      style = "margin-bottom: 1em; color: #555; font-size: 0.95em;",
      htmltools::HTML(markdown::mark(text = desc))
    )
  } else {
    shiny::div(
      class = "tab-help-text",
      style = "margin-bottom: 1em; color: #555; font-size: 0.95em;",
      shiny::p(desc)
    )
  }
  wrapper <- R6::R6Class(
    "TabWithHelpText",
    portable = TRUE,
    public = list(
      UI = function() {
        shiny::tagList(helpUi, inner$UI())
      },
      server = function(input, output, session) {
        if (is.function(inner$server)) {
          inner$server(input, output, session)
        }
      }
    ),
    active = list(
      namespace = function() inner$namespace,
      parentNamespace = function(value) {
        if (missing(value)) inner$parentNamespace else inner$parentNamespace <- value
      }
    )
  )
  wrapper$new()
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

loadFile <- function(file, dbName, runDate, zipVersion, folder, overwrite) {
  if (endsWith(file, ".csv")) {
    print(file)
    tableName <- gsub(".csv$", "", file)
    camelCaseName <- snakeCaseToCamelCase(tableName)
    if (camelCaseName == "attrition") {
      camelCaseName <- "attrition_episodes"
    }

    if (grepl("incidence|prevalence|characteristics", tolower(file))) {
      parts <- unlist(strsplit(tableName, "_"))
      tableName <- parts[length(parts)]
      camelCaseName <- snakeCaseToCamelCase(tableName)
      data <- omopgenerics::importSummarisedResult(file.path(folder, file))
      data <- data %>% dplyr::mutate(across(where(lubridate::is.Date), as.character))
    } else {
      data <- readr::read_csv(file.path(folder, file), col_types = readr::cols(.default = "c"), guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"))
      if ("...1" %in% colnames(data)) {
        data <- data %>%
          dplyr::select(-"...1")
      }
      # make sure there is a column cdm_name
      if (!"cdm_name" %in% colnames(data)) {
        data <- data %>% dplyr::mutate(cdm_name = dbName)
      }
      if (file == "cdm_source.csv" && "cdm_data_hash" %in% colnames(data)) {
        data <- data %>% dplyr::select(-"cdm_data_hash")
      }
    }

    # add version number to cdm_name
    version <- NULL
    if (!is.null(zipVersion)) {
      version <- paste0("_v", as.numeric(substr(zipVersion, 1, 1)))
    } else if ("pkg_version" %in% colnames(data)) {
      version <- data %>% dplyr::pull("pkg_version") %>% unique()
      version <- paste0("_v", as.numeric(substr(version, 1, 1)))
      data <- data %>% dplyr::select(-"pkg_version")
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

    data <- data %>%
      dplyr::select(-dplyr::any_of(c("date_run", "date_export"))) %>%
      dplyr::mutate(cdm_name = dplyr::if_else(cdm_name == "cdm", "EMBD-ULSGE", cdm_name)) %>%
      dplyr::mutate(cdm_name = paste0(cdm_name, version)) %>%
      dplyr::mutate(cdm_name = tolower(cdm_name))

    if (!overwrite && exists(camelCaseName, envir = .GlobalEnv)) {
      existingData <- get(camelCaseName, envir = .GlobalEnv)
      if (nrow(existingData) > 0) {
        if (nrow(data) > 0 &&
            all(colnames(existingData) %in% colnames(data)) &&
            all(colnames(data) %in% colnames(existingData))) {
          data <- data[, colnames(existingData)]
        }

        if (!isTRUE(all.equal(colnames(data), colnames(existingData), check.attributes = FALSE))) {
          stop(
            "Table columns do no match previously seen columns. Columns in ",
            file,
            ":\n",
            paste(colnames(data), collapse = ", "),
            "\nPrevious columns:\n",
            paste(colnames(existingData), collapse = ", ")
          )
        }
      }
      data <- rbind(existingData, data)
    }
    assign(camelCaseName, data, envir = .GlobalEnv)
    invisible(NULL)
  }
}

dataToLong <- function(data, skipCols = c("cdm_name")) {
  # Only skip columns that exist (e.g. age_summary has no colName)
  skipCols <- intersect(skipCols, colnames(data))
  do.call(cbind, lapply(unique(data$cdm_name), FUN = function(db) {
    dbData <- data %>% dplyr::filter(cdm_name == db)
    valColName <- as.character(db)[1L]
    return(dbData %>%
             tidyr::pivot_longer(cols = setdiff(colnames(.), skipCols), names_to = "name", values_to = valColName) %>%
             dplyr::select(-dplyr::any_of(skipCols)))
  })) %>% dplyr::select(unique(colnames(.)))
}

trendsPlot <- function(data, xVar, xLabel, facetVar = NULL, xIntercept = NULL) {
  # Ensure xLabel is a single string for labs()
  if (length(xLabel) > 1) xLabel <- xLabel[1]
  xLabel <- as.character(xLabel)
  if (!nzchar(xLabel)) xLabel <- "Period"
  p <- ggplot(data = data, mapping = aes(x = .data[[xVar]], y = .data[["count"]], color = .data[["column"]], group = .data[["column"]])) +
    geom_line() +
    geom_point() +
    labs(x = xLabel, y = "Count (N)") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6))

  if (!is.null(facetVar)) {
    p <- p + facet_wrap(as.formula(paste("~", facetVar)))
  }

  if (!is.null(xIntercept)) {
    p <- p + geom_vline(mapping = aes(xintercept = xIntercept), linetype = "dashed")
  }
  plotly::ggplotly(p)
}

barPlot <- function(data, xVar, yVar, fillVar = NULL, facetVar = NULL, labelFunction = NULL,
                    label = NULL, position = "dodge", xLabel = NULL, yLabel = NULL, title = NULL,
                    rotateAxisText = FALSE, flipCoordinates = FALSE, facetTextSize = 8, verticalLinesPos = NULL,
                    yLim = NULL) {
  if (!is.null(label)) {
    p <- ggplot(data = data, mapping = aes(x = .data[[xVar]], y = .data[[yVar]], label = .data[[label]]))
  } else {
    p <- ggplot(data = data, mapping = aes(x = .data[[xVar]], y = .data[[yVar]]))
  }

  if (is.null(fillVar)) {
    p <- p + geom_bar(stat = "identity",
                      position = position)
  } else {
    p <- p + geom_bar(stat = "identity",
                      position = position,
                      mapping = aes(fill = .data[[fillVar]]))
  }
  if (!is.null(facetVar)) {
    if (is.null(labelFunction)) {
      p <- p + facet_wrap(as.formula(paste("~", facetVar)))
    } else {
      p <- p + facet_wrap(as.formula(paste("~", facetVar)), labeller = labelFunction)
    }
  }

  if (!is.null(xLabel) && !is.null(yLabel)) {
    p <- p + labs(x = xLabel, y = yLabel)
  }
  if (rotateAxisText) {
    p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                   plot.title = element_text(hjust = 0.5),
                   strip.text = element_text(size = facetTextSize))
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  if (!is.null(yLim) && length(yLim) == 2L) {
    p <- p + coord_cartesian(ylim = yLim)
  }
  if (flipCoordinates) {
    p <- p + coord_flip()
  }
  if (!is.null(verticalLinesPos)) {
    for (pos in verticalLinesPos) {
      p <- p + geom_vline(xintercept = pos, colour = "red")
    }
  }
  plotly::ggplotly(p)
}

boxPlot <- function(data, facetVar = NULL, colorVar = NULL, transform = FALSE, gg_plot = FALSE, horizontal = FALSE) {
  plotData <- data
  if (transform) {
    # assume we have a column per DP, next to the first column
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

    ggplot(plotData, aes(x)) +
      geom_boxplot(aes(ymin = min, lower = Q25, middle = mean, upper = Q75, ymax = max),
                   stat = "identity") +
      facet_wrap(~ cdm_name)
  } else {
    p <- NULL
    if (horizontal) {
      # y = cdm_name (one row per CDM), x = age metric (box extent)
      p <- plot_ly(data = plotData, y = ~ cdm_name) %>%
        add_boxplot(
          lowerfence = ~ min,
          q1 = ~ Q25,
          median = ~ median,
          mean = ~ mean,
          sd = ~ sd,
          q3 = ~ Q75,
          upperfence = ~ max)
    } else {
      if (is.null(colorVar)) {
        p <- plot_ly(data = plotData,
                     x = ~ cdm_name)
      } else {
        p <- plot_ly(data = plotData,
                     x = ~ cdm_name,
                     color = ~ get(colorVar))
      }
      p <- p %>%
        add_boxplot(
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

customDarwinFooter <- function() {
  shiny::tags$footer(
    style = "padding: 3px 0px 0px 0px; text-align: center; bottom: 0; width: 100%;",
    shiny::h6(
      sprintf(
        "Generated with DarwinShinyModules %s | Deployed on: %s | (c) %s - 2023 European Medicines Agency. All rights reserved. Certain parts are licensed under conditions to the European Medicines Agency.",
        utils::packageVersion("DarwinShinyModules"),
        Sys.Date(),
        substr(Sys.Date(), start = 1, stop = 4)
      )
    )
  )
}

suppressCounts <- function(result, colNames, minCellCount = 5) {
  suppressCountCol <- function(values) {
    values[values > 0 & values < minCellCount] <- NA
    return(values)
  }
  # Match columns that exist (case-insensitive: data may have "ipci", allDP may have "IPCI")
  resultNames <- colnames(result)
  colNamesPresent <- resultNames[tolower(resultNames) %in% tolower(colNames)]
  if (length(colNamesPresent) == 0) {
    return(result)
  }
  result %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(colNamesPresent), suppressCountCol))
}
