handleEmptyResult <- function(object, result) {
  if (is.null(result)) {
    return(DarwinShinyModules::Text$new(markdown = "No data available"))
  }
  if (nrow(result) == 0) {
    return(DarwinShinyModules::Text$new(markdown = "No data available"))
  } else {
    return(object)
  }
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

loadFile <- function(file, dbName, folder, overwrite) {
  if (endsWith(file, ".csv")) {
    print(file)
    tableName <- gsub(".csv$", "", file)
    camelCaseName <- snakeCaseToCamelCase(tableName)

    if (grepl("incidence", tolower(file))) {
      data <- omopgenerics::importSummarisedResult(file.path(folder, file))
      data <- data %>% dplyr::mutate(across(where(lubridate::is.Date), as.character))
    } else {
      data <- readr::read_csv(file.path(folder, file), col_types = readr::cols(.default = "c"), guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"))
      if ("...1" %in% colnames(data)) {
        data <- data %>%
          dplyr::select(-"...1")
      }
      # make sure there is a column cdm_name
      data <- data %>% dplyr::mutate(cdm_name= dbName, .before = 1)
    }
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

trendsPlot <- function(data, xVar, xLabel, xIntercept = NULL) {
  p <- ggplot(data = data, mapping = aes_string(x = xVar, y = "count", color = "column", group = "column")) +
    geom_line() +
    geom_point() +
    labs(x = xLabel, y = "Count (N)") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

  if (!is.null(xIntercept)) {
    p <- p + geom_vline(mapping = aes(xintercept = xIntercept), linetype = "dashed")
  }
  plotly::ggplotly(p)
}

barPlot <- function(data, xVar, yVar, fillVar = NULL, label = NULL, position = "stack", xLabel = NULL, yLabel = NULL, title = NULL, rotateAxisText = FALSE, flipCoordinates = FALSE) {
  p <- ggplot(data = data,
              mapping = aes_string(x = xVar, y = yVar, label = label))

  if (is.null(fillVar)) {
    p <- p + geom_bar(stat = "identity",
                      position = position)
  } else {
    p <- p + geom_bar(stat = "identity",
                      position = position,
                      mapping = aes_string(fill = fillVar))
  }

  if (!is.null(xLabel) && !is.null(yLabel)) {
    p <- p + labs(x = xLabel, y = yLabel)
  }
  if (rotateAxisText) {
    p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5))
  }
  if (!is.null(ggtitle)) {
    p <- p + ggtitle(title)
  }
  if (flipCoordinates) {
    p <- p + coord_flip()
  }
  plotly::ggplotly(p)
}

boxPlot <- function(data, colorVar = NULL, transform = FALSE) {
  plotData <- data
  if (transform) {
    # assume we have a column per DP, next to the first column
    nameColumn <- colnames(data)[1]
    dpCols <- colnames(data)[-1]
    plotData <- data %>%
      tidyr::pivot_wider(names_from = nameColumn, values_from = dpCols) %>%
      dplyr::mutate(cdm_name = dpCols)
  }

  p <- NULL
  if (is.null(colorVar)) {
    p <- plot_ly(data = plotData,
                 x = ~ cdm_name)
  } else {
    p <- plot_ly(data = plotData,
                 x = ~ cdm_name,
                 color = ~ get(colorVar))
  }
  p %>%
    add_boxplot(
      lowerfence = ~ min,
      q1 = ~ Q25,
      median = ~ median,
      mean = ~ mean,
      sd = ~ sd,
      q3 = ~ Q75,
      upperfence = ~ max)
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
