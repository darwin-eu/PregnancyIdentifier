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

yearTrendsPlot <- function(data, xIntercept) {
  p <- ggplot(data = data, mapping = aes(x = year, y = count, color = column, group = column)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Count (N)") +
    geom_vline(mapping = aes(xintercept = xIntercept), linetype = "dashed") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  plotly::ggplotly(p)
}

monthTrendsPlot <- function(data) {
  p <- ggplot(data = data, mapping = aes(x = month, y = count, color = column, group = column)) +
    geom_line() +
    geom_point() +
    labs(x = "Month", y = "Count (N)") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  plotly::ggplotly(p)
}

gestationalDurationPlot <- function(data) {
  p <- ggplot(data = data, mapping = aes(x = gestational_weeks, y = log(n))) +
    geom_bar(stat = "identity") +
    labs(
      x = "Weeks",
      y = "log(n)"
    ) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Gestational duration")
  plotly::ggplotly(p)
}

outcomeCategoriesPlot <- function(data) {
  p <- ggplot(data = data, mapping = aes(y = pct, x = outcome_category)) +
    geom_bar(stat = "identity", position = "dodge", mapping = aes(fill = algorithm))
  plotly::ggplotly(p)
}

boxPlot <- function(data) {
  # assume we have a column per DP, next to the first column
  nameColumn <- colnames(data)[1]
  dpCols <- colnames(data)[-1]
  plotData <- data %>%
    tidyr::pivot_wider(names_from = nameColumn, values_from = dpCols) %>%
    dplyr::mutate(cdm_name = dpCols)

  plot_ly(data = plotData,
          x = ~ cdm_name) %>%
    add_boxplot(
      lowerfence = ~ min,
      q1 = ~ Q25,
      median = ~ median,
      mean = ~ mean,
      sd = ~ sd,
      q3 = ~ Q75,
      upperfence = ~ max)
}
