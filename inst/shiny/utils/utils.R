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

#' Find log.txt near a file path and extract the package version.
#' Walks up from the file's directory looking for log.txt within dataFolder.
#' Returns version string (e.g. "3.2.1") or NULL if not found.
getVersionFromLog <- function(filepath, dataFolder) {
  dir <- dirname(filepath)
  dataFolder <- normalizePath(dataFolder, mustWork = FALSE)
  while (nchar(dir) >= nchar(dataFolder)) {
    logPath <- file.path(dir, "log.txt")
    if (file.exists(logPath)) {
      firstLine <- readLines(logPath, n = 1, warn = FALSE)
      m <- regmatches(firstLine, regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", firstLine))
      if (length(m) == 1) return(m)
    }
    parent <- dirname(dir)
    if (parent == dir) break
    dir <- parent
  }
  NULL
}

#' Read result files by regex pattern from dataFolder.
#'
#' Recursively searches dataFolder for files matching `regex`, reads each one,
#' appends package version from log.txt to cdm_name, and combines results.
#'
#' @param dataFolder Path to the data directory
#' @param regex Regular expression to match file names
#' @param reader "summarised_result" to use omopgenerics::importSummarisedResult,
#'   "csv" to use readr::read_csv
#' @return Combined data frame or NULL if no files found
readResults <- function(dataFolder, regex, reader = c("summarised_result", "csv")) {
  reader <- match.arg(reader)
  allFiles <- list.files(dataFolder, recursive = TRUE, full.names = TRUE)
  selectedFiles <- stringr::str_subset(allFiles, regex)

  if (length(selectedFiles) == 0) return(NULL)

  # Error if multiple files match the regex in the same directory

  dirs <- dirname(selectedFiles)
  if (any(duplicated(dirs))) {
    dupDirs <- unique(dirs[duplicated(dirs)])
    dupFiles <- selectedFiles[dirs %in% dupDirs]
    cli::cli_abort(c(
      "Multiple files matching pattern {.val {regex}} found in the same directory.",
      "Each database folder should contain only one copy of each result file.",
      "Duplicates found:",
      set_names(dupFiles, rep("*", length(dupFiles)))
    ))
  }

  results <- list()
  for (f in selectedFiles) {
    message("Loading: ", f)
    res <- tryCatch({
      if (reader == "summarised_result") {
        omopgenerics::importSummarisedResult(f)
      } else {
        readr::read_csv(f, col_types = readr::cols(.default = "c"),
                        guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"),
                        show_col_types = FALSE)
      }
    }, error = function(e) {
      warning("Failed to read ", f, ": ", conditionMessage(e))
      NULL
    })

    if (is.null(res) || nrow(res) == 0) next

    # Clean up column names (some CSVs have quoted headers)
    if (reader == "csv") {
      colnames(res) <- gsub("^['\"]|['\"]$", "", colnames(res))
      if ("...1" %in% colnames(res)) res <- res[, colnames(res) != "...1", drop = FALSE]
    }

    # Get version from log.txt
    version <- getVersionFromLog(f, dataFolder)
    versionSuffix <- if (!is.null(version)) paste0("_v", version) else ""

    # Ensure cdm_name exists for plain CSVs
    if (reader == "csv" && !"cdm_name" %in% colnames(res)) {
      # Try cdm_source.csv in same folder
      cdmSrcPath <- file.path(dirname(f), "cdm_source.csv")
      if (file.exists(cdmSrcPath)) {
        src <- tryCatch(
          readr::read_csv(cdmSrcPath, col_types = readr::cols(.default = "c"),
                          show_col_types = FALSE, n_max = 1),
          error = function(e) NULL
        )
        dbName <- if (!is.null(src) && "cdm_name" %in% colnames(src)) src$cdm_name[1] else basename(dirname(f))
      } else {
        dbName <- basename(dirname(f))
      }
      res$cdm_name <- dbName
    }

    # Apply version suffix and cdm_name normalization
    if ("cdm_name" %in% colnames(res)) {
      res$cdm_name <- ifelse(res$cdm_name == "cdm", "EMBD-ULSGE", res$cdm_name)
      res$cdm_name <- tolower(paste0(res$cdm_name, versionSuffix))

      # For summarised_result objects, also update cdm_name in settings
      if (reader == "summarised_result") {
        s <- omopgenerics::settings(res)
        if ("cdm_name" %in% colnames(s)) {
          s$cdm_name <- ifelse(s$cdm_name == "cdm", "EMBD-ULSGE", s$cdm_name)
          s$cdm_name <- tolower(paste0(s$cdm_name, versionSuffix))
          attr(res, "settings") <- s
        }
      }
    }

    # Remove metadata columns from plain CSVs
    if (reader == "csv") {
      res <- res[, !colnames(res) %in% c("date_run", "date_export", "pkg_version"), drop = FALSE]
    }

    results <- c(results, list(res))
  }

  if (length(results) == 0) return(NULL)

  if (reader == "summarised_result") {
    purrr::compact(results) |> purrr::reduce(omopgenerics::bind)
  } else {
    dplyr::bind_rows(results)
  }
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
