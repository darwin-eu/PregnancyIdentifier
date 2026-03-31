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
    results <- purrr::compact(results)
    if (length(results) == 0) return(NULL)
    safeBindSummarisedResults(results)
  } else {
    dplyr::bind_rows(results)
  }
}

#' Bind summarised_result objects without triggering duplicate validation errors.
#'
#' omopgenerics::bind calls validateSummarisedResultTable which rejects rows
#' that share the same key columns but differ in estimate_value. This happens
#' when multiple result folders exist for the same database or when a dataset
#' contains internal duplicates from re-runs. This function works around that
#' by binding as plain data frames, re-indexing result_id, deduplicating, and
#' reconstructing the summarised_result class.
#' Ensure each result_id maps to a unique set of settings.
#' Merges result_ids that share identical setting values.
deduplicateSettings <- function(sr) {
  if (!inherits(sr, "summarised_result") || !"result_id" %in% colnames(sr)) return(sr)
  s <- omopgenerics::settings(sr)
  if (is.null(s) || !"result_id" %in% colnames(s)) return(sr)

  setting_key_cols <- setdiff(names(s), "result_id")
  if (length(setting_key_cols) == 0 || !any(duplicated(s[setting_key_cols]))) return(sr)

  # Find canonical result_id for each unique set of settings
  unique_s <- s[!duplicated(s[setting_key_cols]), , drop = FALSE]
  sr_df <- as.data.frame(sr)
  for (i in which(duplicated(s[setting_key_cols]))) {
    dup_row <- s[i, setting_key_cols, drop = FALSE]
    for (j in seq_len(nrow(unique_s))) {
      if (identical(as.list(dup_row), as.list(unique_s[j, setting_key_cols, drop = FALSE]))) {
        sr_df$result_id[sr_df$result_id == s$result_id[i]] <- unique_s$result_id[j]
        break
      }
    }
  }
  sr_df <- dplyr::as_tibble(sr_df)
  attr(sr_df, "settings") <- dplyr::as_tibble(unique_s)
  class(sr_df) <- unique(c("summarised_result", "omop_result", class(sr_df)))
  sr_df
}

safeBindSummarisedResults <- function(results) {
  # First try the standard bind — it's the happy path
  combined <- tryCatch(
    purrr::reduce(results, omopgenerics::bind),
    error = function(e) NULL
  )
  if (!is.null(combined)) {
    # Deduplicate rows that share the same key columns (from re-runs)
    key_cols <- setdiff(names(combined), "estimate_value")
    dup_idx <- duplicated(combined[key_cols], fromLast = TRUE)
    if (any(dup_idx)) {
      message(sum(dup_idx), " duplicated rows eliminated from happy-path bind.")
      combined <- combined[!dup_idx, ]
    }
    return(deduplicateSettings(combined))
  }

  message("omopgenerics::bind failed (likely duplicates); falling back to manual bind + dedup.")

  # Collect settings from each result, offsetting result_id to avoid collisions
  all_settings <- list()
  all_data <- list()
  id_offset <- 0L

  for (res in results) {
    s <- omopgenerics::settings(res)
    old_ids <- unique(res$result_id)
    max_old <- if (length(old_ids) > 0) max(old_ids, na.rm = TRUE) else 0L

    # Offset result_id in both data and settings
    res_df <- as.data.frame(res)
    res_df$result_id <- res_df$result_id + id_offset
    s$result_id <- s$result_id + id_offset

    all_data <- c(all_data, list(res_df))
    all_settings <- c(all_settings, list(s))
    id_offset <- id_offset + max_old
  }

  combined_data <- dplyr::bind_rows(all_data)
  combined_settings <- dplyr::bind_rows(all_settings)

  # Deduplicate: keep last occurrence (later files are assumed to be newer)
  # Exclude result_id so that re-runs with different result_ids are still detected as duplicates
  key_cols <- setdiff(names(combined_data), c("estimate_value", "result_id"))
  key_cols <- key_cols[key_cols %in% names(combined_data)]
  combined_data <- combined_data[!duplicated(combined_data[key_cols], fromLast = TRUE), ]

  # Remove settings rows for result_ids no longer in the data
  remaining_ids <- unique(combined_data$result_id)
  combined_settings <- combined_settings[combined_settings$result_id %in% remaining_ids, ]

  # Deduplicate settings and remap result_ids so each result_id maps to a

  # unique set of setting values (required by tableIncidence / tablePrevalence)
  setting_key_cols <- setdiff(names(combined_settings), "result_id")
  dup_idx <- duplicated(combined_settings[setting_key_cols])
  if (any(dup_idx)) {
    # Build a mapping from duplicate result_id -> canonical result_id
    unique_settings <- combined_settings[!dup_idx, ]
    id_map <- integer(0)
    for (i in which(dup_idx)) {
      row_i <- combined_settings[i, setting_key_cols, drop = FALSE]
      for (j in seq_len(nrow(unique_settings))) {
        if (identical(as.list(row_i), as.list(unique_settings[j, setting_key_cols, drop = FALSE]))) {
          id_map[as.character(combined_settings$result_id[i])] <- unique_settings$result_id[j]
          break
        }
      }
    }
    # Remap result_ids in data
    for (old_id_str in names(id_map)) {
      old_id <- as.integer(old_id_str)
      combined_data$result_id[combined_data$result_id == old_id] <- id_map[old_id_str]
    }
    combined_settings <- unique_settings
  }

  # Construct summarised_result, suppressing validation
  combined_data <- dplyr::as_tibble(combined_data)
  attr(combined_data, "settings") <- dplyr::as_tibble(combined_settings)
  class(combined_data) <- unique(c("summarised_result", "omop_result", class(combined_data)))
  combined_data
}

#' Replace version suffix in cdm_name (e.g. _v3.2.1 -> _v3).
#' For summarised_result objects also updates settings attribute.
replaceVersionSuffix <- function(df, newSuffix) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  replaceFn <- function(x) {
    # If there's an existing version suffix, replace it; otherwise append
    out <- gsub("_v[0-9.]+$", paste0("_", newSuffix), x)
    # If no replacement happened (no version suffix found), append
    unchanged <- out == x
    out[unchanged] <- paste0(out[unchanged], "_", newSuffix)
    out
  }
  if ("cdm_name" %in% colnames(df)) {
    df$cdm_name <- replaceFn(df$cdm_name)
    if (inherits(df, "summarised_result")) {
      s <- tryCatch(omopgenerics::settings(df), error = function(e) NULL)
      if (!is.null(s) && "cdm_name" %in% colnames(s)) {
        s$cdm_name <- replaceFn(s$cdm_name)
        attr(df, "settings") <- s
      }
    }
  }
  df
}

#' Add a version column to a data frame.
addVersionColumn <- function(df, ver) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  df$version <- ver
  df
}

#' Combine two data frames, handling NULLs gracefully.
#' For summarised_result objects uses safeBindSummarisedResults.
safeCombine <- function(...) {
  dfs <- Filter(Negate(is.null), list(...))
  if (length(dfs) == 0) return(NULL)
  if (length(dfs) == 1) return(dfs[[1]])
  if (any(vapply(dfs, function(x) inherits(x, "summarised_result"), logical(1)))) {
    safeBindSummarisedResults(dfs)
  } else {
    dplyr::bind_rows(dfs)
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
  skipCols <- union(intersect(skipCols, colnames(data)), intersect("version", colnames(data)))
  dfs <- lapply(unique(data$cdm_name), FUN = function(db) {
    dbData <- data %>% dplyr::filter(cdm_name == db)
    valColName <- as.character(db)[1L]
    dbData %>%
      tidyr::pivot_longer(cols = setdiff(colnames(.), skipCols), names_to = "name", values_to = valColName) %>%
      dplyr::select(-dplyr::any_of(skipCols))
  })
  # Use full_join by "name" to handle varying row counts across databases
  purrr::reduce(dfs, function(a, b) dplyr::full_join(a, b, by = "name", relationship = "many-to-many"))
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
