# # Precomputed boxplot module: architecture and tradeoffs
#
# ## Overview
#
# `boxplot-precomputed.R` provides a single data contract and two rendering paths for boxplots built from **precomputed statistics** (one row per box: whiskers, quartiles, median). No raw sample is required.
#
# - **Interactive**: native Plotly `type = "box"` with `lowerfence`, `q1`, `median`, `q3`, `upperfence`.
# - **Static / download**: ggplot2 `geom_boxplot(stat = "identity")` with the same stats.
#
# The same normalized data drives both so visuals and semantics stay aligned.
#
# ## Data contract
#
# **Required** (after normalization):
#
#   | Canonical | Aliases (accepted) | Meaning |
#   |-----------|--------------------|---------|
#   | `x` | `group` | Category on x-axis (one value per box) |
#   | `ymin` | `min` | Lower whisker |
#   | `lower` | `Q25` | 25th percentile |
#   | `middle` | `median`, `mean` | Median (or mean for legacy) |
#   | `upper` | `Q75` | 75th percentile |
#   | `ymax` | `max` | Upper whisker |
#
#   **Optional**: `outliers` (list of numeric vectors), `n`, `tooltip`, `fill`, `color`, `facet`, `panel`, and any other columns (passed through for faceting/grouping).
#
# Validation is explicit: missing required columns or non-coercible numerics raise informative errors.
#
# ## Architecture
#
# 1. **Validation**
#   `validate_boxplot_data(data)` checks presence of category column (`x` or `group`) and of all five stats (canonical or alias). Optionally ensures stats are numeric.
#
# 2. **Normalization**
#   `normalize_boxplot_data(data, cols)` copies/renames to canonical names (`x`, `ymin`, `lower`, `middle`, `upper`, `ymax`), coerces to numeric, and drops rows with NA in any stat. Optional columns are kept.
#
# 3. **Plot spec / helpers**
# - `build_boxplot_tooltip(row)` builds default hover text (group, whiskers, Q1, median, Q3, n).
# - `order_boxplot_categories(data, order)` sets factor order for x-axis.
# - `expand_outliers_long(data)` turns an `outliers` list-column into a long data frame for scatter/point layers.
#
# 4. **Rendering**
#- **Plotly**: `make_plotly_boxplot_precomputed(data, ...)` builds a single plot or a list of plots for faceting, then combines with `subplot`. Uses native box traces only (no raw points for the box geometry).
# - **ggplot2**: `make_ggplot_boxplot_precomputed(data, ...)` builds one ggplot with `geom_boxplot(stat = "identity")`, optional `geom_point` for outliers, optional `geom_hline` for reference lines, and optional `facet_wrap`.
#
# Reactive/filtering logic stays in the Shiny layer; the plotting functions are pure and reusable.
#
# ## Options (aligned across both)
#
# - **title**, **xlab**, **ylab**
# - **fill**: grouping variable for separate box traces (Plotly) or fill aesthetic (ggplot2).
# - **facet**: column name (Plotly) or `vars(...)` (ggplot2) for panels.
# - **horizontal**: vertical boxes (default in Plotly for this app) vs horizontal (coord_flip / orientation = "h").
# - **show_outliers**: if data has `outliers` list-column, add points.
# - **ref_hline**: numeric vector of horizontal reference lines.
# - **category_order**: order of categories on the axis.
# - **colors**: optional palette for fill (Plotly); ggplot2 can use `scale_fill_*` after the fact.
#
# ## Plotly limitations and handling
#
# - **Precomputed stats**: Plotly’s box trace supports precomputed quartiles via `q1`, `median`, `q3`, `lowerfence`, `upperfence`. We use these directly; we do **not** fake a box from raw points when only summaries exist.
# - **Orientation**: Horizontal boxes use `orientation = "h"` and category on the y-axis; reference lines and tooltips are unchanged.
# - **Faceting**: Plotly has no built-in facet; we build one plot per facet level and combine with `subplot`, with annotations for panel titles.
# - **Outliers**: When `show_outliers` is TRUE and an `outliers` list-column exists, we add a separate scatter trace (and in ggplot2, `geom_point`) so that precomputed outlier values are shown; the box itself remains from the five stats only.
#
# ## Usage in the app
#
# - **06-gestational-age-days-per-category**: Map table columns (`min`, `Q25`, `median`, `Q75`, `max`, `final_outcome_category`, `cdm_name`) to the contract (or pass as-is; the module accepts aliases). Call `make_plotly_boxplot_precomputed` for the interactive plot and `make_ggplot_boxplot_precomputed` for PNG download.
# - **19-age-summary**, **20-age-first-pregnancy**: Same pattern: one reactive data frame with required (+ optional) columns, then both Plotly and ggplot from the same normalized data.
#
# Keeping one source of truth for aesthetics (title, labels, facet, order) in the reactive layer and passing the same options to both functions keeps interactive and downloaded figures aligned.
#
# ## Column name mapping (app → module)
#
# Existing app tables often use different names. The module accepts these so callers do not need to rename:
#
#   | App / table column   | Canonical (internal) |
#   |----------------------|----------------------|
#   | `final_outcome_category`, `colName`, `cdm_name` | Pass as `x = "final_outcome_category"` etc.; category column is resolved via `category_col` in validation. |
#   | `min`                | `ymin`               |
#   | `Q25`                | `lower`               |
#   | `median` (or `mean`) | `middle`              |
#   | `Q75`                | `upper`               |
#   | `max`                | `ymax`                |
#   | `episode_count`      | Used as `n` in tooltip when `n` is missing. |
#
#   So 06-gestational-age can pass data with `final_outcome_category`, `min`, `Q25`, `median`, `Q75`, `max` and call with `x = "final_outcome_category"`; 19 and 20 use `group` or `x = "cdm_name"` / `fill = "final_outcome_category"` as needed.
#



# boxplot-precomputed.R - Reusable precomputed boxplot helpers for Shiny
#
# Data contract: one row per box with canonical columns (after normalization):
#   x (or group) = category on x-axis
#   ymin, lower (Q1), middle (median), upper (Q3), ymax = box/whisker stats
# Optional: outliers (list-col), n, tooltip, fill, color, facet, panel
#
# Outputs: native Plotly for interactive; ggplot2 geom_boxplot(stat="identity") for download.
# Both use the same normalized data so visuals stay aligned.

# -----------------------------------------------------------------------------
# Column name mapping: alternate names used in app data -> canonical names
# -----------------------------------------------------------------------------
BOXPLOT_CANONICAL <- c(
  ymin = "ymin",
  lower = "lower",
  middle = "middle",
  upper = "upper",
  ymax = "ymax",
  x = "x"
)
BOXPLOT_ALIASES <- list(
  ymin = c("ymin", "min"),
  lower = c("lower", "Q25"),
  middle = c("middle", "median", "mean"),
  upper = c("upper", "Q75"),
  ymax = c("ymax", "max"),
  x = c("x", "group")
)

#' Validate that a data frame has the required columns for precomputed boxplots.
#' Accepts either canonical names (ymin, lower, middle, upper, ymax) or
#' common aliases (min, Q25, median, Q75, max). Category column must be 'x', 'group', or
#' a custom name given via category_col.
#'
#' @param data Data frame with one row per box.
#' @param require_numeric If TRUE (default), stats columns must be numeric or coercible.
#' @param category_col Optional character: name of the category column if not 'x' or 'group'.
#' @return Invisibly the column names used for each role; errors if invalid.
#' @export
validate_boxplot_data <- function(data, require_numeric = TRUE, category_col = NULL) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Precomputed boxplot data must be a non-empty data frame.", call. = FALSE)
  }
  nms <- colnames(data)

  # Category: x, group, or custom
  x_col <- NULL
  if (!is.null(category_col) && length(category_col) == 1L && category_col %in% nms) {
    x_col <- category_col
  }
  if (is.null(x_col)) {
    for (candidate in c("x", "group")) {
      if (candidate %in% nms) {
        x_col <- candidate
        break
      }
    }
  }
  if (is.null(x_col)) {
    stop("Precomputed boxplot data must have a category column named 'x', 'group', or specified via category_col.", call. = FALSE)
  }

  # Stats: resolve aliases (only the five box stats; category already in x_col)
  stats_cols <- character(5)
  names(stats_cols) <- c("ymin", "lower", "middle", "upper", "ymax")
  for (canon in names(stats_cols)) {
    found <- FALSE
    for (alias in BOXPLOT_ALIASES[[canon]]) {
      if (alias %in% nms) {
        stats_cols[[canon]] <- alias
        found <- TRUE
        break
      }
    }
    if (!found) {
      stop(
        "Precomputed boxplot data must have column for '", canon, "' (or alias: ",
        paste(BOXPLOT_ALIASES[[canon]], collapse = ", "), ").", call. = FALSE
      )
    }
  }

  if (require_numeric) {
    stat_names <- c("ymin", "lower", "middle", "upper", "ymax")
    for (canon in stat_names) {
      col <- stats_cols[[canon]]
      num <- suppressWarnings(as.numeric(data[[col]]))
      if (any(is.na(num) & !is.na(data[[col]]))) {
        stop("Precomputed boxplot column '", col, "' could not be coerced to numeric.", call. = FALSE)
      }
    }
  }

  invisible(list(x = x_col, ymin = stats_cols[["ymin"]], lower = stats_cols[["lower"]],
                 middle = stats_cols[["middle"]], upper = stats_cols[["upper"]], ymax = stats_cols[["ymax"]]))
}

#' Normalize input data to canonical precomputed boxplot columns.
#' Copies/renames to: x, ymin, lower, middle, upper, ymax; keeps optional columns as-is.
#'
#' @param data Data frame (one row per box).
#' @param cols Optional list from validate_boxplot_data(); if NULL, validation is run.
#' @return Data frame with canonical columns (x, ymin, lower, middle, upper, ymax) plus any optional columns.
#' @export
normalize_boxplot_data <- function(data, cols = NULL) {
  if (is.null(cols)) {
    cols <- validate_boxplot_data(data, require_numeric = TRUE)
  }

  out <- data
  # Ensure x exists (copy from group if needed)
  if (cols$x != "x") {
    out$x <- out[[cols$x]]
  }
  out$ymin <- suppressWarnings(as.numeric(out[[cols$ymin]]))
  out$lower <- suppressWarnings(as.numeric(out[[cols$lower]]))
  out$middle <- suppressWarnings(as.numeric(out[[cols$middle]]))
  out$upper <- suppressWarnings(as.numeric(out[[cols$upper]]))
  out$ymax <- suppressWarnings(as.numeric(out[[cols$ymax]]))

  out <- out %>%
    dplyr::filter(
      !is.na(.data$ymin), !is.na(.data$lower), !is.na(.data$middle),
      !is.na(.data$upper), !is.na(.data$ymax)
    )
  out
}

#' Build default hover/tooltip text for one row of precomputed boxplot data.
#' Uses group/x, whiskers, Q1, median, Q3, and n if present.
#'
#' @param row Single row data frame (or list) with x, ymin, lower, middle, upper, ymax; optional n, tooltip.
#' @param sep HTML line break for Plotly.
#' @param digits Rounding for numeric values.
#' @return Character string for hover.
#' @export
build_boxplot_tooltip <- function(row, sep = "<br>", digits = 2) {
  if (!is.null(row$tooltip) && is.character(row$tooltip) && nzchar(trimws(row$tooltip))) {
    return(row$tooltip)
  }
  rnd <- function(x) round(x, digits)
  group <- if (!is.null(row$x)) as.character(row$x) else ""
  lines <- c(
    if (nzchar(group)) paste0("Group: ", group),
    paste0("Lower whisker: ", rnd(row$ymin)),
    paste0("Q1: ", rnd(row$lower)),
    paste0("Median: ", rnd(row$middle)),
    paste0("Q3: ", rnd(row$upper)),
    paste0("Upper whisker: ", rnd(row$ymax))
  )
  if (!is.null(row$n) && !is.na(row$n)) {
    lines <- c(lines, paste0("n: ", format(row$n, big.mark = ",")))
  }
  paste(lines, collapse = sep)
}

#' Expand outliers list-column into long format for plotting (e.g. geom_point).
#' Returns a data frame with columns x, y (outlier value), and optional grouping columns.
#'
#' @param data Normalized boxplot data with optional 'outliers' list column.
#' @param x_col Name of category column (default "x").
#' @return Data frame with one row per outlier (or zero rows if no outliers).
#' @export
expand_outliers_long <- function(data, x_col = "x") {
  if (!"outliers" %in% colnames(data) || !is.list(data$outliers)) {
    return(tibble::tibble(x = character(0), y = numeric(0)))
  }
  keep <- setdiff(colnames(data), c("ymin", "lower", "middle", "upper", "ymax", "outliers"))
  out_list <- lapply(seq_len(nrow(data)), function(i) {
    o <- data$outliers[[i]]
    if (is.null(o) || length(o) == 0) return(NULL)
    o <- as.numeric(o)
    o <- o[!is.na(o)]
    if (length(o) == 0) return(NULL)
    row <- as.list(data[i, keep, drop = FALSE])
    tibble::tibble(
      x = rep(data[[x_col]][i], length(o)),
      y = o,
      row
    )
  })
  out_list <- out_list[!vapply(out_list, is.null, logical(1))]
  if (length(out_list) == 0) {
    return(tibble::tibble(x = character(0), y = numeric(0)))
  }
  dplyr::bind_rows(out_list)
}

#' Apply category ordering to normalized boxplot data.
#'
#' @param data Normalized data with 'x' column.
#' @param order Character vector of category levels (order on x-axis), or NULL to use existing order.
#' @return Data with x factor levels set.
#' @export
order_boxplot_categories <- function(data, order = NULL) {
  if (!"x" %in% colnames(data)) return(data)
  if (is.null(order) || length(order) == 0) {
    data$x <- factor(data$x, levels = unique(as.character(data$x)))
    return(data)
  }
  data$x <- factor(data$x, levels = order)
  data
}

# -----------------------------------------------------------------------------
# Plotly: native box trace from precomputed stats (no raw points).
# -----------------------------------------------------------------------------

#' Create an interactive boxplot from precomputed statistics using native Plotly.
#' Uses type = "box" with lowerfence, q1, median, q3, upperfence so no raw data is required.
#'
#' @param data Data frame with one row per box. Must have category (x or group) and
#'   stats (ymin, lower, middle, upper, ymax or aliases min, Q25, median, Q75, max).
#' @param x Column name for x-axis categories (default: use "x" or "group" from data).
#'   If a character string (e.g. "final_outcome_category"), that column is used as the category.
#' @param fill Column name for fill/color grouping (optional).
#' @param facet Column name for subplots (optional); creates a list of plotly objects to combine with subplot.
#' @param title Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param show_outliers If TRUE and data has 'outliers' list column, add scatter trace for outliers.
#' @param ref_hline Optional numeric: horizontal reference line(s).
#' @param category_order Optional character vector: order of categories on x-axis.
#' @param colors Optional named or unnamed vector of colors for fill (passed to Plotly).
#' @param horizontal If TRUE, swap x/y to get horizontal boxes.
#' @param ... Passed to plotly::layout (e.g. margin, font).
#' @return A plotly object.
#' @export
make_plotly_boxplot_precomputed <- function(data,
                                            x = NULL,
                                            fill = NULL,
                                            facet = NULL,
                                            title = NULL,
                                            xlab = NULL,
                                            ylab = NULL,
                                            show_outliers = TRUE,
                                            ref_hline = NULL,
                                            category_order = NULL,
                                            colors = NULL,
                                            horizontal = FALSE,
                                            ...) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(plotly::plot_ly() %>%
             plotly::layout(
               xaxis = list(visible = FALSE),
               yaxis = list(visible = FALSE),
               annotations = list(
                 text = "No data",
                 x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                 showarrow = FALSE
               )
             ))
  }

  category_col <- if (is.character(x) && length(x) == 1L) x else NULL
  cols <- validate_boxplot_data(data, require_numeric = TRUE, category_col = category_col)
  data <- normalize_boxplot_data(data, cols)
  data <- order_boxplot_categories(data, category_order)

  # Build hover text for each row (use optional n; optional episode_count -> n for tooltip)
  if ("episode_count" %in% colnames(data) && !"n" %in% colnames(data)) {
    data$n <- data$episode_count
  }
  data$hover_text <- vapply(seq_len(nrow(data)), function(i) {
    build_boxplot_tooltip(as.list(data[i, ]), sep = "<br>")
  }, character(1))

  x_col <- "x"
  fill_col <- fill
  facet_col <- facet

  if (!is.null(facet_col) && facet_col %in% colnames(data)) {
    # Facet: one plot per level
    facets <- unique(as.character(data[[facet_col]]))
    plot_list <- lapply(facets, function(fv) {
      d <- data %>% dplyr::filter(.data[[facet_col]] == .env$fv)
      one_plotly_box(
        d, x_col, fill_col, horizontal, colors,
        show_outliers, ref_hline, title = NULL, xlab, ylab, ...
      )
    })
    n <- length(plot_list)
    ncols <- min(2L, n)
    nrows <- ceiling(n / ncols)
    p <- plotly::subplot(
      plot_list,
      nrows = nrows,
      margin = 0.06,
      shareX = TRUE,
      shareY = FALSE,
      titleY = TRUE,
      titleX = (nrows > 1)
    )
    row_idx <- function(i) (i - 1L) %/% ncols + 1L
    col_idx <- function(i) (i - 1L) %% ncols + 1L
    annotations <- lapply(seq_len(n), function(i) {
      list(
        text = facets[i],
        x = (col_idx(i) - 0.5) / ncols,
        y = 1 - (row_idx(i) - 1L) / nrows - 0.02,
        xref = "paper", yref = "paper",
        xanchor = "center", yanchor = "bottom",
        showarrow = FALSE, font = list(size = 12)
      )
    })
    p <- p %>%
      plotly::layout(
        title = list(text = title %||% ""),
        margin = list(t = 50, b = 60),
        annotations = annotations,
        ...
      )
    return(plotly::config(p, displayModeBar = TRUE))
  }

  one_plotly_box(
    data, x_col, fill_col, horizontal, colors,
    show_outliers, ref_hline, title, xlab, ylab, ...
  )
}

# Internal: single Plotly boxplot (no facet).
# For horizontal boxes we use orientation = "h": category on y, quartiles on x.
one_plotly_box <- function(data, x_col, fill_col, horizontal, colors,
                           show_outliers, ref_hline, title, xlab, ylab, ...) {
  x_formula <- as.formula(paste0("~ `", x_col, "`"))
  if (is.null(fill_col) || !fill_col %in% colnames(data)) {
    # Single box trace per x level
    if (horizontal) {
      p <- plotly::plot_ly(
        data,
        y = x_formula,
        type = "box",
        orientation = "h",
        lowerfence = ~ ymin,
        q1 = ~ lower,
        median = ~ middle,
        q3 = ~ upper,
        upperfence = ~ ymax,
        customdata = ~ hover_text,
        hovertemplate = "%{customdata}<extra></extra>"
      )
    } else {
      p <- plotly::plot_ly(
        data,
        x = x_formula,
        type = "box",
        lowerfence = ~ ymin,
        q1 = ~ lower,
        median = ~ middle,
        q3 = ~ upper,
        upperfence = ~ ymax,
        customdata = ~ hover_text,
        hovertemplate = "%{customdata}<extra></extra>"
      )
    }
    if (!is.null(colors) && length(colors) > 0) {
      p <- p %>% plotly::layout(boxmode = "group")
    }
  } else {
    # Grouped by fill: one trace per fill level
    fill_levs <- unique(as.character(data[[fill_col]]))
    p <- plotly::plot_ly()
    for (fv in fill_levs) {
      d <- data %>% dplyr::filter(.data[[fill_col]] == .env$fv)
      col <- if (is.null(colors)) NULL else (colors[fv] %||% colors[length(colors)])
      if (horizontal) {
        p <- p %>%
          plotly::add_trace(
            data = d,
            y = x_formula,
            type = "box",
            orientation = "h",
            lowerfence = ~ ymin,
            q1 = ~ lower,
            median = ~ middle,
            q3 = ~ upper,
            upperfence = ~ ymax,
            name = fv,
            customdata = ~ hover_text,
            hovertemplate = "%{customdata}<extra></extra>",
            line = if (!is.null(col)) list(color = col) else list(),
            fillcolor = col
          )
      } else {
        p <- p %>%
          plotly::add_trace(
            data = d,
            x = x_formula,
            type = "box",
            lowerfence = ~ ymin,
            q1 = ~ lower,
            median = ~ middle,
            q3 = ~ upper,
            upperfence = ~ ymax,
            name = fv,
            customdata = ~ hover_text,
            hovertemplate = "%{customdata}<extra></extra>",
            line = if (!is.null(col)) list(color = col) else list(),
            fillcolor = col
          )
      }
    }
    p <- p %>% plotly::layout(boxmode = "group")
  }

  if (isTRUE(show_outliers) && "outliers" %in% colnames(data) && is.list(data$outliers)) {
    out_long <- expand_outliers_long(data, x_col)
    if (nrow(out_long) > 0) {
      if (horizontal) {
        p <- p %>%
          plotly::add_trace(
            data = out_long,
            x = ~ y,
            y = ~ x,
            type = "scatter",
            mode = "markers",
            marker = list(symbol = "circle-open", size = 6, opacity = 0.7),
            showlegend = FALSE,
            hoverinfo = "x"
          )
      } else {
        p <- p %>%
          plotly::add_trace(
            data = out_long,
            x = ~ x,
            y = ~ y,
            type = "scatter",
            mode = "markers",
            marker = list(symbol = "circle-open", size = 6, opacity = 0.7),
            showlegend = FALSE,
            hoverinfo = "y"
          )
      }
    }
  }

  if (!is.null(ref_hline) && length(ref_hline) > 0) {
    shapes <- lapply(ref_hline, function(y0) {
      list(
        type = "line",
        y0 = y0, y1 = y0,
        x0 = 0, x1 = 1,
        xref = "paper", yref = "y",
        line = list(dash = "dash", color = "gray")
      )
    })
    p <- p %>% plotly::layout(shapes = shapes)
  }

  ax_x <- list(title = xlab %||% "")
  ax_y <- list(title = ylab %||% "")
  if (horizontal) {
    ax_x$title <- ylab %||% ""
    ax_y$title <- xlab %||% ""
  }

  p <- p %>%
    plotly::layout(
      title = list(text = title %||% ""),
      xaxis = ax_x,
      yaxis = ax_y,
      showlegend = !is.null(fill_col) && nzchar(fill_col),
      ...
    )

  plotly::config(p, displayModeBar = TRUE)
}

# Small helper for default NULL
`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------------------------------------------------------
# ggplot2: geom_boxplot(stat = "identity")
# -----------------------------------------------------------------------------

#' Create a static ggplot2 boxplot from precomputed statistics.
#' Uses geom_boxplot(stat = "identity") so no raw data is needed.
#'
#' @param data Data frame with one row per box (same contract as make_plotly_boxplot_precomputed).
#' @param x Column name for x-axis categories (default: "x" or "group" from data).
#' @param fill Column name for fill aesthetic (optional).
#' @param facet Optional formula or vars() for faceting (e.g. vars(facet_col)).
#' @param title Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param show_outliers If TRUE and 'outliers' list column present, add geom_point for outliers.
#' @param ref_hline Optional numeric: horizontal reference line(s).
#' @param category_order Optional character vector: order of categories.
#' @param horizontal If TRUE, use coord_flip() for horizontal boxes.
#' @param position Position adjustment for geom_boxplot (e.g. position_dodge(width = 0.8) for grouped boxes).
#' @param theme_fun Optional theme function (e.g. theme_minimal); default theme_minimal.
#' @param ... Passed to theme() or labs().
#' @return A ggplot object.
#' @export
make_ggplot_boxplot_precomputed <- function(data,
                                            x = NULL,
                                            fill = NULL,
                                            facet = NULL,
                                            title = NULL,
                                            xlab = NULL,
                                            ylab = NULL,
                                            show_outliers = TRUE,
                                            ref_hline = NULL,
                                            category_order = NULL,
                                            horizontal = TRUE,
                                            position = NULL,
                                            theme_fun = ggplot2::theme_minimal,
                                            ...) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::theme_void() +
             ggplot2::labs(title = title %||% "No data"))
  }

  category_col <- if (is.character(x) && length(x) == 1L) x else NULL
  cols <- validate_boxplot_data(data, require_numeric = TRUE, category_col = category_col)
  data <- normalize_boxplot_data(data, cols)
  data <- order_boxplot_categories(data, category_order)

  x_col <- "x"
  fill_col <- fill
  fill_aes <- if (!is.null(fill_col) && fill_col %in% colnames(data)) {
    rlang::sym(fill_col)
  } else {
    NULL
  }

  aes_args <- list(
    x = rlang::expr(.data[[!!rlang::sym(x_col)]]),
    ymin = quote(ymin), lower = quote(lower), middle = quote(middle),
    upper = quote(upper), ymax = quote(ymax)
  )
  if (!is.null(fill_aes)) aes_args$fill <- fill_aes
  geom_args <- list(stat = "identity")
  if (!is.null(position)) geom_args$position <- position
  p <- ggplot2::ggplot(data, rlang::inject(ggplot2::aes(!!!aes_args))) +
    rlang::inject(ggplot2::geom_boxplot(!!!geom_args))

  if (isTRUE(show_outliers) && "outliers" %in% colnames(data) && is.list(data$outliers)) {
    out_long <- expand_outliers_long(data, x_col)
    if (nrow(out_long) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = out_long,
          ggplot2::aes(x = .data$x, y = .data$y),
          inherit.aes = FALSE,
          position = ggplot2::position_jitter(width = 0.1, height = 0),
          alpha = 0.7,
          shape = 21
        )
    }
  }

  if (!is.null(ref_hline) && length(ref_hline) > 0) {
    p <- p + ggplot2::geom_hline(
      yintercept = ref_hline,
      linetype = "dashed",
      color = "gray50"
    )
  }

  if (isTRUE(horizontal)) {
    p <- p + ggplot2::coord_flip()
  }

  if (!is.null(facet)) {
    p <- p + ggplot2::facet_wrap(facet, scales = "free_y")
  }

  p <- p +
    ggplot2::labs(
      title = title %||% "",
      x = xlab %||% "",
      y = ylab %||% ""
    )

  if (!is.null(theme_fun)) {
    p <- p + theme_fun()
  }

  p <- p + ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    axis.text.y = ggplot2::element_text(size = 9),
    legend.position = "bottom",
    ...
  )

  p
}
