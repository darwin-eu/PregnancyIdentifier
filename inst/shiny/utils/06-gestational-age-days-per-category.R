# 06-gestational-age-days-per-category.R - Gestational age days per category module (standard Shiny module)
# Boxplots from pre-computed table stats. Interactive: plotly native (ggplotly does not support geom_boxplot stat="identity"). PNG: ggplot2.

gestationalAgeDaysPerCategoryUI <- function(id) {

  ns <- NS(id)

  allOutcomes <- if (is.data.frame(gestationalAgeDaysPerCategorySummary) && nrow(gestationalAgeDaysPerCategorySummary) > 0) {
    unique(as.character(gestationalAgeDaysPerCategorySummary$final_outcome_category))
  } else {
    character(0)
  }

  emptyMsg <- is.null(gestationalAgeDaysPerCategorySummary) || nrow(gestationalAgeDaysPerCategorySummary) == 0

  dataTableOut <- tagList(
    h4("Data"),
    downloadButton(ns("download_table_csv"), "Download table (.csv)"),
    DT::DTOutput(ns("dataTable")) %>% shinycssloaders::withSpinner()
  )

  if (emptyMsg) {
    return(tagList(
      p("Results files are empty.", style = "margin: 20px; font-size: 16px; font-weight: bold;"),
      dataTableOut
    ))
  }

  tagList(
    div(class = "tab-help-text",
        "Gestational duration by outcome type. Used to check that outcome-specific durations (e.g. live birth vs miscarriage) are plausible."),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("cdm"), "Database",
                                          choices = allDP, selected = allDP,
                                          multiple = TRUE, options = opt)),
      column(3, shinyWidgets::pickerInput(ns("outcome"), "Outcome",
                                          choices = allOutcomes,
                                          selected = allOutcomes,
                                          multiple = TRUE, options = opt)),
      column(3, checkboxInput(ns("iqrOnly"), "IQR only", value = TRUE))
    ),
    plotly::plotlyOutput(ns("plot"), height = "420px") %>% shinycssloaders::withSpinner(),
    h4("Download figure"),
    fluidRow(
      column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
      column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
      column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
    ),
    downloadButton(ns("download_plot"), "Download plot (PNG)"),
    p(),
    dataTableOut
  )
}

gestationalAgeDaysPerCategoryServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    getData <- reactive({
      if (!is.data.frame(gestationalAgeDaysPerCategorySummary) || nrow(gestationalAgeDaysPerCategorySummary) == 0) {
        return(data.frame())
      }

      data <- filterByCdm(gestationalAgeDaysPerCategorySummary, input$cdm, allDP)

      outcomeSel <- input$outcome
      if (is.null(outcomeSel) || length(outcomeSel) == 0) {
        outcomeSel <- unique(as.character(gestationalAgeDaysPerCategorySummary$final_outcome_category))
      }
      data %>%
        dplyr::filter(.data$final_outcome_category %in% outcomeSel)
    })

    # Shared plot data: table stats (min, Q25, median, Q75, max) → boxplot bounds; IQR only = no min/max whiskers
    getPlotData <- reactive({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      req_cols <- c("min", "Q25", "median", "Q75", "max")
      if (!all(req_cols %in% colnames(data))) return(NULL)
      iqrOnly <- isTRUE(input$iqrOnly)
      outcomeLevels <- c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG")
      data %>%
        dplyr::group_by(.data$cdm_name, .data$final_outcome_category) %>%
        dplyr::slice(1L) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          final_outcome_category = factor(.data$final_outcome_category, levels = outcomeLevels),
          lower = suppressWarnings(as.numeric(.data$Q25)),
          middle = suppressWarnings(as.numeric(.data$median)),
          upper = suppressWarnings(as.numeric(.data$Q75)),
          ymin = if (iqrOnly) .data$lower else suppressWarnings(as.numeric(.data$min)),
          ymax = if (iqrOnly) .data$upper else suppressWarnings(as.numeric(.data$max))
        ) %>%
        tidyr::drop_na(lower, middle, upper, ymin, ymax)
    })

    # Interactive: plotly native box (ggplotly does not support geom_boxplot stat="identity")
    output$plot <- plotly::renderPlotly({
      pd <- getPlotData()
      if (is.null(pd) || nrow(pd) == 0) {
        msg <- if (!is.data.frame(gestationalAgeDaysPerCategorySummary) || nrow(gestationalAgeDaysPerCategorySummary) == 0) {
          "Results files are empty."
        } else {
          "No data for selected filters."
        }
        return(emptyPlotlyMessage(msg))
      }
      iqrOnly <- isTRUE(input$iqrOnly)
      cdms <- unique(pd$cdm_name)
      plot_list <- lapply(cdms, function(cdm) {
        d <- pd %>% dplyr::filter(.data$cdm_name == .env$cdm)
        # Build tooltip: always Outcome + Median; add Episode count when present; add min/fences/max only when !iqrOnly
        sep <- "<br>"
        d$hover_text <- paste0(
          "Outcome: ", as.character(d$final_outcome_category),
          sep, "Median: ", round(d$middle, 1), " days"
        )
        if ("episode_count" %in% colnames(d)) {
          ep_fmt <- ifelse(is.na(d$episode_count), "—", format(d$episode_count, big.mark = ","))
          d$hover_text <- paste0(d$hover_text, sep, "Episode count: ", ep_fmt)
        }
        if (!iqrOnly) {
          d$hover_text <- paste0(
            d$hover_text,
            sep, "Min: ", round(d$ymin, 1),
            sep, "Lower fence (Q1): ", round(d$lower, 1),
            sep, "Upper fence (Q3): ", round(d$upper, 1),
            sep, "Max: ", round(d$ymax, 1)
          )
        }
        plotly::plot_ly(
          d,
          x = ~ final_outcome_category,
          type = "box",
          lowerfence = ~ ymin,
          q1 = ~ lower,
          median = ~ middle,
          q3 = ~ upper,
          upperfence = ~ ymax,
          color = ~ final_outcome_category,
          colors = "Set2",
          customdata = ~ hover_text,
          hovertemplate = "%{customdata}<extra></extra>"
        ) %>%
          plotly::layout(
            xaxis = list(title = "Final outcome category"),
            yaxis = list(title = "Gestational age (days)"),
            showlegend = FALSE
          )
      })
      n <- length(plot_list)
      if (n == 0) return(emptyPlotlyMessage("No data for selected filters."))
      # Facet by cdm_name: wrap into grid (2 columns when multiple CDMs)
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
      # Per-panel titles (subplot can drop individual layout titles, so use annotations)
      row_idx <- function(i) (i - 1L) %/% ncols + 1L
      col_idx <- function(i) (i - 1L) %% ncols + 1L
      annotations <- lapply(seq_len(n), function(i) {
        list(
          text = cdms[i],
          x = (col_idx(i) - 0.5) / ncols,
          y = 1 - (row_idx(i) - 1L) / nrows - 0.02,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 12)
        )
      })
      p %>%
        plotly::layout(
          margin = list(t = 50, b = 60),
          legend = list(orientation = "h", y = 1.02),
          annotations = annotations
        ) %>%
        plotly::config(displayModeBar = TRUE)
    })

    # PNG download: ggplot works with stat="identity" for ggsave
    plot_ggplot <- reactive({
      pd <- getPlotData()
      if (is.null(pd) || nrow(pd) == 0) return(NULL)
      ggplot2::ggplot(pd, ggplot2::aes(
        x = .data$final_outcome_category,
        ymin = ymin,
        lower = lower,
        middle = middle,
        upper = upper,
        ymax = ymax,
        fill = .data$final_outcome_category
      )) +
        ggplot2::geom_boxplot(stat = "identity") +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(ggplot2::vars(.data$cdm_name), scales = "free_y") +
        ggplot2::labs(
          x = "Final outcome category",
          y = "Gestational age (days)",
          fill = "Outcome category"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 9),
          legend.position = "bottom"
        )
    })

    output$download_plot <- downloadHandler(
      filename = function() { "gestationalAgeDaysPerCategoryPlot.png" },
      content = function(file) {
        p <- plot_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = as.numeric(input$download_width),
            height = as.numeric(input$download_height),
            dpi = as.numeric(input$download_dpi),
            units = "cm"
          )
        }
      }
    )

    output$dataTable <- DT::renderDT({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(renderPrettyDT(data.frame()))
      if ("person_count" %in% colnames(d)) d <- d %>% dplyr::select(-"person_count")
      renderPrettyDT(d)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "gestational_age_days_per_category.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) {
          if ("person_count" %in% colnames(d)) d <- d %>% dplyr::select(-"person_count")
          readr::write_csv(d, file)
        }
      }
    )
  })
}
