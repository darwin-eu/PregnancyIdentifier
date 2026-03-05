# 07-temporal-patterns.R - Temporal patterns module (standard Shiny module)

temporalPatternsUI <- function(id) {
  ns <- NS(id)

  emptyMsg <- is.null(trendData) || nrow(trendData) == 0

  dataTableOut <- tagList(
    h4("Data"),
    downloadButton(ns("download_data_csv"), "Download table (.csv)"),
    DT::DTOutput(ns("dataTable")) %>% shinycssloaders::withSpinner()
  )

  missingTableOut <- tagList(
    h4("Missing data"),
    downloadButton(ns("download_missing_csv"), "Download missing data (.csv)"),
    DT::DTOutput(ns("missingDataTable")) %>% shinycssloaders::withSpinner()
  )

  if (emptyMsg) {
    return(tagList(
      p("Results files are empty.", style = "margin: 20px; font-size: 16px; font-weight: bold;"),
      dataTableOut
    ))
  }

  columnChoices <- unique(trendData$column)
  if (length(columnChoices) == 0) columnChoices <- "column"

  # Compute min year slider range from data
  valNum <- suppressWarnings(as.numeric(trendData$value))
  minVal <- if (length(valNum) > 0 && !all(is.na(valNum))) min(valNum, na.rm = TRUE) else 2000
  maxVal <- if (length(valNum) > 0 && !all(is.na(valNum))) max(valNum, na.rm = TRUE) else 2020
  if (!is.finite(minVal)) minVal <- 2000
  if (!is.finite(maxVal)) maxVal <- 2020
  defaultYear <- min(max(minVal, 2000), maxVal)

  tagList(
    div(class = "tab-help-text",
        "Episode counts by year and month for each date type. Used for temporal trends, seasonality, and study-window checks."),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("cdm"), "Database",
                                          choices = allDP, selected = allDP,
                                          multiple = TRUE, options = opt)),
      column(3, shinyWidgets::pickerInput(ns("period"), "Time period",
                                          choices = c("year", "month"),
                                          selected = "year",
                                          multiple = FALSE)),
      column(3, shinyWidgets::pickerInput(ns("column"), "Columns",
                                          choices = columnChoices,
                                          selected = "final_episode_end_date",
                                          multiple = TRUE, options = opt)),
      column(3, sliderInput(ns("minYear"), "Min year",
                            min = minVal, max = maxVal, value = defaultYear))
    ),
    tabsetPanel(
      id = ns("temporalTabs"),
      type = "tabs",
      tabPanel(
        "Episodes",
        plotly::plotlyOutput(ns("plot"), height = "420px") %>% shinycssloaders::withSpinner(),
        h4("Download figure"),
        fluidRow(
          column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
          column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
          column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
        ),
        downloadButton(ns("download_plot"), "Download plot (PNG)"),
        dataTableOut
      ),
      tabPanel(
        "Missing data",
        plotly::plotlyOutput(ns("missingPlot"), height = "420px") %>% shinycssloaders::withSpinner(),
        h4("Download figure"),
        fluidRow(
          column(3, textInput(ns("download_height_missing"), "Height (cm)", value = "10")),
          column(3, textInput(ns("download_width_missing"), "Width (cm)", value = "20")),
          column(3, textInput(ns("download_dpi_missing"), "Resolution (dpi)", value = "300"))
        ),
        downloadButton(ns("download_plot_missing"), "Download plot (PNG)"),
        missingTableOut
      )
    )
  )
}

temporalPatternsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    columnChoices <- unique(trendData$column)
    if (length(columnChoices) == 0) columnChoices <- "column"

    valNum <- suppressWarnings(as.numeric(trendData$value))
    defaultMinYear <- if (length(valNum) > 0 && !all(is.na(valNum))) {
      minVal <- min(valNum, na.rm = TRUE)
      if (!is.finite(minVal)) 2000 else min(max(minVal, 2000), max(valNum, na.rm = TRUE))
    } else {
      2000
    }

    getData <- reactive({
      data <- filterByCdm(trendData, input$cdm, allDP)

      periodSel <- input$period
      if (is.null(periodSel) || length(periodSel) == 0) periodSel <- "year"

      columnSel <- input$column
      if (is.null(columnSel) || length(columnSel) == 0) columnSel <- columnChoices

      data <- data %>%
        dplyr::filter(.data$period %in% periodSel) %>%
        dplyr::filter(.data$column %in% columnSel)

      if (periodSel == "year") {
        minYear <- input$minYear
        if (is.null(minYear)) minYear <- defaultMinYear
        data <- data %>%
          dplyr::filter(as.numeric(.data$value) >= minYear)
      }

      data
    })

    getMissingData <- reactive({
      data <- filterByCdm(trendDataMissing, input$cdm, allDP)

      periodSel <- input$period
      if (is.null(periodSel) || length(periodSel) == 0) periodSel <- "year"

      columnSel <- input$column
      if (is.null(columnSel) || length(columnSel) == 0) columnSel <- columnChoices

      data %>%
        dplyr::filter(.data$period %in% periodSel) %>%
        dplyr::filter(.data$column %in% columnSel)
    })

    plot_ggplot <- reactive({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      period <- input$period
      if (is.null(period) || length(period) == 0) period <- "year"
      xLabel <- if (length(period) == 0) "Period" else paste0(toupper(substring(period[1], 1, 1)), substring(period[1], 2))
      if (period == "year") {
        data <- data %>% dplyr::arrange(as.numeric(.data$value))
      } else if (period == "month") {
        data <- data %>% dplyr::arrange(match(.data$value, base::month.name))
      }
      trendsPlot(data = data, xVar = "value", xLabel = xLabel, facetVar = "cdm_name", return_ggplot = TRUE)
    })

    plot_missing_ggplot <- reactive({
      data <- getMissingData()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      barPlot(data = data,
              xVar = "column",
              yVar = "count",
              fillVar = "column",
              facetVar = "cdm_name",
              rotateAxisText = TRUE,
              flipCoordinates = FALSE,
              return_ggplot = TRUE)
    })

    output$plot <- plotly::renderPlotly({
      p <- plot_ggplot()
      if (is.null(p)) {
        msg <- if (nrow(trendData) == 0) "Results files are empty." else "No data for selected filters."
        return(emptyPlotlyMessage(msg))
      }
      plotly::ggplotly(p)
    })

    output$missingPlot <- plotly::renderPlotly({
      p <- plot_missing_ggplot()
      if (is.null(p)) {
        msg <- if (nrow(trendDataMissing) == 0) "Results files are empty." else "No data for selected filters."
        return(emptyPlotlyMessage(msg))
      }
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "temporalPatternsPlot.png" },
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

    output$download_plot_missing <- downloadHandler(
      filename = function() { "temporalPatternsMissingPlot.png" },
      content = function(file) {
        p <- plot_missing_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = as.numeric(input$download_width_missing),
            height = as.numeric(input$download_height_missing),
            dpi = as.numeric(input$download_dpi_missing),
            units = "cm"
          )
        }
      }
    )

    output$dataTable <- DT::renderDT({
      renderPrettyDT(trendData)
    })

    output$missingDataTable <- DT::renderDT({
      renderPrettyDT(trendDataMissing)
    })

    output$download_data_csv <- downloadHandler(
      filename = function() { "temporal_patterns.csv" },
      content = function(file) {
        if (!is.null(trendData) && nrow(trendData) > 0) readr::write_csv(trendData, file)
      }
    )
    output$download_missing_csv <- downloadHandler(
      filename = function() { "temporal_patterns_missing.csv" },
      content = function(file) {
        if (!is.null(trendDataMissing) && nrow(trendDataMissing) > 0) readr::write_csv(trendDataMissing, file)
      }
    )
  })
}
