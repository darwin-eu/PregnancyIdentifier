# 24-precision-days.R - Precision days module (standard Shiny module)
# Data: precisionDays (columns: cdm_name, esd_precision_days, optionally density)

precisionDaysUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text",
        "Distribution of ESD start-date precision (in days). Lower values indicate more precise inferred start dates."),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database",
                           choices = character(0), selected = character(0),
                           multiple = TRUE, options = opt))
    ),
    tabsetPanel(
      tabPanel("Plot",
               plotly::plotlyOutput(ns("plot"), height = "420px") %>% withSpinner()),
      tabPanel("Summary",
               downloadButton(ns("download_summary_csv"), "Download table (.csv)"),
               DT::DTOutput(ns("summaryTable")) %>% withSpinner())
    )
  )
}

precisionDaysServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    dp <- if ("cdm_name" %in% colnames(precisionDays)) {
      unique(c(allDP, precisionDays$cdm_name))
    } else {
      allDP
    }

    observe({
      updatePickerInput(session, "cdm", choices = dp, selected = dp)
    })

    getData <- reactive({
      data <- precisionDays
      if (!"cdm_name" %in% colnames(data)) return(data)
      filterByCdm(data, input$cdm, dp)
    })

    # Summary table: descriptive statistics per database
    summaryTableData <- reactive({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      data <- data %>%
        dplyr::mutate(esd_precision_days = suppressWarnings(as.numeric(.data$esd_precision_days))) %>%
        dplyr::filter(!is.na(.data$esd_precision_days))
      if (nrow(data) == 0 || !"cdm_name" %in% colnames(data)) return(NULL)
      hasDensity <- "density" %in% colnames(data) &&
        any(!is.na(suppressWarnings(as.numeric(data$density))))
      if (hasDensity) {
        data %>%
          dplyr::mutate(density = suppressWarnings(as.numeric(.data$density))) %>%
          dplyr::filter(!is.na(.data$density)) %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::summarise(
            `N Points` = dplyr::n(),
            `Min` = round(min(.data$esd_precision_days), 1),
            `Weighted Mean` = round(stats::weighted.mean(.data$esd_precision_days, .data$density), 1),
            `Max` = round(max(.data$esd_precision_days), 1),
            `Peak Density At` = round(.data$esd_precision_days[which.max(.data$density)], 1),
            .groups = "drop"
          ) %>%
          dplyr::rename(Database = .data$cdm_name)
      } else {
        data %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::summarise(
            N = dplyr::n(),
            Min = round(min(.data$esd_precision_days), 1),
            `Q1 (25th)` = round(stats::quantile(.data$esd_precision_days, 0.25), 1),
            Median = round(stats::median(.data$esd_precision_days), 1),
            `Q3 (75th)` = round(stats::quantile(.data$esd_precision_days, 0.75), 1),
            Max = round(max(.data$esd_precision_days), 1),
            Mean = round(mean(.data$esd_precision_days), 1),
            `Std Dev` = round(stats::sd(.data$esd_precision_days), 1),
            .groups = "drop"
          ) %>%
          dplyr::rename(Database = .data$cdm_name)
      }
    })
    output$summaryTable <- DT::renderDT({
      summary_df <- summaryTableData()
      if (is.null(summary_df) || nrow(summary_df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No precision days data available."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      DT::datatable(
        summary_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        filter = "none",
        rownames = FALSE
      ) %>%
        DT::formatRound(
          columns = setdiff(colnames(summary_df), "Database"),
          digits = 1
        )
    })
    output$download_summary_csv <- downloadHandler(
      filename = function() { "precision_days_summary.csv" },
      content = function(file) {
        d <- summaryTableData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )

    output$plot <- plotly::renderPlotly({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) {
        return(emptyPlotlyMessage("No precision days data available."))
      }

      # Ensure numeric
      hasDensityCol <- "density" %in% colnames(data)
      data <- data %>%
        dplyr::mutate(
          esd_precision_days = suppressWarnings(as.numeric(.data$esd_precision_days)),
          density = if (hasDensityCol) suppressWarnings(as.numeric(.data$density)) else NA_real_
        ) %>%
        dplyr::filter(!is.na(.data$esd_precision_days))

      if (!"cdm_name" %in% colnames(data)) {
        return(emptyPlotlyMessage("No valid data for selected filters."))
      }

      # If we have pre-computed density, use it; otherwise compute per database
      hasDensity <- "density" %in% colnames(data) && any(!is.na(data$density))
      if (hasDensity) {
        plotData <- data %>%
          dplyr::filter(!is.na(.data$density)) %>%
          dplyr::arrange(.data$cdm_name, .data$esd_precision_days)
      } else {
        plotData <- data %>%
          dplyr::filter(!is.na(.data$esd_precision_days)) %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::group_modify(function(g, ...) {
            x <- g$esd_precision_days
            if (length(x) < 2L) {
              if (length(x) == 0L) return(tibble::tibble(esd_precision_days = numeric(0), density = numeric(0)))
              return(tibble::tibble(esd_precision_days = x, density = 1))
            }
            d <- stats::density(x)
            tibble::tibble(esd_precision_days = d$x, density = d$y)
          }) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(.data$cdm_name, .data$esd_precision_days)
      }

      if (nrow(plotData) == 0) {
        return(emptyPlotlyMessage("No valid data for selected filters."))
      }

      plotly::plot_ly(
        data = plotData,
        x = ~esd_precision_days,
        y = ~density,
        color = ~cdm_name,
        type = "scatter",
        mode = "lines",
        hovertemplate = "Precision: %{x:.0f} days<br>Density: %{y:.4f}<extra></extra>"
      ) %>%
        plotly::layout(
          xaxis = list(title = "ESD Precision (days)"),
          yaxis = list(title = "Density"),
          showlegend = TRUE,
          legend = list(title = list(text = "Database"))
        )
    })
  })
}
