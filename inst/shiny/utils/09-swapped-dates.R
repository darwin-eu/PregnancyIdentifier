# 09-swapped-dates.R - Swapped (reversed) dates module (standard Shiny module)

swappedDatesUI <- function(id) {
  ns <- NS(id)

  dpChoices <- if (is.data.frame(swappedDatesDisplay) && nrow(swappedDatesDisplay) > 0 && "cdm_name" %in% colnames(swappedDatesDisplay)) {
    unique(swappedDatesDisplay$cdm_name)
  } else {
    allDP
  }

  tagList(
    div(class = "tab-help-text",
        paste(
          "Count of episodes with reversed start/end (start date after end date). Used for data quality and algorithm validation.",
          "",
          "Metrics:",
          "- HIP reversed (start after end): pregnancy start is after HIP end date",
          "- PPS reversed (start after end): pregnancy start is after PPS end date",
          "- ESD (final episode) reversed (start after end): final episode start is after end date",
          sep = "\n"
        )),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("cdm"), "Database",
                                          choices = dpChoices, selected = dpChoices,
                                          multiple = TRUE, options = opt))
    ),
    tabsetPanel(
      id = ns("swappedDatesTabs"),
      tabPanel(
        "Plot",
        plotly::plotlyOutput(ns("swappedPlot"), height = "420px") %>%
          shinycssloaders::withSpinner(),
        h4("Download figure"),
        fluidRow(
          column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
          column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
          column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
        ),
        downloadButton(ns("download_plot"), "Download plot (PNG)")
      ),
      tabPanel(
        "Data",
        DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner(),
        downloadButton(ns("download_table_csv"), "Download table (.csv)")
      )
    )
  )
}

swappedDatesServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    dpChoices <- if (is.data.frame(swappedDatesDisplay) && nrow(swappedDatesDisplay) > 0 && "cdm_name" %in% colnames(swappedDatesDisplay)) {
      unique(swappedDatesDisplay$cdm_name)
    } else {
      allDP
    }

    getData <- reactive({
      if (!is.data.frame(swappedDatesDisplay) || nrow(swappedDatesDisplay) == 0) {
        return(data.frame())
      }
      filterByCdm(swappedDatesDisplay, input$cdm, dpChoices)
    })

    swapped_plot_ggplot <- reactive({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)

      metricCol <- if ("metric" %in% colnames(d)) "metric" else NULL
      nCol <- if ("n_swapped" %in% colnames(d)) "n_swapped" else NULL
      pctCol <- if ("pct" %in% colnames(d)) "pct" else NULL

      if (is.null(metricCol) || (is.null(nCol) && is.null(pctCol))) return(NULL)

      plotData <- d %>%
        dplyr::filter(!grepl("total|Total", .data[[metricCol]], ignore.case = TRUE))

      if (nrow(plotData) == 0) return(NULL)

      if (!is.null(pctCol)) {
        plotData <- plotData %>%
          dplyr::mutate(
            y_val = suppressWarnings(as.numeric(.data[[pctCol]])),
            n_val = if (!is.null(nCol)) suppressWarnings(as.numeric(.data[[nCol]])) else NA_real_,
            hover = paste0(
              .data[[metricCol]], "\n",
              .data$cdm_name, "\n",
              if (!is.null(nCol)) paste0("N: ", format(.data$n_val, big.mark = ","), "\n") else "",
              round(.data$y_val, 2), "%"
            )
          )
        yLabel <- "% of Total Episodes"
      } else {
        plotData <- plotData %>%
          dplyr::mutate(
            y_val = suppressWarnings(as.numeric(.data[[nCol]])),
            hover = paste0(
              .data[[metricCol]], "\n",
              .data$cdm_name, "\n",
              "N: ", format(.data$y_val, big.mark = ",")
            )
          )
        yLabel <- "Count"
      }

      plotData <- plotData %>% dplyr::filter(!is.na(.data$y_val))
      if (nrow(plotData) == 0) return(NULL)

      metricColours <- c(
        "HIP reversed (start after end)" = "#E41A1C",
        "PPS reversed (start after end)" = "#377EB8",
        "ESD (final episode) reversed (start after end)" = "#4DAF4A"
      )

      ggplot2::ggplot(plotData, ggplot2::aes(
        x = .data$cdm_name, y = .data$y_val,
        fill = .data[[metricCol]],
        text = .data$hover
      )) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::scale_fill_manual(values = metricColours, name = "Metric") +
        ggplot2::labs(x = "Database", y = yLabel) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "bottom"
        )
    })

    output$swappedPlot <- plotly::renderPlotly({
      p <- swapped_plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No swapped dates data for selected filters."))
      plotly::ggplotly(p, tooltip = "text")
    })

    output$download_plot <- downloadHandler(
      filename = function() { "swappedDatesPlot.png" },
      content = function(file) {
        p <- swapped_plot_ggplot()
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

    output$table <- DT::renderDT({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(renderPrettyDT(data.frame()))
      renderPrettyDT(d)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "swapped_dates.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
