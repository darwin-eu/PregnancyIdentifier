# 10-missing-dates.R - Missing dates module (standard Shiny module)

missingDatesUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text", HTML(paste(
      "<b>Counts and percentages of missing start and end dates</b> for HIP episodes, PPS episodes, and ESD (final) episodes.",
      "<b>N missing</b> = number of episodes with a missing date; <b>Percent missing</b> = percentage of all episodes."
    ))),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database", choices = allDP, selected = allDP, multiple = TRUE, options = opt))
    ),
    tabsetPanel(
      tabPanel("Plot",
               plotlyOutput(ns("plot"), height = "420px") %>% withSpinner(),
               h4("Download figure"),
               fluidRow(
                 column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
                 column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
                 column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
               ),
               downloadButton(ns("download_plot"), "Download plot (PNG)")),
      tabPanel("Data",
               DT::DTOutput(ns("table")) %>% withSpinner(),
               downloadButton(ns("download_table_csv"), "Download table (.csv)"))
    )
  )
}

missingDatesServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePickerInput(session, "cdm", choices = rv$allDP, selected = rv$allDP)
    })

    getData <- reactive({
      filterByCdm(rv$missingDates, input$cdm, rv$allDP)
    })

    missing_plot_ggplot <- reactive({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)

      pctCol <- if ("Percent missing" %in% colnames(d)) "Percent missing"
                else if ("percent_missing" %in% colnames(d)) "percent_missing"
                else NULL
      dateCol <- if ("Date field" %in% colnames(d)) "Date field"
                 else if ("name" %in% colnames(d)) "name"
                 else NULL
      nCol <- if ("N missing" %in% colnames(d)) "N missing"
              else if ("n_missing" %in% colnames(d)) "n_missing"
              else NULL

      if (is.null(pctCol) || is.null(dateCol)) return(NULL)

      plotData <- d %>%
        dplyr::mutate(
          date_field = as.character(.data[[dateCol]]),
          pct_missing = suppressWarnings(as.numeric(.data[[pctCol]]))
        ) %>%
        dplyr::filter(!is.na(.data$pct_missing))

      if (nrow(plotData) == 0) return(NULL)

      if (!is.null(nCol)) {
        plotData <- plotData %>%
          dplyr::mutate(hover = paste0(
            .data$date_field, "\n",
            .data$cdm_name, "\n",
            "N missing: ", format(suppressWarnings(as.numeric(.data[[nCol]])), big.mark = ","), "\n",
            round(.data$pct_missing, 2), "% missing"
          ))
      } else {
        plotData <- plotData %>%
          dplyr::mutate(hover = paste0(
            .data$date_field, "\n",
            .data$cdm_name, "\n",
            round(.data$pct_missing, 2), "% missing"
          ))
      }

      ggplot2::ggplot(plotData, ggplot2::aes(
        x = .data$date_field, y = .data$pct_missing,
        fill = .data$cdm_name,
        text = .data$hover
      )) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(x = "Date Field", y = "% Missing", fill = "Database") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "bottom"
        )
    })

    output$plot <- plotly::renderPlotly({
      p <- missing_plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No missing dates data for selected filters."))
      plotly::ggplotly(p, tooltip = "text")
    })

    output$download_plot <- downloadHandler(
      filename = function() { "missingDatesPlot.png" },
      content = function(file) {
        p <- missing_plot_ggplot()
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
      if (is.null(d) || nrow(d) == 0) {
        return(renderPrettyDT(tibble::tibble()))
      }
      renderPrettyDT(d)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "missing_dates.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
