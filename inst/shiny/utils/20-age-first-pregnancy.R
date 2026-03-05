# 20-age-first-pregnancy.R - Age at first pregnancy module (standard Shiny module)
# Data: ageSummaryFirstPregnancy (columns: cdm_name, min, Q25, median, Q75, max, possibly mean, sd)

ageFirstPregnancyUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text", "Summary of maternal age at first pregnancy."),
    plotly::plotlyOutput(ns("plot"), height = "420px") %>% withSpinner(),
    h4("Download figure"),
    fluidRow(
      column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
      column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
      column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
    ),
    downloadButton(ns("download_plot"), "Download plot (PNG)"),
    h4("Data"),
    downloadButton(ns("download_table_csv"), "Download table (.csv)"),
    DT::DTOutput(ns("table")) %>% withSpinner()
  )
}

ageFirstPregnancyServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    plot_ggplot <- reactive({
      data <- ageSummaryFirstPregnancy
      if (is.null(data) || nrow(data) == 0) return(NULL)

      ageMetrics <- c("min", "Q25", "median", "Q75", "max")
      for (col in ageMetrics) {
        if (col %in% colnames(data)) data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
      }
      data <- data %>%
        dplyr::filter(dplyr::if_all(dplyr::all_of(intersect(ageMetrics, colnames(data))), ~ !is.na(.)))
      if (nrow(data) == 0 || !all(ageMetrics %in% colnames(data))) return(NULL)

      data <- data %>%
        dplyr::mutate(cdm_name = factor(.data$cdm_name, levels = rev(sort(unique(as.character(.data$cdm_name))))))

      ggplot2::ggplot(data, ggplot2::aes(
        x = .data$cdm_name,
        ymin = .data$min,
        lower = .data$Q25,
        middle = .data$median,
        upper = .data$Q75,
        ymax = .data$max
      )) +
        ggplot2::geom_boxplot(stat = "identity") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Age at first pregnancy (end)") +
        ggplot2::theme_minimal()
    })

    output$plot <- plotly::renderPlotly({
      p <- plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data available."))
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "ageFirstPregnancyPlot.png" },
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

    output$table <- DT::renderDT({
      renderPrettyDT(ageSummaryFirstPregnancy)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "age_first_pregnancy.csv" },
      content = function(file) {
        d <- ageSummaryFirstPregnancy
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
