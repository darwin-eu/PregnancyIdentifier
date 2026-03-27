# 12-delivery-mode-by-year.R - Delivery mode by year module (standard Shiny module)

deliveryModeByYearUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text", "Delivery mode by year: % cesarean and % vaginal among those with known delivery mode, stratified by outcome category, year, and database."),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database", choices = allDP, selected = allDP, multiple = TRUE, options = opt)),
      column(3, pickerInput(ns("outcome"), "Outcome", choices = c("DELIV", "LB"), selected = c("DELIV", "LB"), multiple = TRUE, options = opt))
    ),
    tabsetPanel(
      tabPanel("Data",
               DT::DTOutput(ns("table")) %>% withSpinner(),
               downloadButton(ns("download_table_csv"), "Download table (.csv)")),
      tabPanel("Plot",
               plotlyOutput(ns("plot"), height = "420px") %>% withSpinner(),
               h4("Download figure"),
               fluidRow(
                 column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
                 column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
                 column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
               ),
               downloadButton(ns("download_plot"), "Download plot (PNG)"))
    )
  )
}

deliveryModeByYearServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePickerInput(session, "cdm", choices = rv$allDP, selected = rv$allDP)
    })

    getData <- reactive({
      d <- filterByCdm(rv$deliveryModeByYear, input$cdm, rv$allDP)
      if (!is.null(d) && nrow(d) > 0) {
        d <- d %>%
          dplyr::filter(.data$final_outcome_category %in% input$outcome) %>%
          dplyr::filter(!is.na(.data$year))
      }
      d
    })

    output$table <- DT::renderDT({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) {
        return(renderPrettyDT(tibble::tibble()))
      }
      renderPrettyDT(d)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "delivery_mode_by_year.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )

    plot_ggplot <- reactive({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      d <- d %>%
        dplyr::mutate(
          pct = suppressWarnings(as.numeric(.data$pct)),
          year = suppressWarnings(as.integer(.data$year))
        ) %>%
        dplyr::filter(!is.na(.data$pct), !is.na(.data$year))
      if (nrow(d) == 0) return(NULL)

      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$pct, color = .data$mode, linetype = .data$cdm_name)) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 1.5) +
        ggplot2::facet_wrap(~ final_outcome_category) +
        ggplot2::labs(x = "Year", y = "% of known delivery mode", color = "Mode", linetype = "Database") +
        ggplot2::ggtitle("Delivery mode rates by year") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
      p
    })

    output$plot <- plotly::renderPlotly({
      p <- plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No delivery mode by year data for selected filters."))
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "deliveryModeByYearPlot.png" },
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
  })
}
