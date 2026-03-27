# 11-delivery-mode.R - Delivery mode module (standard Shiny module)

deliveryModeUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text", "Delivery mode: % cesarean and % vaginal among those with known delivery mode, by outcome category and database."),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database", choices = allDP, selected = allDP, multiple = TRUE, options = opt))
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

deliveryModeServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePickerInput(session, "cdm", choices = rv$allDP, selected = rv$allDP)
    })

    getData <- reactive({
      d <- filterByCdm(rv$deliveryModeSummary, input$cdm, rv$allDP)
      if (!is.null(d) && nrow(d) > 0) {
        d <- d %>%
          dplyr::mutate(cdm_name = factor(.data$cdm_name, levels = rev(sort(unique(.data$cdm_name)))))
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
      filename = function() { "delivery_mode.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )

    plot_ggplot <- reactive({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      if ("pct" %in% colnames(d)) {
        d <- d %>% dplyr::mutate(pct = suppressWarnings(as.numeric(.data$pct)))
      }
      if ("n" %in% colnames(d)) {
        d <- d %>% dplyr::mutate(n = suppressWarnings(as.numeric(.data$n)))
      }
      barPlot(
        data = d,
        xVar = "cdm_name",
        yVar = "pct",
        fillVar = "mode",
        facetVar = "final_outcome_category",
        label = "n",
        title = "Delivery mode rates",
        rotateAxisText = TRUE,
        flipCoordinates = TRUE,
        return_ggplot = TRUE
      )
    })

    output$plot <- plotly::renderPlotly({
      p <- plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No delivery mode data for selected filters."))
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "deliveryModePlot.png" },
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
