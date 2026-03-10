# 12-outcome-categories.R - Outcome categories module (standard Shiny module)

outcomeCategoriesUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text", "Outcome category counts and percentages by algorithm and database."),
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

outcomeCategoriesServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    getData <- reactive({
      d <- filterByCdm(outcomeCategoriesCount, input$cdm, allDP)
      if (!is.null(d) && nrow(d) > 0) {
        # Ensure numeric columns
        if ("pct" %in% colnames(d)) d <- d %>% dplyr::mutate(pct = suppressWarnings(as.numeric(.data$pct)))
        if ("n" %in% colnames(d)) d <- d %>% dplyr::mutate(n = suppressWarnings(as.numeric(.data$n)))
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

    plot_ggplot <- reactive({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      barPlot(
        data = d,
        xVar = "outcome_category",
        yVar = "pct",
        fillVar = "algorithm",
        facetVar = "cdm_name",
        rotateAxisText = TRUE,
        position = "dodge",
        return_ggplot = TRUE
      )
    })

    output$plot <- plotly::renderPlotly({
      p <- plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data for selected filters."))
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "outcomeCategoriesPlot.png" },
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
    output$download_table_csv <- downloadHandler(
      filename = function() { "outcome_categories.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
