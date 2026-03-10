# 17-observation-period.R - Observation period range module (standard Shiny module)

observationPeriodUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text", "Range of observation periods in the CDM."),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database",
                           choices = if (exists("allDP")) allDP else character(0),
                           selected = if (exists("allDP")) allDP else character(0),
                           multiple = TRUE, options = opt))
    ),
    DT::DTOutput(ns("table")) %>% withSpinner(),
    downloadButton(ns("download_table_csv"), "Download table (.csv)")
  )
}

observationPeriodServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    tableData <- reactive({
      d <- observationPeriodRange
      if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(NULL)
      if ("cdm_name" %in% colnames(d)) {
        d <- filterByCdm(d, input$cdm, allDP)
      }
      d
    })
    output$table <- DT::renderDT({
      d <- tableData()
      if (is.null(d) || nrow(d) == 0) return(renderPrettyDT(tibble::tibble()))
      renderPrettyDT(d)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "observation_period.csv" },
      content = function(file) {
        d <- tableData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
