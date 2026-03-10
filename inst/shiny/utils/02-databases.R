# 02-databases.R - Database information module (standard Shiny module)

databasesUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text",
        "CDM and snapshot metadata. Used to track provenance and identify the source database."),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("cdm"), "Database",
                                          choices = allDP, selected = allDP,
                                          multiple = TRUE, options = opt))
    ),
    DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner(),
    downloadButton(ns("download_table_csv"), "Download table (.csv)")
  )
}

databasesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    tableData <- reactive({
      filterByCdm(dbinfo, input$cdm, allDP)
    })
    output$table <- DT::renderDT({
      renderPrettyDT(tableData())
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "databases.csv" },
      content = function(file) {
        d <- tableData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
