# 23-concept-check.R - Concept check module (standard Shiny module)
# Data: conceptCheck. Simple filter table with CDM picker.

conceptCheckUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text", HTML(paste(
      "Concept-level checks for pregnancy-related concepts.",
      "Validates concept presence and timing relative to each pregnancy episode."
    ))),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database",
                           choices = character(0), selected = character(0),
                           multiple = TRUE, options = opt))
    ),
    downloadButton(ns("download_table_csv"), "Download table (.csv)"),
    DT::DTOutput(ns("table")) %>% withSpinner()
  )
}

conceptCheckServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    dp <- if ("cdm_name" %in% colnames(conceptCheck)) {
      unique(c(allDP, conceptCheck$cdm_name))
    } else {
      allDP
    }

    observe({
      updatePickerInput(session, "cdm", choices = dp, selected = dp)
    })

    tableData <- reactive({
      filterByCdm(conceptCheck, input$cdm, dp)
    })
    output$table <- DT::renderDT({
      renderPrettyDT(tableData())
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "concept_check.csv" },
      content = function(file) {
        d <- tableData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
