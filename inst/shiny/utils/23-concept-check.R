# 23-concept-check.R - Concept check module (standard Shiny module)
# Data: conceptCheck. Simple filter table with CDM picker.

conceptCheckUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text", HTML(paste(
      "Concept-level checks for pregnancy-related concepts: validates concept presence and timing relative to each pregnancy episode.",
      "Counts are restricted to concept occurrences linked to pregnancy episodes (study population/period).",
      "Row totals are the number of occurrences in pregnancy episodes; timing percentages use that row total as denominator."
    ))),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database",
                           choices = character(0), selected = character(0),
                           multiple = TRUE, options = opt))
    ),
    DT::DTOutput(ns("table")) %>% withSpinner(),
    downloadButton(ns("download_table_csv"), "Download table (.csv)")
  )
}

conceptCheckServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      dp <- if ("cdm_name" %in% colnames(rv$conceptCheck)) {
        unique(c(rv$allDP, rv$conceptCheck$cdm_name))
      } else {
        rv$allDP
      }
      updatePickerInput(session, "cdm", choices = dp, selected = dp)
    })

    tableData <- reactive({
      dp <- if ("cdm_name" %in% colnames(rv$conceptCheck)) {
        unique(c(rv$allDP, rv$conceptCheck$cdm_name))
      } else {
        rv$allDP
      }
      filterByCdm(rv$conceptCheck, input$cdm, dp)
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
