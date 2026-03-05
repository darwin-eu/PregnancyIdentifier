# 01-background.R - Study background module (standard Shiny module)

backgroundUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("backgroundContent"))
  )
}

backgroundServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$backgroundContent <- renderUI({
      mdFile <- "./background.md"
      if (file.exists(mdFile)) {
        shiny::includeMarkdown(mdFile)
      } else {
        p("No background information available.")
      }
    })
  })
}
