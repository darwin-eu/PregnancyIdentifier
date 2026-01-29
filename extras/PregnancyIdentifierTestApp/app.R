# app.R
# Pregnancy Identifier Debugger — multipage app; each page is a module with its own sidebar + main.
#
# Pages: Edit input, Run Algorithm, Outputs (RDS), Exports (CSV).
# Shared state: state_server() provides app_state; edit and run pages use it.

library(shiny)
library(bslib)
library(jsonlite)
library(DT)
if (!requireNamespace("readxl", quietly = TRUE)) {
  # readxl used by services_allowed_concepts; optional at load, required for concept restriction
}

source("R/config.R")
source("R/utils.R")
source("R/services_state.R")
source("R/services_excel.R")
source("R/services_concepts.R")
source("R/services_allowed_concepts.R")
source("R/services_safe_outputs.R")
source("R/services_app_logging.R")
source("R/mod_state.R")
source("R/mod_edit.R")
source("R/mod_run.R")
source("R/mod_patient_nav.R")
source("R/mod_timeline_canvas.R")
source("R/mod_event_editor.R")
source("R/mod_import_export.R")
source("R/mod_run_identifier.R")
source("R/mod_outputs_browser.R")

theme <- bslib::bs_theme(
  version = 5,
  primary = "#2563eb",
  secondary = "#64748b",
  success = "#10b981",
  warning = "#f59e0b",
  danger = "#ef4444",
  bg = "#ffffff",
  fg = "#1e293b",
  font_scale = 0.875
)

head_content <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
  tags$script(HTML("
    (function() {
      if (typeof console === 'undefined') return;
      var orig = { warn: console.warn, error: console.error, log: console.log, info: console.info };
      var sup = function(args) { var m = Array.prototype.slice.call(args).map(String).join(' '); return /DEPRECATED/i.test(m) && /bootstrap-datepicker/i.test(m); };
      console.warn = function() { if (!sup(arguments)) orig.warn.apply(console, arguments); };
      console.error = function() { if (!sup(arguments)) orig.error.apply(console, arguments); };
      console.log = function() { if (!sup(arguments)) orig.log.apply(console, arguments); };
      console.info = function() { if (!sup(arguments)) orig.info.apply(console, arguments); };
    })();
  ")),
  tags$style(HTML("
    .sidebar select[id*='person_sex'], #app-person_sex { font-size: 8pt !important; padding: 0.125rem 0.25rem !important; height: 1.75rem !important; }
    .sex-dropdown select { font-size: 8pt !important; height: 1.25rem !important; }
  "))
)

ui <- shiny::tagList(
  head_content,
  bslib::page_navbar(
    title = "Pregnancy Identifier Debugger",
    theme = theme,
    edit_ui("edit"),
    run_ui("run"),
    outputs_rds_ui("outputs_rds"),
    outputs_csv_ui("outputs_csv")
  )
)

server <- function(input, output, session) {
  state <- state_server("state")
  edit_server("edit", state)

  run_res <- run_server("run", state)
  output_dir_rv <- shiny::reactiveVal(NULL)
  shiny::observeEvent(run_res$output_dir(), {
    d <- run_res$output_dir()
    if (!is.null(d)) {
      output_dir_rv(d)
      cat("output_dir_rv updated:", d, "\n")
    }
  })

  outputs_rds_server("outputs_rds", folder_r = shiny::reactive(output_dir_rv()))
  outputs_csv_server("outputs_csv", folder_r = shiny::reactive(output_dir_rv()))
}

shinyApp(ui = ui, server = server)
