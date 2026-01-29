# mod_run.R
# Run Algorithm page: nav_panel with layout_sidebar (options sidebar + main).
# Uses shared state for test_data_r and current_person_id_r.

#' Run page UI: nav_panel with layout_sidebar (sidebar + main).
#'
#' @param id Module id (e.g. "run")
#' @return bslib::nav_panel for Run Algorithm
#' @export
run_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "Run Algorithm",
    value = "run",
    bslib::layout_sidebar(
      sidebar = mod_run_identifier_sidebar_ui(ns("run_identifier")),
      mod_run_identifier_main_ui(ns("run_identifier"))
    )
  )
}

#' Run page Server
#'
#' @param id Module id (e.g. "run")
#' @param state Return value from state_server() (must have state and current_person_id reactives)
#' @return List with output_dir (reactive) for use by outputs browsers
#' @export
run_server <- function(id, state) {
  res <- shiny::moduleServer(id, function(input, output, session) {
    run_res <- mod_run_identifier_server(
      "run_identifier",
      test_data_r = state$state,
      current_person_id_r = state$current_person_id
    )
    list(output_dir = run_res$output_dir)
  })
  res
}
