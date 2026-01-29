# mod_import_export.R
# Import/export module - JSON and Excel upload/download

#' Import/Export UI
#'
#' @param id Module ID
#' @return UI elements
#' @export
mod_import_export_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h5("Import/Export"),
    div(
      style = "display: flex; flex-direction: column; gap: 0.5rem;",
      fileInput(ns("upload_file"), "Upload File",
        accept = c(".json", ".xlsx"),
        buttonLabel = "Browse...",
        placeholder = "JSON or Excel file",
        width = "100%"
      ),
      downloadButton(ns("download_excel"), "Download Excel",
        class = "btn-primary btn-sm",
        style = "width: 100%; font-size: 0.75rem;"
      ),
      downloadButton(ns("download_json"), "Download JSON",
        class = "btn-secondary btn-sm",
        style = "width: 100%; font-size: 0.75rem;"
      )
    )
  )
}

#' Import/Export Server
#'
#' @param id Module ID
#' @param state Reactive state
#' @param commit_state Function to commit state changes
#' @return NULL
#' @export
mod_import_export_server <- function(id, state, commit_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Upload file (handles both JSON and Excel)
    observeEvent(input$upload_file, {
      req(input$upload_file)

      file_ext <- tools::file_ext(input$upload_file$name)
      file_path <- input$upload_file$datapath

      tryCatch(
        {
          if (tolower(file_ext) == "json") {
            new_state <- state_init_from_json(file_path)
            validation <- state_validate(new_state, stop_on_error = FALSE)
            if (!validation$valid) {
              showNotification(
                paste(
                  "Validation errors in imported JSON:",
                  paste(validation$errors, collapse = "\n")
                ),
                type = "error"
              )
              return(NULL)
            }
            commit_state(new_state, "Import JSON")
            showNotification("JSON imported successfully", type = "success")
          } else if (tolower(file_ext) == "xlsx") {
            new_state <- excel_to_state(file_path)
            validation <- state_validate(new_state, stop_on_error = FALSE)
            if (!validation$valid) {
              showNotification(
                paste(
                  "Validation errors in imported Excel:",
                  paste(validation$errors, collapse = "\n")
                ),
                type = "error"
              )
              return(NULL)
            }
            commit_state(new_state, "Import Excel")
            showNotification("Excel imported successfully", type = "success")
          } else {
            showNotification("Unsupported file type. Please upload a .json or .xlsx file.",
              type = "error"
            )
          }
        },
        error = function(e) {
          showNotification(paste("Error importing file:", conditionMessage(e)),
            type = "error"
          )
        }
      )
    })

    # Download Excel
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("omop_patient_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        state_to_excel(state(), file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    # Download JSON (internal state)
    output$download_json <- downloadHandler(
      filename = function() {
        paste0("omop_patient_data_", Sys.Date(), ".json")
      },
      content = function(file) {
        writeLines(state_to_json_string(state()), file)
      },
      contentType = "application/json"
    )
  })
}

# Uncomment below to test this module independently
# library(shiny)
# library(bslib)
# source("R/services_state.R")
# source("R/services_excel.R")
#
# # Create test state
# test_state <- list(
#   person = list(
#     list(person_id = 1L, gender_concept_id = 8507L, year_of_birth = 1980L)
#   ),
#   observation_period = list(
#     list(observation_period_id = 1L, person_id = 1L,
#          observation_period_start_date = "2020-01-01",
#          observation_period_end_date = "2020-12-31", period_type_concept_id = 44814724L)
#   ),
#   condition_occurrence = list(),
#   drug_exposure = list(),
#   procedure_occurrence = list(),
#   measurement = list(),
#   visit_occurrence = list(),
#   visit_detail = list()
# )
#
# ui <- page_sidebar(
#   title = "Test Import/Export",
#   sidebar = mod_import_export_ui("test")
# )
#
# server <- function(input, output, session) {
#   state_rv <- reactiveVal(test_state)
#
#   commit_state <- function(new_state, label) {
#     cat("Commit:", label, "\n")
#     state_rv(new_state)
#   }
#
#   mod_import_export_server(
#     "test",
#     state = reactive(state_rv()),
#     commit_state = commit_state
#   )
# }
#
# shinyApp(ui = ui, server = server)
