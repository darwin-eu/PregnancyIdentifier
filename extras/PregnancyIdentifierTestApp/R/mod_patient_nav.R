# mod_patient_nav.R
# Patient navigation module - paging, new/duplicate person, display header

#' Patient Navigation UI
#'
#' @param id Module ID
#' @return UI elements
#' @export
mod_patient_nav_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML(paste0("
      #", ns("patient_nav_container"), " h5 {
        margin-top: 0 !important;
        margin-bottom: 0.75rem !important;
        padding-bottom: 0.125rem !important;
      }
      #", ns("patient_nav_container"), " > div {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }
      #", ns("patient_dots_container"), " {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
        padding-top: 0 !important;
        padding-bottom: 0 !important;
        line-height: 1 !important;
      }
    "))),
    div(
      id = ns("patient_nav_container"),
      h5("Patient Navigation", style = "margin-bottom: 0.75rem !important; margin-top: 0 !important; padding-bottom: 0.125rem !important;"),
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0 !important; margin-top: 0 !important; gap: 0.5rem; line-height: 1;",
        actionButton(ns("prev_person"), NULL,
          icon = icon("chevron-left", class = "fa-sm"),
          class = "btn-secondary btn-sm", style = "width: 32px; padding: 0.25rem; min-width: 32px; margin: 0 !important;"
        ),
        div(
          textOutput(ns("person_header"), inline = TRUE),
          style = "margin: 0 !important; padding: 0 !important; flex: 1; text-align: center; font-size: 0.8125rem; font-weight: 500; line-height: 1;"
        ),
        actionButton(ns("next_person"), NULL,
          icon = icon("chevron-right", class = "fa-sm"),
          class = "btn-secondary btn-sm", style = "width: 32px; padding: 0.25rem; min-width: 32px; margin: 0 !important;"
        )
      ),
      div(
        id = ns("patient_dots_container"),
        style = "display: flex; justify-content: center; align-items: flex-start; gap: 0.375rem; margin-top: 0 !important; margin-bottom: 0 !important; padding-top: 0 !important; padding-bottom: 0 !important; line-height: 1; min-height: auto;",
        uiOutput(ns("patient_dots"), inline = TRUE)
      ),
      div(
        style = "display: flex; gap: 0.375rem; flex-wrap: wrap; margin-top: 0.75rem !important; margin-bottom: 0 !important;",
        actionButton(ns("new_person"), "New Person",
          class = "btn-primary btn-sm",
          style = "flex: 1; font-size: 0.75rem; margin: 0 !important;"
        ),
        actionButton(ns("duplicate_person"), "Duplicate",
          class = "btn-secondary btn-sm",
          style = "flex: 1; font-size: 0.75rem; margin: 0 !important;"
        ),
        actionButton(ns("undo"), "Undo",
          class = "btn-warning btn-sm",
          style = "flex: 1; font-size: 0.75rem; margin: 0 !important;"
        )
      )
    )
  )
}

#' Patient Navigation Server
#'
#' @param id Module ID
#' @param state Reactive state
#' @param current_person_id Reactive current person ID
#' @param commit_state Function to commit state changes
#' @param undo Function to undo
#' @param update_current_person_id Function to update current person ID
#' @param global_start_date Optional reactive for global start date (used for new person observation period).
#' @param global_end_date Optional reactive for global end date (used for new person observation period).
#' @return List with reactive current_person_id
#' @export
mod_patient_nav_server <- function(id, state, current_person_id, commit_state, undo, update_current_person_id, global_start_date = NULL, global_end_date = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get person IDs
    person_ids <- reactive({
      if (is.null(state()) || !"person" %in% names(state()) || length(state()$person) == 0) {
        return(integer(0))
      }
      sapply(state()$person, function(p) as.integer(p$person_id))
    })

    # Current person index
    current_idx <- reactive({
      pids <- person_ids()
      if (length(pids) == 0) {
        return(0L)
      }
      idx <- which(pids == current_person_id())
      if (length(idx) == 0) {
        return(1L)
      }
      return(as.integer(idx))
    })

    # Display person header
    output$person_header <- renderText({
      pid <- current_person_id()
      if (is.null(pid)) {
        return("No Person")
      }

      return(sprintf("Person %d", pid))
    })

    # Display patient dots visualization
    output$patient_dots <- renderUI({
      pids <- person_ids()
      total_patients <- length(pids)
      idx <- current_idx()

      if (total_patients == 0) {
        return(NULL)
      }

      # Create dots - light grey for all, darker for current
      dots <- lapply(seq_len(total_patients), function(i) {
        is_current <- (i == idx)
        dot_color <- if (is_current) "#64748b" else "#cbd5e1" # darker grey for current, light grey for others
        dot_size <- if (is_current) "6px" else "5px"

        div(
          style = paste0(
            "width: ", dot_size, "; ",
            "height: ", dot_size, "; ",
            "border-radius: 50%; ",
            "background-color: ", dot_color, "; ",
            "display: inline-block; ",
            "transition: all 0.2s ease;"
          )
        )
      })

      return(do.call(tagList, dots))
    })

    # Navigation buttons
    observeEvent(input$prev_person, {
      idx <- current_idx()
      pids <- person_ids()
      if (idx > 1 && length(pids) > 0) {
        new_pid <- pids[idx - 1]
        update_current_person_id_local(new_pid)
      }
    })

    observeEvent(input$next_person, {
      idx <- current_idx()
      pids <- person_ids()
      if (idx < length(pids) && length(pids) > 0) {
        new_pid <- pids[idx + 1]
        update_current_person_id_local(new_pid)
      }
    })

    # Track when we're creating a new person to ensure we switch to it
    creating_new_person <- reactiveVal(FALSE)
    pending_new_person_id <- reactiveVal(NULL)

    # New person
    observeEvent(input$new_person, {
      # Get current state (isolate to prevent reactive issues)
      current_state <- isolate(state())

      # Create new person with observation period = global start/end when available
      g_start <- if (!is.null(global_start_date)) format(isolate(global_start_date())) else NULL
      g_end <- if (!is.null(global_end_date)) format(isolate(global_end_date())) else NULL
      new_state <- state_new_person_blank(current_state, obs_start = g_start, obs_end = g_end)

      # Get the new person ID before committing (it's the last person in the list)
      if (length(new_state$person) > 0) {
        new_pid <- new_state$person[[length(new_state$person)]]$person_id

        # Set flag and store the new person ID
        creating_new_person(TRUE)
        pending_new_person_id(new_pid)

        # Commit the state change
        commit_state(new_state, "New Person")

        # Update to the new person immediately after commit
        # This should switch the view to the new person
        update_current_person_id_local(new_pid)
      }
    })

    # Observer to ensure we switch to the new person after state has updated
    # Use observeEvent on state() to ensure it fires when state changes
    observeEvent(state(),
      {
        is_creating <- creating_new_person()
        pending_pid <- pending_new_person_id()

        # Only act if we're in the process of creating a new person
        if (is_creating && !is.null(pending_pid)) {
          state_val <- state()
          if (!is.null(state_val) &&
            "person" %in% names(state_val) &&
            length(state_val$person) > 0) {
            # Check if the pending person ID exists in the state
            all_pids <- sapply(state_val$person, function(p) as.integer(p$person_id))

            if (pending_pid %in% all_pids) {
              # Ensure we're viewing the new person - always update to the new person
              current_pid <- isolate(current_person_id())
              if (is.null(current_pid) || current_pid != pending_pid) {
                update_current_person_id_local(pending_pid)
              }
              # Clear the flag
              creating_new_person(FALSE)
              pending_new_person_id(NULL)
            }
          }
        }
      },
      ignoreInit = TRUE
    )

    # Duplicate person
    observeEvent(input$duplicate_person, {
      pid <- current_person_id()
      if (!is.null(pid)) {
        new_state <- state_duplicate_person(state(), pid)
        commit_state(new_state, "Duplicate Person")
        # Set current person to the duplicated one (get from new_state, not old state)
        if (length(new_state$person) > 0) {
          new_pid <- new_state$person[[length(new_state$person)]]$person_id
          update_current_person_id_local(new_pid)
        }
      }
    })

    # Undo
    observeEvent(input$undo, {
      undo()
    })

    # Internal function to update current person ID
    update_current_person_id_local <- function(pid) {
      update_current_person_id(pid)
    }

    # Return reactive current person ID
    return(reactive({
      current_person_id()
    }))
  })
}

# Uncomment below to test this module independently
# library(shiny)
# library(bslib)
# source("R/services_state.R")
#
# # Create test state with sample data
# test_state <- list(
#   person = list(
#     list(person_id = 1L, year_of_birth = 1980L, gender_concept_id = 8507L),
#     list(person_id = 2L, year_of_birth = 1975L, gender_concept_id = 8532L)
#   ),
#   observation_period = list(
#     list(observation_period_id = 1L, person_id = 1L,
#          observation_period_start_date = "2020-01-01",
#          observation_period_end_date = "2020-12-31", period_type_concept_id = 44814724L),
#     list(observation_period_id = 2L, person_id = 2L,
#          observation_period_start_date = "2019-01-01",
#          observation_period_end_date = "2019-12-31", period_type_concept_id = 44814724L)
#   )
# )
#
# ui <- page_sidebar(
#   title = "Test Patient Nav",
#   sidebar = mod_patient_nav_ui("test")
# )
#
# server <- function(input, output, session) {
#   state_rv <- reactiveVal(test_state)
#   current_pid_rv <- reactiveVal(1L)
#
#   commit_state <- function(new_state, label) {
#     cat("Commit:", label, "\n")
#     state_rv(new_state)
#   }
#
#   undo <- function() {
#     cat("Undo called\n")
#   }
#
#   update_pid <- function(pid) {
#     current_pid_rv(pid)
#   }
#
#   mod_patient_nav_server(
#     "test",
#     state = reactive(state_rv()),
#     current_person_id = reactive(current_pid_rv()),
#     commit_state = commit_state,
#     undo = undo,
#     update_current_person_id = update_pid
#   )
# }
#
# shinyApp(ui = ui, server = server)
