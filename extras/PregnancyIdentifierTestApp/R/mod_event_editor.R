# mod_event_editor.R
# Event editor: modal for editing one event (domain, concept_id, dates);
# uses %||% from utils.R (sourced before modules in app.R).

#' Event Editor UI
#'
#' @param id Module ID
#' @return UI elements (modal)
#' @export
mod_event_editor_ui <- function(id) {
  ns <- NS(id)
  # Modal is shown programmatically, no UI needed here
  tagList()
}

#' Event Editor Server
#'
#' @param id Module ID
#' @param selected_event Reactive selected event
#' @param state Reactive state
#' @param commit_state Function to commit state changes
#' @return NULL
#' @export
mod_event_editor_server <- function(id, selected_event, state, commit_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show modal when event is selected
    observeEvent(selected_event(), {
      event <- selected_event()
      if (is.null(event)) {
        return
      }

      # Determine concept field name
      concept_field <- switch(event$domain,
        visit_occurrence = "visit_concept_id",
        visit_detail = "visit_detail_concept_id",
        drug_exposure = "drug_concept_id",
        condition_occurrence = "condition_concept_id",
        procedure_occurrence = "procedure_concept_id",
        measurement = "measurement_concept_id"
      )

      current_concept_id <- event$record[[concept_field]] %||% 0L
      choices <- allowed_concept_choices()
      # If current concept is not in allowed set, add a "(Not allowed) <id>" option so user can see it
      current_int <- as.integer(current_concept_id)
      if (length(current_int) == 1L && !is.na(current_int) && !(current_int %in% choices)) {
        not_allowed_label <- paste0("(Not allowed) ", current_int)
        choices <- c(structure(current_int, names = not_allowed_label), choices)
      }

      # Show modal
      showModal(modalDialog(
        title = div(
          style = "font-size: 0.9375rem; font-weight: 600;",
          sprintf("Edit %s", event$domain)
        ),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel", style = "font-size: 0.75rem;"),
          actionButton(ns("delete_event"), "Delete",
            class = "btn-danger btn-sm",
            style = "font-size: 0.75rem;"
          ),
          actionButton(ns("save_event"), "Save",
            class = "btn-primary btn-sm",
            style = "font-size: 0.75rem;"
          )
        ),
        selectizeInput(ns("concept_id"),
          label = div(style = "font-size: 0.8125rem; font-weight: 500;", "Concept (restricted to HIP/PPS/ESD):"),
          choices = choices,
          selected = current_concept_id,
          width = "100%",
          options = list(placeholder = "Select or search concept", maxOptions = 10000)
        ),
        dateInput(ns("start_date"),
          label = div(style = "font-size: 0.8125rem; font-weight: 500;", "Start Date:"),
          value = parse_date_safe(event$start_date, default = Sys.Date()),
          width = "100%"
        ),
        dateInput(ns("end_date"),
          label = div(style = "font-size: 0.8125rem; font-weight: 500;", "End Date:"),
          value = parse_date_safe(event$end_date, default = Sys.Date()),
          width = "100%"
        ),
        selectInput(ns("domain"),
          label = div(style = "font-size: 0.8125rem; font-weight: 500;", "Domain:"),
          choices = c(
            "visit_occurrence", "visit_detail", "drug_exposure",
            "condition_occurrence", "procedure_occurrence", "measurement"
          ),
          selected = event$domain,
          width = "100%"
        )
      ))
    })

    # Save event
    observeEvent(input$save_event, {
      event <- selected_event()
      if (is.null(event)) {
        removeModal()
        return
      }

      # Require concept_id to be in the allowed set before saving
      concept_val <- as.integer(input$concept_id)
      if (length(concept_val) != 1L || is.na(concept_val) || !is_allowed_concept(concept_val)) {
        showNotification(
          "Please select an allowed concept (HIP/PPS/ESD). Current value is not in the allowed set.",
          type = "warning",
          duration = 4
        )
        return
      }

      # Get the domain from input (user may have changed it)
      new_domain <- input$domain
      old_domain <- event$domain

      # Update state
      new_state <- state()

      # If domain changed, we need to move the record to the new domain
      if (new_domain != old_domain) {
        # Delete from old domain
        new_state <- event_delete(new_state, old_domain, event$id_value, event$id_field)

        # Create new record in new domain with new ID
        id_field <- switch(new_domain,
          visit_occurrence = "visit_occurrence_id",
          visit_detail = "visit_detail_id",
          drug_exposure = "drug_exposure_id",
          condition_occurrence = "condition_occurrence_id",
          procedure_occurrence = "procedure_occurrence_id",
          measurement = "measurement_id"
        )

        concept_field <- switch(new_domain,
          visit_occurrence = "visit_concept_id",
          visit_detail = "visit_detail_concept_id",
          drug_exposure = "drug_concept_id",
          condition_occurrence = "condition_concept_id",
          procedure_occurrence = "procedure_concept_id",
          measurement = "measurement_concept_id"
        )

        # Create new record
        new_record <- list(person_id = event$record$person_id)
        new_record[[id_field]] <- state_next_domain_id(new_state, new_domain, id_field)
        new_record[[concept_field]] <- as.integer(input$concept_id)

        # Set date fields based on new domain
        if (new_domain == "visit_occurrence") {
          new_record$visit_start_date <- as.character(input$start_date)
          new_record$visit_end_date <- as.character(input$end_date)
          new_record$visit_type_concept_id <- 32817L
        } else if (new_domain == "visit_detail") {
          new_record$visit_detail_start_date <- as.character(input$start_date)
          new_record$visit_detail_end_date <- as.character(input$end_date)
          new_record$visit_detail_type_concept_id <- 32817L
        } else if (new_domain == "drug_exposure") {
          new_record$drug_exposure_start_date <- as.character(input$start_date)
          new_record$drug_exposure_end_date <- as.character(input$end_date)
          new_record$drug_type_concept_id <- 38000177L
        } else if (new_domain == "condition_occurrence") {
          new_record$condition_start_date <- as.character(input$start_date)
          new_record$condition_end_date <- as.character(input$end_date)
          new_record$condition_type_concept_id <- 44786627L
        } else if (new_domain == "procedure_occurrence") {
          new_record$procedure_date <- as.character(input$start_date)
          new_record$procedure_type_concept_id <- 38000275L
        } else if (new_domain == "measurement") {
          new_record$measurement_date <- as.character(input$start_date)
          new_record$measurement_type_concept_id <- 44818702L
        }

        # Add to new domain
        new_state <- event_add(new_state, new_domain, new_record)
      } else {
        # Domain unchanged, just update fields
        updates <- list()

        if (new_domain == "visit_occurrence") {
          updates$visit_start_date <- as.character(input$start_date)
          updates$visit_end_date <- as.character(input$end_date)
          updates$visit_concept_id <- as.integer(input$concept_id)
        } else if (new_domain == "visit_detail") {
          updates$visit_detail_start_date <- as.character(input$start_date)
          updates$visit_detail_end_date <- as.character(input$end_date)
          updates$visit_detail_concept_id <- as.integer(input$concept_id)
        } else if (new_domain == "drug_exposure") {
          updates$drug_exposure_start_date <- as.character(input$start_date)
          updates$drug_exposure_end_date <- as.character(input$end_date)
          updates$drug_concept_id <- as.integer(input$concept_id)
        } else if (new_domain == "condition_occurrence") {
          updates$condition_start_date <- as.character(input$start_date)
          updates$condition_end_date <- as.character(input$end_date)
          updates$condition_concept_id <- as.integer(input$concept_id)
        } else if (new_domain == "procedure_occurrence") {
          updates$procedure_date <- as.character(input$start_date)
          updates$procedure_concept_id <- as.integer(input$concept_id)
        } else if (new_domain == "measurement") {
          updates$measurement_date <- as.character(input$start_date)
          updates$measurement_concept_id <- as.integer(input$concept_id)
        }

        # Apply updates
        new_state <- event_update(
          new_state, new_domain, event$id_value,
          event$id_field, updates
        )
      }

      commit_state(new_state, "Update Event")
      removeModal()
    })

    # Delete event
    observeEvent(input$delete_event, {
      event <- selected_event()
      if (is.null(event)) {
        removeModal()
        return
      }

      new_state <- event_delete(state(), event$domain, event$id_value, event$id_field)
      commit_state(new_state, "Delete Event")
      removeModal()
    })
  })
}

# Uncomment below to test this module independently
# library(shiny)
# library(bslib)
# source("R/services_state.R")
#
# # Create test state
# test_state <- list(
#   person = list(list(person_id = 1L)),
#   observation_period = list(),
#   condition_occurrence = list(
#     list(condition_occurrence_id = 1L, person_id = 1L, condition_concept_id = 123L,
#          condition_start_date = "2020-03-15", condition_end_date = "2020-03-20",
#          condition_type_concept_id = 44786627L)
#   )
# )
#
# # Create test selected event
# test_event <- reactiveVal(list(
#   domain = "condition_occurrence",
#   id_field = "condition_occurrence_id",
#   id_value = 1L,
#   start_date = "2020-03-15",
#   end_date = "2020-03-20",
#   concept_id = 123L,
#   record = test_state$condition_occurrence[[1]]
# ))
#
# ui <- page_sidebar(
#   title = "Test Event Editor",
#   actionButton("trigger_select", "Select Event (opens modal)")
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
#   observeEvent(input$trigger_select, {
#     test_event(list(
#       domain = "condition_occurrence",
#       id_field = "condition_occurrence_id",
#       id_value = 1L,
#       start_date = "2020-03-15",
#       end_date = "2020-03-20",
#       concept_id = 123L,
#       record = test_state$condition_occurrence[[1]]
#     ))
#   })
#
#   mod_event_editor_server(
#     "test",
#     selected_event = test_event,
#     state = reactive(state_rv()),
#     commit_state = commit_state
#   )
# }
#
# shinyApp(ui = ui, server = server)
