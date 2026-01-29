# mod_timeline_canvas.R
# Timeline canvas: render events on a time axis, handle draw/select/edit;
# uses %||% from utils.R (sourced before modules in app.R).

#' Timeline Canvas UI
#'
#' @param id Module ID
#' @return UI elements
#' @export
mod_timeline_canvas_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$script(src = "d3.v7.min.js"),
      tags$script(src = "timeline_d3.js"),
      tags$script(HTML(sprintf("
        $(document).on('click', '.btn-delete-event', function() {
          var row = $(this).data('row');
          var container = $(this).closest('[data-namespace]');
          var ns = container.attr('data-namespace') || '%s';
          var inputName = ns + 'delete_event';
          console.log('Delete button clicked. Row:', row, 'Input name:', inputName);
          Shiny.setInputValue(inputName, {row: row, nonce: Math.random()}, {priority: 'event'});
        });
        $(document).on('click', '.btn-search-concept', function() {
          var row = $(this).data('row');
          var container = $(this).closest('[data-namespace]');
          var ns = container.attr('data-namespace') || '%s';
          var inputName = ns + 'search_concept';
          var textInput = $(this).siblings('input.concept-id-input');
          var searchText = textInput.val() || '';
          console.log('Search button clicked. Row:', row, 'Search text:', searchText);
          Shiny.setInputValue(inputName, {row: row, query: searchText, nonce: Math.random()}, {priority: 'event'});
        });
        $(document).on('keydown', '.concept-id-input', function(e) {
          if (e.key === 'Enter' || e.keyCode === 13) {
            e.preventDefault();
            var row = $(this).data('row');
            var container = $(this).closest('[data-namespace]');
            var ns = container.attr('data-namespace') || '%s';
            var inputName = ns + 'search_concept';
            var searchText = $(this).val() || '';
            console.log('Enter key pressed in concept ID input. Row:', row, 'Search text:', searchText);
            Shiny.setInputValue(inputName, {row: row, query: searchText, nonce: Math.random()}, {priority: 'event'});
          }
        });
        $(document).on('blur', '.concept-id-input', function() {
          var row = $(this).data('row');
          var container = $(this).closest('[data-namespace]');
          var ns = container.attr('data-namespace') || '%s';
          var inputName = ns + 'update_concept_id';
          var conceptId = $(this).val() || '';
          if (conceptId && /^\\d+$/.test(conceptId.trim())) {
            console.log('Concept ID input changed. Row:', row, 'Concept ID:', conceptId);
            Shiny.setInputValue(inputName, {row: row, concept_id: parseInt(conceptId.trim()), nonce: Math.random()}, {priority: 'event'});
          }
        });
      ", ns(""), ns(""), ns(""), ns(""))))
    ),
    card(
      height = "500px",
      card_header(
        style = "font-size: 0.875rem; padding: 0.5rem 0.75rem;",
        "Patient Timeline"
      ),
      div(
        id = ns("timeline"),
        class = "timeline-container",
        style = "width: 100%; height: 320px; position: relative; padding: 0.5rem;",
        tags$svg(
          id = ns("timeline-svg"),
          style = "border: 1px solid #e2e8f0; border-radius: 0.375rem; cursor: crosshair; width: 100%; height: 100%; display: block;"
        )
      )
    ),
    card(
      height = "500px",
      card_header(
        style = "font-size: 0.875rem; padding: 0.5rem 0.75rem;",
        "Event Data"
      ),
      div(
        style = "padding: 0.5rem;",
        div(
          id = ns("event_table_container"), `data-namespace` = ns(""),
          DT::dataTableOutput(ns("event_table"))
        )
      )
    )
  )
}

#' Timeline Canvas Server
#'
#' @param id Module ID
#' @param state Reactive state
#' @param current_person_id Reactive current person ID
#' @param commit_state Function to commit state changes
#' @param global_start_date Reactive global start date
#' @param global_end_date Reactive global end date
#' @return List with reactive selected_event
#' @export
mod_timeline_canvas_server <- function(id, state, current_person_id, commit_state, global_start_date = NULL, global_end_date = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cat("[timeline] module server started | id = ", id, " | ns('') = '", ns(""), "' | message_type will be '", paste0(ns(""), "update_timeline"), "'\n", sep = "")

    # Selected event reactive
    selected_event <- reactiveVal(NULL)

    # Get events for current person
    person_events <- reactive({
      pid <- current_person_id()
      state_val <- state()

      if (is.null(pid) || is.null(state_val)) {
        return(list())
      }

      events <- list()

      # Get all domain events for this person (observation_period first = top row)
      domains <- c(
        "observation_period", "visit_occurrence", "visit_detail", "drug_exposure",
        "condition_occurrence", "procedure_occurrence", "measurement"
      )

      for (domain in domains) {
        if (domain %in% names(state_val) && length(state_val[[domain]]) > 0) {
          domain_events <- Filter(function(r) same_id(r$person_id, pid), state_val[[domain]])
          for (event in domain_events) {
            # Determine date fields
            start_date <- NULL
            end_date <- NULL

            if (domain == "observation_period") {
              start_date <- event$observation_period_start_date
              end_date <- event$observation_period_end_date
            } else if (domain == "visit_occurrence") {
              start_date <- event$visit_start_date
              end_date <- event$visit_end_date
            } else if (domain == "visit_detail") {
              start_date <- event$visit_detail_start_date
              end_date <- event$visit_detail_end_date
            } else if (domain == "drug_exposure") {
              start_date <- event$drug_exposure_start_date
              end_date <- event$drug_exposure_end_date
            } else if (domain == "condition_occurrence") {
              start_date <- event$condition_start_date
              end_date <- event$condition_end_date
            } else if (domain == "procedure_occurrence") {
              start_date <- event$procedure_date
              end_date <- event$procedure_date
            } else if (domain == "measurement") {
              start_date <- event$measurement_date
              end_date <- event$measurement_date
            }

            if (!is.null(start_date)) {
              # Get source_concept_code (source_value field) for this domain
              source_value_field <- switch(domain,
                observation_period = NA_character_,
                visit_occurrence = "visit_source_value",
                visit_detail = "visit_detail_source_value",
                drug_exposure = "drug_source_value",
                condition_occurrence = "condition_source_value",
                procedure_occurrence = "procedure_source_value",
                measurement = "measurement_source_value"
              )
              source_concept_code <- if (is.na(source_value_field)) "" else (event[[source_value_field]] %||% "")

              events[[length(events) + 1]] <- list(
                domain = domain,
                id_field = switch(domain,
                  observation_period = "observation_period_id",
                  visit_occurrence = "visit_occurrence_id",
                  visit_detail = "visit_detail_id",
                  drug_exposure = "drug_exposure_id",
                  condition_occurrence = "condition_occurrence_id",
                  procedure_occurrence = "procedure_occurrence_id",
                  measurement = "measurement_id"
                ),
                id_value = event[[switch(domain,
                  observation_period = "observation_period_id",
                  visit_occurrence = "visit_occurrence_id",
                  visit_detail = "visit_detail_id",
                  drug_exposure = "drug_exposure_id",
                  condition_occurrence = "condition_occurrence_id",
                  procedure_occurrence = "procedure_occurrence_id",
                  measurement = "measurement_id"
                )]],
                start_date = start_date,
                end_date = end_date %||% start_date,
                concept_id = event[[switch(domain,
                  observation_period = "period_type_concept_id",
                  visit_occurrence = "visit_concept_id",
                  visit_detail = "visit_detail_concept_id",
                  drug_exposure = "drug_concept_id",
                  condition_occurrence = "condition_concept_id",
                  procedure_occurrence = "procedure_concept_id",
                  measurement = "measurement_concept_id"
                )]] %||% 0L,
                source_concept_code = source_concept_code,
                record = event
              )
            }
          }
        }
      }

      return(events)
    })

    # Get observation period for current person
    obs_period <- reactive({
      pid <- current_person_id()
      state_val <- state()

      if (is.null(pid) || is.null(state_val) ||
        !"observation_period" %in% names(state_val)) {
        return(NULL)
      }

      periods <- Filter(function(p) same_id(p$person_id, pid), state_val$observation_period)
      if (length(periods) > 0) {
        return(periods[[1]])
      }
      return(NULL)
    })

    # Get date of birth for current person (as YYYY-MM-DD string)
    current_person_dob <- reactive({
      pid <- current_person_id()
      state_val <- state()

      if (is.null(pid) || is.null(state_val) || !"person" %in% names(state_val)) {
        return(NULL)
      }

      person_rec <- Find(function(p) same_id(p$person_id, pid), state_val$person)
      if (is.null(person_rec)) {
        return(NULL)
      }

      dob <- person_dob(person_rec)
      if (!is.null(dob)) {
        return(format_date_omop(dob))
      }
      return(NULL)
    })

    # Build and send timeline data to JS (shared so we can send on timeline_ready and on data change)
    message_type <- paste0(ns(""), "update_timeline")
    send_timeline_to_js <- function() {
      events <- person_events()
      period <- obs_period()
      dob <- current_person_dob()
      js_data <- lapply(events, function(e) {
        list(
          domain = e$domain,
          id_value = e$id_value,
          start_date = e$start_date,
          end_date = e$end_date,
          concept_id = e$concept_id
        )
      })
      global_start <- if (!is.null(global_start_date)) global_start_date() else NULL
      global_end <- if (!is.null(global_end_date)) global_end_date() else NULL
      start_date <- global_start
      end_date <- global_end
      if (is.null(start_date) || is.null(end_date)) {
        if (!is.null(period)) {
          start_date <- start_date %||% period$observation_period_start_date
          end_date <- end_date %||% period$observation_period_end_date
        }
      }
      message_data <- list(
        events = js_data,
        start_date = start_date,
        end_date = end_date,
        date_of_birth = dob
      )
      session$sendCustomMessage(message_type, message_data)
      message_data
    }

    # When JS registers the handler it sends timeline_ready; send data then so canvas paints (handler exists)
    shiny::observeEvent(input$timeline_ready, {
      cat("[timeline] timeline_ready from JS, sending data (handler is now registered)\n")
      send_timeline_to_js()
      cat("[timeline] sent after timeline_ready\n")
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Send when data or date range changes (for updates after initial paint)
    observeEvent(
      list(
        person_events(), obs_period(), current_person_dob(),
        if (!is.null(global_start_date)) global_start_date() else NULL,
        if (!is.null(global_end_date)) global_end_date() else NULL
      ),
      {
        cat("[timeline] send observer fired (data/date change)\n")
        md <- send_timeline_to_js()
        cat("[timeline] sendCustomMessage done; events count = ", length(md$events), "\n", sep = "")
      },
      ignoreInit = FALSE
    )

    # Track last processed event to prevent duplicates
    last_event <- reactiveVal(NULL)
    processed_nonces <- reactiveVal(character(0)) # Track processed event nonces
    last_event_time <- reactiveVal(NULL) # Track time of last event processing

    # Handle create_event from canvas (drawing)
    # CRITICAL FIX: Isolate current_person_id to prevent reactive invalidation
    # ignoreNULL and ignoreInit prevent spurious firings
    observeEvent(input$create_event, ignoreNULL = TRUE, ignoreInit = TRUE, {
      req(input$create_event)

      # Defensive guard: reject anything not explicitly a create gesture (e.g. edit path must never create)
      if (is.null(input$create_event$gesture_type) || input$create_event$gesture_type != "create") {
        cat("*** create_event REJECTED: gesture_type is not 'create' ***\n")
        return()
      }
      if (is.null(input$create_event$drag_start_x)) {
        cat("*** create_event REJECTED: missing drag_start_x (creation-only field) ***\n")
        return()
      }

      cat("\n=== Canvas Draw Event Received ===\n")
      cat("Handler triggered at:", as.character(Sys.time()), "\n")
      cat("Domain:", input$create_event$domain, "\n")
      cat("Start date:", input$create_event$start_date, "\n")
      cat("End date:", input$create_event$end_date, "\n")

      # CRITICAL: Time-based debounce - ignore events within 500ms of last event
      current_time <- Sys.time()
      last_time <- last_event_time()
      if (!is.null(last_time)) {
        time_diff <- as.numeric(difftime(current_time, last_time, units = "secs")) * 1000 # milliseconds
        if (time_diff < 500) {
          cat("*** EVENT TOO SOON AFTER LAST EVENT (", round(time_diff), "ms) - IGNORING ***\n")
          return()
        }
      }

      # CRITICAL: Check nonce first to catch exact duplicates
      current_event <- input$create_event
      if (!is.null(current_event$nonce)) {
        nonces <- processed_nonces()
        if (current_event$nonce %in% nonces) {
          cat("*** DUPLICATE NONCE DETECTED - IGNORING EVENT ***\n")
          cat("  Nonce:", current_event$nonce, "\n")
          return()
        }
        # Add to processed nonces (keep last 100 to avoid memory issues)
        nonces <- c(current_event$nonce, nonces)
        if (length(nonces) > 100) nonces <- nonces[1:100]
        processed_nonces(nonces)
        cat("Nonce:", current_event$nonce, "- New event, processing\n")
      }

      # ALSO check for exact duplicates by properties (backup check)
      last_event_val <- last_event()

      if (!is.null(last_event_val) && is.list(current_event) && is.list(last_event_val)) {
        # Check if ALL properties match (domain, start_date, end_date)
        same_domain <- identical(current_event$domain, last_event_val$domain)
        same_start <- identical(current_event$start_date, last_event_val$start_date)
        same_end <- identical(current_event$end_date, last_event_val$end_date)

        if (same_domain && same_start && same_end) {
          cat("*** EXACT DUPLICATE EVENT DETECTED (by properties) - IGNORING ***\n")
          cat("  Domain:", current_event$domain, "\n")
          cat("  Start:", current_event$start_date, "\n")
          cat("  End:", current_event$end_date, "\n")
          return()
        }
      }

      # Update last_event and time BEFORE processing to catch rapid duplicates
      last_event(current_event)
      last_event_time(current_time)

      # CRITICAL: Isolate to get current values without triggering reactivity
      pid <- isolate(current_person_id())
      current_state <- isolate(state())

      if (is.null(pid) || is.null(current_state)) {
        cat("Cannot create event: pid or state is NULL\n")
        return()
      }

      cat("Current person ID (isolated):", pid, "\n")

      domain <- input$create_event$domain
      start_date <- input$create_event$start_date
      end_date <- input$create_event$end_date

      # Validate dates
      if (is.null(start_date) || is.null(end_date)) {
        cat("Invalid dates\n")
        return()
      }

      # Create the event record
      new_record <- list(
        person_id = pid
      )

      # Add domain-specific fields
      if (domain == "observation_period") {
        new_record$observation_period_start_date <- start_date
        new_record$observation_period_start_datetime <- start_date
        new_record$observation_period_end_date <- end_date
        new_record$observation_period_end_datetime <- end_date
        new_record$period_type_concept_id <- 32817L
      } else if (domain == "visit_occurrence") {
        new_record$visit_concept_id <- 0L
        new_record$visit_start_date <- start_date
        new_record$visit_start_datetime <- start_date
        new_record$visit_end_date <- end_date
        new_record$visit_end_datetime <- end_date
        new_record$visit_type_concept_id <- 32817L
        new_record$provider_id <- 0L
        new_record$care_site_id <- 0L
      } else if (domain == "visit_detail") {
        new_record$visit_detail_concept_id <- 0L
        new_record$visit_detail_start_date <- start_date
        new_record$visit_detail_start_datetime <- start_date
        new_record$visit_detail_end_date <- end_date
        new_record$visit_detail_end_datetime <- end_date
        new_record$visit_detail_type_concept_id <- 32817L
        new_record$provider_id <- 0L
        new_record$care_site_id <- 0L
      } else if (domain == "drug_exposure") {
        new_record$drug_concept_id <- 0L
        new_record$drug_exposure_start_date <- start_date
        new_record$drug_exposure_start_datetime <- start_date
        new_record$drug_exposure_end_date <- end_date
        new_record$drug_exposure_end_datetime <- end_date
        new_record$drug_type_concept_id <- 38000177L
      } else if (domain == "condition_occurrence") {
        new_record$condition_concept_id <- 0L
        new_record$condition_start_date <- start_date
        new_record$condition_start_datetime <- start_date
        new_record$condition_end_date <- end_date
        new_record$condition_end_datetime <- end_date
        new_record$condition_type_concept_id <- 32879L
        new_record$provider_id <- 0L
      } else if (domain == "procedure_occurrence") {
        new_record$procedure_concept_id <- 0L
        new_record$procedure_date <- start_date
        new_record$procedure_datetime <- start_date
        new_record$procedure_type_concept_id <- 32817L
        new_record$provider_id <- 0L
      } else if (domain == "measurement") {
        new_record$measurement_concept_id <- 0L
        new_record$measurement_date <- start_date
        new_record$measurement_datetime <- start_date
        new_record$measurement_type_concept_id <- 32856L
        new_record$value_as_number <- NULL
        new_record$value_as_concept_id <- NULL
        new_record$provider_id <- 0L
      }

      # Add event to state
      cat("Adding event to domain:", domain, "\n")
      cat(
        "Before add - domain event count:",
        if (domain %in% names(current_state)) length(current_state[[domain]]) else 0, "\n"
      )

      new_state <- event_add(current_state, domain, new_record)

      cat(
        "After add - domain event count:",
        if (domain %in% names(new_state)) length(new_state[[domain]]) else 0, "\n"
      )

      # CRITICAL: Use commit_state to persist the change
      commit_state(new_state, sprintf("Add %s Event from Canvas", domain))

      cat("State committed. Waiting for reactive update...\n")

      # Show notification
      showNotification(
        sprintf("Added %s event: %s to %s", domain, start_date, end_date),
        type = "default",
        duration = 2
      )
    })

    # Separate nonce store for update_event (do not share with create)
    processed_update_nonces <- reactiveVal(character(0))

    # Handle update_event from canvas (drag/edit existing event)
    observeEvent(input$update_event, ignoreNULL = TRUE, ignoreInit = TRUE, {
      req(input$update_event)

      # Defensive guard: only accept explicit edit gestures
      if (is.null(input$update_event$gesture_type) || input$update_event$gesture_type != "edit") {
        cat("*** update_event REJECTED: gesture_type is not 'edit' ***\n")
        return()
      }

      current_update <- input$update_event
      domain <- current_update$domain
      cat("\n=== update_event received ===\n")
      cat("Domain:", domain, "ID:", current_update$id_value, "\n")
      cat("New dates: start =", current_update$start_date, ", end =", current_update$end_date, "\n")
      id_value <- current_update$id_value
      start_date <- current_update$start_date
      end_date <- current_update$end_date

      if (is.null(domain) || is.null(id_value) || is.null(start_date) || is.null(end_date)) {
        cat("*** update_event REJECTED: missing required fields ***\n")
        return()
      }

      # Deduplicate by nonce (separate from create nonces)
      if (!is.null(current_update$nonce)) {
        nonces <- processed_update_nonces()
        if (current_update$nonce %in% nonces) {
          cat("*** DUPLICATE UPDATE NONCE - IGNORING ***\n")
          return()
        }
        nonces <- c(current_update$nonce, nonces)
        if (length(nonces) > 100) nonces <- nonces[1:100]
        processed_update_nonces(nonces)
      }

      pid <- isolate(current_person_id())
      current_state <- isolate(state())
      if (is.null(pid) || is.null(current_state)) {
        cat("Cannot update event: pid or state is NULL\n")
        return()
      }

      id_field <- switch(domain,
        observation_period = "observation_period_id",
        visit_occurrence = "visit_occurrence_id",
        visit_detail = "visit_detail_id",
        drug_exposure = "drug_exposure_id",
        condition_occurrence = "condition_occurrence_id",
        procedure_occurrence = "procedure_occurrence_id",
        measurement = "measurement_id",
        NULL
      )
      if (is.null(id_field)) {
        cat("*** update_event: unknown domain ***\n")
        return()
      }

      # Normalize: start <= end (JS should already send normalized)
      if (end_date < start_date) {
        tmp <- start_date
        start_date <- end_date
        end_date <- tmp
      }

      updates <- list()
      if (domain == "observation_period") {
        updates$observation_period_start_date <- start_date
        updates$observation_period_start_datetime <- start_date
        updates$observation_period_end_date <- end_date
        updates$observation_period_end_datetime <- end_date
      } else if (domain == "visit_occurrence") {
        updates$visit_start_date <- start_date
        updates$visit_start_datetime <- start_date
        updates$visit_end_date <- end_date
        updates$visit_end_datetime <- end_date
      } else if (domain == "visit_detail") {
        updates$visit_detail_start_date <- start_date
        updates$visit_detail_start_datetime <- start_date
        updates$visit_detail_end_date <- end_date
        updates$visit_detail_end_datetime <- end_date
      } else if (domain == "drug_exposure") {
        updates$drug_exposure_start_date <- start_date
        updates$drug_exposure_start_datetime <- start_date
        updates$drug_exposure_end_date <- end_date
        updates$drug_exposure_end_datetime <- end_date
      } else if (domain == "condition_occurrence") {
        updates$condition_start_date <- start_date
        updates$condition_start_datetime <- start_date
        updates$condition_end_date <- end_date
        updates$condition_end_datetime <- end_date
      } else if (domain == "procedure_occurrence") {
        updates$procedure_date <- start_date
        updates$procedure_datetime <- start_date
      } else if (domain == "measurement") {
        updates$measurement_date <- start_date
        updates$measurement_datetime <- start_date
      }

      if (length(updates) == 0) return()

      new_state <- event_update(
        current_state, domain, id_value, id_field, updates
      )
      commit_state(new_state, sprintf("Update %s event from canvas", domain))

      showNotification(
        sprintf("Updated %s event: %s to %s", domain, start_date, end_date),
        type = "default",
        duration = 2
      )
    })

    # Render editable data table
    output$event_table <- DT::renderDataTable({
      events <- person_events()
      cat("Rendering event table with", length(events), "events for person", current_person_id(), "\n")

      if (length(events) == 0) {
        empty_df <- data.frame(
          domain = character(0),
          id_value = integer(0),
          concept_id = character(0),
          source_concept_code = character(0),
          start_date = character(0),
          end_date = character(0),
          stringsAsFactors = FALSE
        )
        return(DT::datatable(empty_df, rownames = FALSE, options = list(dom = "t")))
      }

      # Convert to data frame with text inputs and magnifying glass icons for concept_id
      df <- data.frame(
        domain = sapply(events, function(e) e$domain),
        id_value = sapply(events, function(e) e$id_value),
        concept_id = sapply(seq_along(events), function(i) {
          # Create text input with magnifying glass icon
          concept_val <- events[[i]]$concept_id %||% 0L
          concept_val_str <- if (is.null(concept_val) || concept_val == 0L) "" else as.character(concept_val)
          sprintf(
            '<div style="display: flex; align-items: center; gap: 4px;">
                    <input type="text" class="concept-id-input" value="%s" data-row="%d"
                           style="flex: 1; padding: 2px 4px; border: 1px solid #ccc; border-radius: 3px; font-size: 0.875rem;">
                    <button class="btn-search-concept" data-row="%d"
                            style="background: none; border: none; cursor: pointer; padding: 2px; display: flex; align-items: center;">
                      <svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg">
                        <path d="M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001c.03.04.062.078.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1.007 1.007 0 0 0-.115-.1zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z" fill="currentColor"/>
                      </svg>
                    </button>
                  </div>',
            concept_val_str, i, i
          )
        }),
        source_concept_code = sapply(events, function(e) e$source_concept_code %||% ""),
        start_date = sapply(events, function(e) e$start_date),
        end_date = sapply(events, function(e) e$end_date),
        delete = sapply(seq_along(events), function(i) {
          # Create delete button with row index
          sprintf('<button class="btn-delete-event" data-row="%d" style="background: none; border: none; color: #dc3545; font-size: 16px; cursor: pointer; padding: 2px 6px;">×</button>', i)
        }),
        stringsAsFactors = FALSE
      )

      cat("Event table data frame:", nrow(df), "rows\n")

      DT::datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "t",
          ordering = FALSE,
          columnDefs = list(
            list(targets = 0, orderable = FALSE, width = "120px"), # Domain column
            list(targets = 1, orderable = FALSE, width = "80px"), # ID value column
            list(targets = 2, orderable = FALSE, width = "200px"), # Concept ID column (with input)
            list(targets = 3, orderable = FALSE, width = "200px"), # Source concept code column
            list(targets = 6, orderable = FALSE, searchable = FALSE, width = "40px", className = "dt-center") # Delete column
          ),
          # Handlers are attached using delegated $(document).on(...) in the UI
          # header script. Re-binding here can cause duplicate events (e.g., search
          # firing twice and showing notifications twice), so we avoid binding in
          # drawCallback.
          drawCallback = DT::JS("function(settings) {}")
        ),
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 6))), # Make domain, id_value, concept_id (has input), and delete read-only
        selection = "none",
        escape = FALSE # Allow HTML in columns
      ) %>%
        DT::formatStyle(columns = c(0, 1), backgroundColor = "#f5f5f5") %>% # Make domain and id_value read-only visually
        DT::formatStyle(columns = 6, textAlign = "center") # Center delete button
    })

    # Handle table edits (DT uses input$<table_id>_cell_edit with namespace)
    # CRITICAL: Isolate current_person_id to prevent jumping between persons
    observeEvent(input$event_table_cell_edit, {
      info <- input$event_table_cell_edit
      if (is.null(info)) {
        return()
      }

      cat("\n=== Table Cell Edit ===\n")
      cat("Row:", info$row, "Col:", info$col, "Value:", info$value, "\n")

      # Isolate to prevent reactive invalidation
      events <- isolate(person_events())
      current_state <- isolate(state())

      if (length(events) > 0 && info$row <= length(events) && info$row > 0) {
        # DT uses 1-based row indexing
        event <- events[[info$row]]

        cat("Editing event:", event$domain, "ID:", event$id_value, "\n")

        # Determine which column was edited (0-indexed in DT)
        # Columns: domain(0), id_value(1), concept_id(2), source_concept_code(3), start_date(4), end_date(5), delete(6)
        updates <- list()

        if (info$col == 3) { # source_concept_code column
          source_value_field <- switch(event$domain,
            observation_period = NA_character_,
            visit_occurrence = "visit_source_value",
            visit_detail = "visit_detail_source_value",
            drug_exposure = "drug_source_value",
            condition_occurrence = "condition_source_value",
            procedure_occurrence = "procedure_source_value",
            measurement = "measurement_source_value"
          )
          if (!is.na(source_value_field)) {
            updates[[source_value_field]] <- as.character(info$value)
            cat("Updating", source_value_field, "to", info$value, "\n")
          }
        } else if (info$col == 2) { # concept_id column
          concept_field <- switch(event$domain,
            observation_period = NA_character_,
            visit_occurrence = "visit_concept_id",
            visit_detail = "visit_detail_concept_id",
            drug_exposure = "drug_concept_id",
            condition_occurrence = "condition_concept_id",
            procedure_occurrence = "procedure_concept_id",
            measurement = "measurement_concept_id"
          )
          if (!is.na(concept_field)) {
            updates[[concept_field]] <- as.integer(info$value)
            cat("Updating", concept_field, "to", info$value, "\n")
          }
        } else if (info$col == 4) { # start_date column
          date_field <- switch(event$domain,
            observation_period = "observation_period_start_date",
            visit_occurrence = "visit_start_date",
            visit_detail = "visit_detail_start_date",
            drug_exposure = "drug_exposure_start_date",
            condition_occurrence = "condition_start_date",
            procedure_occurrence = "procedure_date",
            measurement = "measurement_date"
          )
          updates[[date_field]] <- as.character(info$value)
          # Also update datetime field if it exists
          datetime_field <- paste0(date_field, "time")
          if (event$domain != "procedure_occurrence" && event$domain != "measurement") {
            updates[[datetime_field]] <- as.character(info$value)
          }
          if (event$domain == "observation_period") {
            updates[["observation_period_start_datetime"]] <- as.character(info$value)
          }
          cat("Updating", date_field, "to", info$value, "\n")
        } else if (info$col == 5) { # end_date column
          date_field <- switch(event$domain,
            observation_period = "observation_period_end_date",
            visit_occurrence = "visit_end_date",
            visit_detail = "visit_detail_end_date",
            drug_exposure = "drug_exposure_end_date",
            condition_occurrence = "condition_end_date",
            procedure_occurrence = "procedure_date", # procedures only have one date
            measurement = "measurement_date" # measurements only have one date
          )
          updates[[date_field]] <- as.character(info$value)
          # Also update datetime field if it exists
          datetime_field <- paste0(date_field, "time")
          if (event$domain != "procedure_occurrence" && event$domain != "measurement") {
            updates[[datetime_field]] <- as.character(info$value)
          }
          if (event$domain == "observation_period") {
            updates[["observation_period_end_datetime"]] <- as.character(info$value)
          }
          cat("Updating", date_field, "to", info$value, "\n")
        }

        if (length(updates) > 0) {
          new_state <- event_update(
            current_state, event$domain, event$id_value,
            event$id_field, updates
          )
          commit_state(new_state, "Update Event from Table")
          cat("Event updated and committed\n")
        }
      }
    })

    # Handle delete event from table
    # CRITICAL: Isolate current_person_id to prevent jumping
    observeEvent(input$delete_event, {
      req(input$delete_event)
      row_idx <- input$delete_event$row

      cat("\n=== Delete Event ===\n")
      cat("Row index:", row_idx, "\n")

      # Isolate to prevent reactive invalidation
      events <- isolate(person_events())
      current_state <- isolate(state())

      if (length(events) > 0 && row_idx >= 1 && row_idx <= length(events)) {
        event <- events[[row_idx]]

        cat("Deleting event:", event$domain, "ID:", event$id_value, "\n")

        # Delete the event from state
        new_state <- event_delete(current_state, event$domain, event$id_value, event$id_field)

        commit_state(new_state, sprintf("Delete %s event", event$domain))

        cat("Event deleted and committed\n")

        showNotification(
          sprintf("Deleted %s event (ID: %d)", event$domain, event$id_value),
          type = "default",
          duration = 2
        )
      } else {
        cat("Invalid row index or no events\n")
      }
    })

    # Reactive to store search results (data frame) for the modal table
    concept_search_results <- reactiveVal(NULL)
    current_search_row <- reactiveVal(NULL)
    last_concept_selection_key <- reactiveVal(NULL)
    last_concept_selection_time <- reactiveVal(as.POSIXct(0, origin = "1970-01-01"))

    # Stable schema for the concept search modal table (used for proxy updates).
    empty_concept_df <- data.frame(
      concept_id = character(0),
      concept_name = character(0),
      vocabulary_id = character(0),
      domain_id = character(0),
      concept_class_id = character(0),
      standard_concept = character(0),
      record_count = numeric(0),
      score = numeric(0),
      stringsAsFactors = FALSE
    )
    # In modules, DT will namespace using the provided session automatically.
    # Pass the un-namespaced outputId here.
    concept_search_proxy <- DT::dataTableProxy("concept_search_table", session = session)

    # Handle concept search
    #
    # Note: input$search_concept is a list. Observing the whole list can be
    # brittle if Shiny treats repeated list values as unchanged. We send a
    # `nonce` from JS, so observe the nonce explicitly to ensure each click
    # triggers this handler.
    observeEvent(input$search_concept$nonce, {
      req(input$search_concept)

      row_idx <- input$search_concept$row
      query <- input$search_concept$query

      cat("\n=== Concept Search ===\n")
      cat("Row index:", row_idx, "\n")
      cat("Query:", query, "\n")

      # Ensure each search attempt is independent:
      # - close any existing modal
      # - clear prior results so stale/empty state can't leak across searches
      removeModal()
      concept_search_results(NULL)
      # Also clear any previously-rendered table data (best-effort).
      tryCatch(
        {
          DT::replaceData(concept_search_proxy, empty_concept_df, resetPaging = TRUE, rownames = FALSE)
        },
        error = function(e) {
          # Proxy may not be initialized yet if the modal hasn't ever been shown.
        }
      )

      if (is.null(query) || nchar(trimws(query)) == 0) {
        showNotification("Please enter a search term", type = "warning", duration = 2)
        return()
      }

      # Store current row for later use
      current_search_row(row_idx)

      # Get events to determine domain
      events <- isolate(person_events())
      if (length(events) == 0 || row_idx < 1 || row_idx > length(events)) {
        showNotification("Invalid row index", type = "error", duration = 2)
        return()
      }

      event <- events[[row_idx]]
      domain <- event$domain

      # Map domain to domain_id for filtering (observation_period uses Type Concept)
      expected_domain_id <- switch(domain,
        observation_period = "Type Concept",
        visit_occurrence = "Visit",
        visit_detail = "Visit",
        condition_occurrence = "Condition",
        drug_exposure = "Drug",
        procedure_occurrence = "Procedure",
        measurement = "Measurement",
        NULL # Unknown domain
      )

      cat("Expected domain_id for filtering:", expected_domain_id, "\n")

      # Perform search using hecate API
      tryCatch(
        {
          client <- hecate_client()
          search_result_json <- hecate_search_concepts_impl(client, query = query, limit = 20)

          cat("Raw JSON response (first 500 chars):\n")
          cat(substr(search_result_json, 1, min(500, nchar(search_result_json))), "\n")

          search_result <- jsonlite::fromJSON(search_result_json, simplifyVector = FALSE)

          cat("Search result structure:\n")
          cat("Type:", class(search_result), "\n")
          cat("Length:", length(search_result), "\n")
          if (is.list(search_result) && length(search_result) > 0) {
            cat("Names:", paste(names(search_result), collapse = ", "), "\n")
            if (length(search_result) <= 3 && is.list(search_result[[1]])) {
              cat("First element names:", paste(names(search_result[[1]]), collapse = ", "), "\n")
            }
          }

          # Check for errors
          if (!is.null(search_result$error)) {
            showNotification(
              sprintf("Search error: %s", search_result$message %||% "Unknown error"),
              type = "error",
              duration = 3
            )
            cat("Error in search result:", search_result$error, "\n")
            return()
          }

          # Extract results - API returns array of result objects, each with a 'concepts' array
          concepts <- NULL

          if (is.list(search_result) && length(search_result) > 0) {
            # Check if this is the nested structure: array of {concept_name, score, concepts: [...]}
            first_elem <- search_result[[1]]
            if (is.list(first_elem) && "concepts" %in% names(first_elem) && is.list(first_elem$concepts)) {
              # This is the nested structure - flatten all concepts from all result items
              # Preserve the score from the parent result item
              all_concepts <- list()
              for (result_item in search_result) {
                if (is.list(result_item) && "concepts" %in% names(result_item) && is.list(result_item$concepts)) {
                  # Get score from parent result item
                  parent_score <- if ("score" %in% names(result_item)) result_item$score else NULL
                  # Add all concepts from this result item, preserving the score
                  for (concept in result_item$concepts) {
                    if (is.list(concept) && "concept_id" %in% names(concept)) {
                      # Add score to concept if not already present
                      if (!is.null(parent_score) && !"score" %in% names(concept)) {
                        concept$score <- parent_score
                      }
                      all_concepts[[length(all_concepts) + 1]] <- concept
                    }
                  }
                }
              }
              concepts <- all_concepts
              cat("Extracted concepts from nested structure, total count:", length(concepts), "\n")
            }
            # Check if it's a direct array of concept objects
            else if (is.list(first_elem) && "concept_id" %in% names(first_elem)) {
              concepts <- search_result
              cat("Found concepts as direct array, count:", length(concepts), "\n")
            }
            # Check if it's wrapped in 'results' or 'data' field
            else if (!is.null(search_result$results) && is.list(search_result$results)) {
              concepts <- search_result$results
              cat("Found concepts in 'results' field, count:", length(concepts), "\n")
            } else if (!is.null(search_result$data) && is.list(search_result$data)) {
              concepts <- search_result$data
              cat("Found concepts in 'data' field, count:", length(concepts), "\n")
            }
            # Check if single concept object
            else if ("concept_id" %in% names(search_result)) {
              concepts <- list(search_result)
              cat("Found single concept object\n")
            }
          }

          # If still no concepts, log the structure for debugging
          if (is.null(concepts) || length(concepts) == 0) {
            cat("Could not extract concepts. Full structure:\n")
            cat(capture.output(str(search_result, max.level = 3)), sep = "\n")
          }

          # Filter concepts by domain_id if expected_domain_id is specified
          if (!is.null(concepts) && length(concepts) > 0 && !is.null(expected_domain_id)) {
            filtered_concepts <- list()
            for (concept in concepts) {
              if (is.list(concept) && "domain_id" %in% names(concept)) {
                concept_domain <- concept$domain_id
                # Match domain_id (case-insensitive comparison)
                if (!is.null(concept_domain) && tolower(trimws(as.character(concept_domain))) == tolower(trimws(expected_domain_id))) {
                  filtered_concepts[[length(filtered_concepts) + 1]] <- concept
                }
              }
            }
            concepts <- filtered_concepts
            cat("Filtered concepts by domain_id='", expected_domain_id, "', remaining count:", length(concepts), "\n")
          }

          # Convert to data frame for display
          if (!is.null(concepts) && length(concepts) > 0) {
            # Extract concept fields safely
            concept_df <- data.frame(
              concept_id = sapply(concepts, function(c) {
                if (is.list(c) && "concept_id" %in% names(c)) {
                  as.character(c$concept_id)
                } else {
                  ""
                }
              }),
              concept_name = sapply(concepts, function(c) {
                if (is.list(c) && "concept_name" %in% names(c)) {
                  as.character(c$concept_name %||% "")
                } else {
                  ""
                }
              }),
              vocabulary_id = sapply(concepts, function(c) {
                if (is.list(c) && "vocabulary_id" %in% names(c)) {
                  as.character(c$vocabulary_id %||% "")
                } else {
                  ""
                }
              }),
              domain_id = sapply(concepts, function(c) {
                if (is.list(c) && "domain_id" %in% names(c)) {
                  as.character(c$domain_id %||% "")
                } else {
                  ""
                }
              }),
              concept_class_id = sapply(concepts, function(c) {
                if (is.list(c) && "concept_class_id" %in% names(c)) {
                  as.character(c$concept_class_id %||% "")
                } else {
                  ""
                }
              }),
              standard_concept = sapply(concepts, function(c) {
                if (is.list(c) && "standard_concept" %in% names(c)) {
                  as.character(c$standard_concept %||% "")
                } else {
                  ""
                }
              }),
              record_count = sapply(concepts, function(c) {
                if (is.list(c) && "record_count" %in% names(c)) {
                  # Convert to numeric/integer if possible, otherwise keep as character
                  rc <- c$record_count
                  if (is.null(rc) || is.na(rc)) {
                    0
                  } else if (is.numeric(rc)) {
                    as.numeric(rc)
                  } else {
                    # Try to convert to numeric
                    num_rc <- suppressWarnings(as.numeric(rc))
                    if (!is.na(num_rc)) num_rc else 0
                  }
                } else {
                  0
                }
              }),
              score = sapply(concepts, function(c) {
                if (is.list(c) && "score" %in% names(c)) {
                  sc <- c$score
                  if (is.null(sc) || is.na(sc)) {
                    0
                  } else if (is.numeric(sc)) {
                    as.numeric(sc)
                  } else {
                    # Try to convert to numeric
                    num_sc <- suppressWarnings(as.numeric(sc))
                    if (!is.na(num_sc)) num_sc else 0
                  }
                } else {
                  0
                }
              }),
              stringsAsFactors = FALSE
            )

            # Filter out empty rows if any
            concept_df <- concept_df[nchar(concept_df$concept_id) > 0, , drop = FALSE]

            # Sort by record_count descending, then by score descending
            concept_df <- concept_df[order(-concept_df$record_count, -concept_df$score), ]

            cat("Concept data frame created with", nrow(concept_df), "rows, sorted by record_count and score\n")

            if (nrow(concept_df) > 0) {
              concept_search_results(concept_df)

              # Show modal, then push data into the DT via proxy after UI flush.
              # This avoids server-only race conditions where the output binding
              # isn't ready when the modal first appears.
              showModal(modalDialog(
                title = div(
                  style = "font-size: 0.9375rem; font-weight: 600;",
                  "Concept Search Results"
                ),
                size = NULL, # Custom size
                easyClose = TRUE,
                footer = tagList(
                  modalButton("Cancel")
                ),
                tags$style(HTML("
                  .modal-dialog {
                    width: 90vw !important;
                    max-width: 90vw !important;
                  }
                  .modal-content {
                    width: 100% !important;
                  }
                ")),
                DT::dataTableOutput(ns("concept_search_table"))
              ))

              session$onFlushed(
                function() {
                  # If the table binding is still not ready (server latency),
                  # retry once on the next flush.
                  ok <- tryCatch(
                    {
                      DT::replaceData(concept_search_proxy, concept_df, resetPaging = TRUE, rownames = FALSE)
                      TRUE
                    },
                    error = function(e) FALSE
                  )
                  if (!ok) {
                    session$onFlushed(function() {
                      tryCatch(
                        {
                          DT::replaceData(concept_search_proxy, concept_df, resetPaging = TRUE, rownames = FALSE)
                        },
                        error = function(e) {
                          cat("replaceData(concept_search_table) failed:", conditionMessage(e), "\n")
                        }
                      )
                    }, once = TRUE)
                  }
                },
                once = TRUE
              )
            } else {
              cat("No valid concept rows after filtering\n")
              # Explicitly clear results so this search cannot affect subsequent searches.
              concept_search_results(NULL)
              showNotification("No valid results found", type = "default", duration = 2)
            }
          } else {
            cat("No concepts extracted from search result\n")
            cat("Search result summary:", capture.output(str(search_result, max.level = 2)), "\n")
            # Explicitly clear results so this search cannot affect subsequent searches.
            concept_search_results(NULL)
            showNotification(
              sprintf("No results found for query: %s", query),
              type = "default",
              duration = 3
            )
          }
        },
        error = function(e) {
          # Ensure failures don't poison subsequent searches.
          concept_search_results(NULL)
          showNotification(
            sprintf("Search failed: %s", conditionMessage(e)),
            type = "error",
            duration = 3
          )
        }
      )
    })

    # Render concept search results table
    output$concept_search_table <- DT::renderDataTable({
      # Always render a table with the expected schema; actual data is pushed
      # in via DT::replaceData() when a search completes.
      tbl <- DT::datatable(
        empty_concept_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "lftip", # l=length menu, f=filter box, t=table, i=info, p=pagination
          ordering = TRUE,
          selection = "single",
          lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
          columnDefs = list(
            list(targets = 0, width = "80px"), # concept_id
            list(targets = 1, width = "250px"), # concept_name
            list(targets = 2, width = "100px"), # vocabulary_id
            list(targets = 3, width = "100px"), # domain_id
            list(targets = 4, width = "150px"), # concept_class_id
            list(targets = 5, width = "100px"), # standard_concept
            list(targets = 6, width = "100px", className = "dt-right"), # record_count (right-aligned)
            list(targets = 7, width = "80px", className = "dt-right") # score (right-aligned)
          )
        ),
        rownames = FALSE,
        selection = "single"
      )

      DT::formatStyle(tbl, columns = c(6, 7), textAlign = "right")
    })

    # Keep the DataTable output alive even when the modal is hidden/removed.
    # Must be called after output$concept_search_table is registered.
    tryCatch({
      outputOptions(output, "concept_search_table", suspendWhenHidden = FALSE)
    }, error = function(e) {
      # If this ever fails due to timing, do not crash the app.
      cat("outputOptions(concept_search_table) failed:", conditionMessage(e), "\n")
    })

    # Handle manual concept_id update from text input
    observeEvent(input$update_concept_id, {
      req(input$update_concept_id)

      row_idx <- input$update_concept_id$row
      concept_id <- input$update_concept_id$concept_id

      cat("\n=== Manual Concept ID Update ===\n")
      cat("Row index:", row_idx, "\n")
      cat("Concept ID:", concept_id, "\n")

      # Get events and current state
      events <- isolate(person_events())
      current_state <- isolate(state())

      if (length(events) > 0 && row_idx >= 1 && row_idx <= length(events)) {
        event <- events[[row_idx]]

        # Determine concept field
        concept_field <- switch(event$domain,
          visit_occurrence = "visit_concept_id",
          visit_detail = "visit_detail_concept_id",
          drug_exposure = "drug_concept_id",
          condition_occurrence = "condition_concept_id",
          procedure_occurrence = "procedure_concept_id",
          measurement = "measurement_concept_id"
        )

        # Update state
        updates <- list()
        updates[[concept_field]] <- as.integer(concept_id)

        new_state <- event_update(
          current_state, event$domain, event$id_value,
          event$id_field, updates
        )
        commit_state(new_state, "Update Concept ID Manually")

        cat("Concept ID updated and committed\n")
      }
    })

    # Handle concept selection from search results
    observeEvent(input$concept_search_table_rows_selected, {
      selected_idx <- input$concept_search_table_rows_selected
      if (is.null(selected_idx) || length(selected_idx) == 0) {
        return()
      }

      results <- concept_search_results()
      if (is.null(results) || nrow(results) == 0) {
        return()
      }

      if (selected_idx > nrow(results)) {
        return()
      }

      selected_concept <- results[selected_idx, ]
      row_idx <- current_search_row()

      # DT selection can fire more than once (e.g., during table redraws / data replacement).
      # De-duplicate identical selections within a short window so we don't commit twice
      # or show duplicate notifications.
      sel_key <- paste0(row_idx, "::", selected_concept$concept_id)
      now <- Sys.time()
      if (!is.null(last_concept_selection_key()) &&
          identical(last_concept_selection_key(), sel_key) &&
          difftime(now, last_concept_selection_time(), units = "secs") < 1) {
        return()
      }
      last_concept_selection_key(sel_key)
      last_concept_selection_time(now)

      cat("\n=== Concept Selected ===\n")
      cat("Row index:", row_idx, "\n")
      cat("Concept ID:", selected_concept$concept_id, "\n")
      cat("Concept Name:", selected_concept$concept_name, "\n")

      # Get events and current state
      events <- isolate(person_events())
      current_state <- isolate(state())

      if (length(events) > 0 && row_idx >= 1 && row_idx <= length(events)) {
        event <- events[[row_idx]]

        # Determine concept field and source value field (observation_period has no source value)
        concept_field <- switch(event$domain,
          observation_period = "period_type_concept_id",
          visit_occurrence = "visit_concept_id",
          visit_detail = "visit_detail_concept_id",
          drug_exposure = "drug_concept_id",
          condition_occurrence = "condition_concept_id",
          procedure_occurrence = "procedure_concept_id",
          measurement = "measurement_concept_id"
        )

        source_value_field <- switch(event$domain,
          observation_period = NA_character_,
          visit_occurrence = "visit_source_value",
          visit_detail = "visit_detail_source_value",
          drug_exposure = "drug_source_value",
          condition_occurrence = "condition_source_value",
          procedure_occurrence = "procedure_source_value",
          measurement = "measurement_source_value"
        )

        # Update state
        updates <- list()
        updates[[concept_field]] <- as.integer(selected_concept$concept_id)
        if (!is.na(source_value_field)) {
          updates[[source_value_field]] <- as.character(selected_concept$concept_name)
        }

        new_state <- event_update(
          current_state, event$domain, event$id_value,
          event$id_field, updates
        )
        commit_state(new_state, "Update Concept from Search")

        cat("Concept updated and committed\n")

        # Close modal
        removeModal()

        showNotification(
          sprintf("Updated concept: %s (ID: %s)", selected_concept$concept_name, selected_concept$concept_id),
          type = "default",
          duration = 2
        )
      }
    })

    return(list(
      selected_event = selected_event
    ))
  })
}

# Uncomment below to test this module independently
# library(shiny)
# library(bslib)
# source("R/services_state.R")
#
# # Create test state with sample events
# test_state <- list(
#   person = list(list(person_id = 1L, year_of_birth = 1980L)),
#   observation_period = list(
#     list(observation_period_id = 1L, person_id = 1L,
#          observation_period_start_date = "2020-01-01",
#          observation_period_end_date = "2020-12-31", period_type_concept_id = 44814724L)
#   ),
#   condition_occurrence = list(
#     list(condition_occurrence_id = 1L, person_id = 1L, condition_concept_id = 123L,
#          condition_start_date = "2020-03-15", condition_end_date = "2020-03-20",
#          condition_type_concept_id = 44786627L)
#   ),
#   drug_exposure = list(
#     list(drug_exposure_id = 1L, person_id = 1L, drug_concept_id = 456L,
#          drug_exposure_start_date = "2020-06-01", drug_exposure_end_date = "2020-06-14",
#          drug_type_concept_id = 38000177L)
#   )
# )
#
# ui <- page_sidebar(
#   title = "Test Timeline Canvas",
#   mod_timeline_canvas_ui("test")
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
#   result <- mod_timeline_canvas_server(
#     "test",
#     state = reactive(state_rv()),
#     current_person_id = reactive(current_pid_rv()),
#     commit_state = commit_state
#   )
#
#   observeEvent(result$selected_event(), {
#     event <- result$selected_event()
#     if (!is.null(event)) {
#       cat("Selected event:", event$domain, event$id_value, "\n")
#     }
#   })
# }
#
# shinyApp(ui = ui, server = server)
