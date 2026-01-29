# mod_edit.R
# Edit input page: nav_panel with layout_sidebar (sidebar + timeline main).
# Uses shared state from state_server.

#' Edit sidebar inner content (dates, patient nav, DOB, sex, delete, import/export, event editor).
#' @noRd
edit_sidebar_inner <- function(ns) {
  div(
    style = "margin-bottom: 0.5rem;",
    div(
      style = "display: flex; align-items: center; gap: 0.5rem; margin-bottom: 0.25rem;",
      span(style = "font-size: 0.6875rem; font-weight: 500; min-width: 70px;", "Start Date:"),
      dateInput(ns("global_start_date"), label = NULL, value = parse_date_safe("1990-01-01"), width = "100%")
    ),
    div(
      style = "display: flex; align-items: center; gap: 0.5rem;",
      span(style = "font-size: 0.6875rem; font-weight: 500; min-width: 70px;", "End Date:"),
      dateInput(ns("global_end_date"), label = NULL, value = Sys.Date(), width = "100%")
    )
  ,
  div(style = "height: 50px;"),
  mod_patient_nav_ui(ns("patient_nav")),
  div(
    style = "margin-top: 0.5rem; margin-bottom: 0.5rem;",
    div(
      style = "display: flex; align-items: center; gap: 0.5rem;",
      span(style = "font-size: 0.6875rem; font-weight: 500; min-width: 90px;", "Date of Birth:"),
      dateInput(ns("person_dob"), label = NULL, value = parse_date_safe("1970-01-01"), width = "100%")
    ),
    div(
      style = "display: flex; align-items: center; gap: 0.5rem; margin-top: 0.5rem;",
      span(style = "font-size: 0.6875rem; font-weight: 500; min-width: 90px;", "Sex:"),
      div(
        style = "flex: 1;",
        div(
          class = "sex-dropdown",
          selectInput(ns("person_sex"), label = NULL,
            choices = list("Male" = 8507L, "Female" = 8532L),
            selected = 8507L, width = "100%", selectize = FALSE)
        )
      )
    )
  ),
  div(
    style = "margin-top: 0.75rem; margin-bottom: 0.5rem;",
    actionButton(ns("delete_person"), "Delete Person",
      class = "btn-danger btn-sm", style = "width: 100%; font-size: 0.75rem;",
      icon = icon("trash", class = "fa-sm"))
  ),
  div(style = "height: 50px;"),
  mod_import_export_ui(ns("import_export")),
  shiny::hr(),
  mod_event_editor_ui(ns("event_editor"))
  )
}


#' Edit page UI: nav_panel with layout_sidebar (sidebar + main).
#'
#' @param id Module id (e.g. "edit")
#' @return bslib::nav_panel for Edit input
#' @export
edit_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "Edit input",
    value = "edit",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        edit_sidebar_inner(ns)
      ),
      mod_timeline_canvas_ui(ns("timeline"))
    )
  )
}

#' Edit page Server
#'
#' @param id Module id (e.g. "edit")
#' @param state Return value from state_server() (state, current_person_id, commit_state, undo, update_current_person_id, app_state)
#' @return Nothing; used for side effects
#' @export
edit_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    app_state <- state$app_state
    commit_state <- state$commit_state
    undo <- state$undo

    mod_patient_nav_server(
      "patient_nav",
      state = state$state,
      current_person_id = state$current_person_id,
      commit_state = commit_state,
      undo = undo,
      update_current_person_id = state$update_current_person_id,
      global_start_date = shiny::reactive(input$global_start_date),
      global_end_date = shiny::reactive(input$global_end_date)
    )

    updating_dob <- shiny::reactiveVal(FALSE)
    updating_sex <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$person_dob, ignoreNULL = TRUE, ignoreInit = TRUE, {
      if (updating_dob()) return()
      pid <- shiny::isolate(app_state$current_person_id)
      if (is.null(pid) || is.null(input$person_dob)) return()
      current_state <- shiny::isolate(app_state$state)
      if (is.null(current_state) || !"person" %in% names(current_state)) return()
      person_idx <- which(sapply(current_state$person, function(p) same_id(p$person_id, pid)))
      if (length(person_idx) != 1) return()
      person_rec <- current_state$person[[person_idx]]
      current_dob <- person_dob(person_rec)
      new_dob <- parse_date_safe(input$person_dob)
      if (is.null(new_dob)) return()
      if (!is.null(current_dob) && current_dob == new_dob) return()
      new_state <- deep_copy(current_state)
      if (person_idx > 0 && person_idx <= length(new_state$person) &&
          same_id(new_state$person[[person_idx]]$person_id, pid)) {
        new_state$person[[person_idx]]$birth_datetime <- format_date_omop(new_dob)
        new_state$person[[person_idx]]$year_of_birth <- as.integer(format(new_dob, "%Y"))
        new_state$person[[person_idx]]$month_of_birth <- as.integer(format(new_dob, "%m"))
        new_state$person[[person_idx]]$day_of_birth <- as.integer(format(new_dob, "%d"))
        commit_state(new_state, "Update Date of Birth")
      }
    })

    shiny::observeEvent(app_state$current_person_id, ignoreNULL = TRUE, ignoreInit = TRUE, {
      if (updating_dob()) return()
      pid <- app_state$current_person_id
      if (is.null(pid)) return()
      state_val <- shiny::isolate(app_state$state)
      if (is.null(state_val) || !"person" %in% names(state_val)) return()
      person_rec <- Find(function(p) same_id(p$person_id, pid), state_val$person)
      if (is.null(person_rec)) {
        updating_dob(TRUE)
        on.exit(updating_dob(FALSE), add = TRUE)
        updateDateInput(session, "person_dob", value = parse_date_safe("1970-01-01"))
        return()
      }
      dob <- person_dob(person_rec) %||% parse_date_safe("1970-01-01")
      updating_dob(TRUE)
      on.exit(updating_dob(FALSE), add = TRUE)
      updateDateInput(session, "person_dob", value = dob)
    })

    shiny::observeEvent(input$person_sex, ignoreNULL = TRUE, ignoreInit = TRUE, {
      if (updating_sex()) return()
      pid <- shiny::isolate(app_state$current_person_id)
      if (is.null(pid) || is.null(input$person_sex)) return()
      current_state <- shiny::isolate(app_state$state)
      if (is.null(current_state) || !"person" %in% names(current_state)) return()
      person_idx <- which(sapply(current_state$person, function(p) same_id(p$person_id, pid)))
      if (length(person_idx) != 1) return()
      person_rec <- current_state$person[[person_idx]]
      current_sex <- person_rec$gender_concept_id %||% 0L
      new_sex <- as.integer(input$person_sex)
      if (!is.null(current_sex) && current_sex == new_sex) return()
      new_state <- deep_copy(current_state)
      if (person_idx > 0 && person_idx <= length(new_state$person) &&
          same_id(new_state$person[[person_idx]]$person_id, pid)) {
        new_state$person[[person_idx]]$gender_concept_id <- new_sex
        commit_state(new_state, "Update Sex")
      }
    })

    shiny::observeEvent(app_state$current_person_id, ignoreNULL = TRUE, ignoreInit = TRUE, {
      if (updating_sex()) return()
      pid <- app_state$current_person_id
      if (is.null(pid)) return()
      state_val <- shiny::isolate(app_state$state)
      if (is.null(state_val) || !"person" %in% names(state_val)) return()
      person_rec <- Find(function(p) same_id(p$person_id, pid), state_val$person)
      if (is.null(person_rec)) {
        updating_sex(TRUE)
        on.exit(updating_sex(FALSE), add = TRUE)
        updateSelectInput(session, "person_sex", selected = 8507L)
        return()
      }
      sex <- person_rec$gender_concept_id %||% 8507L
      updating_sex(TRUE)
      on.exit(updating_sex(FALSE), add = TRUE)
      updateSelectInput(session, "person_sex", selected = sex)
    })

    shiny::observeEvent(list(input$global_start_date, input$global_end_date), {
      start_date <- input$global_start_date
      end_date <- input$global_end_date
      if (is.null(start_date) || is.null(end_date)) return()
      start_date_obj <- parse_date_safe(start_date)
      end_date_obj <- parse_date_safe(end_date)
      if (is.null(start_date_obj) || is.null(end_date_obj)) return()
      if (end_date_obj < start_date_obj) {
        updateDateInput(session, "global_start_date", value = end_date)
        updateDateInput(session, "global_end_date", value = start_date)
        shiny::showNotification("End date was before start date. Dates have been swapped.", type = "warning", duration = 3)
        return
      }
      days_diff <- as.numeric(end_date_obj - start_date_obj)
      if (days_diff < 20) {
        adjusted_end <- start_date_obj + 20
        updateDateInput(session, "global_end_date", value = adjusted_end)
        shiny::showNotification("Date range was < 20 days. Adjusted end date.", type = "warning", duration = 3)
      }
    })

    timeline_result <- mod_timeline_canvas_server(
      "timeline",
      state = state$state,
      current_person_id = state$current_person_id,
      commit_state = commit_state,
      global_start_date = shiny::reactive(input$global_start_date),
      global_end_date = shiny::reactive(input$global_end_date)
    )

    mod_event_editor_server(
      "event_editor",
      selected_event = timeline_result$selected_event,
      state = state$state,
      commit_state = commit_state
    )

    mod_import_export_server(
      "import_export",
      state = state$state,
      commit_state = commit_state
    )

    shiny::observeEvent(input$delete_person, {
      current_pid <- app_state$current_person_id
      if (is.null(current_pid)) {
        shiny::showNotification("No person selected to delete.", type = "warning", duration = 2)
        return
      }
      current_state <- shiny::isolate(app_state$state)
      if (is.null(current_state) || !"person" %in% names(current_state)) {
        shiny::showNotification("No state available.", type = "error", duration = 2)
        return
      }
      shiny::showModal(shiny::modalDialog(
        title = "Delete Person",
        sprintf("Are you sure you want to delete all data for Person %d? This action cannot be undone.", current_pid),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_delete_person"), "Delete", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_delete_person, {
      shiny::removeModal()
      current_pid <- shiny::isolate(app_state$current_person_id)
      current_state <- shiny::isolate(app_state$state)
      if (is.null(current_pid) || is.null(current_state)) return()
      person_ids <- sapply(current_state$person, function(p) as.integer(p$person_id))
      current_idx <- which(person_ids == current_pid)

      if (length(person_ids) == 1) {
        domains_to_clean <- c(
          "observation_period", "visit_occurrence", "visit_detail",
          "drug_exposure", "condition_occurrence", "procedure_occurrence", "measurement"
        )
        new_state <- current_state
        for (domain in domains_to_clean) {
          if (domain %in% names(new_state) && length(new_state[[domain]]) > 0) {
            new_state[[domain]] <- Filter(function(r) {
              if (is.null(r$person_id)) return(TRUE)
              !same_id(r$person_id, current_pid)
            }, new_state[[domain]])
          }
        }
        new_pid <- current_pid
      } else {
        new_state <- state_delete_person(current_state, current_pid)
        remaining_person_ids <- sapply(new_state$person, function(p) as.integer(p$person_id))
        if (length(remaining_person_ids) == 0) {
          g_start <- format(shiny::isolate(input$global_start_date))
          g_end <- format(shiny::isolate(input$global_end_date))
          new_state <- state_new_person_blank(new_state, default_dob = "1970-01-01", obs_start = g_start, obs_end = g_end)
          remaining_person_ids <- sapply(new_state$person, function(p) as.integer(p$person_id))
          new_pid <- remaining_person_ids[1]
        } else {
          if (current_idx > 1) {
            old_prev_pid <- person_ids[current_idx - 1]
            new_pid <- if (old_prev_pid %in% remaining_person_ids) old_prev_pid else remaining_person_ids[1]
          } else {
            new_pid <- remaining_person_ids[1]
          }
        }
      }

      if (length(person_ids) == 1) {
        commit_state(new_state, sprintf("Clear Person %d Data", current_pid))
        shiny::showNotification(sprintf("Cleared all data for Person %d.", current_pid), type = "default", duration = 3)
      } else {
        commit_state(new_state, sprintf("Delete Person %d", current_pid))
        shiny::showNotification(sprintf("Deleted all data for Person %d. Now viewing Person %d.", current_pid, new_pid), type = "default", duration = 3)
      }
      app_state$current_person_id <- new_pid
    })
  })
}
