# mod_state.R
# Server-only module: shared app state (state, undo_stack, current_person_id),
# commit_state, undo. Used by edit and run page modules.

#' State Server (no UI)
#'
#' Creates and returns shared reactive state for the app. Call from main server only.
#'
#' @param id Module id (e.g. "state")
#' @param initial_state Optional initial state list; if NULL, creates blank or loads from /mnt/data/aml10.json
#' @return List with: state (reactive), current_person_id (reactive), commit_state (function),
#'   undo (function), update_current_person_id (function), app_state (reactiveValues)
#' @export
state_server <- function(id, initial_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    app_state <- shiny::reactiveValues(
      state = NULL,
      undo_stack = list(),
      redo_stack = list(),
      current_person_id = NULL
    )

    default_global_start <- format(parse_date_safe("1990-01-01") %||% Sys.Date())
    default_global_end <- format(Sys.Date())

    initial_state_value <- if (is.null(initial_state)) {
      json_path <- "/mnt/data/aml10.json"
      if (file.exists(json_path)) {
        tryCatch(
          state_init_from_json(json_path),
          error = function(e) {
            warning("Failed to load initial JSON: ", conditionMessage(e))
            blank_state <- list(person = list(), observation_period = list())
            state_new_person_blank(blank_state, default_dob = "1970-01-01")
          }
        )
      } else {
        blank_state <- list(person = list(), observation_period = list())
        state_new_person_blank(blank_state, default_dob = "1970-01-01")
      }
    } else {
      initial_state
    }

    initial_state_value <- state_ensure_observation_periods(
      initial_state_value,
      global_start = default_global_start,
      global_end = default_global_end
    )

    app_state$state <- initial_state_value

    shiny::observe({
      state_val <- app_state$state
      if (!is.null(state_val) &&
        "person" %in% names(state_val) &&
        length(state_val$person) > 0) {
        current_pid <- app_state$current_person_id
        if (is.null(current_pid)) {
          app_state$current_person_id <- state_val$person[[1]]$person_id
        } else {
          person_ids <- sapply(state_val$person, function(p) as.integer(p$person_id))
          if (!(current_pid %in% person_ids)) {
            app_state$current_person_id <- state_val$person[[1]]$person_id
          }
        }
      }
    })

    pending_state_update <- shiny::reactiveVal(NULL)
    shiny::observe({
      new_state <- pending_state_update()
      if (!is.null(new_state)) {
        pending_state_update(NULL)
        commit_state_internal(new_state, "Add person(s)")
      }
    })

    commit_state_internal <- function(new_state, action_label = "Edit") {
      current_state <- app_state$state
      if (!is.null(current_state)) {
        app_state$undo_stack <- undo_stack_push(app_state$undo_stack, current_state)
        app_state$redo_stack <- list()
      }
      state_copy <- deep_copy(new_state)
      app_state$state <- state_copy

      if (action_label == "Add person(s)" &&
        !is.null(current_state) && "person" %in% names(current_state) &&
        !is.null(new_state) && "person" %in% names(new_state)) {
        old_person_ids <- if (length(current_state$person) > 0) {
          sapply(current_state$person, function(p) as.integer(p$person_id))
        } else integer(0)
        new_person_ids <- if (length(new_state$person) > 0) {
          sapply(new_state$person, function(p) as.integer(p$person_id))
        } else integer(0)
        newly_added_ids <- setdiff(new_person_ids, old_person_ids)
        if (length(newly_added_ids) > 0) {
          app_state$current_person_id <- max(newly_added_ids)
        }
      }
    }

    commit_state <- function(new_state, action_label = "Edit") {
      tryCatch(
        commit_state_internal(new_state, action_label),
        error = function(e) pending_state_update(new_state)
      )
    }

    undo <- function() {
      if (length(app_state$undo_stack) > 0) {
        current_state <- shiny::isolate(app_state$state)
        if (!is.null(current_state)) {
          app_state$redo_stack <- undo_stack_push(app_state$redo_stack, current_state)
        }
        result <- undo_stack_pop(app_state$undo_stack)
        app_state$state <- result$state
        app_state$undo_stack <- result$stack
      }
    }

    list(
      state = shiny::reactive(app_state$state),
      current_person_id = shiny::reactive(app_state$current_person_id),
      commit_state = commit_state,
      undo = undo,
      update_current_person_id = function(pid) {
        app_state$current_person_id <- pid
      },
      app_state = app_state
    )
  })
}
