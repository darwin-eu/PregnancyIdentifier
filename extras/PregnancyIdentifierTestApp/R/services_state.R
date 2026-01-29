# services_state.R
# Core state management: OMOP CDM state (list of domain -> list of records),
# validation, ID generation, event add/update/delete, undo stack.

# OMOP CDM state management utilities
OMOP_CONSTANTS <- list(
  PERIOD_TYPE_CONCEPT_ID = 32817L, # EHR (Type Concept)
  UNDO_STACK_MAX_SIZE = 50L,
  BIRTH_YEAR_MIN = 1970L,
  BIRTH_YEAR_MAX = 2000L
)

#' Initialize state from JSON file or object
#'
#' @param json_input Character(1) path to JSON file, character(1) JSON string, or list (already parsed).
#' @return List: domain_name -> list of records (OMOP CDM state).
#' @export
state_init_from_json <- function(json_input) {
  if (is.character(json_input) && length(json_input) == 1 && file.exists(json_input)) {
    json_data <- jsonlite::fromJSON(json_input, simplifyVector = FALSE)
  } else if (is.character(json_input) && length(json_input) == 1) {
    json_data <- jsonlite::fromJSON(json_input, simplifyVector = FALSE)
  } else if (is.list(json_input)) {
    json_data <- json_input
  } else {
    stop(
      "json_input must be a file path (character(1)), a JSON string (character(1)), or a parsed list. Got: ",
      paste(class(json_input), collapse = "/")
    )
  }

  if (!is.list(json_data)) {
    stop("JSON must be an object with domain arrays. Got: ", paste(class(json_data), collapse = "/"))
  }

  # Ensure all domains are lists (arrays)
  for (domain in names(json_data)) {
    if (!is.list(json_data[[domain]])) {
      stop(sprintf("Domain '%s' must be an array", domain))
    }
  }

  return(json_data)
}

#' Validate state structure
#'
#' @param state List: domain_name -> list of records (OMOP CDM state).
#' @param stop_on_error If TRUE, stop() on validation errors; otherwise return a structured result.
#' @return List with \code{valid} (logical) and \code{errors} (character vector).
#' @export
state_validate <- function(state, stop_on_error = TRUE) {
  errors <- character()

  if (!is.list(state)) {
    errors <- c(
      errors,
      sprintf(
        "State must be a list, but got %s.",
        paste(class(state), collapse = "/")
      )
    )
  } else {
    # Check required domains exist (at minimum person should exist)
    if (!"person" %in% names(state)) {
      errors <- c(errors, "State must contain 'person' domain")
    } else if (length(state$person) > 0) {
      # Validate person_id exists in person domain
      person_ids <- lapply(state$person, function(p) p$person_id)
      if (any(vapply(person_ids, is.null, logical(1)))) {
        errors <- c(errors, "All person records must have person_id")
      } else {
        # Coerce to integer and check for NA/duplicates
        ids_int <- safe_as_integer(unlist(person_ids), "person_id")
        if (any(is.na(ids_int))) {
          errors <- c(errors, "All person_id values must be valid integers")
        }
        if (any(duplicated(ids_int))) {
          errors <- c(errors, "Duplicate person_id values found")
        }
      }
    }

    # Validate IDs are unique within each domain
    domain_id_fields <- list(
      person = "person_id",
      observation_period = "observation_period_id",
      visit_occurrence = "visit_occurrence_id",
      visit_detail = "visit_detail_id",
      drug_exposure = "drug_exposure_id",
      condition_occurrence = "condition_occurrence_id",
      procedure_occurrence = "procedure_occurrence_id",
      measurement = "measurement_id"
    )

    for (domain in names(state)) {
      if (domain %in% names(domain_id_fields)) {
        id_field <- domain_id_fields[[domain]]
        if (length(state[[domain]]) > 0) {
          ids_raw <- sapply(state[[domain]], function(r) r[[id_field]])
          ids_int <- safe_as_integer(ids_raw, id_field)
          if (any(is.na(ids_int))) {
            errors <- c(
              errors,
              sprintf(
                "All %s values in domain '%s' must be valid integers",
                id_field,
                domain
              )
            )
          }
          if (any(duplicated(ids_int))) {
            errors <- c(
              errors,
              sprintf("Duplicate IDs found in domain '%s' for field '%s'", domain, id_field)
            )
          }
        }
      }
    }
  }

  if (length(errors) > 0 && isTRUE(stop_on_error)) {
    stop(paste(errors, collapse = "\n"))
  }

  list(
    valid = length(errors) == 0,
    errors = errors
  )
}

#' Get next available ID for a domain
#'
#' @param state List: OMOP CDM state (domain -> list of records).
#' @param domain Character(1); domain name (e.g. \code{"condition_occurrence"}).
#' @param id_field Character(1); name of ID field (e.g. \code{"condition_occurrence_id"}).
#' @return Integer(1); next available ID (1 if domain missing or empty).
#' @export
state_next_id <- function(state, domain, id_field) {
  if (!domain %in% names(state) || length(state[[domain]]) == 0) {
    return(1L)
  }

  ids <- sapply(state[[domain]], function(r) {
    id_val <- r[[id_field]]
    if (is.null(id_val)) {
      return(NA_integer_)
    }
    safe_as_integer(id_val, id_field)
  })

  ids <- ids[!is.na(ids)]
  if (length(ids) == 0) {
    return(1L)
  }

  as.integer(max(ids, na.rm = TRUE) + 1L)
}

#' Get next available person_id
#'
#' @param state List: OMOP CDM state.
#' @return Integer(1); next person_id.
#' @export
state_next_person_id <- function(state) {
  state_next_id(state, "person", "person_id")
}

#' Get next available ID for a domain
#'
#' @param state List: OMOP CDM state.
#' @param domain Character(1); domain name.
#' @param id_field Character(1); ID field name.
#' @return Integer(1); next available ID.
#' @export
state_next_domain_id <- function(state, domain, id_field) {
  state_next_id(state, domain, id_field)
}

#' Ensure every person has exactly one observation period with given dates
#'
#' For each person in state: if they have no observation period, add one with
#' global_start and global_end; if they have one or more, set the first one's
#' start/end to global_start and global_end.
#'
#' @param state List: OMOP CDM state.
#' @param global_start Character(1); start date YYYY-MM-DD.
#' @param global_end Character(1); end date YYYY-MM-DD.
#' @return List: updated state (defensive copy).
#' @export
state_ensure_observation_periods <- function(state, global_start, global_end) {
  state <- deep_copy(state)
  if (!"person" %in% names(state)) return(state)
  if (!is.character(global_start) || length(global_start) != 1 || is.na(global_start)) return(state)
  if (!is.character(global_end) || length(global_end) != 1 || is.na(global_end)) return(state)

  if (!"observation_period" %in% names(state)) {
    state$observation_period <- list()
  }

  for (person_rec in state$person) {
    pid <- person_rec$person_id
    if (is.null(pid)) next
    periods <- Filter(function(p) identical(p$person_id, pid), state$observation_period)
    if (length(periods) == 0) {
      new_obs <- list(
        observation_period_id = state_next_domain_id(state, "observation_period", "observation_period_id"),
        person_id = pid,
        observation_period_start_date = global_start,
        observation_period_end_date = global_end,
        period_type_concept_id = OMOP_CONSTANTS$PERIOD_TYPE_CONCEPT_ID
      )
      state$observation_period <- c(state$observation_period, list(new_obs))
    } else {
      idx <- which(sapply(state$observation_period, function(p) identical(p$person_id, pid)))[1L]
      state$observation_period[[idx]]$observation_period_start_date <- global_start
      state$observation_period[[idx]]$observation_period_end_date <- global_end
      if (!is.null(state$observation_period[[idx]]$observation_period_start_datetime)) {
        state$observation_period[[idx]]$observation_period_start_datetime <- global_start
      }
      if (!is.null(state$observation_period[[idx]]$observation_period_end_datetime)) {
        state$observation_period[[idx]]$observation_period_end_datetime <- global_end
      }
    }
  }
  return(state)
}

#' Create a new blank person
#'
#' @param state Current state (list with at least a 'person' domain)
#' @param default_dob Optional default date of birth. Can be a Date or a
#'   character string in YYYY-MM-DD format. When NULL, a random DOB is
#'   generated between OMOP_CONSTANTS$BIRTH_YEAR_MIN and
#'   OMOP_CONSTANTS$BIRTH_YEAR_MAX.
#' @param obs_start Optional observation period start date (YYYY-MM-DD). When NULL, uses Sys.Date() - 365.
#' @param obs_end Optional observation period end date (YYYY-MM-DD). When NULL, uses Sys.Date().
#' @return Updated state with new blank person and a corresponding
#'   observation period.
#' @export
state_new_person_blank <- function(state, default_dob = NULL, obs_start = NULL, obs_end = NULL) {
  if (!is.list(state)) {
    stop(
      sprintf(
        "state must be a list, but got %s. Please ensure you're passing a valid state object created by state_init_from_json().",
        paste(class(state), collapse = "/")
      )
    )
  }

  new_person_id <- state_next_person_id(state)

  # Use provided DOB or generate random date of birth between configured bounds
  if (is.null(default_dob)) {
    random_year <- sample(OMOP_CONSTANTS$BIRTH_YEAR_MIN:OMOP_CONSTANTS$BIRTH_YEAR_MAX, 1)
    random_month <- sample(1:12, 1)
    # Get number of days in the selected month/year (handle leap years)
    # Calculate by going to first of next month and subtracting 1 day
    if (random_month == 12) {
      next_month_date <- parse_date_safe(paste(random_year + 1, "01", "01", sep = "-"), default = Sys.Date())
    } else {
      next_month_date <- parse_date_safe(paste(random_year, random_month + 1, "01", sep = "-"), default = Sys.Date())
    }
    if (is.null(next_month_date)) next_month_date <- Sys.Date()
    last_day_of_month <- next_month_date - 1
    days_in_month <- as.integer(format(last_day_of_month, "%d"))
    random_day <- sample(1:days_in_month, 1)
    dob <- parse_date_safe(paste(random_year, random_month, random_day, sep = "-"), default = Sys.Date())
    if (is.null(dob)) dob <- Sys.Date()
  } else {
    dob <- parse_date_safe(default_dob)
    if (is.null(dob)) {
      stop("Invalid date format for default_dob. Provide a Date object or date string (YYYY-MM-DD).")
    }
  }

  # Create minimal person record
  new_person <- list(
    person_id = new_person_id,
    gender_concept_id = 0L,
    year_of_birth = as.integer(format(dob, "%Y")),
    month_of_birth = as.integer(format(dob, "%m")),
    day_of_birth = as.integer(format(dob, "%d")),
    birth_datetime = format_date_omop(dob),
    race_concept_id = 0L,
    ethnicity_concept_id = 0L,
    location_id = 0L,
    provider_id = 0L,
    care_site_id = 0L
  )

  # Create observation period
  obs_start_date <- if (!is.null(obs_start)) {
    if (inherits(obs_start, "Date")) format_date_omop(obs_start) else as.character(obs_start)
  } else {
    format_date_omop(Sys.Date() - 365)
  }
  obs_end_date <- if (!is.null(obs_end)) {
    if (inherits(obs_end, "Date")) format_date_omop(obs_end) else as.character(obs_end)
  } else {
    format_date_omop(Sys.Date())
  }
  new_obs_period <- list(
    observation_period_id = state_next_domain_id(state, "observation_period", "observation_period_id"),
    person_id = new_person_id,
    observation_period_start_date = obs_start_date,
    observation_period_end_date = obs_end_date,
    period_type_concept_id = OMOP_CONSTANTS$PERIOD_TYPE_CONCEPT_ID
  )

  # Add to state
  state$person <- c(state$person, list(new_person))

  if (!"observation_period" %in% names(state)) {
    state$observation_period <- list()
  }
  state$observation_period <- c(state$observation_period, list(new_obs_period))

  return(state)
}

#' Duplicate a person (all domains)
#'
#' @param state List: OMOP CDM state.
#' @param person_id Integer(1); person_id to duplicate.
#' @return List: updated state with duplicated person and related records.
#' @export
state_duplicate_person <- function(state, person_id) {
  state <- deep_copy(state)
  # Find all records for this person
  person_records <- list()

  # Get person record
  person_idx <- which(sapply(state$person, function(p) p$person_id == person_id))
  if (length(person_idx) == 0) {
    stop(
      sprintf(
        "Person %d not found. Available person_id values: %s.",
        person_id,
        if (length(state$person) > 0) {
          paste(sapply(state$person, function(p) p$person_id), collapse = ", ")
        } else {
          "(none)"
        }
      )
    )
  }

  new_person_id <- state_next_person_id(state)

  # Copy person record
  old_person <- state$person[[person_idx]]
  new_person <- old_person
  new_person$person_id <- new_person_id
  state$person <- c(state$person, list(new_person))

  # Copy all domain records
  domains_to_copy <- c(
    "observation_period", "visit_occurrence", "visit_detail",
    "drug_exposure", "condition_occurrence", "procedure_occurrence", "measurement"
  )

  for (domain in domains_to_copy) {
    if (domain %in% names(state) && length(state[[domain]]) > 0) {
      # Find records for this person
      domain_records <- Filter(function(r) r$person_id == person_id, state[[domain]])

      if (length(domain_records) > 0) {
        # Determine ID field
        id_field <- switch(domain,
          observation_period = "observation_period_id",
          visit_occurrence = "visit_occurrence_id",
          visit_detail = "visit_detail_id",
          drug_exposure = "drug_exposure_id",
          condition_occurrence = "condition_occurrence_id",
          procedure_occurrence = "procedure_occurrence_id",
          measurement = "measurement_id"
        )

        # Copy each record with new IDs
        for (rec in domain_records) {
          new_rec <- rec
          new_rec$person_id <- new_person_id
          new_rec[[id_field]] <- state_next_domain_id(state, domain, id_field)

          # Update visit_occurrence_id references in other domains if needed
          if ("visit_occurrence_id" %in% names(new_rec) && !is.null(new_rec$visit_occurrence_id)) {
            # Map old visit_occurrence_id to new one
            # For simplicity, we'll need to track visit mappings
            # This is a simplified version - full implementation would track all visit mappings
          }

          state[[domain]] <- c(state[[domain]], list(new_rec))
        }
      }
    }
  }

  return(state)
}

#' Add an event to a domain
#'
#' @param state List: OMOP CDM state.
#' @param domain Character(1); domain name (e.g. \code{"condition_occurrence"}).
#' @param record List; event record (ID assigned if missing).
#' @return List: updated state (defensive copy).
#' @export
event_add <- function(state, domain, record) {
  # Work on a defensive copy to avoid unintended side effects
  state <- deep_copy(state)
  if (!domain %in% names(state)) {
    state[[domain]] <- list()
  }

  KNOWN_DOMAINS <- c(
    "person", "observation_period", "visit_occurrence", "visit_detail",
    "drug_exposure", "condition_occurrence", "procedure_occurrence", "measurement"
  )
  id_field <- switch(domain,
    person = "person_id",
    observation_period = "observation_period_id",
    visit_occurrence = "visit_occurrence_id",
    visit_detail = "visit_detail_id",
    drug_exposure = "drug_exposure_id",
    condition_occurrence = "condition_occurrence_id",
    procedure_occurrence = "procedure_occurrence_id",
    measurement = "measurement_id",
    stop(
      sprintf(
        "Unknown domain: %s. Known domains: %s.",
        domain,
        paste(KNOWN_DOMAINS, collapse = ", ")
      )
    )
  )

  # If ID not provided, generate one
  if (is.null(record[[id_field]])) {
    record[[id_field]] <- state_next_domain_id(state, domain, id_field)
  }

  state[[domain]][[length(state[[domain]]) + 1L]] <- record
  return(state)
}

#' Update an event in a domain
#'
#' @param state List: OMOP CDM state.
#' @param domain Character(1); domain name.
#' @param id_value Scalar; ID value to find.
#' @param id_field Character(1); ID field name.
#' @param updates Named list of fields to update.
#' @return List: updated state (defensive copy).
#' @export
event_update <- function(state, domain, id_value, id_field, updates) {
  state <- deep_copy(state)
  if (!domain %in% names(state) || length(state[[domain]]) == 0) {
    avail <- if (length(names(state)) > 0) paste(names(state), collapse = ", ") else "(none)"
    stop(
      sprintf(
        "Domain '%s' not found or empty. Available domains: %s.",
        domain,
        avail
      )
    )
  }

  idx <- which(sapply(state[[domain]], function(r) r[[id_field]] == id_value))
  if (length(idx) == 0) {
    stop(
      sprintf(
        "Record with %s=%s not found in domain '%s'. Check that the ID exists in that domain.",
        id_field, id_value, domain
      )
    )
  }

  # Update fields
  for (field in names(updates)) {
    state[[domain]][[idx]][[field]] <- updates[[field]]
  }

  return(state)
}

#' Delete an event from a domain
#'
#' @param state List: OMOP CDM state.
#' @param domain Character(1); domain name.
#' @param id_value Scalar; ID value to find.
#' @param id_field Character(1); ID field name.
#' @return List: updated state (defensive copy).
#' @export
event_delete <- function(state, domain, id_value, id_field) {
  state <- deep_copy(state)
  if (!domain %in% names(state) || length(state[[domain]]) == 0) {
    avail <- if (length(names(state)) > 0) paste(names(state), collapse = ", ") else "(none)"
    stop(
      sprintf(
        "Domain '%s' not found or empty. Available domains: %s.",
        domain,
        avail
      )
    )
  }

  idx <- which(sapply(state[[domain]], function(r) r[[id_field]] == id_value))
  if (length(idx) == 0) {
    stop(
      sprintf(
        "Record with %s=%s not found in domain '%s'. Check that the ID exists in that domain.",
        id_field, id_value, domain
      )
    )
  }

  state[[domain]] <- state[[domain]][-idx]
  return(state)
}

#' Delete all data for a person (all domains)
#'
#' @param state List: OMOP CDM state.
#' @param person_id Integer(1); person_id to remove.
#' @return List: updated state with that person and related records removed.
#' @export
state_delete_person <- function(state, person_id) {
  state <- deep_copy(state)
  # All domains that can have person_id
  domains_to_clean <- c(
    "person", "observation_period", "visit_occurrence", "visit_detail",
    "drug_exposure", "condition_occurrence", "procedure_occurrence", "measurement"
  )

  # Remove all records for this person from each domain
  for (domain in domains_to_clean) {
    if (domain %in% names(state) && length(state[[domain]]) > 0) {
      # Filter out records for this person
      state[[domain]] <- Filter(function(r) {
        if (is.null(r$person_id)) {
          return(TRUE)
        }
        r$person_id != person_id
      }, state[[domain]])
    }
  }

  return(state)
}

#' Convert state to JSON string
#'
#' @param state List: OMOP CDM state.
#' @return Character(1); JSON string.
#' @export
state_to_json_string <- function(state) {
  jsonlite::toJSON(state, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

#' Convert JSON string to state
#'
#' @param json_string Character(1); JSON string.
#' @return List: OMOP CDM state.
#' @export
json_string_to_state <- function(json_string) {
  state_init_from_json(json_string)
}

#' Undo stack: push state
#'
#' @param undo_stack List of state snapshots (oldest at end).
#' @param state List: state to push (stored as deep copy).
#' @param max_size Integer(1); maximum stack size (default from \code{app_config()}).
#' @return List: updated undo stack.
#' @export
undo_stack_push <- function(undo_stack, state, max_size = app_config()$app$undo_stack_size) {
  # Deep copy state
  state_copy <- deep_copy(state)

  # Add to stack
  new_stack <- c(list(state_copy), undo_stack)

  # Limit size
  if (length(new_stack) > max_size) {
    new_stack <- new_stack[1:max_size]
  }

  return(new_stack)
}

#' Undo stack: pop state
#'
#' @param undo_stack List of state snapshots.
#' @return List with \code{state} (popped state or NULL) and \code{stack} (remaining list).
#' @export
undo_stack_pop <- function(undo_stack) {
  if (length(undo_stack) == 0) {
    return(list(state = NULL, stack = list()))
  }

  state <- undo_stack[[1]]
  remaining_stack <- undo_stack[-1]

  return(list(state = state, stack = remaining_stack))
}
