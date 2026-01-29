# hecate_tools.R
# Ellmer tools for Hecate API (search_concepts, get_concept_by_id, etc.) and
# add_persons tool that updates OMOP CDM state; person/record matching is by
# input person_id or by position when multiple persons are added.

library(ellmer)
library(jsonlite)
library(dplyr)

#' Create list of Hecate tools for use with ellmer chat
#'
#' @param client Hecate client from \code{hecate_client()}.
#' @param concept_counts_df Data frame of concept counts, or NULL (get_concept_counts will return error).
#' @param current_state List: OMOP CDM state; if provided, \code{add_persons} tool is included.
#' @param state_update_callback Optional function(state) called when add_persons modifies state.
#' @return List of ellmer tools (search_concepts, get_concept_by_id, ..., optionally add_persons).
#' @export
make_hecate_tools <- function(client = hecate_client(), concept_counts_df = NULL, current_state = NULL, state_update_callback = NULL) {
  tool_search_concepts <- ellmer::tool(
    function(query,
             vocabulary_id = NULL,
             standard_concept = NULL,
             domain_id = NULL,
             concept_class_id = NULL,
             limit = 20) {
      hecate_search_concepts_impl(
        client,
        query = query,
        vocabulary_id = vocabulary_id,
        standard_concept = standard_concept,
        domain_id = domain_id,
        concept_class_id = concept_class_id,
        limit = limit
      )
    },
    name = "search_concepts",
    description = paste(
      "Search for OMOP Standardized Vocabulary concepts using text query.",
      "Returns medical concepts with similarity scores from many vocabularies",
      "(SNOMED, ICD10CM, RxNorm, LOINC, etc.).",
      "IMPORTANT: Do no specify vocabulary_id unless specifically asked to."
    ),
    arguments = list(
      query = ellmer::type_string(
        "Search query. Examples: 'diabetes', 'hypertension', '4321435', 'E11.9'.",
        required = TRUE
      ),
      vocabulary_id = ellmer::type_string(
        "Optional vocabulary filter (comma-separated). Examples: 'SNOMED', 'ICD10CM', 'RxNorm'. Use this only when specifically needed.",
        required = FALSE
      ),
      standard_concept = ellmer::type_string(
        "Optional standardization filter: 'S' (Standard), 'C' (Classification), or empty for non-standard. Prefer Standard unless asked otherwise.",
        required = FALSE
      ),
      domain_id = ellmer::type_string(
        "Optional domain filter (comma-separated). Examples: 'Condition', 'Drug', 'Procedure'.",
        required = FALSE
      ),
      concept_class_id = ellmer::type_string(
        "Optional concept class filter (comma-separated). Examples: 'Clinical Finding', 'Ingredient'. Do not use this unless specifically asked to.",
        required = FALSE
      ),
      limit = ellmer::type_integer(
        "Maximum number of results to return (default 20).",
        required = FALSE
      )
    )
  )

  tool_get_concept_by_id <- ellmer::tool(
    function(id) hecate_get_concept_by_id_impl(client, id = id),
    name = "get_concept_by_id",
    description = "Get detailed information about a specific OMOP concept by concept ID.",
    arguments = list(
      id = ellmer::type_integer("OMOP concept ID (positive integer).", required = TRUE)
    )
  )

  tool_get_concept_relationships <- ellmer::tool(
    function(id) hecate_get_concept_relationships_impl(client, id = id),
    name = "get_concept_relationships",
    description = "Get related concepts for a concept ID (e.g., Maps to, Subsumes, Is a).",
    arguments = list(
      id = ellmer::type_integer("OMOP concept ID (positive integer).", required = TRUE)
    )
  )

  tool_get_concept_phoebe <- ellmer::tool(
    function(id) hecate_get_concept_phoebe_impl(client, id = id),
    name = "get_concept_phoebe",
    description = "Get PHOEBE-defined curated relationships for a concept ID.",
    arguments = list(
      id = ellmer::type_integer("OMOP concept ID (positive integer).", required = TRUE)
    )
  )

  tool_expand_concept_hierarchy <- ellmer::tool(
    function(id, childLevels = 5, parentLevels = 0) {
      hecate_expand_concept_hierarchy_impl(
        client,
        id = id,
        childLevels = childLevels,
        parentLevels = parentLevels
      )
    },
    name = "expand_concept_hierarchy",
    description = "Expand concept hierarchy (children + parents) for phenotype building.",
    arguments = list(
      id = ellmer::type_integer("OMOP concept ID (positive integer).", required = TRUE),
      childLevels = ellmer::type_integer("Child levels to expand (0-10, default 5).", required = FALSE),
      parentLevels = ellmer::type_integer("Parent levels to expand (0-10, default 0).", required = FALSE)
    )
  )

  tool_get_concept_counts <- ellmer::tool(
    function(concept_ids) {
      hecate_get_concept_counts_impl(concept_counts_df, concept_ids = concept_ids)
    },
    name = "get_concept_counts",
    description = paste(
      "Get record counts for one or more OMOP concept IDs across all available data sources.",
      "Returns the number of records (occurrences) for each concept in each database.",
      "Useful for assessing feasibility and determining which databases contain data for specific concepts."
    ),
    arguments = list(
      concept_ids = ellmer::type_array(
        items = ellmer::type_integer(),
        description = "One or more OMOP concept IDs (positive integers). Can pass a single ID or multiple IDs.",
        required = TRUE
      )
    )
  )

  # Tool to add new persons to the state
  # This tool modifies the app's data by adding one or more new persons
  tool_add_persons <- NULL
  if (!is.null(current_state)) {
    tool_add_persons <- ellmer::tool(
      function(persons_json) {
        # Parse the JSON for new persons
        new_data <- jsonlite::fromJSON(persons_json, simplifyVector = FALSE)

        # Validate that it has a person domain
        if (!"person" %in% names(new_data) || !is.list(new_data$person)) {
          stop("JSON must contain a 'person' array with at least one person record")
        }

        # Start with a copy of the current state
        modified_state <- jsonlite::fromJSON(
          jsonlite::toJSON(current_state, auto_unbox = TRUE),
          simplifyVector = FALSE
        )

        # Ensure person domain exists
        if (!"person" %in% names(modified_state)) {
          modified_state$person <- list()
        }

        # Get next available person_id using shared state helper
        next_id <- state_next_id(modified_state, "person", "person_id")

        # Track mapping from input person index/ID to new person_id
        # This helps match records when person_id changes
        person_index <- 0L

        # Add each new person with proper IDs
        for (new_person in new_data$person) {
          person_index <- person_index + 1L

          # Store the original person_id from input (if any) for matching records
          input_person_id <- if (is.null(new_person$person_id)) NULL else as.integer(new_person$person_id)

          # Assign new person_id if not provided or if it conflicts
          if (is.null(new_person$person_id) ||
            (length(modified_state$person) > 0 &&
              any(sapply(modified_state$person, function(p) p$person_id == new_person$person_id)))) {
            new_person$person_id <- next_id
            next_id <- next_id + 1L
          } else {
            new_person$person_id <- as.integer(new_person$person_id)
            next_id <- max(next_id, as.integer(new_person$person_id) + 1L)
          }

          # Ensure required fields have defaults
          if (is.null(new_person$gender_concept_id)) new_person$gender_concept_id <- 0L
          if (is.null(new_person$race_concept_id)) new_person$race_concept_id <- 0L
          if (is.null(new_person$ethnicity_concept_id)) new_person$ethnicity_concept_id <- 0L
          if (is.null(new_person$location_id)) new_person$location_id <- 0L
          if (is.null(new_person$provider_id)) new_person$provider_id <- 0L
          if (is.null(new_person$care_site_id)) new_person$care_site_id <- 0L

          # Add person to state
          modified_state$person <- c(modified_state$person, list(new_person))

          # Helper function to match records to this person
          # Matches if: record has no person_id, or record person_id matches input person_id,
          # or if there's only one person, match all records
          match_to_person <- function(record) {
            record_person_id <- if (is.null(record$person_id)) NULL else as.integer(record$person_id)
            # If record has no person_id, match it to this person
            if (is.null(record_person_id)) {
              return(TRUE)
            }
            # If record person_id matches the input person_id, match it
            if (!is.null(input_person_id) && record_person_id == input_person_id) {
              return(TRUE)
            }
            # If there's only one person being added, match all records to that person
            if (length(new_data$person) == 1L) {
              return(TRUE)
            }
            # Otherwise, match by position (person_index)
            # This is a simple heuristic: first person gets records with person_id 1 or no person_id, etc.
            if (record_person_id == person_index) {
              return(TRUE)
            }
            return(FALSE)
          }

          # Add observation period if provided, or create default
          if ("observation_period" %in% names(new_data)) {
            obs_periods <- Filter(match_to_person, new_data$observation_period)

            for (obs_period in obs_periods) {
              obs_period$person_id <- new_person$person_id
              if (is.null(obs_period$observation_period_id)) {
                # Generate new observation_period_id
                obs_period$observation_period_id <- state_next_id(
                  modified_state,
                  "observation_period",
                  "observation_period_id"
                )
              }
              if (!"observation_period" %in% names(modified_state)) {
                modified_state$observation_period <- list()
              }
              modified_state$observation_period <- c(modified_state$observation_period, list(obs_period))
            }
          } else {
            # Create default observation period
            if (!"observation_period" %in% names(modified_state)) {
              modified_state$observation_period <- list()
            }
            obs_start <- Sys.Date() - 365
            obs_end <- Sys.Date()
            if (length(modified_state$observation_period) > 0) {
              existing_obs_ids <- sapply(modified_state$observation_period, function(op) {
                as.integer(op$observation_period_id)
              })
              obs_period_id <- max(existing_obs_ids, na.rm = TRUE) + 1L
            } else {
              obs_period_id <- 1L
            }
            new_obs_period <- list(
              observation_period_id = obs_period_id,
              person_id = new_person$person_id,
              observation_period_start_date = format_date_omop(obs_start),
              observation_period_end_date = format_date_omop(obs_end),
              period_type_concept_id = 32817L
            )
            modified_state$observation_period <- c(modified_state$observation_period, list(new_obs_period))
          }

          # Add other domains if provided
          other_domains <- c(
            "condition_occurrence", "drug_exposure", "procedure_occurrence",
            "measurement", "visit_occurrence", "visit_detail"
          )
          for (domain in other_domains) {
            if (domain %in% names(new_data) && is.list(new_data[[domain]])) {
              # Use the same matching function
              domain_records <- Filter(match_to_person, new_data[[domain]])

              for (record in domain_records) {
                record$person_id <- new_person$person_id
                # Generate ID if needed
                id_field <- switch(domain,
                  condition_occurrence = "condition_occurrence_id",
                  drug_exposure = "drug_exposure_id",
                  procedure_occurrence = "procedure_occurrence_id",
                  measurement = "measurement_id",
                  visit_occurrence = "visit_occurrence_id",
                  visit_detail = "visit_detail_id"
                )
                if (!is.null(id_field) && is.null(record[[id_field]])) {
                  record[[id_field]] <- state_next_id(modified_state, domain, id_field)
                }
                if (!domain %in% names(modified_state)) {
                  modified_state[[domain]] <- list()
                }
                modified_state[[domain]][[length(modified_state[[domain]]) + 1L]] <- record
              }
            }
          }
        }

        # If we have a callback, call it with the modified state
        if (!is.null(state_update_callback) && is.function(state_update_callback)) {
          tryCatch(
            {
              cat("Calling state_update_callback with modified state\n")
              state_update_callback(modified_state)
              cat("State update callback completed\n")
            },
            error = function(e) {
              cat("Error in state_update_callback:", conditionMessage(e), "\n")
            }
          )
        }

        # Return the modified state as JSON
        # The LLM doesn't need to see this, but we'll extract it in llm_apply_edit
        list(
          success = TRUE,
          message = paste("Successfully added", length(new_data$person), "person(s) to the state."),
          modified_state_json = jsonlite::toJSON(modified_state, auto_unbox = TRUE, pretty = FALSE, null = "null")
        )
      },
      name = "add_persons",
      description = paste(
        "Add one or more new test patients (persons) to the app's data.",
        "This tool modifies the application state by adding new person records with their associated data.",
        "The JSON should follow OMOP CDM structure with a 'person' array and optionally other domains",
        "(observation_period, condition_occurrence, drug_exposure, etc.).",
        "Person IDs will be automatically assigned if not provided or if they conflict with existing IDs.",
        "Use this tool when the user asks to create test patients, add patients, or generate patient data."
      ),
      arguments = list(
        persons_json = ellmer::type_string(
          "JSON string containing the person data to add. Must have a 'person' array with at least one person object.",
          required = TRUE
        )
      )
    )
  }

  tools_list <- list(
    tool_search_concepts,
    tool_get_concept_by_id,
    tool_get_concept_relationships,
    tool_get_concept_phoebe,
    tool_expand_concept_hierarchy,
    tool_get_concept_counts
  )

  # Add the add_persons tool if state is provided
  if (!is.null(tool_add_persons)) {
    tools_list <- c(tools_list, list(tool_add_persons))
  }

  return(tools_list)
}
