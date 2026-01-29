# hecate_search_df.R
# Search Hecate concepts and return results as a data frame; uses hecate_client
# and hecate_perform; returns NULL on API error or unexpected response shape.

library(dplyr)
library(jsonlite)

#' Search Hecate concepts and return results as a data frame
#'
#' @param query Character(1); search query (required).
#' @param vocabulary_id Character(1) or NULL; optional vocabulary filter (comma-separated).
#' @param standard_concept Character(1) or NULL; e.g. \code{"S"} (Standard), \code{"C"} (Classification).
#' @param domain_id Character(1) or NULL; optional domain filter (comma-separated).
#' @param concept_class_id Character(1) or NULL; optional concept class filter.
#' @param limit Integer(1); max results (default 20, max 150).
#' @param client Hecate client (default \code{hecate_client()}).
#' @return Data frame of search results, or NULL if an error occurred (API error or bad response shape).
#'
#' @examples
#' # Simple search
#' df <- hecate_search("diabetes")
#'
#' # Search with filters
#' df <- hecate_search("hypertension", domain_id = "Condition", limit = 10)
hecate_search <- function(
  query,
  vocabulary_id = NULL,
  standard_concept = NULL,
  domain_id = NULL,
  concept_class_id = NULL,
  limit = 20,
  client = hecate_client()
) {
  # Basic validation
  if (!is.character(query) || length(query) != 1 || is.na(query) || nchar(query) < 1) {
    stop("`query` must be a non-empty string.")
  }

  # Use the existing validation and request functions from hecate_client.R
  # Build request using hecate_request helper
  req <- hecate_request(
    client,
    path = "search",
    query = list(
      q = query,
      vocabulary_id = vocabulary_id,
      standard_concept = standard_concept,
      domain_id = domain_id,
      concept_class_id = concept_class_id,
      limit = limit
    )
  )

  # Perform request using hecate_perform helper
  res <- hecate_perform(req)

  # Check for errors
  if (is.list(res) && "error" %in% names(res)) {
    warning("API error: ", res$error, if (!is.null(res$message)) paste(" -", res$message) else "")
    return(NULL)
  }

  parsed <- res

  # The API returns a list where each element is a search result group with:
  # - concept_name: the matched concept name
  # - concept_name_lower: lowercase version
  # - score: similarity score
  # - concepts: a nested list of concept objects

  if (!is.list(parsed) || length(parsed) == 0) {
    warning("Unexpected response format or empty response")
    return(NULL)
  }

  # Check if this is the expected structure (list of search result groups)
  # Each group should have concept_name, score, and concepts fields
  if (length(parsed) > 0) {
    first_elem <- parsed[[1]]
    if (!is.list(first_elem) || !"concepts" %in% names(first_elem)) {
      warning("Response does not match expected structure (missing 'concepts' field)")
      return(NULL)
    }
  }

  # Unnest the nested structure: create one row per concept
  # Each row will include the search metadata (concept_name, score) and concept details
  result_rows <- list()

  for (i in seq_along(parsed)) {
    search_result <- parsed[[i]]

    # Extract search metadata
    search_concept_name <- if (is.null(search_result$concept_name)) NA_character_ else search_result$concept_name
    search_concept_name_lower <- if (is.null(search_result$concept_name_lower)) NA_character_ else search_result$concept_name_lower
    search_score <- if (is.null(search_result$score)) NA_real_ else search_result$score

    # Extract the nested concepts list
    concepts_list <- search_result$concepts
    if (is.null(concepts_list) || length(concepts_list) == 0) {
      next
    }

    # For each concept in the nested list, create a row
    for (j in seq_along(concepts_list)) {
      concept <- concepts_list[[j]]

      # Create a row with search metadata + concept details
      row <- list(
        search_concept_name = search_concept_name,
        search_concept_name_lower = search_concept_name_lower,
        search_score = search_score,
        concept_id = if (is.null(concept$concept_id)) NA_integer_ else concept$concept_id,
        concept_name = if (is.null(concept$concept_name)) NA_character_ else concept$concept_name,
        domain_id = if (is.null(concept$domain_id)) NA_character_ else concept$domain_id,
        vocabulary_id = if (is.null(concept$vocabulary_id)) NA_character_ else concept$vocabulary_id,
        concept_class_id = if (is.null(concept$concept_class_id)) NA_character_ else concept$concept_class_id,
        standard_concept = if (is.null(concept$standard_concept)) NA_character_ else concept$standard_concept,
        concept_code = if (is.null(concept$concept_code)) NA_character_ else concept$concept_code,
        invalid_reason = if (is.null(concept$invalid_reason)) NA_character_ else concept$invalid_reason,
        valid_start_date = if (is.null(concept$valid_start_date)) NA_character_ else concept$valid_start_date,
        valid_end_date = if (is.null(concept$valid_end_date)) NA_character_ else concept$valid_end_date,
        record_count = if (is.null(concept$record_count)) NA_integer_ else concept$record_count
      )

      result_rows <- c(result_rows, list(row))
    }
  }

  if (length(result_rows) == 0) {
    warning("No concepts found in response")
    return(NULL)
  }

  # Convert to dataframe
  tryCatch(
    {
      df <- dplyr::bind_rows(result_rows)
      return(df)
    },
    error = function(e) {
      warning("Error converting to dataframe: ", conditionMessage(e))
      return(NULL)
    }
  )
}
