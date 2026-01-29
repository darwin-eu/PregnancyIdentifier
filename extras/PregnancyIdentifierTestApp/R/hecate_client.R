# hecate_client.R
# Hecate API client: request/perform, rate limiting, concept search/get/counts;
# defaults from app_config() when arguments are NULL.

library(dplyr)
library(httr2)
library(jsonlite)

create_rate_limiter <- function(max_calls = 100, per_seconds = 60) {
  call_times <- numeric(0)

  function() {
    now <- as.numeric(Sys.time())
    call_times <<- call_times[call_times > (now - per_seconds)]

    if (length(call_times) >= max_calls) {
      wait_time <- per_seconds - (now - min(call_times))
      Sys.sleep(max(wait_time, 0))
      call_times <<- numeric(0)
    }

    call_times <<- c(call_times, now)
    invisible(NULL)
  }
}

hecate_rate_limiter <- create_rate_limiter(100, 60)

hecate_client <- function(base_url = NULL, timeout_ms = NULL, api_key = NULL) {
  cfg <- app_config()$hecate
  base_url <- base_url %||% cfg$base_url
  timeout_ms <- timeout_ms %||% cfg$timeout_ms
  api_key <- api_key %||% cfg$api_key

  # Validate API key if provided (basic sanity check without logging the key)
  if (nzchar(api_key) && nchar(api_key) < 10) {
    warning("API key appears too short. Please check HECATE_API_KEY environment variable.", call. = FALSE)
  }

  if (isTRUE(getOption("app.debug"))) {
    message("Initializing Hecate client with URL: ", base_url)
    message("API key ", if (nzchar(api_key)) "provided" else "not provided")
  }

  structure(
    list(
      base_url = sub("/+$", "", base_url),
      timeout_ms = timeout_ms,
      api_key = api_key
    ),
    class = "hecate_client"
  )
}

hecate_request <- function(client, path, query = NULL) {
  url <- paste0(client$base_url, "/", sub("^/+", "", path))

  req <- request(url) |>
    req_timeout(client$timeout_ms / 1000)

  if (nzchar(client$api_key)) {
    req <- req_headers(req, Authorization = paste("Bearer", client$api_key))
  }

  if (!is.null(query)) {
    query <- Filter(Negate(is.null), query)
    req <- req_url_query(req, !!!query)
  }

  req
}

hecate_perform <- function(req) {
  hecate_rate_limiter()
  resp <- tryCatch(req_perform(req), error = function(e) e)

  if (inherits(resp, "error")) {
    return(make_error_result(
      message = "Request failed",
      code = "request_failed",
      details = conditionMessage(resp)
    ))
  }

  status <- resp_status(resp)
  body_txt <- resp_body_string(resp)
  parsed <- tryCatch(fromJSON(body_txt, simplifyVector = FALSE), error = function(e) NULL)

  if (status >= 400) {
    return(make_error_result(
      message = sprintf("API returned HTTP %d %s", status, resp_status_desc(resp)),
      code = "api_error",
      details = if (is.null(parsed)) body_txt else parsed,
      status = status,
      status_text = resp_status_desc(resp),
      body = if (is.null(parsed)) body_txt else parsed
    ))
  }

  if (is.null(parsed)) list(raw = body_txt) else parsed
}

assert_string <- function(x, name, min = 0, max = Inf) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) stop(sprintf("`%s` must be a single string.", name))
  if (nchar(x) < min) stop(sprintf("`%s` must be at least %d characters.", name, min))
  if (nchar(x) > max) stop(sprintf("`%s` must be at most %d characters.", name, max))
}

assert_int <- function(x, name, min = -Inf, max = Inf) {
  if (is.null(x)) {
    return(invisible(TRUE))
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x %% 1 != 0) stop(sprintf("`%s` must be an integer.", name))
  if (x < min || x > max) stop(sprintf("`%s` must be between %s and %s.", name, min, max))
}

# -----------------------
# Correct endpoints
# -----------------------

hecate_search_concepts_impl <- function(client,
                                        query,
                                        vocabulary_id = NULL,
                                        standard_concept = NULL,
                                        domain_id = NULL,
                                        concept_class_id = NULL,
                                        limit = 20) {
  assert_string(query, "query", min = 1, max = 500)
  assert_int(limit, "limit", min = 1, max = 150)

  # Endpoint: GET /search?q=...
  req <- hecate_request(
    client,
    path = "search",
    query = list(
      q = query, # IMPORTANT: q not query
      vocabulary_id = vocabulary_id,
      standard_concept = standard_concept,
      domain_id = domain_id,
      concept_class_id = concept_class_id,
      limit = limit
    )
  )

  res <- hecate_perform(req)
  toJSON(res, auto_unbox = TRUE, pretty = TRUE)
}

hecate_get_concept_by_id_impl <- function(client, id) {
  assert_int(id, "id", min = 1)

  req <- hecate_request(client, path = paste0("concepts/", id))
  res <- hecate_perform(req)

  # TS expects an array and returns first element
  if (is.list(res) && !is.null(res$error)) {
    out <- res
  } else if (is.list(res) && length(res) >= 1 && is.list(res[[1]])) {
    out <- res[[1]]
  } else {
    out <- res
  }

  toJSON(out, auto_unbox = TRUE, pretty = TRUE)
}

hecate_get_concept_relationships_impl <- function(client, id) {
  assert_int(id, "id", min = 1)
  req <- hecate_request(client, path = paste0("concepts/", id, "/relationships"))
  res <- hecate_perform(req)
  toJSON(res, auto_unbox = TRUE, pretty = TRUE)
}

hecate_get_concept_phoebe_impl <- function(client, id) {
  assert_int(id, "id", min = 1)
  req <- hecate_request(client, path = paste0("concepts/", id, "/phoebe"))
  res <- hecate_perform(req)
  toJSON(res, auto_unbox = TRUE, pretty = TRUE)
}

hecate_expand_concept_hierarchy_impl <- function(client, id, childLevels = 5, parentLevels = 0) {
  assert_int(id, "id", min = 1)
  assert_int(childLevels, "childLevels", min = 0, max = 10)
  assert_int(parentLevels, "parentLevels", min = 0, max = 10)

  # IMPORTANT: childlevels/parentlevels lowercase
  req <- hecate_request(
    client,
    path = paste0("concepts/", id, "/expand"),
    query = list(childlevels = childLevels, parentlevels = parentLevels)
  )

  res <- hecate_perform(req)

  # TS returns response.data.concepts[0].children or []
  out <- res
  if (is.list(res) && is.null(res$error) && !is.null(res$concepts) && length(res$concepts) >= 1) {
    children <- res$concepts[[1]]$children
    if (is.null(children)) {
      children <- list()
    }
    out <- children
  }

  toJSON(out, auto_unbox = TRUE, pretty = TRUE)
}

hecate_get_concept_counts_impl <- function(concept_counts_df, concept_ids) {
  # Validate concept_ids - can be a single integer or a vector of integers
  if (is.null(concept_counts_df)) {
    return(toJSON(list(
      error = "data_not_loaded",
      message = "Concept counts data is not available."
    ), auto_unbox = TRUE, pretty = TRUE))
  }

  # Convert to integer vector (handles both single values and arrays)
  concept_ids <- as.integer(concept_ids)

  # Validate all are integers
  if (any(is.na(concept_ids)) || any(concept_ids < 1)) {
    return(toJSON(list(
      error = "invalid_input",
      message = "All concept_ids must be positive integers."
    ), auto_unbox = TRUE, pretty = TRUE))
  }

  # Filter the dataframe for the requested concept IDs
  result <- concept_counts_df %>%
    dplyr::filter(concept_id %in% concept_ids) %>%
    dplyr::arrange(concept_id, data_source)

  # Convert to list format for JSON
  if (nrow(result) == 0) {
    return(toJSON(list(
      message = "No records found for the specified concept IDs.",
      concept_ids = concept_ids,
      results = list()
    ), auto_unbox = TRUE, pretty = TRUE))
  }

  # Simplify record_count handling: convert integer64 directly to numeric, and
  # ensure numeric vectors have no NA values by replacing with 0.
  if (inherits(result$record_count, "integer64")) {
    result$record_count <- as.numeric(result$record_count)
  } else if (is.numeric(result$record_count)) {
    result$record_count[is.na(result$record_count)] <- 0
  }

  # Convert to list of records, ensuring record_count is properly formatted
  result_list <- lapply(1:nrow(result), function(i) {
    row_data <- as.list(result[i, ])
    # Ensure record_count is an integer (not scientific notation)
    if (!is.null(row_data$record_count) && !is.na(row_data$record_count)) {
      # Convert to integer if it's a whole number
      if (is.numeric(row_data$record_count) && row_data$record_count == floor(row_data$record_count)) {
        row_data$record_count <- as.integer(row_data$record_count)
      }
    }
    row_data
  })

  # Return structured JSON with options to prevent scientific notation
  output <- list(
    concept_ids = unique(concept_ids),
    total_records = nrow(result),
    results = result_list
  )

  # Use digits=22 and scipen to prevent scientific notation for integers
  # Set scipen option temporarily to ensure no scientific notation
  old_scipen <- getOption("scipen")
  on.exit(options(scipen = old_scipen), add = TRUE)
  options(scipen = 999)

  # Serialize with high precision to avoid scientific notation
  json_output <- toJSON(output, auto_unbox = TRUE, pretty = TRUE, digits = 22)

  json_output
}
