# services_allowed_concepts.R
# Load HIP/PPS/ESD concept Excel files and expose a unified allowed-concepts table
# for restricting concept ID input in the event editor.

# Cache for load_allowed_concepts() so we don't reread Excel on every reactive tick
.allowed_concepts_cache <- new.env(parent = emptyenv())

#' Load allowed concepts from HIP, PPS, and ESD Excel files
#'
#' Reads concept Excel files from the PregnancyIdentifier package inst/concepts,
#' normalizes to concept_id, concept_name, source, and returns a deduplicated
#' data.frame. Results are cached in a local env so repeated calls don't
#' reread the files.
#'
#' @return data.frame with columns concept_id (integer), concept_name (character),
#'   source (one of "HIP", "PPS", "ESD", "APP_HARDCODED").
#' @export
load_allowed_concepts <- function() {
  cache_key <- "allowed_concepts_df"
  if (exists(cache_key, envir = .allowed_concepts_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .allowed_concepts_cache))
  }

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("readxl is required. Install with: install.packages('readxl')")
  }

  out <- list()
  pkg <- "PregnancyIdentifier"

  # HIP: concept_id, concept_name
  hip_path <- system.file("concepts/HIP_concepts.xlsx", package = pkg)
  if (length(hip_path) == 1L && nchar(hip_path) > 0L && file.exists(hip_path)) {
    hip <- readxl::read_excel(hip_path)
    if ("concept_id" %in% names(hip) && "concept_name" %in% names(hip)) {
      hip_df <- data.frame(
        concept_id = as.integer(hip$concept_id),
        concept_name = as.character(hip$concept_name),
        source = "HIP",
        stringsAsFactors = FALSE
      )
      out <- c(out, list(hip_df))
    }
  }

  # PPS: pps_concept_id, pps_concept_name
  pps_path <- system.file("concepts/PPS_concepts.xlsx", package = pkg)
  if (length(pps_path) == 1L && nchar(pps_path) > 0L && file.exists(pps_path)) {
    pps <- readxl::read_excel(pps_path)
    if ("pps_concept_id" %in% names(pps)) {
      pps_df <- data.frame(
        concept_id = as.integer(pps$pps_concept_id),
        concept_name = if ("pps_concept_name" %in% names(pps)) as.character(pps$pps_concept_name) else as.character(pps$pps_concept_id),
        source = "PPS",
        stringsAsFactors = FALSE
      )
      out <- c(out, list(pps_df))
    }
  }

  # ESD: esd_concept_id, esd_concept_name
  esd_path <- system.file("concepts/ESD_concepts.xlsx", package = pkg)
  if (length(esd_path) == 1L && nchar(esd_path) > 0L && file.exists(esd_path)) {
    esd <- readxl::read_excel(esd_path)
    if ("esd_concept_id" %in% names(esd)) {
      esd_df <- data.frame(
        concept_id = as.integer(esd$esd_concept_id),
        concept_name = if ("esd_concept_name" %in% names(esd)) as.character(esd$esd_concept_name) else as.character(esd$esd_concept_id),
        source = "ESD",
        stringsAsFactors = FALSE
      )
      out <- c(out, list(esd_df))
    }
  }

  if (length(out) == 0L) {
    df <- data.frame(
      concept_id = integer(0),
      concept_name = character(0),
      source = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    df <- do.call(rbind, out)
    df <- df[!is.na(df$concept_id), , drop = FALSE]
    # Deduplicate by concept_id (keep first source)
    df <- df[!duplicated(df$concept_id), , drop = FALSE]
    df <- df[order(df$concept_id), , drop = FALSE]
  }

  # Optional: add any app-hardcoded concept IDs used in the app (e.g. type concepts)
  # For now we don't add any; if needed, append rows with source = "APP_HARDCODED"

  assign(cache_key, df, envir = .allowed_concepts_cache)
  df
}

#' Allowed concept choices for selectizeInput
#'
#' Returns a named vector suitable for choices: display "concept_id — concept_name (source)",
#' value = concept_id.
#'
#' @return Named integer vector (names = label, values = concept_id).
#' @export
allowed_concept_choices <- function() {
  df <- load_allowed_concepts()
  if (nrow(df) == 0L) {
    return(structure(integer(0), names = character(0)))
  }
  label <- paste0(df$concept_id, " — ", df$concept_name, " (", df$source, ")")
  structure(as.integer(df$concept_id), names = label)
}

#' Check if a concept_id is in the allowed set
#'
#' @param concept_id Integer (scalar or vector).
#' @return Logical same length as concept_id.
#' @export
is_allowed_concept <- function(concept_id) {
  df <- load_allowed_concepts()
  as.integer(concept_id) %in% df$concept_id
}
