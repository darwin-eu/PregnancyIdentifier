# Copyright (c) 2024 Louisa Smith
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Attrition file name in output directory
#' @noRd
attritionFileName <- function(outputDir) {
  file.path(outputDir, "attrition.csv")
}

#' Get record and person counts from a table (lazy or data frame)
#'
#' @param tbl A lazy table (cdm reference) or a data frame with a `person_id` column.
#' @return A list with `records` (integer) and `persons` (integer).
#' @noRd
getRecordAndPersonCounts <- function(tbl) {
  if (inherits(tbl, "tbl_lazy")) {
    n_records <- tbl %>%
      dplyr::ungroup() %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::collect() %>%
      dplyr::pull("n")
    n_persons <- tbl %>%
      dplyr::ungroup() %>%
      dplyr::summarise(n = dplyr::n_distinct(.data$person_id)) %>%
      dplyr::collect() %>%
      dplyr::pull("n")
  } else {
    n_records <- nrow(tbl)
    n_persons <- dplyr::n_distinct(tbl$person_id)
  }
  list(records = as.integer(n_records), persons = as.integer(n_persons))
}

#' Create initial attrition CSV with HIP and PPS record/person counts
#'
#' Called from \code{initPregnancies()} when \code{outputDir} is provided.
#' Writes \code{attrition.csv} with one row per table (preg_hip_records, preg_pps_records)
#' with initial post counts; prior and dropped are NA for this step.
#'
#' @param outputDir Directory where \code{attrition.csv} will be written.
#' @param cdm CDM reference containing \code{preg_hip_records} and \code{preg_pps_records}.
#' @return Invisibly the path to the written file.
#' @noRd
initAttrition <- function(outputDir, cdm) {
  checkmate::assertCharacter(outputDir, len = 1)
  checkmate::assertClass(cdm, "cdm_reference")
  dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)

  hipCounts <- getRecordAndPersonCounts(cdm$preg_hip_records)
  ppsCounts <- getRecordAndPersonCounts(cdm$preg_pps_records)

  attrition <- tibble::tibble(
    step = c("init", "init"),
    table = c("preg_hip_records", "preg_pps_records"),
    outcome = c(NA_character_, NA_character_),
    prior_records = c(NA_integer_, NA_integer_),
    prior_persons = c(NA_integer_, NA_integer_),
    dropped_records = c(NA_integer_, NA_integer_),
    dropped_persons = c(NA_integer_, NA_integer_),
    post_records = c(hipCounts$records, ppsCounts$records),
    post_persons = c(hipCounts$persons, ppsCounts$persons)
  )
  path <- attritionFileName(outputDir)
  utils::write.csv(attrition, path, row.names = FALSE)
  invisible(path)
}

#' Append one row to the attrition CSV
#'
#' @param outputDir Directory containing \code{attrition.csv}.
#' @param step Character. Pipeline step name (e.g. \code{"hip_episodes"}, \code{"final_episodes"}).
#' @param table Character. Output table or artifact name.
#' @param outcome Character or NA. Outcome category for by-outcome rows; NA for overall.
#' @param prior_records, prior_persons Prior record and person counts.
#' @param dropped_records, dropped_persons Number dropped at this step.
#' @param post_records, post_persons Post step record and person counts.
#' @noRd
appendAttrition <- function(outputDir,
                           step,
                           table,
                           outcome = NA_character_,
                           prior_records,
                           prior_persons,
                           dropped_records,
                           dropped_persons,
                           post_records,
                           post_persons) {
  path <- attritionFileName(outputDir)
  if (!file.exists(path)) {
    rlang::abort(sprintf("Attrition file not found: %s. Run initPregnancies with outputDir first.", path))
  }
  existing <- utils::read.csv(path, stringsAsFactors = FALSE)
  newRow <- data.frame(
    step = step,
    table = table,
    outcome = as.character(outcome),
    prior_records = as.integer(prior_records),
    prior_persons = as.integer(prior_persons),
    dropped_records = as.integer(dropped_records),
    dropped_persons = as.integer(dropped_persons),
    post_records = as.integer(post_records),
    post_persons = as.integer(post_persons),
    stringsAsFactors = FALSE
  )
  if ("outcome" %in% names(existing) && is.logical(existing$outcome)) {
    existing$outcome <- as.character(existing$outcome)
  }
  combined <- rbind(existing, newRow)
  utils::write.csv(combined, path, row.names = FALSE)
  invisible(path)
}

#' Get last post_records and post_persons for a table from attrition.csv
#'
#' Used to obtain prior counts for the next pipeline step.
#'
#' @param outputDir Directory containing \code{attrition.csv}.
#' @param table Character. Table name (e.g. \code{"preg_hip_records"}, \code{"hipps_episodes"}).
#' @return List with \code{post_records} and \code{post_persons}, or NULL if no row found.
#' @noRd
getAttritionPrior <- function(outputDir, table) {
  path <- attritionFileName(outputDir)
  if (!file.exists(path)) {
    return(NULL)
  }
  att <- utils::read.csv(path, stringsAsFactors = FALSE)
  idx <- which(att$table == table)
  if (length(idx) == 0) {
    return(NULL)
  }
  last <- idx[length(idx)]
  list(
    post_records = as.integer(att$post_records[last]),
    post_persons = as.integer(att$post_persons[last])
  )
}
