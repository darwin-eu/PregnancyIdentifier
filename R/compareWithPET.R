# Copyright (c) 2024 Louisa Smith
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

#' Compare runPregnancyIdentifier results with the OMOP Pregnancy Extension Table (PET)
#'
#' Compares algorithm output (from \code{final_pregnancy_episodes.rds}) to the PET
#' table and writes comparison summaries to \code{exportFolder}. Comparisons include:
#' pregnancy episode and person counts; raw and filtered (gestation 0-308, end >= start)
#' person overlap; time-overlap summaries (PET->IPE and IPE->PET, 0 and 1 day);
#' Venn counts (both, PET only, algorithm only); a 2x2 confusion matrix (TP, FN, FP, TN)
#' with sensitivity and PPV (no gold-standard negatives, so specificity and NPV are not defined); outcome confusion matrix and accuracy;
#' outcome comparison by year (same-year pairs, cross-tab LB/SB/AB vs PET outcome);
#' and pregnancy duration distributions.
#'
#' @param cdm A \code{cdm_reference} (from CDMConnector) with a database connection.
#'   The PET table is read via \code{petSchema} and \code{petTable}.
#' @param outputFolder \code{character(1)}. Directory containing pipeline outputs
#'   (\code{final_pregnancy_episodes.rds}), i.e. the episode-level input to the comparison.
#' @param exportFolder \code{character(1)}. Directory where comparison CSVs and
#'   \code{log.txt} are written. Created if it does not exist. Log file is appended
#'   if it already exists.
#' @param petSchema \code{character(1)}. Schema name of the PET table (e.g.
#'   \code{"omop_cmbd"}).
#' @param petTable \code{character(1)}. Table name of the pregnancy episode table
#'   (e.g. \code{"pregnancy_episode"}). Must contain at least \code{person_id},
#'   \code{pregnancy_start_date}, \code{pregnancy_end_date}, and
#'   \code{pregnancy_outcome} (concept_id). Gestational length in days is
#'   computed from start and end dates in the database via
#'   \code{CDMConnector::datediff()}; the table need not have a
#'   \code{gestational_length_in_day} column.
#' @param minOverlapDays \code{integer(1)}. Minimum overlap in days to consider an
#'   algorithm episode and a PET episode as the same pregnancy (default 1).
#' @param removeWithinSourceOverlaps \code{logical(1)}. If \code{TRUE}, before
#'   matching the code removes overlapping episodes within PET and within the
#'   algorithm (greedy non-overlapping by start date per person), which can
#'   reduce many-to-many candidate pairs. Default \code{FALSE}.
#' @param outputLogToConsole \code{logical(1)}. Whether to log to the console as
#'   well as to \code{file.path(exportFolder, "log.txt")}.
#'
#' @return Nothing. The summarised result is written to
#'   \code{file.path(exportFolder, "pet_comparison_summarised_result.csv")}.
#'   Use \code{omopgenerics::importSummarisedResult()} to read it and
#'   \code{visOmopResults::visTable()} to display it.
#' @export
comparePregnancyIdentifierWithPET <- function(cdm,
                                              outputFolder,
                                              exportFolder,
                                              petSchema,
                                              petTable,
                                              minOverlapDays = 1L,
                                              removeWithinSourceOverlaps = FALSE,
                                              outputLogToConsole = TRUE) {
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertCharacter(outputFolder, len = 1L, any.missing = FALSE)
  checkmate::assertCharacter(exportFolder, len = 1L, any.missing = FALSE)
  checkmate::assertCharacter(petSchema, len = 1L, any.missing = FALSE)
  checkmate::assertCharacter(petTable, len = 1L, any.missing = FALSE)
  checkmate::assertIntegerish(minOverlapDays, len = 1L, lower = 0)
  checkmate::assertLogical(removeWithinSourceOverlaps, len = 1L, any.missing = FALSE)
  checkmate::assertLogical(outputLogToConsole, len = 1L, any.missing = FALSE)
  minOverlapDays <- as.integer(minOverlapDays)

  checkmate::assertDirectoryExists(outputFolder)

  dir.create(exportFolder, recursive = TRUE, showWarnings = FALSE)
  checkmate::assertDirectoryExists(exportFolder)
  logFile <- file.path(exportFolder, "log.txt")
  appenders <- list(log4r::file_appender(logFile, append = TRUE))
  if (outputLogToConsole) {
    appenders <- c(log4r::console_appender(), appenders)
  }
  logger <- log4r::logger(threshold = "INFO", appenders = appenders)
  log4r::info(logger, "Starting PET comparison")

  # CDM name for SummarisedResult (all columns character except result_id for settings)
  snap <- tryCatch(CDMConnector::snapshot(cdm), error = function(e) NULL)
  cdm_name_sr <- if (!is.null(snap) && "cdm_name" %in% names(snap) && nrow(snap) > 0) {
    as.character(snap$cdm_name[1])
  } else {
    "unknown"
  }
  result_id_sr <- 1L
  # Use "all" for group_level (not "overall"): omopgenerics::getLabels() treats
  # "overall" as a placeholder and returns 0 elements, so splitGroup() would
  # throw "Column names and levels number does not match" when group_name has 1 element.
  base_row <- list(
    result_id = result_id_sr,
    cdm_name = cdm_name_sr,
    group_name = "pet_comparison",
    group_level = "all",
    strata_name = "overall",
    strata_level = "overall",
    additional_name = "overall",
    additional_level = "overall"
  )
  # Helper: add rows to sr_rows (list of rows) for a variable with one or more estimates
  add_sr_rows <- function(sr_rows, variable_name, variable_level, estimates) {
    for (nm in names(estimates)) {
      est <- estimates[[nm]]
      sr_rows[[length(sr_rows) + 1]] <- c(
        base_row,
        variable_name = variable_name,
        variable_level = as.character(variable_level),
        estimate_name = nm,
        estimate_type = est$type,
        estimate_value = as.character(est$value)
      )
    }
    sr_rows
  }

  # Min/max that return NA without warning when no non-missing values (for empty/filtered groups)
  safe_min <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    min(x)
  }
  safe_max <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    max(x)
  }

  # Path to algorithm output
  algPath <- file.path(outputFolder, "final_pregnancy_episodes.rds")
  if (!file.exists(algPath)) {
    log4r::error(logger, sprintf("Algorithm output not found: %s", algPath))
    stop(sprintf("Algorithm output not found: %s", algPath))
  }

  # Load algorithm episodes
  log4r::info(logger, "Loading algorithm episodes from final_pregnancy_episodes.rds")
  alg <- readRDS(algPath)
  names(alg) <- tolower(names(alg))
  requiredAlg <- c("person_id", "final_episode_start_date", "final_episode_end_date", "final_outcome_category")
  miss <- setdiff(requiredAlg, names(alg))
  if (length(miss) > 0) {
    log4r::error(logger, sprintf("Algorithm table missing columns: %s", paste(miss, collapse = ", ")))
    stop(sprintf("Algorithm table missing columns: %s", paste(miss, collapse = ", ")))
  }

  # PET table from CDM
  petCols <- c("person_id", "pregnancy_start_date", "pregnancy_end_date", "pregnancy_outcome")
  con <- attr(cdm, "dbcon")
  # DuckDB's tbl() does not support DBI::Id / inSchema; use dbReadTable and compute in R
  if (inherits(con, "duckdb_connection")) {
    pet <- DBI::dbReadTable(con, DBI::Id(schema = petSchema, table = petTable))
    if (!all(petCols %in% names(pet))) {
      missPet <- setdiff(petCols, names(pet))
      log4r::error(logger, sprintf("PET table missing columns: %s", paste(missPet, collapse = ", ")))
      stop(sprintf("PET table missing columns: %s", paste(missPet, collapse = ", ")))
    }
    pet <- pet[, petCols, drop = FALSE]
    log4r::info(logger, "Loading PET table from database")
    pet$gestational_length_in_day <- as.numeric(
      difftime(
        as.Date(pet$pregnancy_end_date),
        as.Date(pet$pregnancy_start_date),
        units = "days"
      )
    )
  } else {
    petTbl <- dplyr::tbl(
      src = con,
      CDMConnector::inSchema(schema = petSchema, table = petTable)
    )
    petNames <- colnames(petTbl)
    if (!all(petCols %in% petNames)) {
      missPet <- setdiff(petCols, petNames)
      log4r::error(logger, sprintf("PET table missing columns: %s", paste(missPet, collapse = ", ")))
      stop(sprintf("PET table missing columns: %s", paste(missPet, collapse = ", ")))
    }
    log4r::info(logger, "Loading PET table from database")
    pet <- petTbl %>%
      dplyr::select(dplyr::all_of(petCols)) %>%
      dplyr::mutate(
        gestational_length_in_day = !!CDMConnector::datediff(
          "pregnancy_start_date",
          "pregnancy_end_date",
          interval = "day"
        )
      ) %>%
      dplyr::collect()
  }

  # Normalize PET column names to snake_case if needed
  names(pet) <- tolower(names(pet))
  # Ensure date columns are Date
  for (col in c("pregnancy_start_date", "pregnancy_end_date")) {
    if (col %in% names(pet) && !inherits(pet[[col]], "Date")) {
      pet[[col]] <- as.Date(pet[[col]])
    }
  }

  alg <- alg %>%
    dplyr::select(
      "person_id",
      "final_episode_start_date",
      "final_episode_end_date",
      "final_outcome_category",
      dplyr::any_of("esd_gestational_age_days_calculated")
    ) %>%
    dplyr::mutate(
      alg_start = as.Date(.data$final_episode_start_date),
      alg_end = as.Date(.data$final_episode_end_date)
    )
  pet <- pet %>%
    dplyr::mutate(
      pet_start = as.Date(.data$pregnancy_start_date),
      pet_end = as.Date(.data$pregnancy_end_date)
    )

  # Optional: remove within-source overlapping episodes before matching (reduces many-to-many pairs)
  if (removeWithinSourceOverlaps) {
    log4r::info(logger, "Removing within-source overlapping episodes (PET and algorithm) before matching")
    pet <- conformEpisodePeriods(pet, "person_id", "pet_start", "pet_end", maxDays = 400)
    alg <- conformEpisodePeriods(alg, "person_id", "alg_start", "alg_end", maxDays = 400)
  }

  # ---- 1) Episode and person counts ----
  log4r::info(logger, "Computing episode and person counts")
  n_episodes_alg <- nrow(alg)
  n_persons_alg <- dplyr::n_distinct(alg$person_id)
  n_episodes_pet <- nrow(pet)
  n_persons_pet <- dplyr::n_distinct(pet$person_id)

  episode_counts <- tibble::tibble(
    source = c("algorithm", "pet"),
    n_episodes = c(n_episodes_alg, n_episodes_pet),
    n_persons = c(n_persons_alg, n_persons_pet)
  )

  # ---- 1b) Raw and filtered person overlap ----
  log4r::info(logger, "Computing raw and filtered person overlap")
  raw_person_overlap <- length(unique(intersect(pet$person_id, alg$person_id)))
  pet_f <- pet %>%
    dplyr::mutate(computed_gest = as.numeric(.data$pet_end - .data$pet_start)) %>%
    dplyr::filter(.data$computed_gest >= 0, .data$computed_gest <= 308, .data$pet_end >= .data$pet_start)
  alg_f <- alg %>%
    dplyr::mutate(computed_gest = as.numeric(.data$alg_end - .data$alg_start)) %>%
    dplyr::filter(.data$computed_gest >= 0, .data$computed_gest <= 308, .data$alg_end >= .data$alg_start)
  cohort_person_overlap <- length(unique(intersect(pet_f$person_id, alg_f$person_id)))
  person_overlap <- tibble::tibble(
    metric = c("raw_person_overlap", "cohort_person_overlap"),
    n_persons = c(raw_person_overlap, cohort_person_overlap),
    description = c(
      "Distinct persons with at least one PET and one algorithm episode",
      "Distinct persons after filtering gestation 0-308 days and end >= start"
    )
  )

  # ---- 2) Link episodes (overlap) and date differences ----
  # Match: same person, overlapping period (overlap >= minOverlapDays)
  # Overlap in days: max(0, min(alg_end, pet_end) - max(alg_start, pet_start) + 1)
  log4r::info(logger, "Linking episodes by person and overlapping dates")
  alg <- alg %>% dplyr::mutate(.alg_idx = dplyr::row_number())
  pet <- pet %>% dplyr::mutate(.pet_idx = dplyr::row_number())

  # Cross-join by person and compute overlap days
  overlap_days <- function(s1, e1, s2, e2) {
    pmax(0, as.numeric(pmin(e1, e2) - pmax(s1, s2)) + 1)
  }
  all_pairs <- alg %>%
    dplyr::inner_join(
      pet,
      by = "person_id",
      relationship = "many-to-many",
      suffix = c("_alg", "_pet")
    ) %>%
    dplyr::mutate(
      .overlap = overlap_days(.data$alg_start, .data$alg_end, .data$pet_start, .data$pet_end)
    )
  joined <- all_pairs %>% dplyr::filter(.data$.overlap >= minOverlapDays)

  # One-to-one matching: greedily choose pairs with largest overlap per person so
  # Venn/confusion counts are consistent (no double-counting across PET vs algorithm).
  one_to_one_matches <- joined %>%
    dplyr::arrange(.data$person_id, dplyr::desc(.data$.overlap)) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::group_modify(function(.x, .y) {
      df <- .x
      used_pet <- integer(0)
      used_alg <- integer(0)
      keep <- logical(nrow(df))
      for (i in seq_len(nrow(df))) {
        if (!(df$.pet_idx[i] %in% used_pet) && !(df$.alg_idx[i] %in% used_alg)) {
          keep[i] <- TRUE
          used_pet <- c(used_pet, df$.pet_idx[i])
          used_alg <- c(used_alg, df$.alg_idx[i])
        }
      }
      df[keep, , drop = FALSE]
    }) %>%
    dplyr::ungroup()

  best_match <- one_to_one_matches
  pet_matched <- dplyr::n_distinct(one_to_one_matches$.pet_idx)
  alg_matched <- dplyr::n_distinct(one_to_one_matches$.alg_idx)
  n_both <- nrow(one_to_one_matches)
  n_pet_only <- n_episodes_pet - pet_matched
  n_alg_only <- n_episodes_alg - alg_matched

  venn_counts <- tibble::tibble(
    category = c("both", "pet_only", "algorithm_only"),
    n_episodes = c(n_both, n_pet_only, n_alg_only),
    n_pet_matched = pet_matched,
    n_alg_matched = alg_matched
  )

  # Protocol summary: total PET, total algorithm, total matched (for reporting)
  protocol_summary <- tibble::tibble(
    total_pet_episodes = n_episodes_pet,
    total_algorithm_episodes = n_episodes_alg,
    total_matched_episodes = n_both
  )

  # ---- 2b) Time overlap summaries (PET->IPE and IPE->PET, 0 and 1 day) ----
  log4r::info(logger, "Computing time overlap summaries")
  pet_max <- all_pairs %>%
    dplyr::group_by(.data$person_id, .data$.pet_idx) %>%
    dplyr::summarise(n_ipe_overlap = safe_max(.data$.overlap), .groups = "drop")
  pet_with_max_overlap <- pet %>%
    dplyr::select("person_id", ".pet_idx") %>%
    dplyr::left_join(pet_max, by = c("person_id", ".pet_idx")) %>%
    dplyr::mutate(n_ipe_overlap = dplyr::coalesce(.data$n_ipe_overlap, 0))
  alg_max <- all_pairs %>%
    dplyr::group_by(.data$person_id, .data$.alg_idx) %>%
    dplyr::summarise(n_pet_overlap = safe_max(.data$.overlap), .groups = "drop")
  alg_with_max_overlap <- alg %>%
    dplyr::select("person_id", ".alg_idx") %>%
    dplyr::left_join(alg_max, by = c("person_id", ".alg_idx")) %>%
    dplyr::mutate(n_pet_overlap = dplyr::coalesce(.data$n_pet_overlap, 0))
  res1 <- pet_with_max_overlap %>%
    dplyr::summarise(
      min = safe_min(.data$n_ipe_overlap),
      Q25 = stats::quantile(.data$n_ipe_overlap, 0.25, na.rm = TRUE),
      median = stats::median(.data$n_ipe_overlap, na.rm = TRUE),
      Q75 = stats::quantile(.data$n_ipe_overlap, 0.75, na.rm = TRUE),
      max = safe_max(.data$n_ipe_overlap),
      sd = stats::sd(.data$n_ipe_overlap, na.rm = TRUE),
      n_episodes = dplyr::n(),
      n_persons = dplyr::n_distinct(.data$person_id)
    ) %>%
    dplyr::mutate(label = "PET -> IPE 0 day overlap required")
  res2 <- pet_with_max_overlap %>%
    dplyr::filter(.data$n_ipe_overlap > 0) %>%
    dplyr::summarise(
      min = safe_min(.data$n_ipe_overlap),
      Q25 = stats::quantile(.data$n_ipe_overlap, 0.25, na.rm = TRUE),
      median = stats::median(.data$n_ipe_overlap, na.rm = TRUE),
      Q75 = stats::quantile(.data$n_ipe_overlap, 0.75, na.rm = TRUE),
      max = safe_max(.data$n_ipe_overlap),
      sd = stats::sd(.data$n_ipe_overlap, na.rm = TRUE),
      n_episodes = dplyr::n(),
      n_persons = dplyr::n_distinct(.data$person_id)
    ) %>%
    dplyr::mutate(label = "PET -> IPE 1 day overlap required")
  res3 <- alg_with_max_overlap %>%
    dplyr::summarise(
      min = safe_min(.data$n_pet_overlap),
      Q25 = stats::quantile(.data$n_pet_overlap, 0.25, na.rm = TRUE),
      median = stats::median(.data$n_pet_overlap, na.rm = TRUE),
      Q75 = stats::quantile(.data$n_pet_overlap, 0.75, na.rm = TRUE),
      max = safe_max(.data$n_pet_overlap),
      sd = stats::sd(.data$n_pet_overlap, na.rm = TRUE),
      n_episodes = dplyr::n(),
      n_persons = dplyr::n_distinct(.data$person_id)
    ) %>%
    dplyr::mutate(label = "IPE -> PET 0 day overlap required")
  res4 <- alg_with_max_overlap %>%
    dplyr::filter(.data$n_pet_overlap > 0) %>%
    dplyr::summarise(
      min = safe_min(.data$n_pet_overlap),
      Q25 = stats::quantile(.data$n_pet_overlap, 0.25, na.rm = TRUE),
      median = stats::median(.data$n_pet_overlap, na.rm = TRUE),
      Q75 = stats::quantile(.data$n_pet_overlap, 0.75, na.rm = TRUE),
      max = safe_max(.data$n_pet_overlap),
      sd = stats::sd(.data$n_pet_overlap, na.rm = TRUE),
      n_episodes = dplyr::n(),
      n_persons = dplyr::n_distinct(.data$person_id)
    ) %>%
    dplyr::mutate(label = "IPE -> PET 1 day overlap required")
  time_overlap_summary <- dplyr::bind_rows(res1, res2, res3, res4)

  # 2x2 confusion matrix (PET = reference, Algorithm = test)
  # TP = PET episode has matching algorithm episode; FN = PET episode, no match; FP = algorithm episode, no match; TN = N/A (no negative population at episode level)
  tp <- pet_matched
  fn <- n_pet_only
  fp <- n_alg_only
  tn <- NA_integer_

  confusion_2x2 <- tibble::tibble(
    reference = c("PET_positive", "PET_positive", "PET_negative", "PET_negative"),
    algorithm = c("positive", "negative", "positive", "negative"),
    count = c(tp, fn, fp, tn),
    cell = c("TP", "FN", "FP", "TN")
  )

  # Sensitivity and PPV only (PET as reference; no gold-standard negatives, so specificity and NPV are not defined)
  # Sensitivity = TP / (TP + FN); PPV = TP / (TP + FP)
  sensitivity <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  ppv <- if (tp + fp > 0) tp / (tp + fp) else NA_real_

  ppv_sensitivity <- tibble::tibble(
    metric = c("sensitivity", "ppv"),
    value = c(sensitivity, ppv),
    denominator = c(tp + fn, tp + fp),
    numerator = c(tp, tp)
  )

  # Date differences for best-matched pairs (one row per best-match pair)
  # Sign convention: PET − algorithm (positive = PET start/end is later than algorithm)
  if (nrow(best_match) > 0) {
    date_diff <- best_match %>%
      dplyr::mutate(
        start_diff_days = as.numeric(.data$pet_start - .data$alg_start),
        end_diff_days = as.numeric(.data$pet_end - .data$alg_end),
        duration_alg_days = as.numeric(.data$alg_end - .data$alg_start),
        duration_pet_days = as.numeric(.data$pet_end - .data$pet_start)
      ) %>%
      dplyr::select(
        "person_id",
        "start_diff_days",
        "end_diff_days",
        "duration_alg_days",
        "duration_pet_days"
      )

    date_summary <- tibble::tibble(
      measure = c("start_diff_days", "end_diff_days"),
      mean = c(mean(date_diff$start_diff_days, na.rm = TRUE), mean(date_diff$end_diff_days, na.rm = TRUE)),
      median = c(stats::median(date_diff$start_diff_days, na.rm = TRUE), stats::median(date_diff$end_diff_days, na.rm = TRUE)),
      sd = c(stats::sd(date_diff$start_diff_days, na.rm = TRUE), stats::sd(date_diff$end_diff_days, na.rm = TRUE)),
      min = c(safe_min(date_diff$start_diff_days), safe_min(date_diff$end_diff_days)),
      Q25 = c(stats::quantile(date_diff$start_diff_days, 0.25, na.rm = TRUE), stats::quantile(date_diff$end_diff_days, 0.25, na.rm = TRUE)),
      Q75 = c(stats::quantile(date_diff$start_diff_days, 0.75, na.rm = TRUE), stats::quantile(date_diff$end_diff_days, 0.75, na.rm = TRUE)),
      max = c(safe_max(date_diff$start_diff_days), safe_max(date_diff$end_diff_days)),
      n_matched = nrow(date_diff)
    )
  }

  # ---- 3) Outcome confusion matrix (matched pairs) ----
  # PET outcome: pregnancy_outcome concept_id (4092289 LB, 4067106 Miscarriage, 4081422 Elective termination, 443213 Stillbirth)
  # Algorithm: final_outcome_category (LB, SB, AB, SA, ECT, DELIV, PREG)
  log4r::info(logger, "Computing outcome confusion matrix")
  alg_to_concept <- function(x) {
    dplyr::case_when(
      x == "LB" ~ 4092289L,
      x == "SB" ~ 443213L,
      x == "AB" ~ 4081422L,
      x == "SA" ~ 4067106L,
      x == "ECT" ~ NA_integer_,
      x == "PREG" ~ NA_integer_,
      x == "DELIV" ~ 4092289L,
      TRUE ~ NA_integer_
    )
  }

  outcome_accuracy <- tibble::tibble(n_correct = NA_integer_, n_total = 0L, accuracy = NA_real_)
  confusion <- tibble::tibble(pet_outcome = integer(0))

  if (nrow(best_match) > 0) {
    outcome_pairs <- best_match %>%
      dplyr::mutate(
        pet_outcome = as.integer(.data$pregnancy_outcome),
        alg_outcome_char = .data$final_outcome_category,
        alg_outcome_concept_id = alg_to_concept(.data$final_outcome_category)
      ) %>%
      dplyr::select("person_id", "pet_outcome", "alg_outcome_char", "alg_outcome_concept_id")

    confusion <- outcome_pairs %>%
      dplyr::count(.data$pet_outcome, .data$alg_outcome_char, name = "n") %>%
      tidyr::pivot_wider(names_from = "alg_outcome_char", values_from = "n", values_fill = 0)

    # Accuracy: when comparing same concept_id (PET) to algorithm-mapped concept_id
    outcome_pairs_for_acc <- outcome_pairs %>%
      dplyr::filter(!is.na(.data$alg_outcome_concept_id))
    n_correct <- sum(outcome_pairs_for_acc$pet_outcome == outcome_pairs_for_acc$alg_outcome_concept_id, na.rm = TRUE)
    n_total_acc <- nrow(outcome_pairs_for_acc)
    accuracy <- if (n_total_acc > 0) n_correct / n_total_acc else NA_real_
    outcome_accuracy <- tibble::tibble(
      n_correct = n_correct,
      n_total = n_total_acc,
      accuracy = accuracy
    )
  }

  # ---- 3b) Outcome comparison by year (same-year pairs, cross-tab LB/SB/AB) ----
  # Outcome concept_id: 4092289 Live birth, 4067106 Miscarriage, 4081422 Elective termination, 443213 Stillbirth, 0 = unknown/PREG/DELIV/ECT
  log4r::info(logger, "Computing outcome comparison by year")
  outcome_by_year <- tibble::tibble(
    overall_equal = NA_integer_, overall_diff = NA_integer_,
    lb_lb = NA_integer_, lb_miscarriage = NA_integer_, lb_ab = NA_integer_, lb_sb = NA_integer_, lb_unknown = NA_integer_,
    sb_sb = NA_integer_, sb_miscarriage = NA_integer_, sb_ab = NA_integer_, sb_lb = NA_integer_, sb_unknown = NA_integer_,
    ab_ab = NA_integer_, ab_miscarriage = NA_integer_, ab_lb = NA_integer_, ab_sb = NA_integer_, ab_unknown = NA_integer_
  )
  if (nrow(best_match) > 0) {
    alg_to_concept_id <- function(x) {
      dplyr::case_when(
        x == "PREG" ~ 0L, x == "DELIV" ~ 0L, x == "AB" ~ 4081422L, x == "LB" ~ 4092289L,
        x == "ECT" ~ 0L, x == "SA" ~ 4067106L, x == "SB" ~ 443213L, TRUE ~ 0L
      )
    }
    same_year_pairs <- best_match %>%
      dplyr::mutate(
        year_alg = lubridate::year(.data$alg_start),
        year_pet = lubridate::year(.data$pet_start),
        outcome_concept_id = alg_to_concept_id(.data$final_outcome_category),
        pregnancy_outcome = as.integer(.data$pregnancy_outcome)
      ) %>%
      dplyr::filter(.data$year_alg == .data$year_pet) %>%
      dplyr::mutate(
        same = as.integer(.data$outcome_concept_id == .data$pregnancy_outcome),
        lb_lb = as.integer(.data$outcome_concept_id == 4092289L & .data$pregnancy_outcome == 4092289L),
        lb_miscarriage = as.integer(.data$outcome_concept_id == 4092289L & .data$pregnancy_outcome == 4067106L),
        lb_ab = as.integer(.data$outcome_concept_id == 4092289L & .data$pregnancy_outcome == 4081422L),
        lb_sb = as.integer(.data$outcome_concept_id == 4092289L & .data$pregnancy_outcome == 443213L),
        lb_unknown = as.integer(.data$outcome_concept_id == 4092289L & (.data$pregnancy_outcome == 0L | is.na(.data$pregnancy_outcome))),
        sb_sb = as.integer(.data$outcome_concept_id == 443213L & .data$pregnancy_outcome == 443213L),
        sb_miscarriage = as.integer(.data$outcome_concept_id == 443213L & .data$pregnancy_outcome == 4067106L),
        sb_ab = as.integer(.data$outcome_concept_id == 443213L & .data$pregnancy_outcome == 4081422L),
        sb_lb = as.integer(.data$outcome_concept_id == 443213L & .data$pregnancy_outcome == 4092289L),
        sb_unknown = as.integer(.data$outcome_concept_id == 443213L & (.data$pregnancy_outcome == 0L | is.na(.data$pregnancy_outcome))),
        ab_ab = as.integer(.data$outcome_concept_id == 4081422L & .data$pregnancy_outcome == 4081422L),
        ab_miscarriage = as.integer(.data$outcome_concept_id == 4081422L & .data$pregnancy_outcome == 4067106L),
        ab_lb = as.integer(.data$outcome_concept_id == 4081422L & .data$pregnancy_outcome == 4092289L),
        ab_sb = as.integer(.data$outcome_concept_id == 4081422L & .data$pregnancy_outcome == 443213L),
        ab_unknown = as.integer(.data$outcome_concept_id == 4081422L & (.data$pregnancy_outcome == 0L | is.na(.data$pregnancy_outcome)))
      )
    outcome_by_year <- same_year_pairs %>%
      dplyr::summarise(
        overall_equal = sum(.data$same, na.rm = TRUE),
        overall_diff = sum(1L - .data$same, na.rm = TRUE),
        lb_lb = sum(.data$lb_lb, na.rm = TRUE), lb_miscarriage = sum(.data$lb_miscarriage, na.rm = TRUE),
        lb_ab = sum(.data$lb_ab, na.rm = TRUE), lb_sb = sum(.data$lb_sb, na.rm = TRUE), lb_unknown = sum(.data$lb_unknown, na.rm = TRUE),
        sb_sb = sum(.data$sb_sb, na.rm = TRUE), sb_miscarriage = sum(.data$sb_miscarriage, na.rm = TRUE),
        sb_ab = sum(.data$sb_ab, na.rm = TRUE), sb_lb = sum(.data$sb_lb, na.rm = TRUE), sb_unknown = sum(.data$sb_unknown, na.rm = TRUE),
        ab_ab = sum(.data$ab_ab, na.rm = TRUE), ab_miscarriage = sum(.data$ab_miscarriage, na.rm = TRUE),
        ab_lb = sum(.data$ab_lb, na.rm = TRUE), ab_sb = sum(.data$ab_sb, na.rm = TRUE), ab_unknown = sum(.data$ab_unknown, na.rm = TRUE),
        .groups = "drop"
      )
  }
  # ---- 4) Pregnancy duration comparison ----
  log4r::info(logger, "Computing pregnancy duration distributions")
  duration_matched_summary <- NULL
  duration_alg <- alg %>%
    dplyr::mutate(
      duration_days = as.numeric(.data$alg_end - .data$alg_start),
      source = "algorithm"
    ) %>%
    dplyr::filter(!is.na(.data$duration_days))
  duration_pet <- pet %>%
    dplyr::mutate(
      duration_days = as.numeric(.data$pet_end - .data$pet_start),
      source = "pet"
    ) %>%
    dplyr::filter(!is.na(.data$duration_days))

  # All episodes: summary with min / Q25 / median / Q75 / max / mean / sd (protocol: histogram + summary stats including IQR)
  duration_summary <- dplyr::bind_rows(
    duration_alg %>%
      dplyr::summarise(
        source = "algorithm",
        scope = "all",
        n = dplyr::n(),
        mean = mean(.data$duration_days, na.rm = TRUE),
        median = stats::median(.data$duration_days, na.rm = TRUE),
        sd = stats::sd(.data$duration_days, na.rm = TRUE),
        min = safe_min(.data$duration_days),
        Q25 = stats::quantile(.data$duration_days, 0.25, na.rm = TRUE),
        Q75 = stats::quantile(.data$duration_days, 0.75, na.rm = TRUE),
        max = safe_max(.data$duration_days),
        .groups = "drop"
      ),
    duration_pet %>%
      dplyr::summarise(
        source = "pet",
        scope = "all",
        n = dplyr::n(),
        mean = mean(.data$duration_days, na.rm = TRUE),
        median = stats::median(.data$duration_days, na.rm = TRUE),
        sd = stats::sd(.data$duration_days, na.rm = TRUE),
        min = safe_min(.data$duration_days),
        Q25 = stats::quantile(.data$duration_days, 0.25, na.rm = TRUE),
        Q75 = stats::quantile(.data$duration_days, 0.75, na.rm = TRUE),
        max = safe_max(.data$duration_days),
        .groups = "drop"
      )
  )
  # Matched episodes only: duration PET and Algorithm per episode (protocol: duration of PET/Algorithm for matched)
  duration_matched_summary <- NULL
  if (nrow(best_match) > 0 && exists("date_diff")) {
    duration_matched_summary <- dplyr::bind_rows(
      date_diff %>%
        dplyr::summarise(
          source = "algorithm",
          scope = "matched",
          n = dplyr::n(),
          mean = mean(.data$duration_alg_days, na.rm = TRUE),
          median = stats::median(.data$duration_alg_days, na.rm = TRUE),
          sd = stats::sd(.data$duration_alg_days, na.rm = TRUE),
          min = safe_min(.data$duration_alg_days),
          Q25 = stats::quantile(.data$duration_alg_days, 0.25, na.rm = TRUE),
          Q75 = stats::quantile(.data$duration_alg_days, 0.75, na.rm = TRUE),
          max = safe_max(.data$duration_alg_days),
          .groups = "drop"
        ),
      date_diff %>%
        dplyr::summarise(
          source = "pet",
          scope = "matched",
          n = dplyr::n(),
          mean = mean(.data$duration_pet_days, na.rm = TRUE),
          median = stats::median(.data$duration_pet_days, na.rm = TRUE),
          sd = stats::sd(.data$duration_pet_days, na.rm = TRUE),
          min = safe_min(.data$duration_pet_days),
          Q25 = stats::quantile(.data$duration_pet_days, 0.25, na.rm = TRUE),
          Q75 = stats::quantile(.data$duration_pet_days, 0.75, na.rm = TRUE),
          max = safe_max(.data$duration_pet_days),
          .groups = "drop"
        )
    )
  }

  # Duration distribution (binned) for histogram
  all_durations <- dplyr::bind_rows(
    duration_alg %>% dplyr::select("duration_days", "source"),
    duration_pet %>% dplyr::select("duration_days", "source")
  )

  # ---- Build SummarisedResult and export single CSV ----
  log4r::info(logger, "Building SummarisedResult and exporting pet_comparison_summarised_result.csv")
  sr_rows <- list()
  # Episode counts
  for (i in seq_len(nrow(episode_counts))) {
    src <- episode_counts$source[i]
    sr_rows <- add_sr_rows(sr_rows, "episode_counts", src, list(
      n_episodes = list(type = "integer", value = episode_counts$n_episodes[i]),
      n_persons = list(type = "integer", value = episode_counts$n_persons[i])
    ))
  }
  # Person overlap
  for (i in seq_len(nrow(person_overlap))) {
    sr_rows <- add_sr_rows(sr_rows, "person_overlap", person_overlap$metric[i], list(
      n_persons = list(type = "integer", value = person_overlap$n_persons[i])
    ))
  }
  # Venn counts
  for (i in seq_len(nrow(venn_counts))) {
    sr_rows <- add_sr_rows(sr_rows, "venn_counts", venn_counts$category[i], list(
      n_episodes = list(type = "integer", value = venn_counts$n_episodes[i]),
      n_pet_matched = list(type = "integer", value = venn_counts$n_pet_matched[i]),
      n_alg_matched = list(type = "integer", value = venn_counts$n_alg_matched[i])
    ))
  }
  # Protocol summary (single row)
  sr_rows <- add_sr_rows(sr_rows, "protocol_summary", "overall", list(
    total_pet_episodes = list(type = "integer", value = protocol_summary$total_pet_episodes),
    total_algorithm_episodes = list(type = "integer", value = protocol_summary$total_algorithm_episodes),
    total_matched_episodes = list(type = "integer", value = protocol_summary$total_matched_episodes)
  ))
  # Time overlap summary (one row per label)
  for (i in seq_len(nrow(time_overlap_summary))) {
    lab <- time_overlap_summary$label[i]
    sr_rows <- add_sr_rows(sr_rows, "time_overlap_summary", lab, list(
      min = list(type = "numeric", value = time_overlap_summary$min[i]),
      q25 = list(type = "numeric", value = time_overlap_summary$Q25[i]),
      median = list(type = "numeric", value = time_overlap_summary$median[i]),
      q75 = list(type = "numeric", value = time_overlap_summary$Q75[i]),
      max = list(type = "numeric", value = time_overlap_summary$max[i]),
      sd = list(type = "numeric", value = time_overlap_summary$sd[i]),
      n_episodes = list(type = "integer", value = time_overlap_summary$n_episodes[i]),
      n_persons = list(type = "integer", value = time_overlap_summary$n_persons[i])
    ))
  }
  # Confusion 2x2
  for (i in seq_len(nrow(confusion_2x2))) {
    sr_rows <- add_sr_rows(sr_rows, "confusion_2x2", confusion_2x2$cell[i], list(
      count = list(type = "integer", value = confusion_2x2$count[i])
    ))
  }
  # PPV / sensitivity
  for (i in seq_len(nrow(ppv_sensitivity))) {
    sr_rows <- add_sr_rows(sr_rows, "ppv_sensitivity", ppv_sensitivity$metric[i], list(
      value = list(type = "percentage", value = ppv_sensitivity$value[i]),
      numerator = list(type = "integer", value = ppv_sensitivity$numerator[i]),
      denominator = list(type = "integer", value = ppv_sensitivity$denominator[i])
    ))
  }
  # Date difference summary (if present)
  if (nrow(best_match) > 0 && exists("date_summary")) {
    for (i in seq_len(nrow(date_summary))) {
      m <- date_summary$measure[i]
      sr_rows <- add_sr_rows(sr_rows, "date_difference_summary", m, list(
        mean = list(type = "numeric", value = date_summary$mean[i]),
        median = list(type = "numeric", value = date_summary$median[i]),
        sd = list(type = "numeric", value = date_summary$sd[i]),
        min = list(type = "numeric", value = date_summary$min[i]),
        q25 = list(type = "numeric", value = date_summary$Q25[i]),
        q75 = list(type = "numeric", value = date_summary$Q75[i]),
        max = list(type = "numeric", value = date_summary$max[i]),
        n_matched = list(type = "integer", value = date_summary$n_matched[i])
      ))
    }
  }
  # Outcome accuracy
  sr_rows <- add_sr_rows(sr_rows, "outcome_accuracy", "overall", list(
    n_correct = list(type = "integer", value = outcome_accuracy$n_correct),
    n_total = list(type = "integer", value = outcome_accuracy$n_total),
    accuracy = list(type = "percentage", value = outcome_accuracy$accuracy)
  ))
  # Outcome by year (one estimate per column)
  oby <- outcome_by_year[1, ]
  for (cn in names(oby)) {
    val <- oby[[cn]]
    if (is.numeric(val) || is.integer(val)) {
      lst <- list()
      lst[[cn]] <- list(type = "integer", value = val)
      sr_rows <- add_sr_rows(sr_rows, "outcome_by_year", "same_year_pairs", lst)
    }
  }
  # Duration summary (by source)
  for (i in seq_len(nrow(duration_summary))) {
    src <- duration_summary$source[i]
    sr_rows <- add_sr_rows(sr_rows, "duration_summary", src, list(
      n = list(type = "integer", value = duration_summary$n[i]),
      mean = list(type = "numeric", value = duration_summary$mean[i]),
      median = list(type = "numeric", value = duration_summary$median[i]),
      sd = list(type = "numeric", value = duration_summary$sd[i]),
      min = list(type = "numeric", value = duration_summary$min[i]),
      q25 = list(type = "numeric", value = duration_summary$Q25[i]),
      q75 = list(type = "numeric", value = duration_summary$Q75[i]),
      max = list(type = "numeric", value = duration_summary$max[i])
    ))
  }
  # Duration matched summary
  if (!is.null(duration_matched_summary) && nrow(duration_matched_summary) > 0) {
    for (i in seq_len(nrow(duration_matched_summary))) {
      src <- duration_matched_summary$source[i]
      sr_rows <- add_sr_rows(sr_rows, "duration_matched_summary", src, list(
        n = list(type = "integer", value = duration_matched_summary$n[i]),
        mean = list(type = "numeric", value = duration_matched_summary$mean[i]),
        median = list(type = "numeric", value = duration_matched_summary$median[i]),
        sd = list(type = "numeric", value = duration_matched_summary$sd[i]),
        min = list(type = "numeric", value = duration_matched_summary$min[i]),
        q25 = list(type = "numeric", value = duration_matched_summary$Q25[i]),
        q75 = list(type = "numeric", value = duration_matched_summary$Q75[i]),
        max = list(type = "numeric", value = duration_matched_summary$max[i])
      ))
    }
  }

  # Build results table (all character for CSV compatibility; result_id as integer for omopgenerics)
  results_tbl <- do.call(rbind, lapply(sr_rows, function(r) {
    as.data.frame(r, stringsAsFactors = FALSE)
  }))
  results_tbl$result_id <- result_id_sr
  results_tbl <- tibble::as_tibble(results_tbl)

  # Settings: one row per result_id with function parameters and package metadata
  pkg_version <- as.character(utils::packageVersion("PregnancyIdentifier"))
  settings_tbl <- tibble::tibble(
    result_id = result_id_sr,
    result_type = "compare_pregnancy_identifier_with_pet",
    package_name = "PregnancyIdentifier",
    package_version = pkg_version,
    pet_schema = petSchema,
    pet_table = petTable,
    min_overlap_days = as.character(minOverlapDays),
    remove_within_source_overlaps = as.character(removeWithinSourceOverlaps)
  )

  summarised_result <- omopgenerics::newSummarisedResult(
    x = results_tbl,
    settings = settings_tbl
  )
  omopgenerics::exportSummarisedResult(
    summarised_result,
    path = exportFolder,
    fileName = "pet_comparison_summarised_result.csv"
  )
  log4r::info(logger, sprintf("PET comparison written to %s", file.path(exportFolder, "pet_comparison_summarised_result.csv")))

  log4r::info(logger, "PET comparison complete")
}
