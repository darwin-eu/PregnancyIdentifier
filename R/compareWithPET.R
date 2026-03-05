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
#' person overlap; Venn counts (both, PET only, algorithm only); a 2x2 confusion matrix
#' (TP, FN, FP, TN) with sensitivity and PPV (no gold-standard negatives, so specificity
#' and NPV are not defined); date differences for matched pairs (start, end, duration,
#' overall and stratified by outcome); outcome confusion matrix and accuracy;
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
#' @param minCellCount \code{integer(1)}. Minimum count threshold for suppression.
#'   Any record count or person count less than \code{minCellCount} is replaced
#'   with \code{NA} in the exported summarised result. Default 5.
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
                                              minCellCount = 5L,
                                              outputLogToConsole = TRUE) {
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertCharacter(outputFolder, len = 1L, any.missing = FALSE)
  checkmate::assertCharacter(exportFolder, len = 1L, any.missing = FALSE)
  checkmate::assertCharacter(petSchema, len = 1L, any.missing = FALSE)
  checkmate::assertCharacter(petTable, len = 1L, any.missing = FALSE)
  checkmate::assertIntegerish(minOverlapDays, len = 1L, lower = 0)
  checkmate::assertLogical(removeWithinSourceOverlaps, len = 1L, any.missing = FALSE)
  checkmate::assertIntegerish(minCellCount, len = 1L, lower = 0)
  checkmate::assertLogical(outputLogToConsole, len = 1L, any.missing = FALSE)
  minOverlapDays <- as.integer(minOverlapDays)
  minCellCount <- as.integer(minCellCount)

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
  # Suppress integer counts below minCellCount (return NA so they are exported as NA)
  suppress_count <- function(x) {
    if (is.na(x)) return(NA_integer_)
    x <- as.integer(x)
    if (x < minCellCount) NA_integer_ else x
  }
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
      dplyr::any_of(c(
        "esd_gestational_age_days_calculated",
        "hip_flag", "pps_flag",
        "esd_precision_days", "esd_precision_category"
      ))
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

  # ---- 2a) Group characterization (matched, algorithm-only, PET-only) ----
  log4r::info(logger, "Characterizing episode groups (matched, algorithm-only, PET-only)")

  # Split episodes into 3 groups
  matched_alg_idx <- one_to_one_matches$.alg_idx
  matched_pet_idx <- one_to_one_matches$.pet_idx

  matched_alg <- alg %>% dplyr::filter(.data$.alg_idx %in% matched_alg_idx)
  alg_only_df <- alg %>% dplyr::filter(!(.data$.alg_idx %in% matched_alg_idx))
  pet_only_df <- pet %>% dplyr::filter(!(.data$.pet_idx %in% matched_pet_idx))

  # Helper: compute gestational time summary for a group
  summarise_gestational_time <- function(gest_days) {
    gest_days <- gest_days[!is.na(gest_days)]
    if (length(gest_days) == 0) {
      return(list(
        n = 0L, mean = NA_real_, median = NA_real_, sd = NA_real_,
        min = NA_real_, q25 = NA_real_, q75 = NA_real_, max = NA_real_
      ))
    }
    list(
      n = length(gest_days),
      mean = mean(gest_days),
      median = stats::median(gest_days),
      sd = stats::sd(gest_days),
      min = min(gest_days),
      q25 = unname(stats::quantile(gest_days, 0.25)),
      q75 = unname(stats::quantile(gest_days, 0.75)),
      max = max(gest_days)
    )
  }

  # Helper: compute outcome distribution for a group
  summarise_outcomes <- function(df, outcome_col) {
    if (nrow(df) == 0 || !(outcome_col %in% colnames(df))) return(list())
    counts <- df %>%
      dplyr::count(.data[[outcome_col]], name = "n") %>%
      dplyr::mutate(pct = .data$n / sum(.data$n) * 100)
    stats::setNames(
      lapply(seq_len(nrow(counts)), function(i) {
        list(category = as.character(counts[[outcome_col]][i]), n = counts$n[i], pct = counts$pct[i])
      }),
      counts[[outcome_col]]
    )
  }

  # Helper: compute HIP/PPS source distribution for algorithm episodes
  summarise_hip_pps <- function(df) {
    if (nrow(df) == 0 || !all(c("hip_flag", "pps_flag") %in% colnames(df))) return(list())
    df <- df %>%
      dplyr::mutate(
        source_type = dplyr::case_when(
          .data$hip_flag == 1 & .data$pps_flag == 1 ~ "both",
          .data$hip_flag == 1 & (.data$pps_flag == 0 | is.na(.data$pps_flag)) ~ "hip_only",
          (.data$hip_flag == 0 | is.na(.data$hip_flag)) & .data$pps_flag == 1 ~ "pps_only",
          TRUE ~ "neither"
        )
      )
    counts <- df %>%
      dplyr::count(.data$source_type, name = "n") %>%
      dplyr::mutate(pct = .data$n / sum(.data$n) * 100)
    stats::setNames(
      lapply(seq_len(nrow(counts)), function(i) {
        list(source_type = counts$source_type[i], n = counts$n[i], pct = counts$pct[i])
      }),
      counts$source_type
    )
  }

  # PET outcome concept_id to label mapping
  pet_outcome_label <- function(x) {
    dplyr::case_when(
      x == 4092289L ~ "LB",
      x == 4067106L ~ "SA",
      x == 4081422L ~ "AB",
      x == 443213L ~ "SB",
      is.na(x) | x == 0L ~ "Unknown",
      TRUE ~ paste0("Other_", x)
    )
  }

  # Compute gestational time for each group
  gest_matched <- summarise_gestational_time(
    as.numeric(matched_alg$esd_gestational_age_days_calculated)
  )
  gest_alg_only <- summarise_gestational_time(
    as.numeric(alg_only_df$esd_gestational_age_days_calculated)
  )
  gest_pet_only <- summarise_gestational_time(
    as.numeric(pet_only_df$pet_end - pet_only_df$pet_start)
  )

  # Compute outcome distribution for each group
  outcome_matched <- summarise_outcomes(matched_alg, "final_outcome_category")
  outcome_alg_only <- summarise_outcomes(alg_only_df, "final_outcome_category")
  # For PET-only, map concept_id to label
  pet_only_with_label <- pet_only_df %>%
    dplyr::mutate(pet_outcome_label = pet_outcome_label(as.integer(.data$pregnancy_outcome)))
  outcome_pet_only <- summarise_outcomes(pet_only_with_label, "pet_outcome_label")

  # Compute HIP/PPS source distribution (algorithm groups only)
  source_matched <- summarise_hip_pps(matched_alg)
  source_alg_only <- summarise_hip_pps(alg_only_df)

  # ---- 2a-i) Person-level Venn characterization ----
  log4r::info(logger, "Computing person-level Venn characterization")

  all_alg_persons <- unique(alg$person_id)
  all_pet_persons <- unique(pet$person_id)

  persons_in_both <- intersect(all_alg_persons, all_pet_persons)
  persons_alg_only <- setdiff(all_alg_persons, all_pet_persons)
  persons_pet_only <- setdiff(all_pet_persons, all_alg_persons)

  # Helper: compute episodes-per-person distribution
  summarise_episodes_per_person <- function(df) {
    if (nrow(df) == 0) {
      return(list(
        n_persons = 0L, n_episodes = 0L,
        mean = NA_real_, median = NA_real_, sd = NA_real_,
        min = NA_real_, q25 = NA_real_, q75 = NA_real_, max = NA_real_
      ))
    }
    epp <- df %>%
      dplyr::count(.data$person_id, name = "n_ep") %>%
      dplyr::pull("n_ep")
    list(
      n_persons = length(epp),
      n_episodes = sum(epp),
      mean = mean(epp),
      median = stats::median(epp),
      sd = if (length(epp) > 1) stats::sd(epp) else NA_real_,
      min = min(epp),
      q25 = unname(stats::quantile(epp, 0.25)),
      q75 = unname(stats::quantile(epp, 0.75)),
      max = max(epp)
    )
  }

  # "Both" group: persons with episodes in both sources
  person_both_alg <- summarise_episodes_per_person(
    alg %>% dplyr::filter(.data$person_id %in% persons_in_both)
  )
  person_both_pet <- summarise_episodes_per_person(
    pet %>% dplyr::filter(.data$person_id %in% persons_in_both)
  )
  # "Algorithm only" group
  person_alg_only <- summarise_episodes_per_person(
    alg %>% dplyr::filter(.data$person_id %in% persons_alg_only)
  )
  # "PET only" group
  person_pet_only <- summarise_episodes_per_person(
    pet %>% dplyr::filter(.data$person_id %in% persons_pet_only)
  )

  log4r::info(logger, sprintf(
    "Person-level Venn: both=%d, algorithm_only=%d, pet_only=%d",
    length(persons_in_both), length(persons_alg_only), length(persons_pet_only)
  ))

  # ---- 2a-ii) HIP/PPS records in PET-only episodes ----
  # Look up whether PET-only episodes have any HIP/PPS concept records in the CDM
  has_hip_records <- "preg_hip_records" %in% names(cdm)
  has_pps_records <- "preg_pps_records" %in% names(cdm)

  # Per-episode record counts (initialise empty; populated below if CDM tables exist)
  pet_only_episode_records <- pet_only_df %>%
    dplyr::select(".pet_idx", "person_id", "pet_start", "pet_end") %>%
    dplyr::mutate(n_hip = 0L, n_pps = 0L)

  if (nrow(pet_only_df) > 0 && (has_hip_records || has_pps_records)) {
    log4r::info(logger, "Looking up HIP/PPS records for PET-only episodes")
    pet_only_person_ids <- unique(pet_only_df$person_id)

    # --- HIP records within PET-only episode windows ---
    if (has_hip_records) {
      hip_for_pet_only <- cdm$preg_hip_records %>%
        dplyr::filter(.data$person_id %in% !!pet_only_person_ids) %>%
        dplyr::select("person_id", "visit_date", "category", "concept_id") %>%
        dplyr::collect() %>%
        dplyr::mutate(visit_date = as.Date(.data$visit_date))

      hip_in_pet <- hip_for_pet_only %>%
        dplyr::inner_join(
          pet_only_df %>% dplyr::select("person_id", "pet_start", "pet_end", ".pet_idx"),
          by = "person_id",
          relationship = "many-to-many"
        ) %>%
        dplyr::filter(.data$visit_date >= .data$pet_start, .data$visit_date <= .data$pet_end)

      hip_per_episode <- hip_in_pet %>%
        dplyr::group_by(.data$.pet_idx) %>%
        dplyr::summarise(
          n_hip = dplyr::n(),
          hip_categories = paste(sort(unique(.data$category)), collapse = ","),
          .groups = "drop"
        )

      pet_only_episode_records <- pet_only_episode_records %>%
        dplyr::select(-"n_hip") %>%
        dplyr::left_join(hip_per_episode %>% dplyr::select(".pet_idx", "n_hip"), by = ".pet_idx") %>%
        dplyr::mutate(n_hip = dplyr::coalesce(.data$n_hip, 0L))
    }

    # --- PPS records within PET-only episode windows ---
    if (has_pps_records) {
      pps_for_pet_only <- cdm$preg_pps_records %>%
        dplyr::filter(.data$person_id %in% !!pet_only_person_ids) %>%
        dplyr::select("person_id", "pps_concept_start_date", "pps_concept_id", "pps_concept_name") %>%
        dplyr::collect() %>%
        dplyr::mutate(pps_concept_start_date = as.Date(.data$pps_concept_start_date))

      pps_in_pet <- pps_for_pet_only %>%
        dplyr::inner_join(
          pet_only_df %>% dplyr::select("person_id", "pet_start", "pet_end", ".pet_idx"),
          by = "person_id",
          relationship = "many-to-many"
        ) %>%
        dplyr::filter(.data$pps_concept_start_date >= .data$pet_start, .data$pps_concept_start_date <= .data$pet_end)

      pps_per_episode <- pps_in_pet %>%
        dplyr::group_by(.data$.pet_idx) %>%
        dplyr::summarise(n_pps = dplyr::n(), .groups = "drop")

      pet_only_episode_records <- pet_only_episode_records %>%
        dplyr::select(-"n_pps") %>%
        dplyr::left_join(pps_per_episode %>% dplyr::select(".pet_idx", "n_pps"), by = ".pet_idx") %>%
        dplyr::mutate(n_pps = dplyr::coalesce(.data$n_pps, 0L))
    }
  } else if (nrow(pet_only_df) > 0) {
    log4r::warn(logger, "preg_hip_records / preg_pps_records not found in CDM; skipping PET-only record lookup")
  }

  # Summarise HIP/PPS coverage for PET-only episodes
  n_pet_only_total <- nrow(pet_only_df)
  pet_only_has_hip <- sum(pet_only_episode_records$n_hip > 0)
  pet_only_has_pps <- sum(pet_only_episode_records$n_pps > 0)
  pet_only_has_any <- sum(pet_only_episode_records$n_hip > 0 | pet_only_episode_records$n_pps > 0)

  # HIP record count distribution (among all PET-only episodes, including zeros)
  pet_only_hip_dist <- summarise_gestational_time(pet_only_episode_records$n_hip)
  pet_only_pps_dist <- summarise_gestational_time(pet_only_episode_records$n_pps)

  # HIP category distribution: how many PET-only episodes have each category
  pet_only_hip_cat_counts <- if (has_hip_records && exists("hip_in_pet") && nrow(hip_in_pet) > 0) {
    hip_in_pet %>%
      dplyr::distinct(.data$.pet_idx, .data$category) %>%
      dplyr::count(.data$category, name = "n") %>%
      dplyr::mutate(pct = .data$n / n_pet_only_total * 100)
  } else {
    tibble::tibble(category = character(0), n = integer(0), pct = numeric(0))
  }

  # Cross-tabulation: PET outcome x most common HIP category per episode
  pet_only_outcome_vs_hip <- if (has_hip_records && exists("hip_in_pet") && nrow(hip_in_pet) > 0) {
    # Find dominant HIP category per episode (most records)
    dominant_hip <- hip_in_pet %>%
      dplyr::count(.data$.pet_idx, .data$category, name = "cnt") %>%
      dplyr::group_by(.data$.pet_idx) %>%
      dplyr::slice_max(.data$cnt, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(".pet_idx", dominant_hip_category = "category")

    pet_only_df %>%
      dplyr::mutate(pet_outcome_label = pet_outcome_label(as.integer(.data$pregnancy_outcome))) %>%
      dplyr::inner_join(dominant_hip, by = ".pet_idx") %>%
      dplyr::count(.data$pet_outcome_label, .data$dominant_hip_category, name = "n") %>%
      dplyr::mutate(pct = .data$n / sum(.data$n) * 100)
  } else {
    tibble::tibble(pet_outcome_label = character(0), dominant_hip_category = character(0), n = integer(0), pct = numeric(0))
  }

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
        duration_pet_days = as.numeric(.data$pet_end - .data$pet_start),
        duration_diff_days = .data$duration_pet_days - .data$duration_alg_days,
        alg_outcome = .data$final_outcome_category
      ) %>%
      dplyr::select(
        "person_id",
        "start_diff_days",
        "end_diff_days",
        "duration_alg_days",
        "duration_pet_days",
        "duration_diff_days",
        "alg_outcome"
      )

    # Helper: summarise a numeric column into the standard stats row
    .summarise_diff <- function(df, col, label, n = nrow(df)) {
      vals <- df[[col]]
      tibble::tibble(
        measure = label,
        mean = mean(vals, na.rm = TRUE),
        median = stats::median(vals, na.rm = TRUE),
        sd = stats::sd(vals, na.rm = TRUE),
        min = safe_min(vals),
        Q25 = stats::quantile(vals, 0.25, na.rm = TRUE),
        Q75 = stats::quantile(vals, 0.75, na.rm = TRUE),
        max = safe_max(vals),
        n_matched = n
      )
    }

    # Overall date difference summary (start, end, duration)
    date_summary <- dplyr::bind_rows(
      .summarise_diff(date_diff, "start_diff_days", "Start date difference (PET - Algorithm, days)"),
      .summarise_diff(date_diff, "end_diff_days", "End date difference (PET - Algorithm, days)"),
      .summarise_diff(date_diff, "duration_diff_days", "Duration difference (PET - Algorithm, days)")
    )

    # Outcome-stratified date difference summary
    outcomes_in_data <- sort(unique(date_diff$alg_outcome))
    date_summary_by_outcome <- dplyr::bind_rows(purrr::map(outcomes_in_data, function(oc) {
      sub <- date_diff %>% dplyr::filter(.data$alg_outcome == .env$oc)
      if (nrow(sub) == 0) return(NULL)
      dplyr::bind_rows(
        .summarise_diff(sub, "start_diff_days", paste0("Start date difference (PET - Algorithm, days) [", oc, "]")),
        .summarise_diff(sub, "end_diff_days", paste0("End date difference (PET - Algorithm, days) [", oc, "]")),
        .summarise_diff(sub, "duration_diff_days", paste0("Duration difference (PET - Algorithm, days) [", oc, "]"))
      )
    }))
  }

  # ---- 2d) Binned date-difference distributions (for alignment visualisation) ----
  # Bins: ≤-30, (-30,-14], (-14,-7], (-7,-1], 0, [1,7), [7,14), [14,30), ≥30
  .bin_diff <- function(x) {
    breaks <- c(-Inf, -30, -14, -7, -1, 0, 1, 7, 14, 30, Inf)
    labels <- c("\u2264 -30", "-29 to -15", "-14 to -8", "-7 to -1",
                "0", "1 to 7", "8 to 14", "15 to 29", "\u2265 30")
    # cut with right = TRUE: (-Inf,-30] maps to "≤ -30", (-30,-14] maps to "-29 to -15", ...
    # But we need 0 to be its own bin. Adjust: use right = FALSE for half-open [a, b) intervals
    # Actually, let's use a simpler approach with dplyr::case_when for clarity
    dplyr::case_when(
      x <= -30        ~ "\u2264 -30",
      x >= -29 & x <= -15 ~ "-29 to -15",
      x >= -14 & x <= -8  ~ "-14 to -8",
      x >= -7  & x <= -1  ~ "-7 to -1",
      x == 0              ~ "0",
      x >= 1   & x <= 7   ~ "1 to 7",
      x >= 8   & x <= 14  ~ "8 to 14",
      x >= 15  & x <= 29  ~ "15 to 29",
      x >= 30             ~ "\u2265 30",
      TRUE               ~ NA_character_
    )
  }

  .bin_order <- c("\u2264 -30", "-29 to -15", "-14 to -8", "-7 to -1",
                  "0", "1 to 7", "8 to 14", "15 to 29", "\u2265 30")

  date_diff_distribution <- NULL
  date_diff_distribution_by_outcome <- NULL

  if (nrow(best_match) > 0 && exists("date_diff")) {
    # Overall binned distribution
    date_diff_distribution <- dplyr::bind_rows(
      date_diff %>%
        dplyr::mutate(bin = .bin_diff(.data$start_diff_days)) %>%
        dplyr::count(.data$bin, name = "n") %>%
        dplyr::mutate(measure = "start_diff", bin = factor(.data$bin, levels = .bin_order)),
      date_diff %>%
        dplyr::mutate(bin = .bin_diff(.data$end_diff_days)) %>%
        dplyr::count(.data$bin, name = "n") %>%
        dplyr::mutate(measure = "end_diff", bin = factor(.data$bin, levels = .bin_order)),
      date_diff %>%
        dplyr::mutate(bin = .bin_diff(.data$duration_diff_days)) %>%
        dplyr::count(.data$bin, name = "n") %>%
        dplyr::mutate(measure = "duration_diff", bin = factor(.data$bin, levels = .bin_order))
    ) %>%
      # Ensure all bins present (fill zeros) so the plot is uniform
      tidyr::complete(.data$measure, bin = factor(.bin_order, levels = .bin_order),
                      fill = list(n = 0L))

    # Outcome-stratified binned distribution
    date_diff_distribution_by_outcome <- dplyr::bind_rows(
      purrr::map(outcomes_in_data, function(oc) {
        sub <- date_diff %>% dplyr::filter(.data$alg_outcome == .env$oc)
        if (nrow(sub) == 0) return(NULL)
        dplyr::bind_rows(
          sub %>%
            dplyr::mutate(bin = .bin_diff(.data$start_diff_days)) %>%
            dplyr::count(.data$bin, name = "n") %>%
            dplyr::mutate(measure = "start_diff", outcome = oc,
                          bin = factor(.data$bin, levels = .bin_order)),
          sub %>%
            dplyr::mutate(bin = .bin_diff(.data$end_diff_days)) %>%
            dplyr::count(.data$bin, name = "n") %>%
            dplyr::mutate(measure = "end_diff", outcome = oc,
                          bin = factor(.data$bin, levels = .bin_order)),
          sub %>%
            dplyr::mutate(bin = .bin_diff(.data$duration_diff_days)) %>%
            dplyr::count(.data$bin, name = "n") %>%
            dplyr::mutate(measure = "duration_diff", outcome = oc,
                          bin = factor(.data$bin, levels = .bin_order))
        ) %>%
          tidyr::complete(.data$measure, bin = factor(.bin_order, levels = .bin_order),
                          fill = list(n = 0L, outcome = oc))
      })
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
      n_episodes = list(type = "integer", value = suppress_count(episode_counts$n_episodes[i])),
      n_persons = list(type = "integer", value = suppress_count(episode_counts$n_persons[i]))
    ))
  }
  # Person overlap
  for (i in seq_len(nrow(person_overlap))) {
    sr_rows <- add_sr_rows(sr_rows, "person_overlap", person_overlap$metric[i], list(
      n_persons = list(type = "integer", value = suppress_count(person_overlap$n_persons[i]))
    ))
  }
  # Venn counts
  for (i in seq_len(nrow(venn_counts))) {
    sr_rows <- add_sr_rows(sr_rows, "venn_counts", venn_counts$category[i], list(
      n_episodes = list(type = "integer", value = suppress_count(venn_counts$n_episodes[i])),
      n_pet_matched = list(type = "integer", value = suppress_count(venn_counts$n_pet_matched[i])),
      n_alg_matched = list(type = "integer", value = suppress_count(venn_counts$n_alg_matched[i]))
    ))
  }
  # Protocol summary (single row)
  sr_rows <- add_sr_rows(sr_rows, "protocol_summary", "overall", list(
    total_pet_episodes = list(type = "integer", value = suppress_count(protocol_summary$total_pet_episodes)),
    total_algorithm_episodes = list(type = "integer", value = suppress_count(protocol_summary$total_algorithm_episodes)),
    total_matched_episodes = list(type = "integer", value = suppress_count(protocol_summary$total_matched_episodes))
  ))
  # Confusion 2x2
  for (i in seq_len(nrow(confusion_2x2))) {
    sr_rows <- add_sr_rows(sr_rows, "confusion_2x2", confusion_2x2$cell[i], list(
      count = list(type = "integer", value = suppress_count(confusion_2x2$count[i]))
    ))
  }
  # PPV / sensitivity (suppress percentage when numerator or denominator is suppressed)
  for (i in seq_len(nrow(ppv_sensitivity))) {
    num_s <- suppress_count(ppv_sensitivity$numerator[i])
    den_s <- suppress_count(ppv_sensitivity$denominator[i])
    val_s <- ppv_sensitivity$value[i]
    if (is.na(num_s) || is.na(den_s)) val_s <- NA_real_
    sr_rows <- add_sr_rows(sr_rows, "ppv_sensitivity", ppv_sensitivity$metric[i], list(
      value = list(type = "percentage", value = val_s),
      numerator = list(type = "integer", value = num_s),
      denominator = list(type = "integer", value = den_s)
    ))
  }
  # Date difference summary: overall (if present)
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
        n_matched = list(type = "integer", value = suppress_count(date_summary$n_matched[i]))
      ))
    }
  }
  # Date difference summary: stratified by algorithm outcome (if present)
  if (nrow(best_match) > 0 && exists("date_summary_by_outcome") && nrow(date_summary_by_outcome) > 0) {
    for (i in seq_len(nrow(date_summary_by_outcome))) {
      m <- date_summary_by_outcome$measure[i]
      sr_rows <- add_sr_rows(sr_rows, "date_difference_by_outcome", m, list(
        mean = list(type = "numeric", value = date_summary_by_outcome$mean[i]),
        median = list(type = "numeric", value = date_summary_by_outcome$median[i]),
        sd = list(type = "numeric", value = date_summary_by_outcome$sd[i]),
        min = list(type = "numeric", value = date_summary_by_outcome$min[i]),
        q25 = list(type = "numeric", value = date_summary_by_outcome$Q25[i]),
        q75 = list(type = "numeric", value = date_summary_by_outcome$Q75[i]),
        max = list(type = "numeric", value = date_summary_by_outcome$max[i]),
        n_matched = list(type = "integer", value = suppress_count(date_summary_by_outcome$n_matched[i]))
      ))
    }
  }
  # Date difference distribution: binned counts for alignment visualisation
  if (!is.null(date_diff_distribution) && nrow(date_diff_distribution) > 0) {
    for (i in seq_len(nrow(date_diff_distribution))) {
      bin_label <- paste0(date_diff_distribution$measure[i], "::", as.character(date_diff_distribution$bin[i]))
      sr_rows <- add_sr_rows(sr_rows, "date_difference_distribution", bin_label, list(
        n = list(type = "integer", value = suppress_count(date_diff_distribution$n[i]))
      ))
    }
  }
  # Date difference distribution by outcome
  if (!is.null(date_diff_distribution_by_outcome) && nrow(date_diff_distribution_by_outcome) > 0) {
    for (i in seq_len(nrow(date_diff_distribution_by_outcome))) {
      bin_label <- paste0(
        date_diff_distribution_by_outcome$measure[i], "::",
        date_diff_distribution_by_outcome$outcome[i], "::",
        as.character(date_diff_distribution_by_outcome$bin[i])
      )
      sr_rows <- add_sr_rows(sr_rows, "date_difference_distribution_by_outcome", bin_label, list(
        n = list(type = "integer", value = suppress_count(date_diff_distribution_by_outcome$n[i]))
      ))
    }
  }
  # Outcome accuracy (suppress percentage too when counts are suppressed)
  n_correct_s <- suppress_count(outcome_accuracy$n_correct)
  n_total_s <- suppress_count(outcome_accuracy$n_total)
  accuracy_s <- outcome_accuracy$accuracy
  if (is.na(n_correct_s) || is.na(n_total_s)) accuracy_s <- NA_real_
  sr_rows <- add_sr_rows(sr_rows, "outcome_accuracy", "overall", list(
    n_correct = list(type = "integer", value = n_correct_s),
    n_total = list(type = "integer", value = n_total_s),
    accuracy = list(type = "percentage", value = accuracy_s)
  ))
  # Outcome by year (one estimate per column)
  oby <- outcome_by_year[1, ]
  for (cn in names(oby)) {
    val <- oby[[cn]]
    if (is.numeric(val) || is.integer(val)) {
      lst <- list()
      lst[[cn]] <- list(type = "integer", value = suppress_count(val))
      sr_rows <- add_sr_rows(sr_rows, "outcome_by_year", "same_year_pairs", lst)
    }
  }
  # Duration summary (by source)
  for (i in seq_len(nrow(duration_summary))) {
    src <- duration_summary$source[i]
    sr_rows <- add_sr_rows(sr_rows, "duration_summary", src, list(
      n = list(type = "integer", value = suppress_count(duration_summary$n[i])),
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
        n = list(type = "integer", value = suppress_count(duration_matched_summary$n[i])),
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

  # ---- Group characterization SR rows ----
  # Gestational time distribution per group
  group_gest_list <- list(matched = gest_matched, algorithm_only = gest_alg_only, pet_only = gest_pet_only)
  for (grp in names(group_gest_list)) {
    g <- group_gest_list[[grp]]
    sr_rows <- add_sr_rows(sr_rows, "group_gestational_time", grp, list(
      n = list(type = "integer", value = suppress_count(g$n)),
      mean = list(type = "numeric", value = g$mean),
      median = list(type = "numeric", value = g$median),
      sd = list(type = "numeric", value = g$sd),
      min = list(type = "numeric", value = g$min),
      q25 = list(type = "numeric", value = g$q25),
      q75 = list(type = "numeric", value = g$q75),
      max = list(type = "numeric", value = g$max)
    ))
  }

  # Outcome distribution per group
  group_outcome_list <- list(matched = outcome_matched, algorithm_only = outcome_alg_only, pet_only = outcome_pet_only)
  for (grp in names(group_outcome_list)) {
    outcomes <- group_outcome_list[[grp]]
    for (cat_name in names(outcomes)) {
      o <- outcomes[[cat_name]]
      sr_rows <- add_sr_rows(sr_rows, "group_outcome", paste0(grp, ":", cat_name), list(
        n = list(type = "integer", value = suppress_count(o$n)),
        pct = list(type = "numeric", value = o$pct)
      ))
    }
  }

  # HIP/PPS source distribution per algorithm group (not applicable for PET-only)
  group_source_list <- list(matched = source_matched, algorithm_only = source_alg_only)
  for (grp in names(group_source_list)) {
    sources <- group_source_list[[grp]]
    for (src_name in names(sources)) {
      s <- sources[[src_name]]
      sr_rows <- add_sr_rows(sr_rows, "group_source", paste0(grp, ":", src_name), list(
        n = list(type = "integer", value = suppress_count(s$n)),
        pct = list(type = "numeric", value = s$pct)
      ))
    }
  }

  # ---- Person-level Venn SR rows ----
  # Person Venn counts
  sr_rows <- add_sr_rows(sr_rows, "person_venn_counts", "both", list(
    n_persons = list(type = "integer", value = suppress_count(person_both_alg$n_persons)),
    n_alg_episodes = list(type = "integer", value = suppress_count(person_both_alg$n_episodes)),
    n_pet_episodes = list(type = "integer", value = suppress_count(person_both_pet$n_episodes))
  ))
  sr_rows <- add_sr_rows(sr_rows, "person_venn_counts", "algorithm_only", list(
    n_persons = list(type = "integer", value = suppress_count(person_alg_only$n_persons)),
    n_episodes = list(type = "integer", value = suppress_count(person_alg_only$n_episodes))
  ))
  sr_rows <- add_sr_rows(sr_rows, "person_venn_counts", "pet_only", list(
    n_persons = list(type = "integer", value = suppress_count(person_pet_only$n_persons)),
    n_episodes = list(type = "integer", value = suppress_count(person_pet_only$n_episodes))
  ))
  # Episodes-per-person distribution
  person_epp_list <- list(
    "both:algorithm" = person_both_alg,
    "both:pet" = person_both_pet,
    "algorithm_only" = person_alg_only,
    "pet_only" = person_pet_only
  )
  for (grp in names(person_epp_list)) {
    g <- person_epp_list[[grp]]
    sr_rows <- add_sr_rows(sr_rows, "person_episodes_per_person", grp, list(
      mean = list(type = "numeric", value = g$mean),
      median = list(type = "numeric", value = g$median),
      sd = list(type = "numeric", value = g$sd),
      min = list(type = "numeric", value = g$min),
      q25 = list(type = "numeric", value = g$q25),
      q75 = list(type = "numeric", value = g$q75),
      max = list(type = "numeric", value = g$max)
    ))
  }

  # ---- PET-only HIP/PPS record characterization SR rows ----
  if (n_pet_only_total > 0) {
    # Coverage: how many PET-only episodes have HIP, PPS, or any records
    sr_rows <- add_sr_rows(sr_rows, "pet_only_hip_coverage", "overall", list(
      n_total = list(type = "integer", value = suppress_count(n_pet_only_total)),
      n_with_hip = list(type = "integer", value = suppress_count(pet_only_has_hip)),
      n_without_hip = list(type = "integer", value = suppress_count(n_pet_only_total - pet_only_has_hip)),
      pct_with_hip = list(type = "numeric", value = if (n_pet_only_total > 0) pet_only_has_hip / n_pet_only_total * 100 else NA_real_)
    ))
    sr_rows <- add_sr_rows(sr_rows, "pet_only_pps_coverage", "overall", list(
      n_with_pps = list(type = "integer", value = suppress_count(pet_only_has_pps)),
      n_without_pps = list(type = "integer", value = suppress_count(n_pet_only_total - pet_only_has_pps)),
      pct_with_pps = list(type = "numeric", value = if (n_pet_only_total > 0) pet_only_has_pps / n_pet_only_total * 100 else NA_real_)
    ))
    sr_rows <- add_sr_rows(sr_rows, "pet_only_any_record_coverage", "overall", list(
      n_with_any = list(type = "integer", value = suppress_count(pet_only_has_any)),
      n_without_any = list(type = "integer", value = suppress_count(n_pet_only_total - pet_only_has_any)),
      pct_with_any = list(type = "numeric", value = if (n_pet_only_total > 0) pet_only_has_any / n_pet_only_total * 100 else NA_real_)
    ))

    # HIP record count distribution per episode
    sr_rows <- add_sr_rows(sr_rows, "pet_only_hip_record_count", "overall", list(
      n = list(type = "integer", value = suppress_count(pet_only_hip_dist$n)),
      mean = list(type = "numeric", value = pet_only_hip_dist$mean),
      median = list(type = "numeric", value = pet_only_hip_dist$median),
      sd = list(type = "numeric", value = pet_only_hip_dist$sd),
      min = list(type = "numeric", value = pet_only_hip_dist$min),
      q25 = list(type = "numeric", value = pet_only_hip_dist$q25),
      q75 = list(type = "numeric", value = pet_only_hip_dist$q75),
      max = list(type = "numeric", value = pet_only_hip_dist$max)
    ))

    # PPS record count distribution per episode
    sr_rows <- add_sr_rows(sr_rows, "pet_only_pps_record_count", "overall", list(
      n = list(type = "integer", value = suppress_count(pet_only_pps_dist$n)),
      mean = list(type = "numeric", value = pet_only_pps_dist$mean),
      median = list(type = "numeric", value = pet_only_pps_dist$median),
      sd = list(type = "numeric", value = pet_only_pps_dist$sd),
      min = list(type = "numeric", value = pet_only_pps_dist$min),
      q25 = list(type = "numeric", value = pet_only_pps_dist$q25),
      q75 = list(type = "numeric", value = pet_only_pps_dist$q75),
      max = list(type = "numeric", value = pet_only_pps_dist$max)
    ))

    # HIP category distribution: number of PET-only episodes with each HIP category
    if (nrow(pet_only_hip_cat_counts) > 0) {
      for (i in seq_len(nrow(pet_only_hip_cat_counts))) {
        sr_rows <- add_sr_rows(sr_rows, "pet_only_hip_category", pet_only_hip_cat_counts$category[i], list(
          n = list(type = "integer", value = suppress_count(pet_only_hip_cat_counts$n[i])),
          pct = list(type = "numeric", value = pet_only_hip_cat_counts$pct[i])
        ))
      }
    }

    # Cross-tabulation: PET outcome x dominant HIP category
    if (nrow(pet_only_outcome_vs_hip) > 0) {
      for (i in seq_len(nrow(pet_only_outcome_vs_hip))) {
        level <- paste0(pet_only_outcome_vs_hip$pet_outcome_label[i], ":", pet_only_outcome_vs_hip$dominant_hip_category[i])
        sr_rows <- add_sr_rows(sr_rows, "pet_only_outcome_vs_hip", level, list(
          n = list(type = "integer", value = suppress_count(pet_only_outcome_vs_hip$n[i])),
          pct = list(type = "numeric", value = pet_only_outcome_vs_hip$pct[i])
        ))
      }
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
    remove_within_source_overlaps = as.character(removeWithinSourceOverlaps),
    min_cell_count = as.character(minCellCount)
  )

  summarised_result <- omopgenerics::newSummarisedResult(
    x = results_tbl,
    settings = settings_tbl
  )
  omopgenerics::exportSummarisedResult(
    summarised_result,
    minCellCount = 0,
    path = exportFolder,
    fileName = "pet_comparison_summarised_result.csv"
  )
  log4r::info(logger, sprintf("PET comparison written to %s", file.path(exportFolder, "pet_comparison_summarised_result.csv")))

  log4r::info(logger, "PET comparison complete")
}
