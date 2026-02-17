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
#' table and writes comparison summaries to \code{outputFolder}. Comparisons include:
#' pregnancy episode and person counts; pregnancy start/end date differences for
#' linked episodes; Venn counts (both, PET only, algorithm only); a 2x2 confusion
#' matrix (TP, FN, FP, TN) with sensitivity, specificity, PPV, and NPV; outcome
#' confusion matrix and accuracy; and pregnancy duration distributions.
#'
#' @param cdm A \code{cdm_reference} (from CDMConnector) with a database connection.
#'   The PET table is read via \code{petSchema} and \code{petTable}.
#' @param outputDir \code{character(1)}. Directory containing pipeline outputs, in
#'   particular \code{final_pregnancy_episodes.rds}.
#' @param outputFolder \code{character(1)}. Directory where comparison CSVs and
#'   optional plots will be written. Created if it does not exist.
#' @param petSchema \code{character(1)}. Schema name of the PET table (e.g.
#'   \code{"omop_cmbd"}).
#' @param petTable \code{character(1)}. Table name of the pregnancy episode table
#'   (e.g. \code{"pregnancy_episode"}). Must contain at least \code{person_id},
#'   \code{pregnancy_start_date}, \code{pregnancy_end_date}, and
#'   \code{pregnancy_outcome} (concept_id).
#' @param minOverlapDays \code{integer(1)}. Minimum overlap in days to consider an
#'   algorithm episode and a PET episode as the same pregnancy (default 1).
#' @param logger Optional \code{log4r::logger}. If \code{NULL}, a logger is
#'   created via \code{makeLogger(outputFolder)}.
#' @param outputLogToConsole \code{logical(1)}. Used only when \code{logger} is
#'   \code{NULL}. Whether to log to the console as well as to the log file.
#'
#' @return Invisibly returns a list with elements \code{episode_counts},
#'   \code{venn_counts}, \code{confusion_2x2}, \code{ppv_sensitivity},
#'   \code{date_differences}, \code{outcome_confusion}, \code{outcome_accuracy},
#'   \code{duration_summary}, and \code{duration_distribution}. \code{confusion_2x2}
#'   is the 2x2 table (TP, FN, FP, TN; TN is \code{NA} at episode level).
#'   \code{ppv_sensitivity} contains sensitivity, specificity, PPV, and NPV
#'   (specificity and NPV are \code{NA} when TN is not defined). All comparison
#'   artifacts are also written to \code{outputFolder} as CSVs (and optionally PNGs).
#' @export
comparePregnancyIdentifierWithPET <- function(cdm,
                                              outputDir,
                                              outputFolder,
                                              petSchema,
                                              petTable,
                                              minOverlapDays = 1L,
                                              logger = NULL,
                                              outputLogToConsole = TRUE) {
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertCharacter(outputDir, len = 1L, any.missing = FALSE)
  checkmate::assertCharacter(outputFolder, len = 1L, any.missing = FALSE)
  checkmate::assertCharacter(petSchema, len = 1L, any.missing = FALSE)
  checkmate::assertCharacter(petTable, len = 1L, any.missing = FALSE)
  checkmate::assertIntegerish(minOverlapDays, len = 1L, lower = 0)
  minOverlapDays <- as.integer(minOverlapDays)

  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  checkmate::assertDirectoryExists(outputFolder)

  if (is.null(logger)) {
    logger <- makeLogger(outputFolder, outputLogToConsole = outputLogToConsole)
  }
  log4r::info(logger, "Starting PET comparison")

  # Path to algorithm output
  algPath <- file.path(outputDir, "final_pregnancy_episodes.rds")
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
  petTbl <- dplyr::tbl(
    src = attr(cdm, "dbcon"),
    CDMConnector::inSchema(schema = petSchema, table = petTable)
  )
  petCols <- c("person_id", "pregnancy_start_date", "pregnancy_end_date", "pregnancy_outcome")
  # Optional: gestational_length_in_day
  petNames <- colnames(petTbl)
  if (!all(petCols %in% petNames)) {
    missPet <- setdiff(petCols, petNames)
    log4r::error(logger, sprintf("PET table missing columns: %s", paste(missPet, collapse = ", ")))
    stop(sprintf("PET table missing columns: %s", paste(missPet, collapse = ", ")))
  }
  log4r::info(logger, "Loading PET table from database")
  pet <- petTbl %>%
    dplyr::select(dplyr::all_of(petCols), dplyr::any_of("gestational_length_in_day")) %>%
    dplyr::collect()

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
  epPath <- file.path(outputFolder, "pet_comparison_episode_counts.csv")
  utils::write.csv(episode_counts, epPath, row.names = FALSE)
  log4r::info(logger, sprintf("Episode counts written to %s", epPath))

  # ---- 2) Link episodes (overlap) and date differences ----
  # Match: same person, overlapping period (overlap >= minOverlapDays)
  # Overlap in days: max(0, min(alg_end, pet_end) - max(alg_start, pet_start) + 1)
  log4r::info(logger, "Linking episodes by person and overlapping dates")
  alg <- alg %>% dplyr::mutate(.alg_idx = dplyr::row_number())
  pet <- pet %>% dplyr::mutate(.pet_idx = dplyr::row_number())

  # Cross-join by person and filter overlapping
  overlap_days <- function(s1, e1, s2, e2) {
    pmax(0, as.numeric(pmin(e1, e2) - pmax(s1, s2)) + 1)
  }
  joined <- alg %>%
    dplyr::inner_join(
      pet,
      by = "person_id",
      relationship = "many-to-many",
      suffix = c("_alg", "_pet")
    ) %>%
    dplyr::mutate(
      .overlap = overlap_days(.data$alg_start, .data$alg_end, .data$pet_start, .data$pet_end)
    ) %>%
    dplyr::filter(.data$.overlap >= minOverlapDays)

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
  vennPath <- file.path(outputFolder, "pet_comparison_venn_counts.csv")
  utils::write.csv(venn_counts, vennPath, row.names = FALSE)
  log4r::info(logger, sprintf("Venn counts written to %s", vennPath))

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
  conf2x2Path <- file.path(outputFolder, "pet_comparison_confusion_2x2.csv")
  utils::write.csv(confusion_2x2, conf2x2Path, row.names = FALSE)
  log4r::info(logger, sprintf("2x2 confusion matrix written to %s", conf2x2Path))

  # Sensitivity, specificity, PPV, NPV (PET as reference)
  # Sensitivity = TP / (TP + FN); Specificity = TN / (TN + FP); PPV = TP / (TP + FP); NPV = TN / (TN + FN)
  sensitivity <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  specificity <- if (!is.na(tn) && (tn + fp) > 0) tn / (tn + fp) else NA_real_
  ppv <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  npv <- if (!is.na(tn) && (tn + fn) > 0) tn / (tn + fn) else NA_real_

  ppv_sensitivity <- tibble::tibble(
    metric = c("sensitivity", "specificity", "ppv", "npv"),
    value = c(sensitivity, specificity, ppv, npv),
    denominator = c(tp + fn, tn + fp, tp + fp, tn + fn),
    numerator = c(tp, tn, tp, tn)
  )
  ppvPath <- file.path(outputFolder, "pet_comparison_ppv_sensitivity.csv")
  utils::write.csv(ppv_sensitivity, ppvPath, row.names = FALSE)
  log4r::info(logger, sprintf("Sensitivity/Specificity/PPV/NPV written to %s", ppvPath))

  # Date differences for best-matched pairs (one row per best-match pair)
  if (nrow(best_match) > 0) {
    date_diff <- best_match %>%
      dplyr::mutate(
        start_diff_days = as.numeric(.data$alg_start - .data$pet_start),
        end_diff_days = as.numeric(.data$alg_end - .data$pet_end),
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
      min = c(min(date_diff$start_diff_days, na.rm = TRUE), min(date_diff$end_diff_days, na.rm = TRUE)),
      max = c(max(date_diff$start_diff_days, na.rm = TRUE), max(date_diff$end_diff_days, na.rm = TRUE)),
      n_matched = nrow(date_diff)
    )
    dateSummaryPath <- file.path(outputFolder, "pet_comparison_date_difference_summary.csv")
    utils::write.csv(date_summary, dateSummaryPath, row.names = FALSE)
    utils::write.csv(date_diff, file.path(outputFolder, "pet_comparison_date_differences.csv"), row.names = FALSE)
    log4r::info(logger, sprintf("Date difference summary written to %s", dateSummaryPath))

    # Histogram data for start_diff_days (only when we have valid breaks)
    x <- date_diff$start_diff_days[!is.na(date_diff$start_diff_days)]
    if (length(x) > 0L) {
      br <- seq(
        floor(min(x)) - 0.5,
        ceiling(max(x)) + 0.5,
        by = 1
      )
      if (length(br) >= 2L) {
        hist_start <- as.data.frame(table(cut(date_diff$start_diff_days, breaks = br), useNA = "no"))
        colnames(hist_start) <- c("bin", "count")
        utils::write.csv(hist_start, file.path(outputFolder, "pet_comparison_start_diff_histogram.csv"), row.names = FALSE)
      }
    }
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
    confPath <- file.path(outputFolder, "pet_comparison_outcome_confusion_matrix.csv")
    utils::write.csv(confusion, confPath, row.names = FALSE)
    log4r::info(logger, sprintf("Outcome confusion matrix written to %s", confPath))

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
    accPath <- file.path(outputFolder, "pet_comparison_outcome_accuracy.csv")
    utils::write.csv(outcome_accuracy, accPath, row.names = FALSE)
    log4r::info(logger, sprintf("Outcome accuracy written to %s", accPath))
  } else {
    utils::write.csv(outcome_accuracy, file.path(outputFolder, "pet_comparison_outcome_accuracy.csv"), row.names = FALSE)
    utils::write.csv(confusion, file.path(outputFolder, "pet_comparison_outcome_confusion_matrix.csv"), row.names = FALSE)
  }

  # ---- 4) Pregnancy duration comparison ----
  log4r::info(logger, "Computing pregnancy duration distributions")
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

  duration_summary <- dplyr::bind_rows(
    duration_alg %>%
      dplyr::summarise(
        source = "algorithm",
        n = dplyr::n(),
        mean = mean(.data$duration_days, na.rm = TRUE),
        median = stats::median(.data$duration_days, na.rm = TRUE),
        sd = stats::sd(.data$duration_days, na.rm = TRUE),
        min = min(.data$duration_days, na.rm = TRUE),
        max = max(.data$duration_days, na.rm = TRUE),
        .groups = "drop"
      ),
    duration_pet %>%
      dplyr::summarise(
        source = "pet",
        n = dplyr::n(),
        mean = mean(.data$duration_days, na.rm = TRUE),
        median = stats::median(.data$duration_days, na.rm = TRUE),
        sd = stats::sd(.data$duration_days, na.rm = TRUE),
        min = min(.data$duration_days, na.rm = TRUE),
        max = max(.data$duration_days, na.rm = TRUE),
        .groups = "drop"
      )
  )
  durSumPath <- file.path(outputFolder, "pet_comparison_duration_summary.csv")
  utils::write.csv(duration_summary, durSumPath, row.names = FALSE)
  log4r::info(logger, sprintf("Duration summary written to %s", durSumPath))

  # Duration distribution (binned) for histogram
  all_durations <- dplyr::bind_rows(
    duration_alg %>% dplyr::select("duration_days", "source"),
    duration_pet %>% dplyr::select("duration_days", "source")
  )
  utils::write.csv(all_durations, file.path(outputFolder, "pet_comparison_duration_distribution.csv"), row.names = FALSE)

  # Optional: base R histograms to PNG
  tryCatch(
    {
      if (nrow(best_match) > 0 && exists("date_diff")) {
        grDevices::png(file.path(outputFolder, "pet_comparison_start_diff_histogram.png"), width = 600, height = 400, res = 100)
        graphics::hist(date_diff$start_diff_days, main = "Start date difference (algorithm - PET) [days]", xlab = "Days", breaks = 30)
        grDevices::dev.off()
      }
      grDevices::png(file.path(outputFolder, "pet_comparison_duration_histogram.png"), width = 600, height = 400, res = 100)
      xlim <- range(c(duration_alg$duration_days, duration_pet$duration_days), na.rm = TRUE)
      graphics::hist(duration_alg$duration_days, col = grDevices::rgb(0.2, 0.4, 0.8, 0.5), main = "Pregnancy duration (days)", xlab = "Days", breaks = 30, xlim = xlim)
      graphics::hist(duration_pet$duration_days, col = grDevices::rgb(0.8, 0.2, 0.2, 0.5), add = TRUE)
      graphics::legend("topright", legend = c("Algorithm", "PET"), fill = c(grDevices::rgb(0.2, 0.4, 0.8, 0.5), grDevices::rgb(0.8, 0.2, 0.2, 0.5)))
      grDevices::dev.off()
    },
    error = function(e) {
      log4r::warn(logger, sprintf("Could not write PNG histograms: %s", conditionMessage(e)))
    }
  )

  log4r::info(logger, "PET comparison complete")

  out <- list(
    episode_counts = episode_counts,
    venn_counts = venn_counts,
    confusion_2x2 = confusion_2x2,
    ppv_sensitivity = ppv_sensitivity,
    duration_summary = duration_summary,
    date_differences = NULL,
    outcome_confusion = NULL,
    outcome_accuracy = outcome_accuracy,
    duration_distribution = all_durations
  )
  if (nrow(best_match) > 0) {
    out$date_differences <- date_diff
    out$outcome_confusion <- confusion
  }
  invisible(out)
}
