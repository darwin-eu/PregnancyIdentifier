# Copyright (c) 2024 Louisa Smith
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Run the HIP pregnancy identification algorithm
#'
#' This function runs the HIP outcome based pregnancy identification algorithm.
#' It assumes that the `initPregnancies` function has already been run and the
#' preg_hip_records and preg_hip_concepts tables are in the cdm. The algorithm
#' identifies start and end dates for pregnancies by first prioritizing pregnancy
#' outcomes and then using inference to set a pregnancy start date. Each pregnancy
#' is classified into one of several categories.
#' - "LB": Live birth
#' - "SB": Still birth
#' - "AB": abortion,
#' - "SA": miscarriage,
#' - "DELIV": unspecified delivery
#' - "ECT": ectopic pregnancy
#' - "PREG": unspecified/ongoing pregnancy
#'
#'
#' @param cdm (`cdm_reference`) A cdm reference object from CDMConnector.
#' @param outputDir (`character(1)`) Output directory to write output to.
#' @param startDate (`Date(1)`: `as.Date("1900-01-01"`) Start date of data to use. By default 1900-01-01
#' @param endDate (`Date(1)`: `Sys.Date()`) End date of data to use. By default today.
#' @param logger (`logger`) A log4r logger object created with `makeLogger()`.
#' @param justGestation (`logical(1)`: `TRUE`) Should episodes that only have gestational concepts be considered?
#'
#' @returns
#' The input cdm object with the `cdm$preg_hip_episodes` table added. This table contains episode-level
#' information about each identified pregnancy per individual. The columns are:
#' - `person_id`: Unique identifier for the person.
#' - `hip_episode`: Sequential episode number for the person (1 = first episode, etc.).
#' - `hip_pregnancy_start`: Estimated start date of the pregnancy episode, based on outcome and available gestational age information.
#' - `hip_pregnancy_end`: Date of the principal pregnancy outcome event (the pregnancy end date).
#' - `hip_outcome_category`: HIP-assigned pregnancy outcome type. One of "LB" (live birth), "SB" (stillbirth), "AB" (abortion), "SA" (miscarriage), "DELIV" (unspecified delivery), "ECT" (ectopic pregnancy), or "PREG" (ongoing/unspecified).
#' - `hip_first_gest_date`: First gestation date in the episode (if available). NA if not identified.
#' - `hip_gest_flag`: Indicates if gestational age concepts were found in the episode ("yes" or "no").
#' - `hip_episode_length`: Length of the pregnancy episode, in days (from `hip_pregnancy_start` to `hip_pregnancy_end`).
#'
#' A file named `hip_episodes.rds` is saved in the `outputDir` directory. This file
#' contains a dataframe with the same columns as listed above for the `cdm$preg_hip_episodes` table.
#'
#' @export
runHip <- function(cdm, outputDir = NULL, startDate = as.Date("1900-01-01"), endDate = Sys.Date(), justGestation = TRUE, logger) {

  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertCharacter(outputDir, len = 1, any.missing = FALSE)
  checkmate::assertDate(startDate, len = 1, any.missing = FALSE)
  checkmate::assertDate(endDate, len = 1, any.missing = FALSE)
  checkmate::assertLogical(justGestation, len = 1, any.missing = FALSE)
  checkmate::assertClass(logger, "logger", null.ok = FALSE)
  dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
  checkmate::assertDirectoryExists(outputDir)

  log4r::info(logger, "START Running HIP")

  if (!("preg_hip_records" %in% names(cdm))) {
    rlang::abort("preg_hip_records is not in the cdm! Run `initPregnancies` function first.")
  }

  if (getTblRowCount(cdm$preg_hip_records) == 0) {
    log4r::warn(logger, "No records after initializing pregnancy cohort")
    if (!is.null(outputDir)) {
      hipEmpty <- emptyHipEpisodes() %>%
        dplyr::select(
          "person_id", "hip_episode", "hip_pregnancy_start", "hip_pregnancy_end",
          "hip_outcome_category", "hip_first_gest_date", "hip_gest_flag", "hip_episode_length"
        )
      saveRDS(hipEmpty, file.path(outputDir, "hip_episodes.rds"))
    }
    return(cdm)
  }

  gestationalAgeConcepts <- utils::read.csv(
    system.file("concepts", "gestational_age_concepts.csv", package = "PregnancyIdentifier", mustWork = TRUE),
    colClasses = c(concept_id = "integer")
  )
  gestConceptIds <- as.integer(gestationalAgeConcepts$concept_id)

  log4r::info(logger, "Inserting matcho_outcome_limits table into cdm")
  matchoOutcomeLimits <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "Matcho_outcome_limits.xlsx", mustWork = TRUE))
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "matcho_outcome_limits", table = matchoOutcomeLimits)

  # Stage 1: outcome episodes.
  # Inputs: cdm$preg_hip_records, cdm$matcho_outcome_limits.
  # Outputs: finalVisitsList (list of 5 lazy tbls: finalAbSaVisits, finalDelivVisits, finalEctVisits, finalSbVisits, finalLbVisits); no new cdm tables.
  finalVisitsList <- buildFinalOutcomeVisits(cdm, logger)
  # Inputs: cdm, finalVisitsList, cdm$matcho_outcome_limits.
  # Outputs: cdm with new table cdm$outcome_episodes_df (person_id, outcome_category, outcome_date, outcome_id).
  cdm <- buildOutcomeEpisodes(cdm, logger, finalVisitsList)
  # Inputs: cdm$outcome_episodes_df, cdm$preg_matcho_term_durations.
  # Outputs: outcomeEpisodesWithStartsTbl (lazy tbl; not stored in cdm). Adds min_term, max_term, retry, min_start_date, max_start_date.
  outcomeEpisodesWithStartsTbl <- estimateOutcomeStarts(cdm)

  # Stage 2: gestation episodes.
  # Inputs: cdm$preg_hip_records.
  # Outputs: cdm with new table cdm$gest_episodes_df (person_id, gest_id, episode, max_gest_date, max_gest_week, min_gest_date, min_gest_week, max_gest_start_date, min_gest_start_date, end_gest_date, etc.).
  cdm <- buildGestationEpisodes(cdm, logger = logger, gestConceptIds = gestConceptIds)

  # Stage 3: merge, clean, resolve overlaps, finalize length.
  # Inputs: outcomeEpisodesWithStartsTbl, cdm$gest_episodes_df.
  # Outputs: cdm with new table cdm$merged_episodes_df (final_episode_id, final_visit_date, final_category, has_outcome, has_gestation, is_under_max, is_over_min, days_diff, etc.).
  cdm <- mergeOutcomeAndGestation(cdm, outcomeEpisodesWithStartsTbl, justGestation = justGestation, logger = logger)
  # Inputs: cdm$merged_episodes_df
  # Outputs: cdm with cdm$merged_episodes_df overwritten (reclassified PREG where term rules violated; adds removed_outcome, removed_category, final_visit_date updates).
  cdm <- cleanMergedEpisodes(cdm, logger = logger)
  # Inputs: cdm$merged_episodes_df
  # Outputs: cdm with new table cdm$final_episodes_df (one row per final_episode_id; adds final_start_date, overlap/retry logic).
  cdm <- resolveOverlaps(cdm, logger = logger)
  # Inputs: cdm$final_episodes_df, cdm$preg_hip_records (gestation visit-level derived inside).
  # Outputs: cdm with new table cdm$preg_hip_episodes (final; person_id, hip_first_gest_date, category, hip_pregnancy_end, hip_pregnancy_start, episode, gest_flag, episode_length).
  cdm <- attachGestationAndLength(cdm, gestConceptIds = gestConceptIds)

  # 5) Collect final table, order columns, and save RDS.
  hipDf <- cdm$preg_hip_episodes %>% dplyr::collect()
  if (nrow(hipDf) == 0) {
    hipDf <- emptyHipEpisodes()
  }
  hipDf <- hipDf %>%
    dplyr::select(
      "person_id",
      "hip_episode",
      "hip_pregnancy_start",
      "hip_pregnancy_end",
      "hip_outcome_category",
      "hip_first_gest_date",
      "hip_gest_flag",
      "hip_episode_length"
    )
  validateEpisodePeriods(
    hipDf,
    personIdCol = "person_id",
    startDateCol = "hip_pregnancy_start",
    endDateCol = "hip_pregnancy_end",
    logger = logger
  )
  saveRDS(hipDf, file.path(outputDir, "hip_episodes.rds"))

  # Attrition: preg_hip_episodes from preg_hip_records
  prior <- getAttritionPrior(outputDir, "preg_hip_records")
  if (!is.null(prior)) {
    postR <- nrow(hipDf)
    postP <- dplyr::n_distinct(hipDf$person_id)
    appendAttrition(
      outputDir,
      step = "preg_hip_episodes",
      table = "preg_hip_episodes",
      outcome = NA_character_,
      prior_records = prior$post_records,
      prior_persons = prior$post_persons,
      dropped_records = prior$post_records - postR,
      dropped_persons = prior$post_persons - postP,
      post_records = postR,
      post_persons = postP
    )
  }

  # 6) Drop intermediate stage tables (nested functions do not drop their inputs; cleanup only here).
  cdm <- omopgenerics::dropSourceTable(
    cdm,
    c(
      "outcome_episodes_df",
      "gest_episodes_df",
      "merged_episodes_df",
      "final_episodes_df",
      "matcho_outcome_limits"
    )
  )

  return(cdm)
}

# Return min_days from Matcho outcome limits for (first_preg_category, outcome_preg_category).
getMatchoMinDays <- function(cdm, firstPregCategory, outcomePregCategory) {
  cdm$matcho_outcome_limits %>%
    dplyr::filter(.data$first_preg_category == .env$firstPregCategory & .data$outcome_preg_category == .env$outcomePregCategory) %>%
    dplyr::pull(.data$min_days)
}

# buildFinalOutcomeVisits()
# Inputs: cdm$preg_hip_records (person_id, visit_date, category), cdm$matcho_outcome_limits.
# Outputs: list with finalAbSaVisits, finalDelivVisits, finalEctVisits, finalSbVisits, finalLbVisits (each: person_id, outcome_date, outcome_category).
# Column mutations: adds outcome_date (= visit_date), outcome_category (= category); internal days/prev_visit_date not persisted.
buildFinalOutcomeVisits <- function(cdm, logger) {
  categories <- list(
    c("AB", "SA"),
    "DELIV",
    "ECT",
    "SB",
    "LB"
  )
  tableNames <- c("finalAbSaVisits", "finalDelivVisits", "finalEctVisits", "finalSbVisits", "finalLbVisits")
  out <- list()
  for (i in seq_along(categories)) {
    log4r::info(logger, sprintf("Running Category: %s [%s/%s]", paste(categories[[i]], collapse = ", "), i, length(categories)))
    cat <- categories[[i]]
    firstCat <- cat[1]
    minDay <- getMatchoMinDays(cdm, firstCat, firstCat)
    tempTbl <- cdm$preg_hip_records %>%
      dplyr::filter(.data$category %in% cat) %>%
      dplyr::group_by(.data$person_id, .data$visit_date) %>%
      # slicing by minimum concept id just a choice to make the code work
      # could have also done something like filter(row_number() == 1) but doesn't work on databases
      dplyr::slice_min(order_by = .data$concept_id, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$person_id) %>%
      dbplyr::window_order(.data$visit_date) %>%
      dplyr::mutate(prev_visit_date = dplyr::lag(.data$visit_date)) %>%
      dplyr::mutate(days = !!CDMConnector::datediff(start = "prev_visit_date", end = "visit_date", interval = "day")) %>%
      dplyr::ungroup()
    firstRows <- tempTbl %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::slice_min(.data$visit_date) %>%
      dplyr::ungroup()
    otherRows <- tempTbl %>%
      dplyr::filter(.data$days >= minDay)
    allRows <- dplyr::union_all(firstRows, otherRows) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        outcome_date = .data$visit_date,
        outcome_category = .data$category
      ) %>%
      dplyr::select("person_id", "outcome_date", "outcome_category")
    out[[tableNames[i]]] <- allRows
    log4r::info(logger, sprintf("  * Preliminary total number of episodes: %s", getTblRowCount(allRows)))
  }
  out
}

# buildOutcomeEpisodes()
# Inputs: finalVisitsList (finalLbVisits, finalSbVisits, finalEctVisits, finalAbSaVisits, finalDelivVisits: person_id, outcome_date, outcome_category), matcho_outcome_limits.
# Outputs: cdm$outcomeEpisodes (materialized).
# Output columns: person_id, outcome_category, outcome_date, outcome_id.
# Column mutations: adds outcome_id; hierarchical addition logic preserved (LB -> +SB -> +ECT -> +AB/SA -> +DELIV with spacing rules).
buildOutcomeEpisodes <- function(cdm, logger, finalVisitsList) {
  finalLbVisits <- finalVisitsList$finalLbVisits
  finalSbVisits <- finalVisitsList$finalSbVisits
  finalEctVisits <- finalVisitsList$finalEctVisits
  finalAbSaVisits <- finalVisitsList$finalAbSaVisits
  finalDelivVisits <- finalVisitsList$finalDelivVisits
  # Pull Matcho limits used across steps
  beforeMinLbSb <- getMatchoMinDays(cdm, "LB", "SB")
  afterMinSbLb <- getMatchoMinDays(cdm, "SB", "LB")

  # SB: add stillbirth to livebirth (same logic as addStillbirth)
  sbCandidates <- finalSbVisits %>%
    dplyr::union_all(finalLbVisits) %>%
    dplyr::select(-dplyr::any_of(c("gest_value", "value_as_number"))) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$outcome_date) %>%
    dplyr::mutate(
      previous_category = dplyr::lag(.data$outcome_category),
      prev_visit = dplyr::lag(.data$outcome_date),
      next_category = dplyr::lead(.data$outcome_category),
      next_visit = dplyr::lead(.data$outcome_date)
    ) %>%
    dplyr::mutate(
      after_days = !!CDMConnector::datediff("prev_visit", "outcome_date", "day"),
      before_days = !!CDMConnector::datediff("outcome_date", "next_visit", "day")
    ) %>%
    dplyr::filter(.data$outcome_category == "SB") %>%
    dplyr::filter(
      (is.na(.data$before_days) & is.na(.data$after_days)) |
        (.data$previous_category != "LB" & is.na(.data$next_category)) |
        (.data$next_category != "LB" & is.na(.data$previous_category)) |
        (.data$previous_category != "LB" & .data$next_category != "LB") |
        (.data$previous_category == "LB" & .data$after_days >= beforeMinLbSb & is.na(.data$next_category)) |
        (.data$next_category == "LB" & .data$before_days >= afterMinSbLb & is.na(.data$previous_category)) |
        (.data$next_category == "LB" & .data$before_days >= afterMinSbLb & .data$previous_category == "LB" & .data$after_days >= beforeMinLbSb)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("person_id", "outcome_category", "outcome_date")
  afterSb <- finalLbVisits %>%
    dplyr::union_all(sbCandidates) %>%
    dplyr::distinct()

  # ECT: add ectopic (same logic as addEctopic)
  beforeMinLbEct <- getMatchoMinDays(cdm, "LB", "ECT")
  afterMinEctLb <- getMatchoMinDays(cdm, "ECT", "LB")
  afterMinEctSb <- getMatchoMinDays(cdm, "ECT", "SB")
  ectCandidates <- afterSb %>%
    dplyr::union_all(
      finalEctVisits %>% dplyr::select(-dplyr::any_of(c("gest_value", "value_as_number")))
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$outcome_date) %>%
    dplyr::mutate(
      previous_category = dplyr::lag(.data$outcome_category),
      next_category = dplyr::lead(.data$outcome_category),
      prev_visit = dplyr::lag(.data$outcome_date),
      next_visit = dplyr::lead(.data$outcome_date)
    ) %>%
    dplyr::mutate(
      after_days = !!CDMConnector::datediff("prev_visit", "outcome_date", "day"),
      before_days = !!CDMConnector::datediff("outcome_date", "next_visit", "day")
    ) %>%
    dplyr::filter(.data$outcome_category == "ECT") %>%
    dplyr::filter(
      (is.na(.data$before_days) & is.na(.data$after_days)) |
        (!.data$previous_category %in% c("LB", "SB") & is.na(.data$next_category)) |
        (!.data$next_category %in% c("LB", "SB") & is.na(.data$previous_category)) |
        (!.data$previous_category %in% c("LB", "SB") & !.data$next_category %in% c("LB", "SB")) |
        (.data$previous_category %in% c("LB", "SB") & .data$after_days >= beforeMinLbEct & is.na(.data$next_category)) |
        (.data$next_category == "LB" & .data$before_days >= afterMinEctLb & is.na(.data$previous_category)) |
        (.data$next_category == "SB" & .data$before_days >= afterMinEctSb & is.na(.data$previous_category)) |
        (.data$next_category == "LB" & .data$before_days >= afterMinEctLb & .data$previous_category %in% c("LB", "SB") & .data$after_days >= beforeMinLbEct) |
        (.data$next_category == "SB" & .data$before_days >= afterMinEctSb & .data$previous_category %in% c("LB", "SB") & .data$after_days >= beforeMinLbEct)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("person_id", "outcome_category", "outcome_date")
  afterEct <- afterSb %>%
    dplyr::union_all(ectCandidates) %>%
    dplyr::distinct()

  # AB/SA: add abortion (same logic as addAbortion)
  beforeMinLbAb <- getMatchoMinDays(cdm, "LB", "AB")
  beforeMinEctAb <- getMatchoMinDays(cdm, "ECT", "AB")
  afterMinAbLb <- getMatchoMinDays(cdm, "AB", "LB")
  afterMinAbSb <- getMatchoMinDays(cdm, "AB", "SB")
  afterMinAbEct <- getMatchoMinDays(cdm, "AB", "ECT")
  abSaTbl <- finalAbSaVisits %>%
    dplyr::mutate(temp_category = dplyr::if_else(.data$outcome_category == "SA", "AB", .data$outcome_category))
  abCandidates <- afterEct %>%
    dplyr::union_all(abSaTbl %>% dplyr::select(-dplyr::any_of(c("gest_value", "value_as_number")))) %>%
    dplyr::mutate(temp_category = dplyr::if_else(.data$outcome_category == "SA", "AB", .data$outcome_category)) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$outcome_date) %>%
    dplyr::mutate(
      previous_category = dplyr::lag(.data$temp_category),
      next_category = dplyr::lead(.data$temp_category),
      prev_visit = dplyr::lag(.data$outcome_date),
      next_visit = dplyr::lead(.data$outcome_date)
    ) %>%
    dplyr::mutate(
      after_days = !!CDMConnector::datediff("prev_visit", "outcome_date", "day"),
      before_days = !!CDMConnector::datediff("outcome_date", "next_visit", "day")
    ) %>%
    dplyr::filter(.data$temp_category == "AB") %>%
    dplyr::filter(
      (is.na(.data$before_days) & is.na(.data$after_days)) |
        (!.data$previous_category %in% c("LB", "SB", "ECT") & is.na(.data$next_category)) |
        (!.data$next_category %in% c("LB", "SB", "ECT") & is.na(.data$previous_category)) |
        (!.data$previous_category %in% c("LB", "SB", "ECT") & !.data$next_category %in% c("LB", "SB", "ECT")) |
        (.data$previous_category %in% c("LB", "SB") & .data$after_days >= beforeMinLbAb & is.na(.data$next_category)) |
        (.data$next_category == "LB" & .data$before_days >= afterMinAbLb & is.na(.data$previous_category)) |
        (.data$next_category == "SB" & .data$before_days >= afterMinAbSb & is.na(.data$previous_category)) |
        (.data$next_category == "LB" & .data$previous_category %in% c("LB", "SB") & .data$before_days >= afterMinAbLb & .data$after_days >= beforeMinLbAb) |
        (.data$next_category == "SB" & .data$previous_category %in% c("LB", "SB") & .data$before_days >= afterMinAbSb & .data$after_days >= beforeMinLbAb) |
        (.data$previous_category == "ECT" & .data$after_days >= beforeMinEctAb & is.na(.data$next_category)) |
        (.data$next_category == "ECT" & .data$before_days >= afterMinAbEct & is.na(.data$previous_category)) |
        (.data$next_category == "ECT" & .data$previous_category == "ECT" & .data$before_days >= afterMinAbEct & .data$after_days >= beforeMinEctAb) |
        (.data$next_category == "ECT" & .data$previous_category %in% c("LB", "SB") & .data$before_days >= afterMinAbEct & .data$after_days >= beforeMinLbAb) |
        (.data$next_category == "LB" & .data$previous_category == "ECT" & .data$before_days >= afterMinAbLb & .data$after_days >= beforeMinEctAb) |
        (.data$next_category == "SB" & .data$previous_category == "ECT" & .data$before_days >= afterMinAbSb & .data$after_days >= beforeMinEctAb)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("person_id", "outcome_category", "outcome_date")
  afterAb <- afterEct %>%
    dplyr::union_all(abCandidates) %>%
    dplyr::distinct()

  # DELIV: add delivery + LB/SB date adjustment (same logic as addDelivery)
  beforeMinLbDeliv <- getMatchoMinDays(cdm, "LB", "DELIV")
  beforeMinEctDeliv <- getMatchoMinDays(cdm, "ECT", "DELIV")
  afterMinDelivLb <- getMatchoMinDays(cdm, "DELIV", "LB")
  afterMinDelivSb <- getMatchoMinDays(cdm, "DELIV", "SB")
  afterMinDelivEct <- getMatchoMinDays(cdm, "DELIV", "ECT")
  delivTbl <- afterAb %>%
    dplyr::union_all(
      finalDelivVisits %>% dplyr::select(-dplyr::any_of(c("gest_value", "value_as_number")))
    ) %>%
    dplyr::mutate(temp_category = dplyr::if_else(.data$outcome_category == "SA", "AB", .data$outcome_category)) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$outcome_date) %>%
    dplyr::mutate(
      previous_category = dplyr::lag(.data$temp_category),
      prev_visit = dplyr::lag(.data$outcome_date),
      next_category = dplyr::lead(.data$temp_category),
      next_visit = dplyr::lead(.data$outcome_date)
    ) %>%
    dplyr::mutate(
      after_days = !!CDMConnector::datediff("prev_visit", "outcome_date", "day"),
      before_days = !!CDMConnector::datediff("outcome_date", "next_visit", "day")
    )
  # Materialize so later union/compute does not emit unqualified "prev_visit" in SQL (PostgreSQL)
  cdm$deliv_tbl_staging <- delivTbl %>%
    .compute(name = "deliv_tbl_staging", temporary = FALSE, overwrite = TRUE)
  delivTbl <- cdm$deliv_tbl_staging
  # Non-DELIV rows: move LB/SB outcome_date earlier when DELIV precedes and spacing < afterMinDelivSb
  nonDeliv <- delivTbl %>%
    dplyr::filter(.data$outcome_category != "DELIV") %>%
    dplyr::mutate(
      outcome_date = dplyr::if_else(
        !is.na(.data$previous_category) &
          .data$previous_category == "DELIV" &
          .data$outcome_category %in% c("LB", "SB") & .data$after_days < afterMinDelivSb,
        .data$prev_visit,
        .data$outcome_date
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("person_id", "outcome_category", "outcome_date")
  delivCandidates <- delivTbl %>%
    dplyr::filter(.data$outcome_category == "DELIV") %>%
    dplyr::filter(
      (is.na(.data$before_days) & is.na(.data$after_days)) |
        (!.data$previous_category %in% c("LB", "SB", "ECT", "AB") & is.na(.data$next_category)) |
        (!.data$next_category %in% c("LB", "SB", "ECT", "AB") & is.na(.data$previous_category)) |
        (!.data$previous_category %in% c("LB", "SB", "ECT", "AB") & !.data$next_category %in% c("LB", "SB", "ECT", "AB")) |
        (.data$previous_category %in% c("LB", "SB") & .data$after_days >= beforeMinLbDeliv & is.na(.data$next_category)) |
        (.data$next_category == "LB" & .data$before_days >= afterMinDelivLb & is.na(.data$previous_category)) |
        (.data$next_category == "SB" & .data$before_days >= afterMinDelivSb & is.na(.data$previous_category)) |
        (.data$next_category == "LB" & .data$previous_category %in% c("LB", "SB") & .data$before_days >= afterMinDelivLb & .data$after_days >= beforeMinLbDeliv) |
        (.data$next_category == "SB" & .data$previous_category %in% c("LB", "SB") & .data$before_days >= afterMinDelivSb & .data$after_days >= beforeMinLbDeliv) |
        (.data$previous_category %in% c("ECT", "AB") & .data$after_days >= beforeMinEctDeliv & is.na(.data$next_category)) |
        (.data$next_category %in% c("ECT", "AB") & .data$before_days >= afterMinDelivEct & is.na(.data$previous_category)) |
        (.data$next_category %in% c("ECT", "AB") & .data$previous_category %in% c("ECT", "AB") & .data$before_days >= afterMinDelivEct & .data$after_days >= beforeMinEctDeliv) |
        (.data$next_category %in% c("ECT", "AB") & .data$previous_category %in% c("LB", "SB") & .data$before_days >= afterMinDelivEct & .data$after_days >= beforeMinLbDeliv) |
        (.data$next_category == "LB" & .data$previous_category %in% c("ECT", "AB") & .data$before_days >= afterMinDelivLb & .data$after_days >= beforeMinEctDeliv) |
        (.data$next_category == "SB" & .data$previous_category %in% c("ECT", "AB") & .data$before_days >= afterMinDelivSb & .data$after_days >= beforeMinEctDeliv)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("person_id", "outcome_category", "outcome_date")
  allOutcomes <- nonDeliv %>%
    dplyr::union_all(delivCandidates) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      outcome_id = paste0(as.character(.data$person_id), "_O_", as.character(.data$outcome_date), "_", .data$outcome_category)
    )
  cdm$outcome_episodes_df <- allOutcomes %>%
    .compute(name = "outcome_episodes_df", temporary = FALSE, overwrite = TRUE)
  cdm <- omopgenerics::dropSourceTable(cdm, "deliv_tbl_staging")
  log4r::info(logger, "Stage 1: outcome episodes materialized")
  cdm
}

# Default term (days) when outcome category is missing from Matcho_term_durations (e.g. PREG).
# Used so outcome-only episodes still get a non-NA start date (outcome_date - default_max_term).
DEFAULT_MATCHO_MAX_TERM_DAYS <- 301L
DEFAULT_MATCHO_MIN_TERM_DAYS <- 140L

# estimateOutcomeStarts()
# Inputs: outcomeEpisodes (person_id, outcome_category, outcome_date, outcome_id), preg_matcho_term_durations.
# Outputs: cdm$outcome_episodes_with_starts_df (lazy chain; not materialized).
# Column mutations: adds min_term, max_term, retry, min_start_date, max_start_date; changes: none.
# When outcome_category is not in Matcho (e.g. PREG), min_term/max_term are NA from the join;
# we coalesce to defaults so min_start_date/max_start_date are never NA.
estimateOutcomeStarts <- function(cdm) {
  cdm$outcome_episodes_df %>%
    dplyr::left_join(cdm$preg_matcho_term_durations, by = c("outcome_category" = "category")) %>%
    dplyr::mutate(
      min_term = as.integer(dplyr::coalesce(.data$min_term, DEFAULT_MATCHO_MIN_TERM_DAYS)),
      max_term = as.integer(dplyr::coalesce(.data$max_term, DEFAULT_MATCHO_MAX_TERM_DAYS)),
      neg_min_term = -!!rlang::sym("min_term"),
      neg_max_term = -!!rlang::sym("max_term")
    ) %>%
    dplyr::mutate(
      min_start_date = as.Date(!!CDMConnector::dateadd(date = "outcome_date", number = "neg_min_term", interval = "day")),
      max_start_date = as.Date(!!CDMConnector::dateadd(date = "outcome_date", number = "neg_max_term", interval = "day"))
    ) %>%
    dplyr::select(-"neg_min_term", -"neg_max_term")
}

# buildGestationEpisodes()
# Inputs: preg_hip_records (person_id, visit_date, gest_value / value_as_number, concept_id).
# Outputs: cdm$gest_episodes_df (materialized). Columns: person_id, gestId, episode, max_gest_date, max_gest_week, min_gest_date, min_gest_week,
#   max_gest_start_date, min_gest_start_date, end_gest_date, min_gest_date_2, max_gest_day, min_gest_day, gest_start_date_diff, etc.
# Column mutations: filters/derives gestational records; defines episodes; per-episode min/max gest week and dates.
# Cast value_as_number to numeric so filters work on all backends (e.g. when stored as varchar). Use coalesce with gest_value for condition/obs.
buildGestationEpisodes <- function(cdm, logger, minDays = 70, bufferDays = 28, gestConceptIds) {
  # Effective gestation week: record value (measurement) or concept-level gest_value (condition/procedure/observation).
  pregWithGest <- cdm$preg_hip_records %>%
    dplyr::mutate(
      value_num = as.numeric(.data$value_as_number),
      gest_num = as.numeric(.data$gest_value),
      effective_gest = dplyr::coalesce(.data$value_num, .data$gest_num)
    ) %>%
    dplyr::filter(
      !is.na(.data$effective_gest),
      .data$effective_gest > 0,
      .data$effective_gest <= 44
    )
  # GA concepts (gestational_age_concepts.csv) or concepts with gest_value in HIP sheet.
  gestFromValue <- pregWithGest %>%
    dplyr::filter(
      .data$concept_id %in% .env$gestConceptIds | !is.na(.data$gest_value)
    ) %>%
    dplyr::mutate(gest_value = as.integer(.data$effective_gest))

  gestationVisitsTbl <- gestFromValue %>%
    dplyr::distinct(.data$person_id, .data$visit_date, .data$gest_value, .keep_all = TRUE)
  gestationEpisodesTbl <- gestationVisitsTbl %>%
    dplyr::filter(!is.na(.data$visit_date), .data$gest_value > 0, .data$gest_value <= 44) %>%
    dplyr::group_by(.data$person_id, .data$visit_date) %>%
    dplyr::mutate(gest_week = max(.data$gest_value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$gest_value == .data$gest_week) %>%
    dplyr::mutate(gest_day = .data$gest_week * 7) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$visit_date) %>%
    dplyr::mutate(
      prev_week = dplyr::lag(.data$gest_week, 1),
      prev_date = dplyr::lag(.data$visit_date, 1),
      week_diff = .data$gest_week - .data$prev_week,
      day_diff = .data$week_diff * 7 + bufferDays
    ) %>%
    dplyr::mutate(
      date_diff = !!CDMConnector::datediff("prev_date", "visit_date", "day"),
      new_diff = dplyr::if_else(.data$date_diff < minDays & .data$week_diff <= 0, 1, .data$week_diff),
      new_diff2 = dplyr::if_else(.data$date_diff >= .data$day_diff & .data$week_diff > 0, -1, .data$new_diff),
      index = dplyr::row_number(),
      episode = as.integer(cumsum(ifelse(.data$new_diff2 <= 0 | .data$index == 1, 1, 0)))
    ) %>%
    dplyr::ungroup()
  # Materialize so later join/compute does not reference gest_value from wrong scope (PostgreSQL)
  cdm$gest_episodes_staging <- gestationEpisodesTbl %>%
    .compute(name = "gest_episodes_staging", temporary = FALSE, overwrite = TRUE)
  gestationEpisodesTbl <- cdm$gest_episodes_staging
  newFirst <- gestationEpisodesTbl %>%
    dplyr::group_by(.data$person_id, .data$episode) %>%
    dplyr::slice_min(.data$visit_date, n = 1) %>%
    dplyr::summarise(first_gest_week = max(.data$gest_week, na.rm = TRUE), .groups = "drop")
  tempMin <- gestationEpisodesTbl %>%
    dplyr::group_by(.data$person_id, .data$episode) %>%
    dplyr::slice_min(.data$gest_week, n = 1) %>%
    dplyr::mutate(min_gest_week = .data$gest_week) %>%
    dplyr::ungroup()
  newMin <- tempMin %>%
    dplyr::group_by(.data$person_id, .data$episode, .data$min_gest_week) %>%
    dplyr::summarise(min_gest_date = min(.data$visit_date, na.rm = TRUE), .groups = "drop")
  secondMin <- tempMin %>%
    dplyr::group_by(.data$person_id, .data$episode, .data$gest_week) %>%
    dplyr::summarise(min_gest_date_2 = max(.data$visit_date, na.rm = TRUE), .groups = "drop")
  tempEnd <- gestationEpisodesTbl %>%
    dplyr::group_by(.data$person_id, .data$episode) %>%
    dplyr::slice_max(.data$visit_date, n = 1) %>%
    dplyr::mutate(end_gest_date = .data$visit_date)
  newEnd <- tempEnd %>%
    dplyr::group_by(.data$person_id, .data$episode, .data$end_gest_date) %>%
    dplyr::summarise(end_gest_week = max(.data$gest_week, na.rm = TRUE), .groups = "drop")
  tempMax <- gestationEpisodesTbl %>%
    dplyr::group_by(.data$person_id, .data$episode) %>%
    dplyr::slice_max(.data$gest_week, n = 1) %>%
    dplyr::mutate(max_gest_week = .data$gest_week) %>%
    dplyr::ungroup()
  newMax <- tempMax %>%
    dplyr::group_by(.data$person_id, .data$episode, .data$max_gest_week) %>%
    dplyr::summarise(max_gest_date = min(.data$visit_date, na.rm = TRUE), .groups = "drop")
  joined <- newFirst %>%
    dplyr::inner_join(newEnd, by = c("person_id", "episode")) %>%
    dplyr::inner_join(newMin, by = c("person_id", "episode")) %>%
    dplyr::inner_join(secondMin, by = c("person_id", "episode")) %>%
    dplyr::inner_join(newMax, by = c("person_id", "episode")) %>%
    dplyr::mutate(
      gest_id = paste0(as.character(.data$person_id), "_G_", as.character(.data$max_gest_date))
    ) %>%
    dplyr::mutate(
      max_gest_day = as.integer(.data$max_gest_week * 7),
      min_gest_day = as.integer(.data$min_gest_week * 7),
      neg_max_gest_day = -!!rlang::sym("max_gest_day"),
      neg_min_gest_day = -!!rlang::sym("min_gest_day")
    ) %>%
    dplyr::mutate(
      max_gest_start_date = !!CDMConnector::dateadd(date = "max_gest_date", number = "neg_max_gest_day", interval = "day"),
      min_gest_start_date = !!CDMConnector::dateadd(date = "min_gest_date", number = "neg_min_gest_day", interval = "day")
    ) %>%
    dplyr::mutate(
      max_gest_start_date_further = dplyr::if_else(
        .data$max_gest_start_date > .data$min_gest_start_date,
        .data$min_gest_start_date,
        .data$max_gest_start_date
      ),
      min_gest_start_date = dplyr::if_else(
        .data$max_gest_start_date > .data$min_gest_start_date,
        .data$max_gest_start_date,
        .data$min_gest_start_date
      ),
      max_gest_start_date = .data$max_gest_start_date_further
    ) %>%
    dplyr::mutate(
      gest_start_date_diff = !!CDMConnector::datediff("min_gest_start_date", "max_gest_start_date", "day")
    )
  cdm$gest_episodes_df <- joined %>%
    dplyr::select(-"max_gest_start_date_further", -"neg_max_gest_day", -"neg_min_gest_day") %>%
    .compute(name = "gest_episodes_df", temporary = FALSE, overwrite = TRUE)
  cdm <- omopgenerics::dropSourceTable(cdm, "gest_episodes_staging")

  log4r::info(logger, "Stage 2: gestation episodes materialized")
  cdm
}

# mergeOutcomeAndGestation()
# Inputs: outcomeEpisodesWithStarts (person_id, outcome_date, outcome_category, outcome_id, min_start_date, max_start_date, min_term, max_term, retry),
#   gestEpisodes (person_id, gestId, max_gest_date, max_gest_start_date, ...), justGestation.
# Outputs: cdm$merged_episodes_df (materialized). Columns: final_episode_id, final_visit_date, final_category, has_outcome, has_gestation, is_under_max, is_over_min, days_diff, etc.
# Column mutations: adds final_episode_id, final_visit_date, final_category; overlap logic; does not overwrite outcome_date/outcome_category.
mergeOutcomeAndGestation <- function(cdm, outcomeEpisodesWithStartsTbl, justGestation = TRUE, logger, bufferDays = 28) {
  outcomeWithVisitId <- outcomeEpisodesWithStartsTbl %>%
    dplyr::mutate(visit_id = paste0(as.character(.data$person_id), as.character(.data$outcome_date)))
  gestWithId <- cdm$gest_episodes_df
  # Join on person_id then filter for overlapping intervals [max_start_date, outcome_date] and [max_gest_start_date, max_gest_date].
  # Overlap <=> max_start_date <= max_gest_date AND max_gest_start_date <= outcome_date (avoids join_by(overlaps()) for SQL reliability).
  bothTbl <- outcomeWithVisitId %>%
    dplyr::inner_join(gestWithId, by = "person_id") %>%
    dplyr::filter(.data$max_start_date <= .data$max_gest_date, .data$max_gest_start_date <= .data$outcome_date) %>%
    dplyr::mutate(
      gest_at_outcome = !!CDMConnector::datediff("max_gest_start_date", "outcome_date", "day")
    ) %>%
    dplyr::mutate(
      is_under_max = ifelse(.data$gest_at_outcome <= .data$max_term, 1, 0),
      is_over_min = ifelse(.data$gest_at_outcome >= .data$min_term, 1, 0)
    ) %>%
    dplyr::mutate(
      days_diff = !!CDMConnector::datediff("max_gest_date", "outcome_date", "day"),
      days_diff = dplyr::if_else(.data$is_over_min == 1 | .data$is_under_max == 1 | .data$days_diff < -bufferDays, 10000, .data$days_diff)
    ) %>%
    dplyr::group_by(.data$visit_id) %>%
    dplyr::slice_min(order_by = abs(.data$days_diff), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$gest_id) %>%
    dplyr::slice_min(order_by = abs(.data$days_diff), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  # Workaround for dbplyr get_env() with filtering joins (tidyverse/dbplyr#1534, #1606, #1659):
  # materialize bothTbl then use filter + collected IDs instead of anti_join(lazy y).
  bothTbl <- .compute(bothTbl, name = "merge_both_tmp", temporary = TRUE)
  bothVisitIds <- bothTbl %>%
    dplyr::select("visit_id") %>%
    dplyr::distinct() %>%
    dplyr::pull("visit_id")
  bothGestIds <- bothTbl %>%
    dplyr::select("gest_id") %>%
    dplyr::distinct() %>%
    dplyr::pull("gest_id")
  justOutcomeTbl <- outcomeWithVisitId %>%
    dplyr::filter(!.data$visit_id %in% .env$bothVisitIds) %>%
    dplyr::mutate(gest_id = NA_character_)
  justGestationTbl <- gestWithId %>%
    dplyr::filter(!.data$gest_id %in% .env$bothGestIds) %>%
    dplyr::mutate(
      final_category = "PREG",
      final_visit_date = .data$max_gest_date,
      visit_id = NA_character_
    )
  # Ensure all tables have the same columns for union_all (gestation-only lacks outcome cols; outcome-only lacks gest cols).
  # When including gestation-only: drop PREG outcome+gest rows when the same person has a gestation-only
  # episode with max_gest_date > outcome_date, so we emit one episode (the later one). Keep non-PREG outcomes.
  if (justGestation) {
    gestOnlyMax <- justGestationTbl %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::summarise(gest_only_max = max(.data$max_gest_date, na.rm = TRUE), .groups = "drop")
    bothTbl <- bothTbl %>%
      dplyr::left_join(gestOnlyMax, by = "person_id") %>%
      dplyr::filter(
        .data$outcome_category != "PREG" |
          is.na(.data$gest_only_max) |
          .data$outcome_date >= .data$gest_only_max
      ) %>%
      dplyr::select(-"gest_only_max") %>%
      .compute(name = "merge_both_filtered_tmp", temporary = TRUE)
  }
  bothTblMutated <- bothTbl %>%
    dplyr::mutate(final_category = .data$outcome_category, final_visit_date = .data$outcome_date)
  justOutcomeTblFull <- justOutcomeTbl %>%
    dplyr::mutate(
      final_category = .data$outcome_category,
      final_visit_date = .data$outcome_date,
      episode = NA_integer_,
      first_gest_week = NA_integer_,
      end_gest_date = as.Date(NA),
      end_gest_week = NA_integer_,
      min_gest_week = NA_integer_,
      min_gest_date = as.Date(NA),
      min_gest_date_2 = as.Date(NA),
      max_gest_week = NA_integer_,
      max_gest_date = as.Date(NA),
      max_gest_day = NA_integer_,
      min_gest_day = NA_integer_,
      max_gest_start_date = as.Date(NA),
      min_gest_start_date = as.Date(NA),
      gest_start_date_diff = NA_integer_,
      gest_at_outcome = NA_integer_,
      is_under_max = NA_integer_,
      is_over_min = NA_integer_,
      days_diff = NA_integer_
    )
  justGestationTblFull <- justGestationTbl %>%
    dplyr::mutate(
      outcome_date = as.Date(NA),
      outcome_category = NA_character_,
      outcome_id = NA_character_,
      min_start_date = as.Date(NA),
      max_start_date = as.Date(NA),
      min_term = NA_integer_,
      max_term = NA_integer_,
      retry = NA_integer_,
      gest_at_outcome = NA_integer_,
      is_under_max = NA_integer_,
      is_over_min = NA_integer_,
      days_diff = NA_integer_
    )
  tblList <- if (justGestation) {
    list(bothTblMutated, justOutcomeTblFull, justGestationTblFull)
  } else {
    list(bothTblMutated, justOutcomeTblFull)
  }
  # Materialize union first to avoid very long SQL on some database platforms
  unionTbl <- purrr::reduce(tblList, dplyr::union_all) %>%
    .compute(name = "merged_episodes_tmp", temporary = TRUE)
  mergedTbl <- unionTbl %>%
    dplyr::mutate(
      final_episode_id = dplyr::coalesce(.data$visit_id, .data$gest_id),
      has_outcome = !is.na(.data$visit_id),
      has_gestation = !is.na(.data$gest_id)
    ) %>%
    dplyr::mutate(days_diff = !!CDMConnector::datediff("max_gest_date", "final_visit_date", "day"))
  # One row per (person_id, final_episode_id). Prefer the row where gest_id matches final_episode_id for gestation-only
  # episodes (so we keep the correct gestation row and avoid duplicates from union column misalignment).
  mergedTbl <- mergedTbl %>%
    dplyr::group_by(.data$person_id, .data$final_episode_id) %>%
    dplyr::filter(
      dplyr::n() == 1L |
        (.data$final_episode_id == .data$gest_id) |
        (is.na(.data$gest_id) & .data$final_episode_id == .data$visit_id)
    ) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::ungroup()
  cdm$merged_episodes_df <- mergedTbl %>%
    .compute(name = "merged_episodes_df", temporary = FALSE, overwrite = TRUE)
  log4r::info(logger, "Stage 3: merged episodes materialized")
  cdm
}

# cleanMergedEpisodes()
# Inputs: mergedEpisodes (final_category, final_visit_date, is_under_max, is_over_min, days_diff, gest_id, visit_id, ...).
# Outputs: cdm$cleanEpisodes (replaces mergedEpisodes in pipeline; materialized as mergedEpisodes then overwritten by clean result).
# Column mutations: reclassifies to PREG when term rules violated; adds removed_outcome, removed_category; sets final_visit_date to max_gest_date when reclassifying.
cleanMergedEpisodes <- function(cdm, logger, bufferDays = 28) {
  overMax <- cdm$merged_episodes_df %>%
    dplyr::filter(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$is_under_max == 0) %>%
    dplyr::mutate(
      removed_category = .data$final_category,
      final_category = "PREG",
      final_visit_date = .data$max_gest_date,
      removed_outcome = 1L
    )
  rest1 <- cdm$merged_episodes_df %>%
    dplyr::filter(!(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$is_under_max == 0)) %>%
    dplyr::mutate(removed_outcome = 0L)
  log4r::info(logger, sprintf("Total number of episodes over maximum term duration: %s", getTblRowCount(overMax)))
  combined1 <- rest1 %>% dplyr::union_all(overMax)
  underMin <- combined1 %>%
    dplyr::filter(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$is_over_min == 0 & .data$days_diff < -bufferDays) %>%
    dplyr::mutate(
      removed_category = .data$final_category,
      final_category = "PREG",
      final_visit_date = .data$max_gest_date,
      removed_outcome = 1L
    )
  rest2 <- combined1 %>%
    dplyr::filter(!(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$is_over_min == 0 & .data$days_diff < -bufferDays))
  log4r::info(logger, sprintf("Total number of episodes under minimum term duration: %s", getTblRowCount(underMin)))
  combined2 <- rest2 %>% dplyr::union_all(underMin)
  negDays <- combined2 %>%
    dplyr::filter(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$days_diff < -bufferDays) %>%
    dplyr::mutate(
      removed_category = .data$final_category,
      final_category = "PREG",
      final_visit_date = .data$max_gest_date,
      removed_outcome = 1L
    )
  rest3 <- combined2 %>%
    dplyr::filter(!(!is.na(.data$gest_id) & !is.na(.data$visit_id) & .data$days_diff < -bufferDays))
  log4r::info(logger, sprintf("Total number of episodes with negative days between outcome and max_gest_date: %s", getTblRowCount(negDays)))
  # Write to a staging name first so overwrite does not drop the table we are selecting from (Snowflake etc.)
  cleanTbl <- rest3 %>%
    dplyr::union_all(negDays) %>%
    dplyr::mutate(
      gest_at_outcome = !!CDMConnector::datediff("max_gest_start_date", "final_visit_date", "day"),
      min_gest_date_diff = !!CDMConnector::datediff("min_gest_date", "min_gest_date_2", "day"),
      date_diff_max_end = !!CDMConnector::datediff("end_gest_date", "max_gest_date", "day")
    ) %>%
    .compute(name = "merged_episodes_df_clean", temporary = FALSE, overwrite = TRUE)
  cdm$merged_episodes_df_clean <- cleanTbl
  cdm <- omopgenerics::dropSourceTable(cdm, "merged_episodes_df")
  cdm$merged_episodes_df <- cdm$merged_episodes_df_clean %>%
    .compute(name = "merged_episodes_df", temporary = FALSE, overwrite = TRUE)
  cdm <- omopgenerics::dropSourceTable(cdm, "merged_episodes_df_clean")
  cdm
}

# resolveOverlaps()
# Inputs: mergedEpisodes (after clean): final_episode_id, final_visit_date, final_category, prev_date, prev_category, prev_retry, gest_id, max_gest_start_date, max_start_date, max_gest_week, concept_id, min_term, max_term.
# Outputs: cdm$final_episodes_df (materialized). Columns: person_id, final_episode_id, final_category, final_visit_date, final_start_date (estimated_start_date), retry, etc.
# Column mutations: removes overlapping PREG gestation episodes; adds final_start_date (retry-period or gestation/outcome start); reclassifies is_over_min==0 to PREG with removed_outcome.
resolveOverlaps <- function(cdm, logger) {
  withPrev <- cdm$merged_episodes_df %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$final_visit_date) %>%
    dplyr::mutate(
      prev_date = dplyr::lag(.data$final_visit_date),
      prev_category = dplyr::lag(.data$final_category),
      prev_retry = dplyr::lag(.data$retry),
      prev_gest_id = dplyr::lag(.data$gest_id)
    ) %>%
    dplyr::mutate(
      prev_date_diff = dplyr::case_when(
        !is.na(.data$max_gest_start_date) ~ !!CDMConnector::datediff("prev_date", "max_gest_start_date", "day"),
        TRUE ~ !!CDMConnector::datediff("prev_date", "max_start_date", "day")
      ),
      has_overlap = dplyr::if_else(.data$prev_date_diff < 0, 1L, 0L)
    ) %>%
    dplyr::ungroup()
  overlapRows <- withPrev %>%
    dplyr::filter(.data$has_overlap == 1 & .data$prev_category == "PREG")
  # Remove only (person_id, gest_id) pairs that appear as (person_id, prev_gest_id) in overlapRows,
  # so overlapping PREG is removed within the same person only (avoids cross-person removal if lag is wrong).
  pairsToRemoveTbl <- overlapRows %>%
    dplyr::filter(!is.na(.data$prev_gest_id)) %>%
    dplyr::distinct(.data$person_id, .data$prev_gest_id) %>%
    dplyr::rename(gest_id = "prev_gest_id") %>%
    .compute(name = "resolve_pairs_to_remove_tmp", temporary = TRUE)
  afterRemove <- withPrev %>%
    dplyr::anti_join(pairsToRemoveTbl, by = c("person_id", "gest_id")) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$final_visit_date) %>%
    dplyr::mutate(
      prev_date = dplyr::lag(.data$final_visit_date),
      prev_category = dplyr::lag(.data$final_category),
      prev_retry = dplyr::lag(.data$retry),
      prev_gest_id = dplyr::lag(.data$gest_id)
    ) %>%
    dplyr::mutate(
      prev_date_diff_gest_tmp = !!CDMConnector::datediff("prev_date", "max_gest_start_date", "day"),
      prev_date_diff_start_tmp = !!CDMConnector::datediff("prev_date", "max_start_date", "day")
    ) %>%
    dplyr::mutate(
      prev_date_diff = dplyr::case_when(
        !is.na(.data$max_gest_start_date) ~ .data$prev_date_diff_gest_tmp,
        TRUE ~ .data$prev_date_diff_start_tmp
      ),
      has_overlap = dplyr::if_else(.data$prev_date_diff < 0, 1L, 0L),
      prev_retry = as.integer(.data$prev_retry)
    ) %>%
    dplyr::select(-"prev_date_diff_gest_tmp", -"prev_date_diff_start_tmp") %>%
    dplyr::mutate(
      final_start_date = dplyr::case_when(
        .data$has_overlap == 1 & !is.na(.data$prev_retry) ~ !!CDMConnector::dateadd(date = "prev_date", number = "prev_retry", interval = "day"),
        is.na(.data$max_gest_start_date) ~ .data$max_start_date,
        TRUE ~ .data$max_gest_start_date
      )
    ) %>%
    dplyr::mutate(
      gest_at_outcome = !!CDMConnector::datediff("final_start_date", "final_visit_date", "day"),
      is_under_max = ifelse(.data$gest_at_outcome <= .data$max_term, 1, 0),
      is_over_min = ifelse(.data$gest_at_outcome >= .data$min_term, 1, 0)
    ) %>%
    dplyr::ungroup()
  # Reclassify to PREG when under minimum term (outcome+gestation with is_over_min == 0).
  # restFinal = everyone else. Use explicit complement so rows with is_over_min == NA (e.g. gestation-only)
  # are kept in restFinal; !(A & B) evaluates to NA when B is NA and would drop those rows otherwise.
  reclassPreg <- afterRemove %>%
    dplyr::filter(!is.na(.data$max_gest_week) & .data$is_over_min == 0) %>%
    dplyr::mutate(
      removed_category = .data$final_category,
      final_category = "PREG",
      final_visit_date = .data$max_gest_date,
      removed_outcome = 1L
    )
  restFinal <- afterRemove %>%
    dplyr::filter(
      is.na(.data$max_gest_week) |
      .data$is_over_min != 0L |
      is.na(.data$is_over_min)
    ) %>%
    dplyr::mutate(
      removed_category = NA_character_,
      removed_outcome = 0L
    )
  cdm$final_episodes_df <- restFinal %>%
    dplyr::union_all(reclassPreg) %>%
    .compute(name = "final_episodes_df", temporary = FALSE, overwrite = TRUE)
  nRemoved <- cdm$final_episodes_df %>%
    dplyr::filter(.data$removed_outcome == 1) %>%
    dplyr::tally() %>%
    dplyr::pull(.data$n)
  log4r::info(logger, sprintf("Total number of episodes with removed outcome: %s", nRemoved))
  cdm
}

# attachGestationAndLength()
# Inputs: finalEpisodes (person_id, final_episode_id, final_category, final_visit_date, final_start_date); preg_hip_records for gestation visit-level.
# Outputs: cdm$preg_hip_episodes (materialized). Columns: person_id, hip_first_gest_date, category, hip_pregnancy_end, hip_pregnancy_start, episode, gest_flag, episode_length.
# Column mutations: maps final_category->category, final_visit_date->hip_pregnancy_end, final_start_date->hip_pregnancy_start, gest_date->hip_first_gest_date; adds episode_order->episode once; gest_flag, episode_length.
attachGestationAndLength <- function(cdm, gestConceptIds) {
  finalWithOrder <- cdm$final_episodes_df %>%
    dplyr::distinct(.data$person_id, .data$final_category, .data$final_visit_date, .data$final_start_date) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$final_visit_date) %>%
    dplyr::mutate(episode_order = dplyr::row_number()) %>%
    dplyr::ungroup()
  # Same effective gestation as buildGestationEpisodes (numeric cast + coalesce with gest_value).
  pregWithGest <- cdm$preg_hip_records %>%
    dplyr::mutate(
      value_num = as.numeric(.data$value_as_number),
      gest_num = as.numeric(.data$gest_value),
      effective_gest = dplyr::coalesce(.data$value_num, .data$gest_num)
    ) %>%
    dplyr::filter(
      !is.na(.data$effective_gest),
      .data$effective_gest > 0,
      .data$effective_gest <= 44,
      .data$concept_id %in% .env$gestConceptIds | !is.na(.data$gest_value)
    ) %>%
    dplyr::mutate(gest_value = as.integer(.data$effective_gest))
  gestationVisitsTbl <- pregWithGest %>%
    dplyr::select("person_id", "gest_value", "visit_date") %>%
    dplyr::rename(gest_date = "visit_date")
  # Start from finalWithOrder and left_join gestation so we never drop input rows.
  # Join on person_id only, then filter to episode window (keep NA = no matching visit).
  # Per episode: keep the earliest gestation visit (min gest_date); if ties, highest gest_value.
  # Use group_by + filter + summarize (no slice_min/max) so it works on database backends.
  merged <- finalWithOrder %>%
    dplyr::left_join(gestationVisitsTbl, by = "person_id") %>%
    dplyr::filter(
      is.na(.data$gest_date) |
      (.data$gest_date >= .data$final_start_date & .data$gest_date <= .data$final_visit_date)
    ) %>%
    dplyr::group_by(.data$person_id, .data$episode_order) %>%
    dplyr::mutate(min_gest_date = min(.data$gest_date, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data$gest_date == .data$min_gest_date |
      (is.na(.data$gest_date) & is.na(.data$min_gest_date))
    ) %>%
    dplyr::group_by(.data$person_id, .data$episode_order) %>%
    dplyr::summarise(
      gest_date = min(.data$gest_date, na.rm = TRUE),
      gest_value = max(.data$gest_value, na.rm = TRUE),
      # Constant per group; min() is standard SQL and works on all backends (first() does not)
      final_category = min(.data$final_category, na.rm = TRUE),
      final_visit_date = min(.data$final_visit_date, na.rm = TRUE),
      final_start_date = min(.data$final_start_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(gest_flag = dplyr::if_else(is.na(.data$gest_date), NA_character_, "yes"))
  withLength <- merged %>%
    dplyr::mutate(
      episode_length = dplyr::if_else(
        !is.na(.data$gest_date),
        !!CDMConnector::datediff("gest_date", "final_visit_date", "day"),
        1L
      )
    ) %>%
    dplyr::mutate(episode_length = dplyr::if_else(.data$episode_length == 0, 1L, .data$episode_length))
  cdm$preg_hip_episodes <- withLength %>%
    dplyr::mutate(
      hip_outcome_category = .data$final_category,
      hip_pregnancy_end = .data$final_visit_date,
      hip_pregnancy_start = .data$final_start_date,
      hip_first_gest_date = .data$gest_date,
      hip_episode = .data$episode_order,
      hip_gest_flag = .data$gest_flag,
      hip_episode_length = .data$episode_length
    ) %>%
    dplyr::select("person_id", "hip_first_gest_date", "hip_outcome_category", "hip_pregnancy_end", "hip_pregnancy_start", "hip_episode", "hip_gest_flag", "hip_episode_length") %>%
    dplyr::distinct() %>%
    .compute(name = "preg_hip_episodes", temporary = FALSE, overwrite = TRUE)
  cdm
}
