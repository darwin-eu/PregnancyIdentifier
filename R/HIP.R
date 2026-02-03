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
#' - `episode`: Sequential episode number for the person (1 = first episode, etc.).
#' - `estimated_start_date`: Estimated start date of the pregnancy episode, based on outcome and available gestational age information.
#' - `visit_date`: Date of the principal pregnancy outcome event (the pregnancy end date).
#' - `category`: HIP-assigned pregnancy outcome type. One of "LB" (live birth), "SB" (stillbirth), "AB" (abortion), "SA" (miscarriage), "DELIV" (unspecified delivery), "ECT" (ectopic pregnancy), or "PREG" (ongoing/unspecified).
#' - `gest_date`: Gestational age in days at the time of pregnancy outcome (if available). NA if not identified.
#' - `gest_flag`: Indicates if gestational age concepts were found in the episode ("yes" or "no").
#' - `episode_length`: Length of the pregnancy episode, in days (from `estimated_start_date` to `visit_date`).
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
          "person_id", "episode", "estimated_start_date", "visit_date",
          "category", "gest_date", "gest_flag", "episode_length"
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
  # Outputs: cdm with new table cdm$preg_hip_episodes (final; person_id, gest_date, category, visit_date, estimated_start_date, episode, gest_flag, episode_length).
  cdm <- attachGestationAndLength(cdm, gestConceptIds = gestConceptIds)

  # 5) Collect final table, order columns, and save RDS.
  hipDf <- cdm$preg_hip_episodes %>% dplyr::collect()
  if (nrow(hipDf) == 0) {
    hipDf <- emptyHipEpisodes()
  }
  hipDf <- hipDf %>%
    dplyr::select(
      "person_id",
      "episode",
      "estimated_start_date",
      "visit_date",
      "category",
      "gest_date",
      "gest_flag",
      "episode_length"
    )
  saveRDS(hipDf, file.path(outputDir, "hip_episodes.rds"))

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
      previousCategory = dplyr::lag(.data$outcome_category),
      prevVisit = dplyr::lag(.data$outcome_date),
      nextCategory = dplyr::lead(.data$outcome_category),
      nextVisit = dplyr::lead(.data$outcome_date)
    ) %>%
    dplyr::mutate(
      afterDays = !!CDMConnector::datediff("prevVisit", "outcome_date", "day"),
      beforeDays = !!CDMConnector::datediff("outcome_date", "nextVisit", "day")
    ) %>%
    dplyr::filter(.data$outcome_category == "SB") %>%
    dplyr::filter(
      (is.na(.data$beforeDays) & is.na(.data$afterDays)) |
        (.data$previousCategory != "LB" & is.na(.data$nextCategory)) |
        (.data$nextCategory != "LB" & is.na(.data$previousCategory)) |
        (.data$previousCategory != "LB" & .data$nextCategory != "LB") |
        (.data$previousCategory == "LB" & .data$afterDays >= beforeMinLbSb & is.na(.data$nextCategory)) |
        (.data$nextCategory == "LB" & .data$beforeDays >= afterMinSbLb & is.na(.data$previousCategory)) |
        (.data$nextCategory == "LB" & .data$beforeDays >= afterMinSbLb & .data$previousCategory == "LB" & .data$afterDays >= beforeMinLbSb)
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
      previousCategory = dplyr::lag(.data$outcome_category),
      nextCategory = dplyr::lead(.data$outcome_category),
      prevVisit = dplyr::lag(.data$outcome_date),
      nextVisit = dplyr::lead(.data$outcome_date)
    ) %>%
    dplyr::mutate(
      afterDays = !!CDMConnector::datediff("prevVisit", "outcome_date", "day"),
      beforeDays = !!CDMConnector::datediff("outcome_date", "nextVisit", "day")
    ) %>%
    dplyr::filter(.data$outcome_category == "ECT") %>%
    dplyr::filter(
      (is.na(.data$beforeDays) & is.na(.data$afterDays)) |
        (!.data$previousCategory %in% c("LB", "SB") & is.na(.data$nextCategory)) |
        (!.data$nextCategory %in% c("LB", "SB") & is.na(.data$previousCategory)) |
        (!.data$previousCategory %in% c("LB", "SB") & !.data$nextCategory %in% c("LB", "SB")) |
        (.data$previousCategory %in% c("LB", "SB") & .data$afterDays >= beforeMinLbEct & is.na(.data$nextCategory)) |
        (.data$nextCategory == "LB" & .data$beforeDays >= afterMinEctLb & is.na(.data$previousCategory)) |
        (.data$nextCategory == "SB" & .data$beforeDays >= afterMinEctSb & is.na(.data$previousCategory)) |
        (.data$nextCategory == "LB" & .data$beforeDays >= afterMinEctLb & .data$previousCategory %in% c("LB", "SB") & .data$afterDays >= beforeMinLbEct) |
        (.data$nextCategory == "SB" & .data$beforeDays >= afterMinEctSb & .data$previousCategory %in% c("LB", "SB") & .data$afterDays >= beforeMinLbEct)
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
    dplyr::mutate(tempCategory = dplyr::if_else(.data$outcome_category == "SA", "AB", .data$outcome_category))
  abCandidates <- afterEct %>%
    dplyr::union_all(abSaTbl %>% dplyr::select(-dplyr::any_of(c("gest_value", "value_as_number")))) %>%
    dplyr::mutate(tempCategory = dplyr::if_else(.data$outcome_category == "SA", "AB", .data$outcome_category)) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$outcome_date) %>%
    dplyr::mutate(
      previousCategory = dplyr::lag(.data$tempCategory),
      nextCategory = dplyr::lead(.data$tempCategory),
      prevVisit = dplyr::lag(.data$outcome_date),
      nextVisit = dplyr::lead(.data$outcome_date)
    ) %>%
    dplyr::mutate(
      afterDays = !!CDMConnector::datediff("prevVisit", "outcome_date", "day"),
      beforeDays = !!CDMConnector::datediff("outcome_date", "nextVisit", "day")
    ) %>%
    dplyr::filter(.data$tempCategory == "AB") %>%
    dplyr::filter(
      (is.na(.data$beforeDays) & is.na(.data$afterDays)) |
        (!.data$previousCategory %in% c("LB", "SB", "ECT") & is.na(.data$nextCategory)) |
        (!.data$nextCategory %in% c("LB", "SB", "ECT") & is.na(.data$previousCategory)) |
        (!.data$previousCategory %in% c("LB", "SB", "ECT") & !.data$nextCategory %in% c("LB", "SB", "ECT")) |
        (.data$previousCategory %in% c("LB", "SB") & .data$afterDays >= beforeMinLbAb & is.na(.data$nextCategory)) |
        (.data$nextCategory == "LB" & .data$beforeDays >= afterMinAbLb & is.na(.data$previousCategory)) |
        (.data$nextCategory == "SB" & .data$beforeDays >= afterMinAbSb & is.na(.data$previousCategory)) |
        (.data$nextCategory == "LB" & .data$previousCategory %in% c("LB", "SB") & .data$beforeDays >= afterMinAbLb & .data$afterDays >= beforeMinLbAb) |
        (.data$nextCategory == "SB" & .data$previousCategory %in% c("LB", "SB") & .data$beforeDays >= afterMinAbSb & .data$afterDays >= beforeMinLbAb) |
        (.data$previousCategory == "ECT" & .data$afterDays >= beforeMinEctAb & is.na(.data$nextCategory)) |
        (.data$nextCategory == "ECT" & .data$beforeDays >= afterMinAbEct & is.na(.data$previousCategory)) |
        (.data$nextCategory == "ECT" & .data$previousCategory == "ECT" & .data$beforeDays >= afterMinAbEct & .data$afterDays >= beforeMinEctAb) |
        (.data$nextCategory == "ECT" & .data$previousCategory %in% c("LB", "SB") & .data$beforeDays >= afterMinAbEct & .data$afterDays >= beforeMinLbAb) |
        (.data$nextCategory == "LB" & .data$previousCategory == "ECT" & .data$beforeDays >= afterMinAbLb & .data$afterDays >= beforeMinEctAb) |
        (.data$nextCategory == "SB" & .data$previousCategory == "ECT" & .data$beforeDays >= afterMinAbSb & .data$afterDays >= beforeMinEctAb)
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
    dplyr::mutate(tempCategory = dplyr::if_else(.data$outcome_category == "SA", "AB", .data$outcome_category)) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$outcome_date) %>%
    dplyr::mutate(
      previousCategory = dplyr::lag(.data$tempCategory),
      prevVisit = dplyr::lag(.data$outcome_date),
      nextCategory = dplyr::lead(.data$tempCategory),
      nextVisit = dplyr::lead(.data$outcome_date)
    ) %>%
    dplyr::mutate(
      afterDays = !!CDMConnector::datediff("prevVisit", "outcome_date", "day"),
      beforeDays = !!CDMConnector::datediff("outcome_date", "nextVisit", "day")
    )
  # Non-DELIV rows: move LB/SB outcome_date earlier when DELIV precedes and spacing < afterMinDelivSb
  nonDeliv <- delivTbl %>%
    dplyr::filter(.data$outcome_category != "DELIV") %>%
    dplyr::mutate(
      outcome_date = dplyr::if_else(
        !is.na(.data$previousCategory) &
          .data$previousCategory == "DELIV" &
          .data$outcome_category %in% c("LB", "SB") & .data$afterDays < afterMinDelivSb,
        .data$prevVisit,
        .data$outcome_date
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("person_id", "outcome_category", "outcome_date")
  delivCandidates <- delivTbl %>%
    dplyr::filter(.data$outcome_category == "DELIV") %>%
    dplyr::filter(
      (is.na(.data$beforeDays) & is.na(.data$afterDays)) |
        (!.data$previousCategory %in% c("LB", "SB", "ECT", "AB") & is.na(.data$nextCategory)) |
        (!.data$nextCategory %in% c("LB", "SB", "ECT", "AB") & is.na(.data$previousCategory)) |
        (!.data$previousCategory %in% c("LB", "SB", "ECT", "AB") & !.data$nextCategory %in% c("LB", "SB", "ECT", "AB")) |
        (.data$previousCategory %in% c("LB", "SB") & .data$afterDays >= beforeMinLbDeliv & is.na(.data$nextCategory)) |
        (.data$nextCategory == "LB" & .data$beforeDays >= afterMinDelivLb & is.na(.data$previousCategory)) |
        (.data$nextCategory == "SB" & .data$beforeDays >= afterMinDelivSb & is.na(.data$previousCategory)) |
        (.data$nextCategory == "LB" & .data$previousCategory %in% c("LB", "SB") & .data$beforeDays >= afterMinDelivLb & .data$afterDays >= beforeMinLbDeliv) |
        (.data$nextCategory == "SB" & .data$previousCategory %in% c("LB", "SB") & .data$beforeDays >= afterMinDelivSb & .data$afterDays >= beforeMinLbDeliv) |
        (.data$previousCategory %in% c("ECT", "AB") & .data$afterDays >= beforeMinEctDeliv & is.na(.data$nextCategory)) |
        (.data$nextCategory %in% c("ECT", "AB") & .data$beforeDays >= afterMinDelivEct & is.na(.data$previousCategory)) |
        (.data$nextCategory %in% c("ECT", "AB") & .data$previousCategory %in% c("ECT", "AB") & .data$beforeDays >= afterMinDelivEct & .data$afterDays >= beforeMinEctDeliv) |
        (.data$nextCategory %in% c("ECT", "AB") & .data$previousCategory %in% c("LB", "SB") & .data$beforeDays >= afterMinDelivEct & .data$afterDays >= beforeMinLbDeliv) |
        (.data$nextCategory == "LB" & .data$previousCategory %in% c("ECT", "AB") & .data$beforeDays >= afterMinDelivLb & .data$afterDays >= beforeMinEctDeliv) |
        (.data$nextCategory == "SB" & .data$previousCategory %in% c("ECT", "AB") & .data$beforeDays >= afterMinDelivSb & .data$afterDays >= beforeMinEctDeliv)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("person_id", "outcome_category", "outcome_date")
  allOutcomes <- nonDeliv %>%
    dplyr::union_all(delivCandidates) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      outcome_id = paste0(as.character(.data$person_id), "_O_", as.character(.data$outcome_date), "_", .data$outcome_category)
    )
  suppressWarnings({ # sql is long but it should be ok.
    cdm$outcome_episodes_df <- allOutcomes %>%
      dplyr::compute(name = "outcome_episodes_df", temporary = FALSE, overwrite = TRUE)
  })
  log4r::info(logger, "Stage 1: outcome episodes materialized")
  cdm
}

# estimateOutcomeStarts()
# Inputs: outcomeEpisodes (person_id, outcome_category, outcome_date, outcome_id), preg_matcho_term_durations.
# Outputs: cdm$outcome_episodes_with_starts_df (lazy chain; not materialized).
# Column mutations: adds min_term, max_term, retry, min_start_date, max_start_date; changes: none.
estimateOutcomeStarts <- function(cdm) {
  cdm$outcome_episodes_df %>%
    dplyr::left_join(cdm$preg_matcho_term_durations, by = c("outcome_category" = "category")) %>%
    dplyr::mutate(
      min_term = as.integer(.data$min_term),
      max_term = as.integer(.data$max_term)
    ) %>%
    dplyr::mutate(
      min_start_date = as.Date(!!CDMConnector::dateadd(date = "outcome_date", number = "-min_term", interval = "day")),
      max_start_date = as.Date(!!CDMConnector::dateadd(date = "outcome_date", number = "-max_term", interval = "day"))
    )
}

# buildGestationEpisodes()
# Inputs: preg_hip_records (person_id, visit_date, gest_value / value_as_number, concept_id).
# Outputs: cdm$gest_episodes_df (materialized). Columns: person_id, gestId, episode, max_gest_date, max_gest_week, min_gest_date, min_gest_week,
#   max_gest_start_date, min_gest_start_date, end_gest_date, min_gest_date_2, max_gest_day, min_gest_day, gest_start_date_diff, etc.
# Column mutations: filters/derives gestational records; defines episodes; per-episode min/max gest week and dates.
buildGestationEpisodes <- function(cdm, logger, minDays = 70, bufferDays = 28, gestConceptIds) {
  gestFromValue <- cdm$preg_hip_records %>%
    dplyr::filter(!is.na(.data$gest_value))
  gestationVisitsTbl <- cdm$preg_hip_records %>%
    dplyr::filter(
      .data$concept_id %in% .env$gestConceptIds,
      !is.na(.data$value_as_number),
      .data$value_as_number > 0,
      .data$value_as_number <= 44
    ) %>%
    dplyr::mutate(gest_value = as.integer(.data$value_as_number)) %>%
    dplyr::union_all(gestFromValue)
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
      min_gest_day = as.integer(.data$min_gest_week * 7)
    ) %>%
    dplyr::mutate(
      max_gest_start_date = !!CDMConnector::dateadd(date = "max_gest_date", number = "-max_gest_day", interval = "day"),
      min_gest_start_date = !!CDMConnector::dateadd(date = "min_gest_date", number = "-min_gest_day", interval = "day")
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
  suppressWarnings({
    # SQL it quite long but should be ok.
    cdm$gest_episodes_df <- joined %>%
      dplyr::select(-"max_gest_start_date_further") %>%
      dplyr::compute(name = "gest_episodes_df", temporary = FALSE, overwrite = TRUE)
  })

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
  bothTbl <- dplyr::compute(bothTbl, name = "merge_both_tmp", temporary = TRUE)
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
  tblList <- if (justGestation) {
    list(
      bothTbl %>% dplyr::mutate(final_category = .data$outcome_category, final_visit_date = .data$outcome_date),
      justOutcomeTbl %>% dplyr::mutate(final_category = .data$outcome_category, final_visit_date = .data$outcome_date),
      justGestationTbl
    )
  } else {
    list(
      bothTbl %>% dplyr::mutate(final_category = .data$outcome_category, final_visit_date = .data$outcome_date),
      justOutcomeTbl %>% dplyr::mutate(final_category = .data$outcome_category, final_visit_date = .data$outcome_date)
    )
  }
  # Materialize union first to avoid very long SQL on some database platforms
  unionTbl <- purrr::reduce(tblList, dplyr::union_all) %>%
    dplyr::compute(name = "merged_episodes_tmp", temporary = TRUE)
  mergedTbl <- unionTbl %>%
    dplyr::mutate(
      final_episode_id = dplyr::coalesce(.data$visit_id, .data$gest_id),
      has_outcome = !is.na(.data$visit_id),
      has_gestation = !is.na(.data$gest_id)
    ) %>%
    dplyr::mutate(days_diff = !!CDMConnector::datediff("max_gest_date", "final_visit_date", "day"))
  cdm$merged_episodes_df <- mergedTbl %>%
    dplyr::compute(name = "merged_episodes_df", temporary = FALSE, overwrite = TRUE)
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
  cdm$merged_episodes_df <- rest3 %>%
    dplyr::union_all(negDays) %>%
    dplyr::mutate(
      gest_at_outcome = !!CDMConnector::datediff("max_gest_start_date", "final_visit_date", "day"),
      min_gest_date_diff = !!CDMConnector::datediff("min_gest_date", "min_gest_date_2", "day"),
      date_diff_max_end = !!CDMConnector::datediff("end_gest_date", "max_gest_date", "day")
    ) %>%
    dplyr::compute(name = "merged_episodes_df", temporary = FALSE, overwrite = TRUE)
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
  gestIdToRemove <- overlapRows %>%
    dplyr::distinct(.data$prev_gest_id) %>%
    dplyr::pull()
  # Use sentinel when none to remove: real gest_id are like "12_G_2017-12-15"; "" would make NULL gest_id rows drop in SQL (NULL IN ('') -> NULL, NOT NULL excludes row).
  if (length(gestIdToRemove) == 0) gestIdToRemove <- ".__NO_GEST_IDS_TO_REMOVE__"
  afterRemove <- withPrev %>%
    dplyr::filter(!(.data$gest_id %in% .env$gestIdToRemove & .data$final_category == "PREG")) %>%
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
  reclassPreg <- afterRemove %>%
    dplyr::filter(!is.na(.data$max_gest_week) & !is.na(.data$concept_name) & .data$is_over_min == 0) %>%
    dplyr::mutate(
      removed_category = .data$final_category,
      final_category = "PREG",
      final_visit_date = .data$max_gest_date,
      removed_outcome = 1L
    )
  restFinal <- afterRemove %>%
    dplyr::filter(!(!is.na(.data$max_gest_week) & .data$is_over_min == 0))
  cdm$final_episodes_df <- restFinal %>%
    dplyr::union_all(reclassPreg) %>%
    dplyr::compute(name = "final_episodes_df", temporary = FALSE, overwrite = TRUE)
  nRemoved <- cdm$final_episodes_df %>%
    dplyr::filter(.data$removed_outcome == 1) %>%
    dplyr::tally() %>%
    dplyr::pull(.data$n)
  log4r::info(logger, sprintf("Total number of episodes with removed outcome: %s", nRemoved))
  cdm
}

# attachGestationAndLength()
# Inputs: finalEpisodes (person_id, final_episode_id, final_category, final_visit_date, final_start_date); preg_hip_records for gestation visit-level.
# Outputs: cdm$preg_hip_episodes (materialized). Columns: person_id, gest_date, category, visit_date, estimated_start_date, episode, gest_flag, episode_length.
# Column mutations: maps final_category->category, final_visit_date->visit_date, final_start_date->estimated_start_date; adds episode_order->episode once; gest_flag, episode_length.
attachGestationAndLength <- function(cdm, gestConceptIds) {
  finalWithOrder <- cdm$final_episodes_df %>%
    dplyr::distinct(.data$person_id, .data$final_category, .data$final_visit_date, .data$final_start_date) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$final_visit_date) %>%
    dplyr::mutate(episode_order = dplyr::row_number()) %>%
    dplyr::ungroup()
  gestFromValue <- cdm$preg_hip_records %>%
    dplyr::filter(!is.na(.data$gest_value))
  gestationVisitsTbl <- cdm$preg_hip_records %>%
    dplyr::filter(
      .data$concept_id %in% .env$gestConceptIds,
      !is.na(.data$value_as_number),
      .data$value_as_number > 0,
      .data$value_as_number <= 44
    ) %>%
    dplyr::mutate(gest_value = as.integer(.data$value_as_number)) %>%
    dplyr::union_all(gestFromValue) %>%
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
      category = .data$final_category,
      visit_date = .data$final_visit_date,
      estimated_start_date = .data$final_start_date,
      episode = .data$episode_order
    ) %>%
    dplyr::select("person_id", "gest_date", "category", "visit_date", "estimated_start_date", "episode", "gest_flag", "episode_length") %>%
    dplyr::distinct() %>%
    dplyr::compute(name = "preg_hip_episodes", temporary = FALSE, overwrite = TRUE)
  cdm
}
