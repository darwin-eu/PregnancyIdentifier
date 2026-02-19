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

#' Create Initial Pregnancy Table
#'
#' Get initial cohort based on HIP concepts for women who are age 15 to 55
#' at the time of pregnancy
#'
#' @param cdm A CDM reference
#' @param startDate Earliest date to look for pregnancies in the CDM
#' @param endDate Latest date to look for pregnancies in the CDM
#' @param ageBounds The upper and lower bounds for age at pregnancy end date
#' represented using a length 2 integer vector. By default this will be
#' c(15, 56) and will include anyone >= 15 and < 56.
#' @param logger A log4r logger object that can be created with `makeLogger()`
#' @param outputDir Optional directory path. When provided, an \code{attrition.csv}
#'   file is created with initial record and person counts for \code{preg_hip_records}
#'   and \code{preg_pps_records}.
#'
#' @returns The input CDM with new tables added called preg_hip_records, preg_pps_records,
#' preg_hip_concepts, preg_pps_concepts.
#' @export
#'
#' @examples
#' \dontrun{
#' logger <- makeLogger(tempdir())
#' cdm <- mockPregnancyCdm()
#' cdm <- initPregnancies(cdm, logger = logger)
#' }
initPregnancies <- function(cdm,
                            startDate  = as.Date("1900-01-01"),
                            endDate    = Sys.Date(),
                            ageBounds  = c(15L, 56L),
                            logger,
                            outputDir  = NULL) {

  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertDate(startDate, any.missing = FALSE)
  checkmate::assertDate(endDate, any.missing = FALSE)
  checkmate::assertIntegerish(ageBounds, len = 2, any.missing = FALSE)

  # Require logger: must be a logger object, not NULL
  checkmate::assertClass(logger, "logger", null.ok = FALSE)

  lowerAge <- as.integer(ageBounds[1])
  upperAge <- as.integer(ageBounds[2]) + 1L
  if (upperAge < lowerAge) {
    rlang::abort("ageBounds[1] must be < ageBounds[2]")
  }

  log4r::info(logger, "Loading + inserting pregnancy concept tables into the CDM")

  hipConcepts <- suppressMessages(readxl::read_excel(
    system.file("concepts", "HIP_concepts_reviewed17022026.xlsx", package = "PregnancyIdentifier"),
    col_types = "text"
  )) %>%
    dplyr::select("concept_id", "concept_name", "category", "gest_value") %>%
    dplyr::mutate(concept_id = suppressWarnings(as.integer(.data$concept_id))) %>%
    dplyr::mutate(gest_value = as.numeric(.data$gest_value))

  # Only include concepts that are used for episode definition: outcome categories (LB, SB, DELIV, ECT, AB, SA),
  # concepts with gest_value set (gestation weeks from condition/procedure/observation), or concepts in
  # gestational_age_concepts.csv (used with value_as_number for gestation episodes).
  hipOutcomeCategories <- c("LB", "SB", "DELIV", "ECT", "AB", "SA")
  gestationalAgeConcepts <- utils::read.csv(
    system.file("concepts", "gestational_age_concepts.csv", package = "PregnancyIdentifier", mustWork = TRUE),
    colClasses = c(concept_id = "integer")
  )
  gestationalAgeConceptIds <- as.integer(gestationalAgeConcepts$concept_id)
  hipConceptsUsed <- hipConcepts |>
    dplyr::filter(
      .data$category %in% .env$hipOutcomeCategories |
        !is.na(.data$gest_value) |
        .data$concept_id %in% .env$gestationalAgeConceptIds
    )
  nHipDropped <- nrow(hipConcepts) - nrow(hipConceptsUsed)
  if (nHipDropped > 0) {
    log4r::info(logger, sprintf(
      "HIP concepts: using %d of %d (dropped %d PREG-only concepts not used for episode definition)",
      nrow(hipConceptsUsed), nrow(hipConcepts), nHipDropped
    ))
  }
  hipConcepts <- hipConceptsUsed

  ppsConcepts <- suppressMessages(readxl::read_excel(
    system.file("concepts", "PPS_concepts_reviewed1702026.xlsx", package = "PregnancyIdentifier")
  )) |>
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(pps_concept_id = suppressWarnings(as.integer(.data$pps_concept_id)))

  matchoTermDurations <- readxl::read_excel(
    system.file("concepts", "Matcho_term_durations.xlsx", package = "PregnancyIdentifier")
  )

  cdm <- CDMConnector::insertTable(cdm, "preg_hip_concepts", hipConcepts, overwrite = TRUE)
  cdm <- CDMConnector::insertTable(cdm, "preg_pps_concepts", ppsConcepts, overwrite = TRUE)
  cdm <- CDMConnector::insertTable(cdm, "preg_matcho_term_durations", matchoTermDurations, overwrite = TRUE)

  log4r::info(logger, "Added preg_hip_concepts, preg_pps_concepts, preg_matcho_term_durations to the CDM")

  # ---- helper: pull person_id + date + concept_id from an OMOP domain ----
  pullDomain <- function(tbl, conceptCol, dateCol, extra = list()) {
    tbl |>
      dplyr::filter(.data[[dateCol]] >= startDate, .data[[dateCol]] <= endDate) |>
      dplyr::transmute(
        person_id   = .data$person_id,
        concept_id  = .data[[conceptCol]],
        event_date  = .data[[dateCol]],
        !!!extra
      )
  }

  # ============================================================
  # HIP records (condition/procedure/observation/measurement)
  # ============================================================
  log4r::info(logger, "Pulling HIP concept records from OMOP domain tables")

  # Include gest_value (gestational weeks from HIP_concepts_reviewed17022026.xlsx) so condition/procedure/observation
  # rows without value_as_number can still contribute to gestation episodes in buildGestationEpisodes.
  hip <- cdm$preg_hip_concepts |>
    dplyr::select("concept_id", "category", dplyr::any_of("gest_value"))
  if (!"gest_value" %in% colnames(hip)) {
    hip <- hip |> dplyr::mutate(gest_value = NA_real_)
  }

  hipSpecs <- tibble::tribble(
    ~tbl,                         ~conceptCol,               ~dateCol,               ~valueCol,
    cdm$condition_occurrence,     "condition_concept_id",     "condition_start_date",  NA_character_,
    cdm$procedure_occurrence,     "procedure_concept_id",     "procedure_date",        NA_character_,
    cdm$observation,              "observation_concept_id",   "observation_date",      NA_character_,
    cdm$measurement,              "measurement_concept_id",   "measurement_date",      "value_as_number"
  )

  # Cast value_as_number so UNION has a single type (avoids "UNION types text and numeric cannot be matched")
  # Single pipeline to preg_hip_records (no staging table) so prefixed write-schema works on all DBs
  cdm$preg_hip_records <- purrr::pmap(hipSpecs, function(tbl, conceptCol, dateCol, valueCol) {
    extra <- if (!is.na(valueCol)) {
      list(value_as_number = rlang::expr(as.numeric(!!rlang::sym(valueCol))))
    } else {
      list(value_as_number = rlang::expr(as.numeric(NA)))
    }
    pullDomain(tbl, conceptCol, dateCol, extra)
  }) |>
    purrr::reduce(dplyr::union_all) |>
    dplyr::inner_join(hip, by = "concept_id") |>
    dplyr::left_join(dplyr::select(cdm$concept, "concept_id", "concept_name"), by = "concept_id") |>
    dplyr::rename(visit_date = "event_date") |>
    addAgeSex("visit_date") |>
    dplyr::filter(.data$sex == "female", .data$age >= lowerAge, .data$age < upperAge) |>
    dplyr::distinct(
      .data$person_id, .data$visit_date, .data$category, .data$concept_id,
      .data$value_as_number, .data$gest_value
    ) |>
    .compute(name = "preg_hip_records", temporary = FALSE, overwrite = TRUE)

  nHipRecords <- cdm$preg_hip_records %>% dplyr::ungroup() %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull("n")
  log4r::info(logger, paste0("Added preg_hip_records (", nHipRecords," records) to the CDM"))

  # HIP concept counts (record_count, person_count) for outputDir / export
  if (!is.null(outputDir)) {
    hipConceptCounts <- cdm$preg_hip_records %>%
      dplyr::group_by(.data$concept_id) %>%
      dplyr::summarise(
        record_count = dplyr::n(),
        person_count = dplyr::n_distinct(.data$person_id),
        .groups = "drop"
      ) %>%
      dplyr::left_join(
        dplyr::select(cdm$preg_hip_concepts, "concept_id", "concept_name"),
        by = "concept_id"
      ) %>%
      dplyr::collect() %>%
      dplyr::select("concept_id", "concept_name", "record_count", "person_count")
    utils::write.csv(hipConceptCounts, file.path(outputDir, "hip_concept_counts.csv"), row.names = FALSE)
  }

  # ============================================================
  # PPS records (condition/procedure/measurement)
  # ============================================================
  log4r::info(logger, "Pulling PPS concept records from OMOP domain tables")

  ppsSpecs <- tibble::tribble(
    ~tbl,                     ~conceptCol,              ~dateCol,
    cdm$condition_occurrence, "condition_concept_id",    "condition_start_date",
    cdm$procedure_occurrence, "procedure_concept_id",    "procedure_date",
    cdm$measurement,          "measurement_concept_id",  "measurement_date"
  )

  # Single pipeline to preg_pps_records (no staging table) so prefixed write-schema works on all DBs
  cdm$preg_pps_records <- purrr::pmap(ppsSpecs, function(tbl, conceptCol, dateCol) {
    pullDomain(tbl, conceptCol, dateCol) |>
      dplyr::transmute(
        person_id,
        pps_concept_start_date = .data$event_date,
        pps_concept_id         = .data$concept_id
      )
  }) |>
    purrr::reduce(dplyr::union_all) |>
    dplyr::inner_join(cdm$preg_pps_concepts, by = "pps_concept_id") |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(.data$pps_concept_start_date)) |>
    addAgeSex("pps_concept_start_date") |>
    dplyr::filter(.data$sex == "female", .data$age >= lowerAge, .data$age < upperAge) |>
    dplyr::select(-"sex", -"age") |>
    .compute(name = "preg_pps_records", temporary = FALSE, overwrite = TRUE)

  nPpsRecords <- cdm$preg_pps_records %>% dplyr::ungroup() %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull("n")
  log4r::info(logger, paste0("Added preg_pps_records (", nPpsRecords," records) to the CDM"))

  # PPS concept counts (record_count, person_count) for outputDir / export
  if (!is.null(outputDir)) {
    ppsConceptCounts <- cdm$preg_pps_records %>%
      dplyr::group_by(.data$pps_concept_id, .data$pps_concept_name) %>%
      dplyr::summarise(
        record_count = dplyr::n(),
        person_count = dplyr::n_distinct(.data$person_id),
        .groups = "drop"
      ) %>%
      dplyr::collect() %>%
      dplyr::select("pps_concept_id", "pps_concept_name", "record_count", "person_count")
    utils::write.csv(ppsConceptCounts, file.path(outputDir, "pps_concept_counts.csv"), row.names = FALSE)
  }

  if (!is.null(outputDir)) {
    checkmate::assertCharacter(outputDir, len = 1)
    initAttrition(outputDir, cdm)
  }

  cdm
}
