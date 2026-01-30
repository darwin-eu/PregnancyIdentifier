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
#'
#' @returns The input CDM with new tables added called preg_hip_records, preg_pps_records,
#' preg_hip_concepts, preg_pps_concepts.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockPregnancyCdm()
#' cdm <- initPregnancies(cdm)
#' }
initPregnancies <- function(cdm,
                            startDate  = as.Date("1900-01-01"),
                            endDate    = Sys.Date(),
                            ageBounds  = c(15L, 56L),
                            logger     = NULL) {

  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertDate(startDate, any.missing = FALSE)
  checkmate::assertDate(endDate, any.missing = FALSE)
  checkmate::assertIntegerish(ageBounds, len = 2, any.missing = FALSE)
  checkmate::assertClass(logger, "logger", null.ok = TRUE)

  lowerAge <- as.integer(ageBounds[1])
  upperAge <- as.integer(ageBounds[2]) + 1L
  if (upperAge < lowerAge) {
    rlang::abort("ageBounds[1] must be < ageBounds[2]")
  }

  logInfo(logger, "Loading + inserting pregnancy concept tables into the CDM")

  hipConcepts <- readxl::read_excel(
    system.file("concepts", "HIP_concepts.xlsx", package = "PregnancyIdentifier")
  )
  ppsConcepts <- readxl::read_excel(
    system.file("concepts", "PPS_concepts.xlsx", package = "PregnancyIdentifier")
  ) |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(pps_concept_id = as.integer(.data$pps_concept_id))

  matchoTermDurations <- readxl::read_excel(
    system.file("concepts", "Matcho_term_durations.xlsx", package = "PregnancyIdentifier")
  )

  cdm <- CDMConnector::insertTable(cdm, "preg_hip_concepts", hipConcepts, overwrite = TRUE)
  cdm <- CDMConnector::insertTable(cdm, "preg_pps_concepts", ppsConcepts, overwrite = TRUE)
  cdm <- CDMConnector::insertTable(cdm, "preg_matcho_term_durations", matchoTermDurations, overwrite = TRUE)

  logInfo(logger, "Added preg_hip_concepts, preg_pps_concepts, preg_matcho_term_durations to the CDM")

  # ---- helper: pull person_id + date + concept_id from an OMOP domain ----
  pullDomain <- function(tbl, concept_col, date_col, extra = list()) {
    tbl |>
      dplyr::filter(.data[[date_col]] >= startDate, .data[[date_col]] <= endDate) |>
      dplyr::transmute(
        person_id   = .data$person_id,
        concept_id  = .data[[concept_col]],
        event_date  = .data[[date_col]],
        !!!extra
      )
  }

  # ============================================================
  # HIP records (condition/procedure/observation/measurement)
  # ============================================================
  logInfo(logger, "Pulling HIP concept records from OMOP domain tables")

  hip <- cdm$preg_hip_concepts |>
    dplyr::select(concept_id, category)

  hipSpecs <- tibble::tribble(
    ~tbl,                         ~concept_col,               ~date_col,               ~value_col,
    cdm$condition_occurrence,     "condition_concept_id",     "condition_start_date",  NA_character_,
    cdm$procedure_occurrence,     "procedure_concept_id",     "procedure_date",        NA_character_,
    cdm$observation,              "observation_concept_id",   "observation_date",      NA_character_,
    cdm$measurement,              "measurement_concept_id",   "measurement_date",      "value_as_number"
  )

  hipEvents <- purrr::pmap(hipSpecs, function(tbl, concept_col, date_col, value_col) {
    extra <- if (!is.na(value_col)) list(value_as_number = rlang::sym(value_col)) else list(value_as_number = NA_real_)
    pullDomain(tbl, concept_col, date_col, extra)
  }) |>
    purrr::reduce(dplyr::union_all) |>
    dplyr::inner_join(hip, by = "concept_id") |>
    dplyr::left_join(dplyr::select(cdm$concept, "concept_id", "concept_name"), by = "concept_id") |>
    dplyr::rename(visit_date = "event_date") |>
    dplyr::compute(name = "hip_events_staging", temporary = FALSE, overwrite = TRUE)

  cdm$preg_hip_records <- hipEvents |>
    addAgeSex("visit_date") |>
    dplyr::filter(.data$sex == "female", .data$age >= lowerAge, .data$age < upperAge) |>
    dplyr::distinct(.data$person_id, .data$visit_date, .data$category, .data$concept_id, .data$value_as_number) |>
    dplyr::compute(name = "preg_hip_records", temporary = FALSE, overwrite = TRUE)

  nHipRecords <- cdm$preg_hip_records %>% dplyr::ungroup() %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull("n")
  logInfo(logger, paste0("Added preg_hip_records (", nHipRecords," records) to the CDM"))

  # ============================================================
  # PPS records (condition/procedure/measurement)
  # ============================================================
  logInfo(logger, "Pulling PPS concept records from OMOP domain tables")

  ppsSpecs <- tibble::tribble(
    ~tbl,                     ~concept_col,              ~date_col,
    cdm$condition_occurrence, "condition_concept_id",    "condition_start_date",
    cdm$procedure_occurrence, "procedure_concept_id",    "procedure_date",
    cdm$measurement,          "measurement_concept_id",  "measurement_date"
  )

  ppsEvents <- purrr::pmap(ppsSpecs, function(tbl, concept_col, date_col) {
    pullDomain(tbl, concept_col, date_col) |>
      dplyr::transmute(
        person_id,
        pps_concept_start_date = .data$event_date,
        pps_concept_id         = .data$concept_id
      )
  }) |>
    purrr::reduce(dplyr::union_all) |>
    dplyr::inner_join(cdm$preg_pps_concepts, by = "pps_concept_id") |>
    dplyr::distinct() |>
    dplyr::compute(name = "pps_events_staging", temporary = FALSE, overwrite = TRUE)

  cdm$preg_pps_records <- ppsEvents |>
    dplyr::filter(!is.na(.data$pps_concept_start_date)) |>
    addAgeSex("pps_concept_start_date") |>
    dplyr::filter(.data$sex == "female", .data$age >= lowerAge, .data$age < upperAge) |>
    dplyr::select(-"sex", -"age") |>
    dplyr::compute(name = "preg_pps_records", temporary = FALSE, overwrite = TRUE)

  nPpsRecords <- cdm$preg_pps_records %>% dplyr::ungroup() %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull("n")
  logInfo(logger, paste0("Added preg_pps_records (", nPpsRecords," records) to the CDM"))

  cdm <- omopgenerics::dropSourceTable(cdm, c("hip_events_staging", "pps_events_staging"))
  cdm
}
