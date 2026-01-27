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
initPregnancies <- function(cdm, startDate = as.Date("1900-01-01"), endDate = Sys.Date(), ageBounds = c(15L, 56L), logger = NULL) {

  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertDate(startDate, any.missing = FALSE)
  checkmate::assertDate(endDate, any.missing = FALSE)
  checkmate::assertIntegerish(ageBounds, len = 2, any.missing = FALSE)
  lowerAgeBound <- as.integer(ageBounds[1])
  upperAgeBound <- as.integer(ageBounds[2]) + 1L
  if (upperAgeBound < lowerAgeBound) {
    rlang::abort("The lower age bound (ageBounds[1]) must be less than the upper age bound (ageBounds[2])")
  }
  checkmate::assertClass(logger, "logger", null.ok = TRUE)

  logInfo(logger, "Inserting HIP concepts into the CDM")
  hipConcepts <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "HIP_concepts.xlsx"))
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "preg_hip_concepts", table = hipConcepts, overwrite = TRUE)

  logInfo(logger, "Inserting PPS concepts into the CDM")
  ppsConcepts <- readxl::read_excel(
    system.file("concepts", "PPS_concepts.xlsx", package = "PregnancyIdentifier")
  ) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(pps_concept_id = as.integer(.data$pps_concept_id))

  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "preg_pps_concepts",
    table = ppsConcepts
  )

  logInfo(logger, "Inserting Matcho term durations into the CDM")
  matchoTermDurations <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "Matcho_term_durations.xlsx"))
  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "preg_matcho_term_durations",
    table = matchoTermDurations,
  )

  logInfo(logger, "Getting all initial pregnancy records")
  hip <- dplyr::select(cdm$preg_hip_concepts, concept_id, category)

  cdm$all_pregnancy_records <-
    purrr::pmap(
      list(
        tbl     = list(cdm$measurement, cdm$procedure_occurrence, cdm$observation, cdm$condition_occurrence),
        concept = c("measurement_concept_id","procedure_concept_id","observation_concept_id","condition_concept_id"),
        date    = c("measurement_date","procedure_date","observation_date","condition_start_date")
      ),
      function (tbl, concept, date) {
        valueExpr <- if (concept == "measurement_concept_id") {
          rlang::sym("value_as_number")
        } else {
          rlang::expr(dplyr::sql("NULL"))
        }

        tbl %>%
          dplyr::inner_join(hip, by = stats::setNames("concept_id", concept)) %>%
          dplyr::filter(!!rlang::sym(date) >= startDate, !!rlang::sym(date) <= endDate) %>%
          dplyr::transmute(
            person_id = .data$person_id,
            concept_id  = !!rlang::sym(concept),
            visit_date  = !!rlang::sym(date),
            category = .data$category,
            value_as_number = !!valueExpr
          ) %>%
          dplyr::left_join(dplyr::select(cdm$concept, "concept_id", "concept_name"), by = "concept_id")
      }
    ) %>%
    purrr::reduce(dplyr::union_all) %>%
    dplyr::compute()

  # keep only person_ids of women of reproductive age at some visit
  cdm$preg_hip_records <- cdm$all_pregnancy_records %>%
    addAgeSex("visit_date") %>%
    dplyr::filter(.data$age >= lowerAgeBound, .data$age < upperAgeBound, .data$sex == "female") %>%
    dplyr::distinct() %>%
    dplyr::select("person_id", "visit_date", "category", "concept_id", "value_as_number") %>%
    dplyr::compute(name = "preg_hip_records", temporary = FALSE, overwrite = TRUE)

  # ============================================================
  # Pull PPS concepts from OMOP domain tables
  # ============================================================

  # Pulls concepts from a single OMOP table and normalizes column names so that
  # all downstream logic can operate on a consistent schema.
  # PPS concepts (in the PPS_concepts excel file) occur in the condition, measurement, and procedure domains
  logInfo(logger, "Pulling PPS concept records")
  domainSpecs <- tibble::tribble(
    ~table_name,            ~date_column,            ~concept_id_column,
    "condition_occurrence", "condition_start_date",  "condition_concept_id",
    "procedure_occurrence", "procedure_date",        "procedure_concept_id",
    "measurement",          "measurement_date",      "measurement_concept_id",
  )

  cdm$all_timing_records <- purrr::map(seq_len(nrow(domainSpecs)), \(i) {
    spec <- domainSpecs[i, ]

    tableName <- spec$table_name[[1]]
    dateCol   <- spec$date_column[[1]]
    cidCol    <- spec$concept_id_column[[1]]

    logInfo(logger, sprintf("Pulling data from %s", tableName))

    cdm[[tableName]] %>%
      dplyr::filter(.data[[dateCol]] >= startDate, .data[[dateCol]] <= endDate) %>%
      dplyr::transmute(
        person_id,
        pps_concept_start_date = .data[[dateCol]],
        pps_concept_id         = .data[[cidCol]]
      ) %>%
      dplyr::inner_join(cdm$preg_pps_concepts, by = "pps_concept_id") %>%
      dplyr::distinct()
  }) %>% purrr::reduce(dplyr::union_all) %>%
    dplyr::compute()

  cdm$preg_pps_records <- cdm$all_timing_records %>%
    dplyr::filter(!is.na(.data$pps_concept_start_date)) %>%
    addAgeSex("pps_concept_start_date") %>%
    dplyr::filter(.data$sex == "female", .data$age >= 15, .data$age < 56) %>%
    dplyr::select(-"sex", -"age") %>%
    dplyr::compute(name = "preg_pps_records")

  cdm <- omopgenerics::dropSourceTable(
    cdm,
    c("all_pregnancy_records", "all_timing_records")
  )

  return(cdm)
}
