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
#'
#' @returns The input CDM with a new table added called preg_initial_cohort with
#' the following columns:"person_id" "concept_id" "visit_date" "value_as_number" "concept_name"
#' "category" "gest_value" "date_of_birth" "date_diff" "age"
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockPregnancyCdm()
#' cdm <- initPregnancies(cdm)
#' }
initPregnancies <- function(cdm, startDate = as.Date("1900-01-01"), endDate = Sys.Date(), ageBounds = c(15L, 56L)) {

  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertDate(startDate, any.missing = FALSE)
  checkmate::assertDate(endDate, any.missing = FALSE)
  checkmate::assertIntegerish(ageBounds, len = 2, any.missing = FALSE)
  lowerAgeBound <- as.integer(ageBounds[1])
  upperAgeBound <- as.integer(ageBounds[2]) + 1L
  if (upperAgeBound < lowerAgeBound) {
    rlang::abort("The lower age bound (ageBounds[1]) must be less than the upper age bound (ageBounds[2])")
  }

  message("Inserting HIP concepts into the CDM")
  hipConcepts <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "HIP_concepts.xlsx"))
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "preg_hip_concepts", table = hipConcepts, overwrite = TRUE)

  message("Inserting PPS concepts into the CDM")
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

  message("Inserting Matcho term durations into the CDM")
  matchoTermDurations <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "Matcho_term_durations.xlsx"))
  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "preg_matcho_term_durations",
    table = matchoTermDurations,
  )

  message("Getting all initial pregnancy records")
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

  # get unique person ids for women of reproductive age
  cdm$person_df <- cdm$person %>%
    dplyr::filter(
      # 45878463: Female
      # 46273637: Intersex
      # 45880669: Male
      # 1177221: I prefer not to answer
      # 903096: Skip
      # 4124462: None
      # TODO: Add option to specify specific column and/or concept ID(s)
      .data$gender_concept_id == 8532
      # .data$sex_at_birth_concept_id != 45880669
      # the majority of the people in the other non-Female or Male categories
      # also report female gender
    ) %>%
    dplyr::mutate(
      day_of_birth = as.integer(dplyr::if_else(is.na(.data$day_of_birth), 1L, .data$day_of_birth)),
      month_of_birth = as.integer(dplyr::if_else(is.na(.data$month_of_birth), 1L, .data$month_of_birth)),
      date_of_birth = as.Date(paste0(as.character(as.integer(.data$year_of_birth)), "-", as.character(as.integer(.data$month_of_birth)), "-", as.character(as.integer(.data$day_of_birth))))
    ) %>%
    dplyr::select("person_id", "date_of_birth") %>%
    dplyr::compute()

  # keep only person_ids of women of reproductive age at some visit
  cdm$preg_initial_cohort <- cdm$all_pregnancy_records %>%
    dplyr::inner_join(cdm$person_df, by = "person_id") %>%
    dplyr::mutate(
      date_diff = !!CDMConnector::datediff("date_of_birth", "visit_date", interval = "day")
    ) %>%
    dplyr::mutate(
      age = .data$date_diff / 365.25
    ) %>%
    dplyr::filter(.data$age >= lowerAgeBound) %>%
    dplyr::filter(.data$age < upperAgeBound) %>%
    dplyr::distinct() %>%
    dplyr::select("person_id", "visit_date", "category", "concept_id", "value_as_number") %>%
    dplyr::compute(name = "preg_initial_cohort", temporary = FALSE, overwrite = TRUE)

  cdm <- omopgenerics::dropSourceTable(
    cdm,
    c("condition_df", "measurement_df", "procedure_df",
      "all_pregnancy_records", "person_df", "observation_df")
  )

  return(cdm)
}
