

#' Create Initial Pregnancy Table
#'
#' Get initial cohort based on HIP concepts for women who are age 15 to 55
#' at the time of pregnancy
#'
#' @param cdm A CDM reference
#' @param startDate Earliest date to look for pregnancies in the CDM
#' @param endDate Latest date to look for pregnancies in the CDM
#' @param ageBound The upper and lower bounds for age at pregnancy end date
#' represented using a length 2 integer vector. By default this will be
#' c(15, 56) and will include anyone >= 15 and < 56.
#'
#' @returns The input CDM with a new table added called preg_initial_cohort with
#' the following columns:"person_id" "concept_id" "visit_date" "value_as_number" "concept_name"
#' "category" "gest_value" "date_of_birth" "date_diff" "age"
#' @export
#'
#' @examples
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

  HIP_concepts <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "HIP_concepts.xlsx"))
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "preg_hip_concepts", table = HIP_concepts, overwrite = TRUE)
  message("Inserted HIP concepts into the CDM")

  cdm$observation_df <- cdm$observation %>%
    dplyr::filter(
      .data$observation_date >= startDate,
      .data$observation_date <= endDate
    ) %>%
    dplyr::select(
      "person_id",
      concept_id = "observation_concept_id",
      visit_date = "observation_date",
      "value_as_number"
    ) %>%
    dplyr::inner_join(cdm$preg_hip_concepts, by = "concept_id") %>%
    dplyr::compute()

  cdm$measurement_df <- cdm$measurement %>%
    dplyr::filter(
      .data$measurement_date >= startDate,
      .data$measurement_date <= endDate
    ) %>%
    dplyr::select(
      "person_id",
      concept_id = "measurement_concept_id",
      visit_date = "measurement_date",
      "value_as_number"
    ) %>%
    dplyr::inner_join(cdm$preg_hip_concepts, by = "concept_id") %>%
    dplyr::compute()

  cdm$procedure_df <- cdm$procedure_occurrence %>%
    dplyr::filter(
      .data$procedure_date >= startDate,
      .data$procedure_date <= endDate
    ) %>%
    dplyr::select(
      "person_id",
      concept_id = "procedure_concept_id",
      visit_date = "procedure_date"
    ) %>%
    dplyr::inner_join(cdm$preg_hip_concepts, by = "concept_id") %>%
    dplyr::compute()

  # filter condition table
  cdm$condition_df <- cdm$condition_occurrence %>%
    dplyr::filter(
      .data$condition_start_date >= startDate,
      .data$condition_start_date <= endDate
    ) %>%
    dplyr::select(
      "person_id",
      concept_id = "condition_concept_id",
      visit_date = "condition_start_date"
    ) %>%
    dplyr::inner_join(cdm$preg_hip_concepts, by = "concept_id") %>%
    dplyr::compute()

  # combine tables
  all_dfs <- list(cdm$measurement_df, cdm$procedure_df, cdm$observation_df, cdm$condition_df)
  cdm$union_df <- purrr::reduce(all_dfs, dplyr::union_all) %>%
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
  cdm$preg_initial_cohort <- cdm$union_df %>%
    dplyr::inner_join(cdm$person_df, by = "person_id") %>%
    dplyr::mutate(
      date_diff = !!CDMConnector::datediff("date_of_birth", "visit_date", interval = "day")
    ) %>%
    dplyr::mutate(
      age = .data$date_diff / 365.25
    ) %>%
    dplyr::filter(.data$age >= .env$lowerAgeBound) %>%
    dplyr::filter(.data$age < .env$upperAgeBound) %>%
    dplyr::distinct() %>%
    dplyr::select("person_id", "visit_date", "category") %>%
    dplyr::compute(name = "preg_initial_cohort", temporary = FALSE, overwrite = TRUE)

  cdm <- omopgenerics::dropSourceTable(
    cdm,
    c("condition_df", "measurement_df", "procedure_df",
      "union_df", "person_df", "observation_df")
  )

  return(cdm)
}
