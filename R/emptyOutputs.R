# emptyOutputs.R
# Zero-row schema-correct tibbles for pipeline outputs.
# Used when HIP/PPS/merge/ESD produce 0 episodes so written files have correct schema.

#' @noRd
emptyHipEpisodes <- function() {
  dplyr::tibble(
    person_id = integer(0),
    episode = integer(0),
    estimated_start_date = as.Date(character(0)),
    visit_date = as.Date(character(0)),
    category = character(0),
    gest_date = as.Date(character(0)),
    gest_flag = character(0),
    episode_length = integer(0)
  )
}

#' @noRd
emptyPpsEpisodes <- function() {
  dplyr::tibble(
    person_id = integer(0),
    person_episode_number = integer(0),
    episode_min_date = as.Date(character(0)),
    episode_max_date = as.Date(character(0)),
    episode_max_date_plus_two_months = as.Date(character(0)),
    algo2_category = character(0),
    algo2_outcome_date = as.Date(character(0)),
    n_gt_concepts = integer(0)
  )
}

#' @noRd
emptyHippsEpisodes <- function() {
  dplyr::tibble(
    person_id = integer(0),
    episode_number = integer(0),
    recorded_episode_start = as.Date(character(0)),
    recorded_episode_end = as.Date(character(0)),
    hip_end_date = as.Date(character(0)),
    pps_end_date = as.Date(character(0)),
    hip_outcome_category = character(0),
    pps_outcome_category = character(0),
    hip_flag = integer(0),
    pps_flag = integer(0),
    recorded_episode_length = numeric(0),
    pregnancy_start = as.Date(character(0)),
    first_gest_date = as.Date(character(0)),
    episode_min_date = as.Date(character(0)),
    episode_max_date = as.Date(character(0)),
    episode_max_date_plus_two_months = as.Date(character(0)),
    episode = integer(0),
    person_episode_number = integer(0),
    algo1_id = character(0),
    algo2_id = character(0)
  )
}

#' @noRd
emptyFinalPregnancyEpisodes <- function() {
  dplyr::tibble(
    person_id = integer(0),
    episode_number = integer(0),
    inferred_episode_start = as.Date(character(0)),
    inferred_episode_end = as.Date(character(0)),
    final_outcome_category = character(0),
    recorded_episode_start = as.Date(character(0)),
    recorded_episode_end = as.Date(character(0)),
    hip_end_date = as.Date(character(0)),
    pps_end_date = as.Date(character(0)),
    hip_outcome_category = character(0),
    pps_outcome_category = character(0),
    precision_days = integer(0),
    precision_category = character(0),
    gestational_age_days_calculated = integer(0),
    gw_flag = numeric(0),
    gr3m_flag = numeric(0),
    outcome_match = integer(0),
    term_duration_flag = integer(0),
    outcome_concordance_score = integer(0),
    preterm_status_from_calculation = integer(0),
    pregnancy_start = as.Date(character(0))
  )
}

#' @noRd
emptyPpsGestTiming <- function() {
  dplyr::tibble(
    person_id = integer(0),
    person_episode_number = integer(0),
    pps_concept_start_date = as.Date(character(0)),
    pps_concept_id = integer(0),
    pps_concept_name = character(0),
    pps_min_month = numeric(0),
    pps_max_month = numeric(0)
  )
}

#' @noRd
emptyPpsMinMax <- function() {
  dplyr::tibble(
    person_id = integer(0),
    person_episode_number = integer(0),
    episode_min_date = as.Date(character(0)),
    episode_max_date = as.Date(character(0)),
    episode_max_date_plus_two_months = as.Date(character(0)),
    n_gt_concepts = integer(0)
  )
}

#' @noRd
emptyEsd <- function() {
  dplyr::tibble(
    person_id = integer(0),
    episode_number = integer(0),
    inferred_episode_start = as.Date(character(0)),
    precision_days = numeric(0),
    precision_category = character(0)
  )
}
