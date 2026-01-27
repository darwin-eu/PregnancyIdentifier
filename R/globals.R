#' @keywords internal
NULL

# Silence R CMD check "no visible binding for global variable" NOTES for
# dplyr/dbplyr non-standard evaluation.
utils::globalVariables(
  unique(c(
    "person_id",
    "pregnancy_start",
    "pregnancy_end",
    "episode_max_date_plus_two_months",
    "episode_min_date",
    "age_pregnancy_start",
    "category",
    "concept_id",
    "domain_concept_start_date",
    "episode_max_date_minus_lookback_window",
    "episode_max_date_plus_lookahead_window",
    "episode_max_date_plus_two_months",
    "episode_min_date",
    "estimated_start_date",
    "gest_date",
    "gestation_based",
    "max_gest_date",
    "max_gest_start_date",
    "max_start_date",
    "min_gest_start_date",
    "outcome_based",
    "overlapCountDict",
    "recorded_episode_end",
    "removed_outcome",
    "start_date",
    "visit_date",
    "visit_id",
    "overlaps"
  ))
)
