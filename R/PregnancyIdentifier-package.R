#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import CDMConnector
#' @import dplyr
#' @import readxl
#' @import lubridate
#' @import DBI
#' @import dbplyr
#' @import purrr
#' @import stringr
#' @import TestGenerator
#' @import magrittr
#' @import tidyr
#' @import log4r
#' @import stats
## usethis namespace: end
NULL

utils::globalVariables(
  c(
    "%like%", ".data", "PPS_flag", "PPS_outcome_category", "V1", "across", "algo1_dup",
    "algo1_id", "algo2_dup", "algo2_id", "algo2_outcome_date", "all_of", "any_of", "arrange",
    "between", "birth_datetime", "category", "cdm", "concept_id", "contains", "date_diff",
    "density", "desc", "domain_concept_id", "domain_concept_name",
    "domain_concept_start_date", "domain_value",
    "episode_max_date_minus_lookback_window",
    "episode_max_date_plus_lookahead_window",
    "episode_max_date_plus_two_months", "episode_min_date",
    "estimated_start_date", "everything", "first_preg_category", "gest_date",
    "gestation_based", "head", "inferred_episode_end", "inferred_episode_start",
    "keep_value", "lag", "lead", "lst", "max_gest_date", "max_gest_start_date",
    "max_start_date", "min_days", "min_gest_start_date", "n",
    "next_closest_episode_date", "outcome_based", "outcome_preg_category",
    "outcomes_list", "overlaps", "person_id", "pregnancy_end", "pregnancy_start",
    "prev_end", "quantile", "read.csv", "recorded_episode_end", "removed_outcome",
    "rev_hip", "rev_pps", "row_number", "start_date", "temp_category", "visit_date",
    "visit_id", "write.csv"
  )
)
