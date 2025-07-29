#' runHipps
#'
#' Runs the HIPPS algorithm (HIP, PPS, and ESD) from: https://github.com/louisahsmith/allofus-pregnancy/
#' without requiring the `allofus` R package.
#'
#' @param cdm (`cdm_reference`) A CDM-Reference object from CDMConnector.
#' @param outputDir (`character(1)`) Output directory to write output to.
#' @param fileName (`character(1)`) Filename to write to
#'
#' @returns `NULL`
#'
#' @export
runHipps <- function(cdm, outputDir, fileName) {
  message("> Classifying Pregnancy using HIP, PPS, and ESD")
  dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)

  person_tbl <- cdm$person
  concept_tbl <- cdm$concept
  observation_tbl <- cdm$observation
  measurement_tbl <- cdm$measurement
  condition_occurrence_tbl <- cdm$condition_occurrence
  procedure_occurrence_tbl <- cdm$procedure_occurrence
  visit_occurrence_tbl <- cdm$visit_occurrence

  HIP_concepts <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "HIP_concepts.xlsx"))
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "hip_concepts", table = HIP_concepts)

  PPS_concepts <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "PPS_concepts.xlsx")) %>%
    dplyr::mutate(domain_concept_id = as.integer(.data$domain_concept_id))
  names(PPS_concepts) <- tolower(names(PPS_concepts))
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "pps_concepts", table = PPS_concepts)

  matcho_outcome_limits <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "Matcho_outcome_limits.xlsx"))

  matcho_term_durations <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "Matcho_term_durations.xlsx"))
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "matcho_term_durations", table = matcho_term_durations)

  # HIP -----------------------------------------------------------------------
  message("  * Running HIP")
  ## Outcome-based episodes

  # get initial cohort based on hip_concepts
  # this returns a dataset with person_id, concept_id, visit_date, domain, etc.
  # for all the the HIP concepts that for women who were 15-55
  cdm <- cdm %>%
    initial_pregnant_cohort()

  if (getTblRowCount(cdm$initial_pregnant_cohort_df) == 0) {
    warning("  ! No records after initializing pregnant cohort")
    return(NULL)
  }

  # get outcome visits from Matcho et al.
  # this function takes the HIP concepts matching the category of interest
  # calculates days between each visit and selects the first episode
  # and any episodes that are separated by at least that many days

  categories <- list(
    c("AB", "SA"),
    "DELIV",
    "ECT",
    "SB",
    "LB"
  )

  for (category in categories) {
    cdm <- final_visits(
      cdm,
      matcho_outcome_limits = matcho_outcome_limits,
      categories = category
    )
  }

  # add stillbirth episodes to livebirth episodes
  # after making sure they are sufficiently spaced
  add_stillbirth_df <- add_stillbirth(final_stillbirth_visits_df, final_livebirth_visits_df, matcho_outcome_limits)

  # add ectopic episodes to previous
  add_ectopic_df <- add_ectopic(add_stillbirth_df, matcho_outcome_limits, final_ectopic_visits_df)

  # add abortion episodes to previous
  add_abortion_df <- add_abortion(add_ectopic_df, matcho_outcome_limits, final_abortion_visits_df)

  # add delivery-only episodes to previous
  add_delivery_df <- add_delivery(add_abortion_df, matcho_outcome_limits, final_delivery_visits_df)

  # calculate start of pregnancies based on outcomes
  # min start date = latest possible start date if shortest term
  # max start date = earliest possible start date if max term
  calculate_start_df <- calculate_start(add_delivery_df, cdm$matcho_term_durations) %>%
    dplyr::compute()

  ## Gestation-based episodes

  # now go back to the initial set of concepts and find ones with gestation weeks
  gestation_visits_df <- gestation_visits(cdm$initial_pregnant_cohort_df)

  # identify the start of episodes based on the difference in time between gestational age-related concepts
  # and the actual difference in days
  gestation_episodes_df <- gestation_episodes(gestation_visits_df)

  # get various mins and maxes of gestational age and dates
  get_min_max_gestation_df <- get_min_max_gestation(gestation_episodes_df)

  ## Combine gestation-based and outcome-based episodes

  # add gestation episodes to outcome episodes
  add_gestation_df <- add_gestation(calculate_start_df, get_min_max_gestation_df)

  # clean episodes by removing duplicate episodes and reclassifying outcome-based episodes
  clean_episodes_df <- clean_episodes(add_gestation_df)

  # remove any episodes that overlap and keep only the latter episode if the previous episode is PREG
  remove_overlaps_df <- remove_overlaps(clean_episodes_df)

  # keep subset of columns with episode start and end as well as category
  final_episodes_df <- final_episodes(remove_overlaps_df)

  # find the first gestation record within an episode and calculate the episode length
  # based on the date of the first gestation record and the visit date
  HIP_episodes_df <- final_episodes_with_length(final_episodes_df, gestation_visits_df) %>%
    dplyr::compute()

  # PPS -----------------------------------------------------------------------
  message("  * Running PPS")
  # pull PPS concepts from each table
  input_GT_concepts_df <- input_GT_concepts(
    condition_occurrence_tbl, procedure_occurrence_tbl, observation_tbl,
    measurement_tbl, visit_occurrence_tbl, cdm$PPS_concepts
  )

  # get the gestational timing information for each concept
  get_PPS_episodes_df <- get_PPS_episodes(input_GT_concepts_df, cdm$PPS_concepts, person_tbl)

  # get the min and max dates for each episode
  PPS_episodes_df <- get_episode_max_min_dates(get_PPS_episodes_df)

  # Merge HIPPS ---------------------------------------------------------------
  message("  * Merging HIP and PPS into HIPPS")
  # collect outcomes for PPS algorithm from lookahead window
  outcomes_per_episode_df <- outcomes_per_episode(PPS_episodes_df, get_PPS_episodes_df, cdm$initial_pregnant_cohort_df)

  # add outcomes to PPS episodes
  PPS_episodes_with_outcomes_df <- add_outcomes(outcomes_per_episode_df, PPS_episodes_df)

  # bring HIP episodes into environment
  HIP_episodes_local_df <- HIP_episodes_df %>%
    dplyr::collect()

  # merge HIPS and PPS episodes
  final_merged_episodes_df <- final_merged_episodes(HIP_episodes_local_df, PPS_episodes_with_outcomes_df)

  # remove any duplicated episodes
  final_merged_episodes_no_duplicates_df <- final_merged_episodes_no_duplicates(final_merged_episodes_df)

  # add (some) demographic details
  final_merged_episode_detailed_df <- final_merged_episode_detailed(final_merged_episodes_no_duplicates_df)

  # ESD -----------------------------------------------------------------------
  message("  * Running ESD")

  # get timing concepts
  get_timing_concepts_df <- get_timing_concepts(
    concept_tbl, condition_occurrence_tbl,
    observation_tbl, measurement_tbl,
    procedure_occurrence_tbl,
    final_merged_episode_detailed_df, cdm$PPS_concepts
  )

  # get gestational timing info
  episodes_with_gestational_timing_info_df <- episodes_with_gestational_timing_info(get_timing_concepts_df)

  # merge with metadata
  merged_episodes_with_metadata_df <- merged_episodes_with_metadata(
    episodes_with_gestational_timing_info_df,
    final_merged_episode_detailed_df,
    cdm$matcho_term_durations
  )

  outputPath <- file.path(outputDir, sprintf("%s.rds", fileName))

  saveRDS(merged_episodes_with_metadata_df, outputPath)
  message(sprintf("  * Wrote output to %s", outputPath))
  return(NULL)
}
