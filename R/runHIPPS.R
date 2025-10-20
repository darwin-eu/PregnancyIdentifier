initPregnancyCohort <- function() {}

uploadConceptSets <- function(cdm) {
  HIP_concepts <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "HIP_concepts.xlsx"))
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "hip_concepts", table = HIP_concepts)

  PPS_concepts <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "PPS_concepts.xlsx")) %>%
    dplyr::mutate(domain_concept_id = as.integer(.data$domain_concept_id))
  names(PPS_concepts) <- tolower(names(PPS_concepts))
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "pps_concepts", table = PPS_concepts)

  matcho_outcome_limits <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "Matcho_outcome_limits.xlsx"))
  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "matcho_outcome_limits",
    table = matcho_outcome_limits,
    overwrite = TRUE,
    temporary = FALSE
  )

  matcho_term_durations <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "Matcho_term_durations.xlsx"))
  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "matcho_term_durations",
    table = matcho_term_durations,
    overwrite = TRUE,
    temporary = FALSE
  )
  return(cdm)
}

#' runHipps
#'
#' Runs the HIPPS algorithm (HIP, PPS, and ESD) from: https://github.com/louisahsmith/allofus-pregnancy/
#' without requiring the `allofus` R package.
#'
#' @param cdm (`cdm_reference`) A CDM-Reference object from CDMConnector.
#' @param outputDir (`character(1)`) Output directory to write output to.
#' @param fileName (`character(1)`) Filename to write to
#' @param ... Dev params
#'
#' @returns `NULL`
#'
#' @export
runHipps <- function(cdm, outputDir, fileName, ...) {
  message("> Classifying Pregnancy using HIP, PPS, and ESD")
  dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)

  cdm <- uploadConceptSets(cdm)

  hipRes <- runHip(cdm, outputDir, fileName, ...)
  ppsRes <- runPps(cdm)

  # Merge HIPPS ---------------------------------------------------------------
  # message("  * Merging HIP and PPS into HIPPS")
  # # collect outcomes for PPS algorithm from lookahead window
  # outcomes_per_episode_df <- outcomes_per_episode(PPS_episodes_df, get_PPS_episodes_df, cdm$initial_pregnant_cohort_df)
  #
  # # add outcomes to PPS episodes
  # PPS_episodes_with_outcomes_df <- add_outcomes(outcomes_per_episode_df, PPS_episodes_df)
  #
  # # bring HIP episodes into environment
  # HIP_episodes_local_df <- HIP_episodes_df %>%
  #   dplyr::collect()
  #
  # # merge HIPS and PPS episodes
  # final_merged_episodes_df <- final_merged_episodes(HIP_episodes_local_df, PPS_episodes_with_outcomes_df)
  #
  # # remove any duplicated episodes
  # final_merged_episodes_no_duplicates_df <- final_merged_episodes_no_duplicates(final_merged_episodes_df)
  #
  # # add (some) demographic details
  # final_merged_episode_detailed_df <- final_merged_episode_detailed(final_merged_episodes_no_duplicates_df)

  # ESD -----------------------------------------------------------------------
  # message("  * Running ESD")
  #
  # # get timing concepts
  # get_timing_concepts_df <- get_timing_concepts(
  #   cdm$cdm$concept, condition_occurrence,
  #   cdm$cdm$observation, measurement,
  #   cdm$procedure_occurrence,
  #   final_merged_episode_detailed_df, cdm$PPS_concepts
  # )
  #
  # # get gestational timing info
  # episodes_with_gestational_timing_info_df <- episodes_with_gestational_timing_info(get_timing_concepts_df)
  #
  # # merge with metadata
  # merged_episodes_with_metadata_df <- merged_episodes_with_metadata(
  #   episodes_with_gestational_timing_info_df,
  #   final_merged_episode_detailed_df,
  #   cdm$matcho_term_durations
  # )
  #
  # outputPath <- file.path(outputDir, sprintf("%s.rds", fileName))
  #
  # saveRDS(merged_episodes_with_metadata_df, outputPath)
  # message(sprintf("  * Wrote output to %s", outputPath))
  return(NULL)
}
