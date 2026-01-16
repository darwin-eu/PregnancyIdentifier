makeLogger <- function(outputDir) {
  logFile <- file.path(outputDir, "log.txt")
  file.create(logFile)
  logger <- log4r::create.logger(logfile = logFile)
  logger <- log4r::logger(threshold = "INFO", appenders = list(
    log4r::console_appender(),
    log4r::file_appender(logFile)
  ))
  return(logger)
}

outputDir = here::here("testOutput")
startDate = as.Date("1900-01-01"); endDate = Sys.Date(); justGestation = TRUE

#' runHipps
#'
#' Runs the HIPPS algorithm (HIP, PPS, and ESD) from: https://github.com/louisahsmith/allofus-pregnancy/
#' without requiring the `allofus` R package.
#'
#' @param cdm (`cdm_reference`) A CDM-Reference object from CDMConnector.
#' @param outputDir (`character(1)`) Output directory to write output to.
#' @param startDate (`Date(1)`: `as.Date("1900-01-01"`) Start date of data to use. By default 1900-01-01
#' @param endDate (`Date(1)`: `Sys.Date()`) End date of data to use. By default today.
#' @param justGestation (`logical(1)`: `TRUE`) Should episodes that only have gestational concepts be concidered?
#'
#' @returns `NULL`
#'
#' @export
runHipps <- function(cdm, outputDir, startDate = as.Date("1900-01-01"), endDate = Sys.Date(), justGestation = TRUE) {

  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertDate(startDate, len = 1, any.missing = FALSE)
  checkmate::assertDate(endDate, len = 1, any.missing = FALSE)
  checkmate::assertLogical(justGestation, len = 1, any.missing = FALSE)
  checkmate::assertCharacter(outputDir, len = 1, any.missing = FALSE)

  runStart <- data.frame(
    start = as.integer(Sys.time())
  )

  dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
  checkmate::assertDirectoryExists(outputDir)

  logger <- makeLogger(outputDir)

  log4r::info(logger, "Classifying Pregnancy using HIP, PPS, and ESD")

  log4r::info(logger, "running `initPregnancies`")
  # Add a table cdm$preg_initial_cohort (person_id, visit_date, category)
  # Also inserts the cdm$preg_hip_concepts table (concept_id, concept_name, category, gest_value)
  cdm <- initPregnancies(cdm, startDate = startDate, endDate = endDate, ageBounds = c(15L, 56L))

  log4r::info(logger, "running `runHip`")
  # Adds cdm$preg_hip_episodes (person_id gest_date  category visit_date estimated_start_date episode gest_flag episode_length)
  # Also writes this table to outputDir/HIP_episodes.rds
  cdm <- runHip(cdm, outputDir, startDate = startDate, endDate = endDate, justGestation = justGestation, logger = logger)

  log4r::info(logger, "running `runPps`")
  cdm <- runPps(cdm, outputDir, startDate = startDate, endDate = endDate, logger = logger)

  PPS_episodes_df <- readRDS(file.path(outputDir, "PPS_min_max_episodes.rds"))
  get_PPS_episodes_df <- readRDS(file.path(outputDir, "PPS_gest_timing_episodes.rds"))
  HIP_episodes_df <- readRDS(file.path(outputDir, "HIP_episodes.rds"))


  # Merge HIPPS ---------------------------------------------------------------
  log4r::info(logger, "START Merging HIP and PPS into HIPPS")
  # collect outcomes for PPS algorithm from lookahead window
  outcomes_per_episode_df <- outcomes_per_episode(PPS_episodes_df, get_PPS_episodes_df, cdm, logger = logger)

  # add outcomes to PPS episodes
  PPS_episodes_with_outcomes_df <- add_outcomes(outcomes_per_episode_df, PPS_episodes_df)

  # merge HIPS and PPS episodes
  final_merged_episodes_df <- final_merged_episodes(HIP_episodes_df, PPS_episodes_with_outcomes_df, logger = logger)

  # remove any duplicated episodes
  final_merged_episodes_no_duplicates_df <- final_merged_episodes_no_duplicates(final_merged_episodes_df, logger = logger)

  # add (some) demographic details
  final_merged_episode_detailed_df <- final_merged_episode_detailed(final_merged_episodes_no_duplicates_df)

  # ESD -----------------------------------------------------------------------
  log4r::info(logger, "START Running ESD")

  # get timing concepts
  get_timing_concepts_df <- get_timing_concepts(
    cdm,
    final_merged_episode_detailed_df,
    startDate = startDate,
    endDate = endDate
  )

  # get gestational timing info
  episodes_with_gestational_timing_info_df <- episodes_with_gestational_timing_info(get_timing_concepts_df, logger = logger)

  # merge with metadata
  merged_episodes_with_metadata_df <- merged_episodes_with_metadata(
    episodes_with_gestational_timing_info_df,
    final_merged_episode_detailed_df,
    cdm,
    logger = logger
  )

  # apply study period
  merged_episodes_with_metadata_df <- merged_episodes_with_metadata_df %>%
    filter((inferred_episode_start >= startDate | is.na(inferred_episode_start)) &
             (inferred_episode_end <= endDate | is.na(inferred_episode_end)))

  outputPath <- file.path(outputDir, "identified_pregancy_episodes.rds")

  saveRDS(merged_episodes_with_metadata_df, outputPath)
  log4r::info(logger, sprintf("Wrote output to %s", outputPath))
  return(NULL)
}
