uploadConceptSets <- function(cdm, logger) {
  HIP_concepts <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "HIP_concepts.xlsx"))
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "hip_concepts", table = HIP_concepts)

  log4r::info(logger, "Inserted HIP concepts")

  PPS_concepts <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "PPS_concepts.xlsx")) %>%
    dplyr::mutate(domain_concept_id = as.integer(.data$domain_concept_id))
  names(PPS_concepts) <- tolower(names(PPS_concepts))
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "pps_concepts", table = PPS_concepts)

  log4r::info(logger, "Inserted PPS concepts")

  matcho_outcome_limits <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "Matcho_outcome_limits.xlsx"))
  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "matcho_outcome_limits",
    table = matcho_outcome_limits,
    overwrite = TRUE,
    temporary = FALSE
  )

  log4r::info(logger, "Inserted Matcho concepts")

  matcho_term_durations <- readxl::read_excel(system.file(package = "PregnancyIdentifier", "concepts", "Matcho_term_durations.xlsx"))
  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "matcho_term_durations",
    table = matcho_term_durations,
    overwrite = TRUE,
    temporary = FALSE
  )

  log4r::info(logger, "Inserted Matcho term durations")
  return(cdm)
}

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
#' @param ... Dev params
#'
#' @returns `NULL`
#'
#' @export
runHipps <- function(cdm, outputDir, startDate = as.Date("1900-01-01"), endDate = Sys.Date(), justGestation = TRUE, ...) {
  runStart <- data.frame(
    start = as.integer(Sys.time())
  )

  dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
  logger <- makeLogger(outputDir)

  dots <- list(...)
  log4r::info(logger, "Classifying Pregnancy using HIP, PPS, and ESD")

  write.csv(runStart, file.path(outputDir, "runStart.csv"))

  cdm <- uploadConceptSets(cdm, logger = logger)

  cdm <- runHip(cdm, outputDir, startDate = startDate, endDate = endDate, justGestation = justGestation, logger = logger, ...)
  cdm <- runPps(cdm, outputDir, startDate = startDate, endDate = endDate, justGestation = justGestation, logger = logger, ...)

  PPS_episodes_df <- readRDS(file.path(outputDir, "PPS_min_max_episodes.rds"))
  get_PPS_episodes_df <- readRDS(file.path(outputDir, "PPS_gest_timing_episodes.rds"))
  HIP_episodes_df <- readRDS(file.path(outputDir, "HIP_episodes.rds"))

  cdm <- CDMConnector::readSourceTable(cdm = cdm, name = "initial_pregnant_cohort_df")

  # Merge HIPPS ---------------------------------------------------------------
  log4r::info(logger, "START Merging HIP and PPS into HIPPS")
  # collect outcomes for PPS algorithm from lookahead window
  outcomes_per_episode_df <- outcomes_per_episode(PPS_episodes_df, get_PPS_episodes_df, cdm, logger = logger)

  # add outcomes to PPS episodes
  PPS_episodes_with_outcomes_df <- add_outcomes(outcomes_per_episode_df, PPS_episodes_df)

  # bring HIP episodes into environment
  HIP_episodes_local_df <- HIP_episodes_df %>%
    dplyr::collect()

  # merge HIPS and PPS episodes
  final_merged_episodes_df <- final_merged_episodes(HIP_episodes_local_df, PPS_episodes_with_outcomes_df, logger = logger)

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

  # add delivery mode
  merged_episodes_with_metadata_df <- addDeliveryMode(cdm, merged_episodes_with_metadata_df)

  # apply study period
  merged_episodes_with_metadata_df <- merged_episodes_with_metadata_df %>%
    filter((inferred_episode_start >= startDate | is.na(inferred_episode_start)) &
             (inferred_episode_end <= endDate | is.na(inferred_episode_end)))

  outputPath <- file.path(outputDir, "identified_pregancy_episodes.rds")

  saveRDS(merged_episodes_with_metadata_df, outputPath)
  log4r::info(logger, sprintf("Wrote output to %s", outputPath))
  return(NULL)
}

addDeliveryMode <- function(cdm, df, intersectWindow = c(-30, 30)) {
  dfColNames <- colnames(df)
  colnames(df) <- tolower(dfColNames)

  # create cdm table to be able to use conceptIntersectFlag
  tableName = "merged_episodes_with_metadata_df"
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = tableName,
                                   table = df)

  conceptSet <- CodelistGenerator::codesFromConceptSet(
    path = system.file(package = "PregnancyIdentifier", "concepts/delivery_mode"),
    cdm = cdm)
  names(conceptSet) <- unlist(lapply(names(conceptSet), FUN = function(name) {
    unlist(strsplit(name, "^\\d+-"))[2]
  }))

  result <- cdm[[tableName]] %>%
    PatientProfiles::addConceptIntersectFlag(conceptSet = conceptSet,
                                             indexDate = "inferred_episode_end",
                                             window = intersectWindow,
                                             nameStyle = '{concept_name}_{window_name}') %>%
    PatientProfiles::addConceptIntersectCount(conceptSet = conceptSet,
                                              indexDate = "inferred_episode_end",
                                              window = intersectWindow,
                                              nameStyle = '{concept_name}_{window_name}_count') %>%
    dplyr::collect()
  colnames(result)[1:length(dfColNames)] <- dfColNames
  return(result)
}
