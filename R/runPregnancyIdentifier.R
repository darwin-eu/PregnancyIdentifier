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

#' runPregancyIdentifier
#'
#' Runs the HIPPS algorithm (HIP, PPS, and ESD) from: https://github.com/louisahsmith/allofus-pregnancy/
#' without requiring the `allofus` R package.
#'
#' @param cdm (`cdm_reference`) A CDM-Reference object from CDMConnector.
#' @param outputDir (`character(1)`) Output directory to write output to.
#' @param startDate (`Date(1)`: `as.Date("1900-01-01"`) Start date of data to use. By default 1900-01-01
#' @param endDate (`Date(1)`: `Sys.Date()`) End date of data to use. By default today.
#' @param justGestation (`logical(1)`: `TRUE`) Should episodes that only have gestational concepts be considered?
#' @param minCellCount (`integer(1)`: `5`) What should the minimum cell count be in the results?
#'
#' @returns `NULL`
#'
#' @export
runPregancyIdentifier <- function(cdm, outputDir, startDate = as.Date("1900-01-01"), endDate = Sys.Date(), justGestation = TRUE, minCellCount = 5L) {

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

  log4r::info(logger, "running `mergeHips`")
  # reads PPS_min_max_episodes.rds, PPS_gest_timing_episodes.rds, HIP_episodes.rds from outputDir
  # writes HIPPS_episodes.rds to outputDir
  mergeHips(cdm, outputDir = outputDir, logger = logger)

  log4r::info(logger, "running `runEsd`")
  runEsd(cdm, outputDir = outputDir, startDate = startDate, endDate = endDate, logger = logger)

  log4r::info(logger, "running `exportPregnancies`")
  exportPregnancies(cdm, outputDir = outputDir, exportDir = file.path(outputDir, "export"), minCellCount = minCellCount)
  return(NULL)
}
