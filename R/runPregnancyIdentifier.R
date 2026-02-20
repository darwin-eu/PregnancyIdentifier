# Copyright (c) 2024 Louisa Smith
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Create a new logger
#'
#' @param outputDir The directory where the log should be created
#' @param outputLogToConsole (`logical(1)`) If `TRUE` (default), log messages are
#'   written to the console as well as the log file. If `FALSE`, only the file
#'   appender is used (useful in tests to keep output clean).
#'
#' @returns A log4r logger object
#' @export
makeLogger <- function(outputDir, outputLogToConsole = TRUE) {
  checkmate::assertCharacter(outputDir, len = 1, any.missing = FALSE)
  checkmate::assertDirectoryExists(outputDir)
  checkmate::assertLogical(outputLogToConsole, len = 1, any.missing = FALSE)
  logFile <- file.path(outputDir, "log.txt")
  file.create(logFile)
  appenders <- list(log4r::file_appender(logFile))
  if (outputLogToConsole) {
    appenders <- c(log4r::console_appender(), appenders)
  }
  logger <- log4r::logger(threshold = "INFO", appenders = appenders)
  return(logger)
}

#' Run PregnancyIdentifier end-to-end (HIP + PPS + merge + ESD, optionally export)
#'
#' Orchestrates the full PregnancyIdentifier pipeline (adapted from the HIPPS
#' implementation at https://github.com/louisahsmith/allofus-pregnancy/) on an
#' OMOP CDM with `CDMConnector`.
#'
#' The pipeline performs:
#' 1) cohort initialization (`initPregnancies()`): creates pregnancy-related concept
#'    tables and an initial cohort in the CDM,
#' 2) HIP episode identification (`runHip()`): identifies episodes based on HIP rules,
#' 3) PPS episode identification (`runPps()`): identifies episodes based on PPS rules,
#' 4) merge (`mergeHipps()`): merges HIP and PPS into combined HIPPS episodes,
#' 5) ESD refinement (`runEsd()`): derives inferred pregnancy start/precision and
#'    enriches merged episodes,
#' 6) optionally, export (`exportPregnancies()`): writes shareable summary outputs
#'    (with optional small-cell suppression) when `exportPregnancies = TRUE`.
#'
#' @param cdm (`cdm_reference`) A CDM reference created by `CDMConnector` pointing
#'   to an OMOP CDM instance.
#' @param outputDir (`character(1)`) Directory where intermediate artifacts (RDS,
#'   logs) and exports will be written. Created if it does not exist.
#' @param startDate (`Date(1)`) Lower bound for concept/event dates included in the
#'   run. Default `1900-01-01`.
#' @param endDate (`Date(1)`) Upper bound for concept/event dates included in the
#'   run. Default `Sys.Date()`.
#' @param justGestation (`logical(1)`) If `TRUE`, allow episodes consisting only of
#'   gestational concepts (HIP behavior). Passed through to `runHip()`.
#' @param minCellCount (`integer(1)`) Minimum cell count used for suppression in
#'   exported summaries. Passed through to `exportPregnancies()` when `runExport = TRUE`.
#' @param debugMode (`logical(1)`) Should extra intermediate datasets be written to
#'   the outputDir for debugging? `TRUE` or `FALSE` (default)
#' @param runExport (`logical(1)`) If `TRUE`, run `exportPregnancies()` after
#'   ESD and write shareable CSVs and ZIP to `file.path(outputDir, "export")`.
#'   Default `FALSE`.
#' @param outputLogToConsole (`logical(1)`) If `TRUE` (default), log messages are
#'   written to the console. If `FALSE`, only to the log file (e.g. for tests).
#' @param conformToValidation (`logical(1)`) If `TRUE`, after validation modify episode
#'   output to conform: remove overlapping episodes and episodes longer than 308 days.
#'   If `FALSE` (default), only validate and log issues; do not modify the result.
#'   Validation and logging always run regardless of this parameter.
#'
#' @return Invisibly returns `NULL`. Side effects:
#'   - Adds/updates tables inside `cdm` (e.g., `cdm$preg_hip_records`, concept
#'     tables, and intermediate algorithm tables).
#'   - Writes intermediate RDS artifacts under `outputDir`.
#'   - If `runExport = TRUE`, writes shareable csv exports under
#'     `file.path(outputDir, "export")`.
#' @export
runPregnancyIdentifier <- function(cdm,
                                   outputDir,
                                   startDate = as.Date("1900-01-01"),
                                   endDate = Sys.Date(),
                                   justGestation = TRUE,
                                   minCellCount = 5L,
                                   debugMode = FALSE,
                                   runExport = FALSE,
                                   outputLogToConsole = TRUE,
                                   conformToValidation = FALSE) {

  # ---- Validate inputs -------------------------------------------------------
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertCharacter(outputDir, len = 1, any.missing = FALSE)
  checkmate::assertDate(startDate, len = 1, any.missing = FALSE)
  checkmate::assertDate(endDate, len = 1, any.missing = FALSE)
  checkmate::assertLogical(justGestation, len = 1, any.missing = FALSE)
  checkmate::assertLogical(runExport, len = 1, any.missing = FALSE)
  checkmate::assertLogical(outputLogToConsole, len = 1, any.missing = FALSE)
  checkmate::assertLogical(conformToValidation, len = 1, any.missing = FALSE)
  checkmate::assertIntegerish(minCellCount, len = 1, lower = 0)
  minCellCount <- as.integer(minCellCount)

  # ---- Prepare output location + logger --------------------------------------
  # Inputs: outputDir
  # Outputs: a created directory and a log file via `makeLogger()`
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  checkmate::assertDirectoryExists(outputDir)

  logger <- makeLogger(outputDir, outputLogToConsole = outputLogToConsole)
  pkgVersion <- as.character(utils::packageVersion("PregnancyIdentifier"))
  cdmNm <- CDMConnector::cdmName(cdm)
  if (length(cdmNm) == 0 || is.na(cdmNm)) cdmNm <- "unknown"
  log4r::info(logger, paste0("PregnancyIdentifier version: ", pkgVersion))
  log4r::info(logger, paste0("CDM name: ", cdmNm))
  log4r::info(logger, "Classifying Pregnancy using HIP, PPS, and ESD")

  runStart <- data.frame(start = as.integer(Sys.time()))
  utils::write.csv(runStart, file.path(outputDir, "runStart.csv"))

  # ---- Step 1: Initialize cohort + concept sets ------------------------------
  # Inputs:
  #   - cdm (OMOP CDM)
  #   - startDate/endDate (study window)
  # Outputs:
  #   - updated cdm with tables such as:
  #       * cdm$preg_hip_records (person_id, visit_date, category)
  #       * cdm$preg_hip_concepts
  #       * cdm$preg_pps_records
  #       * cdm$preg_pps_concepts
  log4r::info(logger, "Running `initPregnancies`")
  cdm <- initPregnancies(
    cdm,
    startDate = startDate,
    endDate = endDate,
    ageBounds = c(15L, 56L),
    logger = logger,
    outputDir = outputDir
  )

  # ---- Step 2: HIP episodes ---------------------------------------------------
  # Inputs:
  #   - cdm with initialized cohort/concepts
  #   - justGestation determines whether gestation-only episodes are allowed
  # Outputs:
  #   - updated cdm (algorithm tables created/updated)
  #   - writes: outputDir/hip_episodes.rds
  log4r::info(logger, "Running `runHip`")
  cdm <- runHip(
    cdm,
    outputDir = outputDir,
    startDate = startDate,
    endDate = endDate,
    justGestation = justGestation,
    logger = logger
  )

  # ---- Step 3: PPS episodes ---------------------------------------------------
  # Inputs:
  #   - cdm + study window
  # Outputs:
  #   - writes: outputDir/pps_gest_timing_episodes.rds
  #   - writes: outputDir/pps_min_max_episodes.rds
  #   - may add supporting PPS tables in the CDM
  log4r::info(logger, "Running `runPps`")
  cdm <- runPps(
    cdm,
    outputDir = outputDir,
    startDate = startDate,
    endDate = endDate,
    logger = logger,
    debugMode = debugMode
  )

  # ---- Step 4: Merge HIP + PPS => HIPPS episodes -----------------------------
  # Inputs:
  #   - RDS artifacts written by HIP and PPS steps in outputDir
  # Outputs:
  #   - writes: outputDir/hipps_episodes.rds
  log4r::info(logger, "Running `mergeHipps`")
  mergeHipps(outputDir = outputDir, logger = logger)

  # ---- Step 5: ESD refinement -------------------------------------------------
  # Inputs:
  #   - HIPPS (and other intermediate artifacts) in outputDir
  # Outputs:
  #   - writes: outputDir/final_pregnancy_episodes.rds (final patient-level episodes)
  log4r::info(logger, "Running `runEsd`")
  runEsd(
    cdm,
    outputDir = outputDir,
    startDate = startDate,
    endDate = endDate,
    logger = logger,
    debugMode = debugMode,
    conformToValidation = conformToValidation
  )

  if (runExport) {
    log4r::info(logger, "Running `exportPregnancies`")
    exportPregnancies(
      cdm = cdm,
      outputDir = outputDir,
      exportDir = file.path(outputDir, "export"),
      minCellCount = minCellCount
    )
  }

  invisible(NULL)
}
