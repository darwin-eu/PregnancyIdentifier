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

#' Run PregnancyIdentifier end-to-end (HIP + PPS + merge + ESD + export)
#'
#' Orchestrates the full PregnancyIdentifier pipeline (adapted from the HIPPS
#' implementation at https://github.com/louisahsmith/allofus-pregnancy/) on an
#' OMOP CDM via `CDMConnector`, without requiring the `allofus` R package.
#'
#' The pipeline performs:
#' 1) cohort initialization (`initPregnancies()`): creates pregnancy-related concept
#'    tables and an initial cohort in the CDM,
#' 2) HIP episode identification (`runHip()`): identifies episodes based on HIP rules,
#' 3) PPS episode identification (`runPps()`): identifies episodes based on PPS rules,
#' 4) merge (`mergeHipps()`): merges HIP and PPS into combined HIPPS episodes,
#' 5) ESD refinement (`runEsd()`): derives inferred pregnancy start/precision and
#'    enriches merged episodes,
#' 6) export (`exportPregnancies()`): writes shareable summary outputs (with optional
#'    small-cell suppression).
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
#'   exported summaries. Passed through to `exportPregnancies()`.
#'
#' @return Invisibly returns `NULL`. Side effects:
#'   - Adds/updates tables inside `cdm` (e.g., `cdm$preg_initial_cohort`, concept
#'     tables, and intermediate algorithm tables).
#'   - Writes intermediate RDS artifacts under `outputDir`.
#'   - Writes shareable exports under `file.path(outputDir, "export")`.
#' @export
runPregnancyIdentifier <- function(cdm,
                                   outputDir,
                                   startDate = as.Date("1900-01-01"),
                                   endDate = Sys.Date(),
                                   justGestation = TRUE,
                                   minCellCount = 5L) {

  # ---- Validate inputs -------------------------------------------------------
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertCharacter(outputDir, len = 1, any.missing = FALSE)
  checkmate::assertDate(startDate, len = 1, any.missing = FALSE)
  checkmate::assertDate(endDate, len = 1, any.missing = FALSE)
  checkmate::assertLogical(justGestation, len = 1, any.missing = FALSE)
  checkmate::assertIntegerish(minCellCount, len = 1, lower = 0)
  minCellCount <- as.integer(minCellCount)

  # ---- Prepare output location + logger --------------------------------------
  # Inputs: outputDir
  # Outputs: a created directory and a log file via `makeLogger()`
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  checkmate::assertDirectoryExists(outputDir)

  logger <- makeLogger(outputDir)
  log4r::info(logger, "Classifying Pregnancy using HIP, PPS, and ESD")

  runStart <- data.frame(start = as.integer(Sys.time()))
  utils::write.csv(runStart, file.path(outputDir, "runStart.csv"))

  # ---- Step 1: Initialize cohort + concept sets ------------------------------
  # Inputs:
  #   - cdm (OMOP CDM)
  #   - startDate/endDate (study window)
  # Outputs:
  #   - updated cdm with tables such as:
  #       * cdm$preg_initial_cohort (person_id, visit_date, category)
  #       * cdm$preg_hip_concepts (concept_id, concept_name, category, gest_value)
  log4r::info(logger, "Running `initPregnancies`")
  cdm <- initPregnancies(
    cdm,
    startDate = startDate,
    endDate = endDate,
    ageBounds = c(15L, 56L)
  )

  # ---- Step 2: HIP episodes ---------------------------------------------------
  # Inputs:
  #   - cdm with initialized cohort/concepts
  #   - justGestation determines whether gestation-only episodes are allowed
  # Outputs:
  #   - updated cdm (algorithm tables created/updated)
  #   - writes: outputDir/HIP_episodes.rds
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
    logger = logger
  )

  # ---- Step 4: Merge HIP + PPS => HIPPS episodes -----------------------------
  # Inputs:
  #   - RDS artifacts written by HIP and PPS steps in outputDir
  # Outputs:
  #   - writes: outputDir/HIPPS_episodes.rds
  log4r::info(logger, "Running `mergeHipps`")
  mergeHipps(outputDir = outputDir, logger = logger)

  # ---- Step 5: ESD refinement -------------------------------------------------
  # Inputs:
  #   - HIPPS (and other intermediate artifacts) in outputDir
  # Outputs:
  #   - writes: outputDir/identified_pregancy_episodes.rds (final patient-level episodes)
  log4r::info(logger, "Running `runEsd`")
  runEsd(
    cdm,
    outputDir = outputDir,
    startDate = startDate,
    endDate = endDate,
    logger = logger
  )

  invisible(NULL)
}
