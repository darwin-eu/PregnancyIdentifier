# Unit tests for attrition tracking (initAttrition, appendAttrition, getAttritionPrior,
# getRecordAndPersonCounts, and pipeline/export integration).

test_that("initPregnancies with outputDir creates attrition.csv with initial counts", {
  cdm <- mockPregnancyCdm()
  logger <- makeLogger(tempdir(), outputLogToConsole = FALSE)
  outDir <- file.path(tempdir(), "test_attrition_init")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

  cdm <- initPregnancies(cdm, logger = logger, outputDir = outDir)

  path <- file.path(outDir, "attrition.csv")
  expect_true(file.exists(path), info = "attrition.csv should be created when outputDir is provided")

  att <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(att), 2L)
  expect_equal(att$step, c("init", "init"))
  expect_equal(att$table, c("preg_hip_records", "preg_pps_records"))
  expect_true(all(is.na(att$prior_records) | att$prior_records == ""))
  expect_true(all(is.na(att$prior_persons) | att$prior_persons == ""))
  expect_true(all(is.na(att$dropped_records) | att$dropped_records == ""))
  expect_true(all(is.na(att$dropped_persons) | att$dropped_persons == ""))
  expect_type(att$post_records, "integer")
  expect_type(att$post_persons, "integer")
  expect_gte(att$post_records[att$table == "preg_hip_records"], 0L)
  expect_gte(att$post_persons[att$table == "preg_hip_records"], 0L)
  expect_gte(att$post_records[att$table == "preg_pps_records"], 0L)
  expect_gte(att$post_persons[att$table == "preg_pps_records"], 0L)

  unlink(outDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("initPregnancies without outputDir does not create attrition.csv", {
  cdm <- mockPregnancyCdm()
  logger <- makeLogger(tempdir(), outputLogToConsole = FALSE)
  outDir <- file.path(tempdir(), "test_attrition_no_dir")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  # Use a subdir that we know is empty so we can check no attrition.csv is created there
  subDir <- file.path(outDir, "sub")
  dir.create(subDir, recursive = TRUE, showWarnings = FALSE)

  cdm <- initPregnancies(cdm, logger = logger)
  # outputDir was not passed, so no attrition file anywhere; subDir is untouched
  expect_false(file.exists(file.path(subDir, "attrition.csv")))
  expect_false(file.exists(file.path(outDir, "attrition.csv")))

  unlink(outDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("full pipeline produces attrition with expected steps and columns", {
  cdm <- mockPregnancyCdm()
  outDir <- file.path(tempdir(), "test_attrition_pipeline")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(
    cdm = cdm,
    outputDir = outDir,
    outputLogToConsole = FALSE
  )

  path <- file.path(outDir, "attrition.csv")
  expect_true(file.exists(path))

  att <- utils::read.csv(path, stringsAsFactors = FALSE)
  requiredCols <- c(
    "step", "table", "outcome",
    "prior_records", "prior_persons",
    "dropped_records", "dropped_persons",
    "post_records", "post_persons"
  )
  expect_true(all(requiredCols %in% names(att)), info = paste("Missing columns:", setdiff(requiredCols, names(att))))

  steps <- unique(att$step)
  expect_true("init" %in% steps)
  expect_true("hip_episodes" %in% steps)
  expect_true("pps_episodes" %in% steps)
  expect_true("hipps_episodes" %in% steps)
  expect_true("final_episodes" %in% steps)
  expect_true("final_episodes_by_outcome" %in% steps)

  # Init: two rows (hip, pps)
  initRows <- att[att$step == "init", ]
  expect_equal(nrow(initRows), 2L)

  # At least one overall final_episodes row
  finalOverall <- att[att$step == "final_episodes" & (is.na(att$outcome) | att$outcome == ""), ]
  expect_gte(nrow(finalOverall), 1L)

  unlink(outDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("attrition prior minus dropped equals post for pipeline steps", {
  cdm <- mockPregnancyCdm()
  outDir <- file.path(tempdir(), "test_attrition_consistency")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(
    cdm = cdm,
    outputDir = outDir,
    outputLogToConsole = FALSE
  )

  att <- utils::read.csv(file.path(outDir, "attrition.csv"), stringsAsFactors = FALSE)
  # Rows with numeric prior/dropped/post (exclude init and by-outcome)
  for (i in seq_len(nrow(att))) {
    pr <- att$prior_records[i]
    dr <- att$dropped_records[i]
    po <- att$post_records[i]
    if (is.na(pr) || pr == "" || is.na(dr) || dr == "" || is.na(po)) next
    pr <- as.integer(pr)
    dr <- as.integer(dr)
    po <- as.integer(po)
    expect_equal(pr - dr, po, info = sprintf("step=%s table=%s: prior - dropped = post", att$step[i], att$table[i]))
  }
  for (i in seq_len(nrow(att))) {
    pp <- att$prior_persons[i]
    dp <- att$dropped_persons[i]
    po <- att$post_persons[i]
    if (is.na(pp) || pp == "" || is.na(dp) || dp == "" || is.na(po)) next
    pp <- as.integer(pp)
    dp <- as.integer(dp)
    po <- as.integer(po)
    expect_equal(pp - dp, po, info = sprintf("step=%s table=%s: prior - dropped = post (persons)", att$step[i], att$table[i]))
  }

  unlink(outDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("getAttritionPrior returns last post counts for table or NULL", {
  cdm <- mockPregnancyCdm()
  logger <- makeLogger(tempdir(), outputLogToConsole = FALSE)
  outDir <- file.path(tempdir(), "test_attrition_prior")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

  cdm <- initPregnancies(cdm, logger = logger, outputDir = outDir)

  prior <- PregnancyIdentifier:::getAttritionPrior(outDir, "preg_hip_records")
  expect_type(prior, "list")
  expect_equal(names(prior), c("post_records", "post_persons"))
  expect_type(prior$post_records, "integer")
  expect_type(prior$post_persons, "integer")

  priorNone <- PregnancyIdentifier:::getAttritionPrior(outDir, "nonexistent_table")
  expect_null(priorNone)

  priorNoneDir <- PregnancyIdentifier:::getAttritionPrior(file.path(outDir, "nonexistent"), "preg_hip_records")
  expect_null(priorNoneDir)

  unlink(outDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("getRecordAndPersonCounts works on data frame and matches manual count", {
  df <- data.frame(
    person_id = c(1L, 1L, 2L, 3L),
    x = 1:4
  )
  counts <- PregnancyIdentifier:::getRecordAndPersonCounts(df)
  expect_equal(counts$records, 4L)
  expect_equal(counts$persons, 3L)
})

test_that("appendAttrition adds row and getAttritionPrior sees it", {
  cdm <- mockPregnancyCdm()
  logger <- makeLogger(tempdir(), outputLogToConsole = FALSE)
  outDir <- file.path(tempdir(), "test_attrition_append")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

  cdm <- initPregnancies(cdm, logger = logger, outputDir = outDir)
  prior <- PregnancyIdentifier:::getAttritionPrior(outDir, "preg_hip_records")
  expect_false(is.null(prior))

  PregnancyIdentifier:::appendAttrition(
    outDir,
    step = "test_step",
    table = "test_table",
    outcome = NA_character_,
    prior_records = prior$post_records,
    prior_persons = prior$post_persons,
    dropped_records = 0L,
    dropped_persons = 0L,
    post_records = prior$post_records,
    post_persons = prior$post_persons
  )

  att <- utils::read.csv(file.path(outDir, "attrition.csv"), stringsAsFactors = FALSE)
  expect_equal(nrow(att), 3L)
  expect_equal(att$step[3], "test_step")
  expect_equal(att$table[3], "test_table")

  priorTest <- PregnancyIdentifier:::getAttritionPrior(outDir, "test_table")
  expect_equal(priorTest$post_records, prior$post_records)
  expect_equal(priorTest$post_persons, prior$post_persons)

  unlink(outDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("appendAttrition errors when attrition file does not exist", {
  outDir <- file.path(tempdir(), "test_attrition_append_no_file")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  expect_false(file.exists(file.path(outDir, "attrition.csv")))

  expect_error(
    PregnancyIdentifier:::appendAttrition(
      outDir,
      step = "x",
      table = "y",
      outcome = NA_character_,
      prior_records = 0L,
      prior_persons = 0L,
      dropped_records = 0L,
      dropped_persons = 0L,
      post_records = 0L,
      post_persons = 0L
    ),
    "Attrition file not found"
  )

  unlink(outDir, recursive = TRUE)
})

test_that("exportPregnancies copies attrition.csv to exportDir", {
  cdm <- mockPregnancyCdm()
  outputDir <- file.path(tempdir(), "test_attrition_export_out")
  exportDir <- file.path(tempdir(), "test_attrition_export_dir")
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  dir.create(exportDir, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(
    cdm = cdm,
    outputDir = outputDir,
    outputLogToConsole = FALSE
  )
  expect_true(file.exists(file.path(outputDir, "attrition.csv")))

  exportPregnancies(
    cdm = cdm,
    outputDir = outputDir,
    exportDir = exportDir
  )

  exportPath <- file.path(exportDir, "attrition.csv")
  expect_true(file.exists(exportPath), info = "attrition.csv should be copied to exportDir")
  attExport <- utils::read.csv(exportPath, stringsAsFactors = FALSE)
  attOutput <- utils::read.csv(file.path(outputDir, "attrition.csv"), stringsAsFactors = FALSE)
  expect_equal(attExport, attOutput, info = "exported attrition content should match outputDir")

  unlink(outputDir, recursive = TRUE)
  unlink(exportDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})

test_that("final_episodes_by_outcome rows have outcome and post counts", {
  cdm <- mockPregnancyCdm()
  outDir <- file.path(tempdir(), "test_attrition_by_outcome")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

  runPregnancyIdentifier(
    cdm = cdm,
    outputDir = outDir,
    outputLogToConsole = FALSE
  )

  att <- utils::read.csv(file.path(outDir, "attrition.csv"), stringsAsFactors = FALSE)
  byOutcome <- att[att$step == "final_episodes_by_outcome", ]
  if (nrow(byOutcome) > 0) {
    expect_false(any(is.na(byOutcome$outcome) | byOutcome$outcome == ""))
    expect_true(all(byOutcome$post_records >= 0L))
    expect_true(all(byOutcome$post_persons >= 0L))
    # Sum of by-outcome records should not exceed total final episodes (may be equal)
    finalOverall <- att[att$step == "final_episodes" & (is.na(att$outcome) | att$outcome == ""), ]
    if (nrow(finalOverall) >= 1L) {
      totalPost <- as.integer(finalOverall$post_records[1])
      sumByOutcome <- sum(as.integer(byOutcome$post_records))
      expect_lte(sumByOutcome, totalPost + 1L) # allow small tolerance for rounding/NA handling
    }
  }

  unlink(outDir, recursive = TRUE)
  cleanupCdmDb(cdm)
})
