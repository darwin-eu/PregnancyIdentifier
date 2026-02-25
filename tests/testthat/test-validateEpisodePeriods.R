# Unit tests for PregnancyIdentifier:::validateEpisodePeriods()

test_that("validateEpisodePeriods returns df invisibly unchanged", {
  outputFolder <- file.path(tempdir(), "test_validate_ep_invisible")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)

  df <- data.frame(
    person_id = 1L,
    start = as.Date("2020-01-01"),
    end = as.Date("2020-06-01")
  )
  out <- PregnancyIdentifier:::validateEpisodePeriods(
    df,
    personIdCol = "person_id",
    startDateCol = "start",
    endDateCol = "end",
    logger = logger
  )
  expect_identical(out, df)
  expect_invisible(PregnancyIdentifier:::validateEpisodePeriods(df, "person_id", "start", "end", logger))

  unlink(outputFolder, recursive = TRUE)
})

test_that("empty df produces no warnings", {
  outputFolder <- file.path(tempdir(), "test_validate_ep_empty")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)

  df <- data.frame(
    person_id = integer(),
    start = as.Date(character()),
    end = as.Date(character())
  )
  PregnancyIdentifier:::validateEpisodePeriods(df, "person_id", "start", "end", logger = logger)

  logLines <- readLines(file.path(outputFolder, "log.txt"), warn = FALSE)
  expect_false(any(grepl("overlap with another record", logLines)))
  expect_false(any(grepl("period length greater than", logLines)))

  unlink(outputFolder, recursive = TRUE)
})

test_that("no overlaps and no long periods produces no warnings and logs validation messages", {
  outputFolder <- file.path(tempdir(), "test_validate_ep_clean")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)

  # Two people, one episode each; spans under 308 days
  df <- data.frame(
    person_id = c(1L, 2L),
    start = as.Date(c("2020-01-01", "2021-01-01")),
    end = as.Date(c("2020-10-01", "2021-10-01"))
  )
  PregnancyIdentifier:::validateEpisodePeriods(df, "person_id", "start", "end", logger = logger)

  logLines <- readLines(file.path(outputFolder, "log.txt"), warn = FALSE)
  expect_false(any(grepl("overlap with another record", logLines)))
  expect_false(any(grepl("period length greater than", logLines)))
  expect_true(any(grepl("validation: no overlapping episodes found", logLines)))
  expect_true(any(grepl("validation: all episodes are less than", logLines)))

  unlink(outputFolder, recursive = TRUE)
})

test_that("overlapping periods within person produce overlap warning", {
  outputFolder <- file.path(tempdir(), "test_validate_ep_overlap")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)

  # Person 1: two overlapping periods (Jan–Dec and Jun–Dec)
  df <- data.frame(
    person_id = c(1L, 1L, 2L),
    start = as.Date(c("2020-01-01", "2020-06-01", "2020-01-01")),
    end = as.Date(c("2020-12-01", "2020-12-01", "2020-11-01"))
  )
  PregnancyIdentifier:::validateEpisodePeriods(df, "person_id", "start", "end", logger = logger)

  logLines <- readLines(file.path(outputFolder, "log.txt"), warn = FALSE)
  expect_true(any(grepl("overlap with another record", logLines)))
  expect_true(any(grepl("2 record\\(s\\) overlap", logLines)))

  unlink(outputFolder, recursive = TRUE)
})

test_that("periods longer than maxDays produce long-period warning", {
  outputFolder <- file.path(tempdir(), "test_validate_ep_long")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)

  # One episode of 400 days (> 308)
  df <- data.frame(
    person_id = 1L,
    start = as.Date("2020-01-01"),
    end = as.Date("2021-02-05") # 400 days
  )
  PregnancyIdentifier:::validateEpisodePeriods(df, "person_id", "start", "end", logger = logger)

  logLines <- readLines(file.path(outputFolder, "log.txt"), warn = FALSE)
  expect_true(any(grepl("period length greater than", logLines)))
  expect_true(any(grepl("1 record\\(s\\) have period length", logLines)))

  unlink(outputFolder, recursive = TRUE)
})

test_that("custom maxDays is respected", {
  outputFolder <- file.path(tempdir(), "test_validate_ep_maxdays")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)

  # 100 days: over 50, under 308
  df <- data.frame(
    person_id = 1L,
    start = as.Date("2020-01-01"),
    end = as.Date("2020-04-10")
  )
  PregnancyIdentifier:::validateEpisodePeriods(df, "person_id", "start", "end", logger = logger, maxDays = 50)

  logLines <- readLines(file.path(outputFolder, "log.txt"), warn = FALSE)
  expect_true(any(grepl("period length greater than 50 days", logLines)))

  unlink(outputFolder, recursive = TRUE)
})

test_that("NA start or end are excluded from checks", {
  outputFolder <- file.path(tempdir(), "test_validate_ep_na")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)

  # One complete row (no overlap, short span), one with NA end
  df <- data.frame(
    person_id = c(1L, 1L),
    start = as.Date(c("2020-01-01", "2020-06-01")),
    end = as.Date(c("2020-03-01", NA))
  )
  expect_silent(
    PregnancyIdentifier:::validateEpisodePeriods(df, "person_id", "start", "end", logger = logger)
  )

  # Only the complete row is considered; no overlap with self
  logLines <- readLines(file.path(outputFolder, "log.txt"), warn = FALSE)
  expect_false(any(grepl("overlap with another record", logLines)))
  expect_false(any(grepl("period length greater than", logLines)))

  unlink(outputFolder, recursive = TRUE)
})

test_that("all NA start/end yields no warnings", {
  outputFolder <- file.path(tempdir(), "test_validate_ep_all_na")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)

  df <- data.frame(
    person_id = c(1L, 2L),
    start = as.Date(c(NA, NA)),
    end = as.Date(c(NA, NA))
  )
  PregnancyIdentifier:::validateEpisodePeriods(df, "person_id", "start", "end", logger = logger)

  logLines <- readLines(file.path(outputFolder, "log.txt"), warn = FALSE)
  expect_false(any(grepl("overlap with another record", logLines)))
  expect_false(any(grepl("period length greater than", logLines)))

  unlink(outputFolder, recursive = TRUE)
})

test_that("missing column names error", {
  outputFolder <- file.path(tempdir(), "test_validate_ep_bad_col")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)

  df <- data.frame(
    person_id = 1L,
    start = as.Date("2020-01-01"),
    end = as.Date("2020-06-01")
  )
  expect_error(
    PregnancyIdentifier:::validateEpisodePeriods(df, "person_id", "wrong_start", "end", logger = logger),
    "names\\(df\\)"
  )
  expect_error(
    PregnancyIdentifier:::validateEpisodePeriods(df, "person_id", "start", "wrong_end", logger = logger),
    "names\\(df\\)"
  )

  unlink(outputFolder, recursive = TRUE)
})

test_that("non-overlapping periods within person do not trigger overlap warning", {
  outputFolder <- file.path(tempdir(), "test_validate_ep_no_overlap")
  dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  logger <- PregnancyIdentifier:::makeLogger(outputFolder, outputLogToConsole = FALSE)

  # Person 1: Jan–Mar and Jun–Sep (no overlap)
  df <- data.frame(
    person_id = c(1L, 1L),
    start = as.Date(c("2020-01-01", "2020-06-01")),
    end = as.Date(c("2020-03-01", "2020-09-01"))
  )
  PregnancyIdentifier:::validateEpisodePeriods(df, "person_id", "start", "end", logger = logger)

  logLines <- readLines(file.path(outputFolder, "log.txt"), warn = FALSE)
  expect_false(any(grepl("overlap with another record", logLines)))

  unlink(outputFolder, recursive = TRUE)
})
