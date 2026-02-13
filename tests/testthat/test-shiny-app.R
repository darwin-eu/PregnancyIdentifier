# shinytest2 tests for the PregnancyIdentifier Shiny app (viewResults / inst/shiny)
# Requires shinytest2. Uses SHINY_DATA_FOLDER to point to an empty dir so the app
# shows the "No data" UI and we can assert on it without export zip files.

test_that("Shiny app loads and shows no-data message when data folder has no zips", {
  skip_if_not_installed("shinytest2")
  testthat::skip_on_cran() # headless browser and load time

  app_dir <- system.file("shiny", package = "PregnancyIdentifier")
  testthat::skip_if_not(dir.exists(app_dir), "Shiny app dir not found (package not installed?)")

  # Force app to use an empty data folder (inherited by shinytest2's child R process)
  empty_data_dir <- tempfile("shiny_data_")
  dir.create(empty_data_dir, showWarnings = FALSE)
  on.exit(unlink(empty_data_dir, recursive = TRUE), add = TRUE)
  Sys.setenv(SHINY_DATA_FOLDER = empty_data_dir)
  on.exit(Sys.unsetenv("SHINY_DATA_FOLDER"), add = TRUE)

  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "pregnancy_identifier",
    load_timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)

  # With no zip files, app shows the minimal "No data available" UI
  page_text <- app$get_text("body")
  expect_true(
    grepl("PregnancyIdentifier", page_text, fixed = TRUE),
    info = paste("Page should show title. Snapshot:", substr(page_text, 1L, 500L))
  )
  expect_true(
    grepl("No data available", page_text, fixed = TRUE),
    info = paste("Page should show no-data message. Snapshot:", substr(page_text, 1L, 500L))
  )
})
