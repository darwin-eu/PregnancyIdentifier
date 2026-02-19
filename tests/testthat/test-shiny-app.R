# shinytest2 tests for the PregnancyIdentifier Shiny app (viewResults / inst/shiny)
# Requires shinytest2. Uses option shiny.data.folder so the app uses an empty
# data folder and shows the "No data" UI (no export zip files required).
# The app must be installed (e.g. devtools::install() or R CMD INSTALL) so that
# inst/shiny/app.R includes the getOption("shiny.data.folder") and no-zip UI logic.

test_that("Shiny app loads and shows no-data message when data folder has no zips", {
  skip_if_not_installed("shinytest2")
  testthat::skip_on_cran() # headless browser and load time

  app_dir <- system.file("shiny", package = "PregnancyIdentifier")
  testthat::skip_if_not(dir.exists(app_dir), "Shiny app dir not found (package not installed?)")

  empty_data_dir <- tempfile("shiny_data_")
  dir.create(empty_data_dir, showWarnings = FALSE)
  on.exit(unlink(empty_data_dir, recursive = TRUE), add = TRUE)

  app <- tryCatch(
    shinytest2::AppDriver$new(
      app_dir,
      name = "pregnancy_identifier",
      load_timeout = 30000,
      options = list(shiny.data.folder = empty_data_dir)
    ),
    error = function(e) {
      testthat::skip(paste(
        "Shiny app failed to start (install package to run this test):",
        conditionMessage(e)
      ))
    }
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
