# mod_run_identifier.R
# Run Pregnancy Identifier: export test data → CDM → runPregnancyIdentifier().
# Zero-row safe: ensure_outputs_exist() guarantees expected RDS/CSV; Run log panel; HIP=0 messaging.

#' Run Identifier UI (sidebar + main as one layout, for use in a single card/column)
#'
#' @param id Module ID
#' @return UI elements
#' @export
mod_run_identifier_ui <- function(id) {
  bslib::layout_sidebar(
    sidebar = mod_run_identifier_sidebar_ui(id),
    main = mod_run_identifier_main_ui(id)
  )
}

#' Run Identifier sidebar UI (options + Run button)
#'
#' @param id Module ID
#' @return UI elements for sidebar
#' @export
mod_run_identifier_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Options"),
    bslib::card_body(
      shiny::div(
        class = "form-group",
        shiny::tags$label(shiny::tags$span("Output directory", class = "control-label")),
        shiny::div(
          style = "display: flex; gap: 0.5rem; align-items: center;",
          shiny::textInput(ns("output_dir"),
            label = NULL,
            value = "",
            placeholder = "e.g. ./_output or leave empty for temp"
          ),
          shiny::actionButton(ns("use_temp_dir"), "Use temp folder", class = "btn-sm")
        )
      ),
      shiny::dateInput(ns("start_date"), "Start date", value = as.Date("1900-01-01")),
      shiny::dateInput(ns("end_date"), "End date", value = Sys.Date()),
      shiny::checkboxInput(ns("just_gestation"), "Just gestation", value = TRUE),
      shiny::numericInput(ns("min_cell_count"), "Min cell count", value = 0L, min = 0, step = 1),
      shiny::checkboxInput(ns("debug_mode"), "Debug mode", value = FALSE),
      shiny::hr(),
      shiny::actionButton(ns("run"), "Run", class = "btn-primary", style = "width: 100%;")
    )
  )
}

#' Run Identifier main UI (status, outcome, error, files, run log)
#'
#' @param id Module ID
#' @return UI elements for main content
#' @export
mod_run_identifier_main_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Run Pregnancy Identifier"),
    bslib::card_body(
      shiny::verbatimTextOutput(ns("status")),
      shiny::uiOutput(ns("outcome_ui")),
      shiny::uiOutput(ns("error_ui")),
      shiny::uiOutput(ns("files_ui")),
      shiny::hr(),
      shiny::tags$strong("Run log"),
      shiny::tags$div(
        style = "max-height: 280px; overflow-y: auto; font-family: monospace; font-size: 0.8rem; white-space: pre-wrap; background: #f8f9fa; padding: 0.5rem; border-radius: 4px;",
        shiny::verbatimTextOutput(ns("run_log"))
      ),
      shiny::uiOutput(ns("log_txt_ui"))
    )
  )
}

#' Run Identifier Server
#'
#' @param id Module ID
#' @param test_data_r Reactive state (OMOP CDM state list)
#' @param current_person_id_r Optional reactive current person ID for subsetting CDM; if NULL, run on full CDM
#' @return List with \code{output_dir} (reactive character) and \code{files} (reactive character vector)
#' @export
mod_run_identifier_server <- function(id, test_data_r, current_person_id_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output_dir_r <- shiny::reactiveVal(NULL)
    last_run_time_r <- shiny::reactiveVal(NULL)
    error_msg_r <- shiny::reactiveVal(NULL)
    produced_files_r <- shiny::reactiveVal(character(0))
    outcome_msg_r <- shiny::reactiveVal(NULL)
    run_log_lines_r <- shiny::reactiveVal(character(0))
    new_files_r <- shiny::reactiveVal(character(0))

    append_log <- function(line) {
      prev <- shiny::isolate(run_log_lines_r())
      run_log_lines_r(tail(c(prev, line), 200L))
    }

    shiny::observeEvent(input$use_temp_dir, {
      d <- file.path(tempdir(), "PregnancyIdentifier_output")
      shiny::updateTextInput(session, "output_dir", value = d)
    })

    shiny::observeEvent(input$run, {
      error_msg_r(NULL)
      outcome_msg_r(NULL)
      test_data <- test_data_r()
      if (is.null(test_data)) {
        shiny::showNotification("No test data. Add or import data first.", type = "warning")
        return()
      }

      required <- c("PregnancyIdentifier", "TestGenerator", "CDMConnector", "readxl", "openxlsx")
      missing <- required[!vapply(required, function(p) requireNamespace(p, quietly = TRUE), logical(1))]
      if (length(missing) > 0) {
        err <- paste("Missing required packages:", paste(missing, collapse = ", "))
        error_msg_r(err)
        shiny::showNotification(err, type = "error", duration = 8)
        return()
      }

      out_dir <- trimws(input$output_dir)
      if (out_dir == "") {
        out_dir <- file.path(tempdir(), "PregnancyIdentifier_output")
      }
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(out_dir)) {
        err <- paste("Could not create output directory:", out_dir)
        error_msg_r(err)
        shiny::showNotification(err, type = "error")
        return()
      }

      log_app("Run started.", outputDir = out_dir, append_cb = append_log)
      files_before <- character(0)
      if (dir.exists(out_dir)) {
        files_before <- list.files(out_dir, recursive = TRUE, full.names = FALSE)
        export_sub <- file.path(out_dir, "export")
        if (dir.exists(export_sub)) {
          files_before <- c(files_before, file.path("export", list.files(export_sub, recursive = FALSE)))
        }
      }

      test_name <- "test"
      temp_excel <- tempfile(fileext = ".xlsx")
      temp_json_dir <- tempfile(pattern = "json_")
      dir.create(temp_json_dir, recursive = TRUE, showWarnings = FALSE)

      result <- tryCatch(
        {
          state_to_excel(test_data, temp_excel)
          TestGenerator::readPatients(
            filePath = temp_excel,
            testName = test_name,
            outputPath = temp_json_dir,
            extraTable = TRUE
          )
          cdm <- TestGenerator::patientsCDM(
            pathJson = temp_json_dir,
            testName = test_name
          )
          pid <- if (!is.null(current_person_id_r)) current_person_id_r() else NULL
          if (!is.null(pid)) {
            cdm <- CDMConnector::cdmSubset(cdm, personId = pid)
          }
          PregnancyIdentifier::runPregnancyIdentifier(
            cdm = cdm,
            outputDir = out_dir,
            startDate = input$start_date,
            endDate = input$end_date,
            justGestation = input$just_gestation,
            minCellCount = as.integer(input$min_cell_count),
            debugMode = input$debug_mode
          )
          rds_files <- list.files(out_dir, pattern = "\\.rds$", full.names = FALSE)
          export_dir <- file.path(out_dir, "export")
          csv_files <- if (dir.exists(export_dir)) {
            list.files(export_dir, pattern = "\\.csv$", full.names = FALSE)
          } else {
            character(0)
          }
          list(
            output_dir = out_dir,
            rds = rds_files,
            csv = csv_files,
            error = NULL
          )
        },
        error = function(e) {
          log_app(
            paste("Run error:", conditionMessage(e)),
            outputDir = out_dir,
            level = "ERROR",
            append_cb = append_log
          )
          list(
            output_dir = out_dir,
            rds = character(0),
            csv = character(0),
            error = conditionMessage(e)
          )
        }
      )

      run_failed <- !is.null(result$error)
      ensure_outputs_exist(
        outputDir = result$output_dir,
        debugMode = isTRUE(input$debug_mode),
        log_append_cb = append_log,
        run_failed = run_failed
      )

      hip_count <- detect_hip_zero(result$output_dir)
      if (run_failed) {
        error_msg_r(result$error)
        shiny::showNotification(paste("Run failed:", result$error), type = "error", duration = 8)
        outcome_msg_r("Run failed; placeholder outputs were created so browsers and exports do not error.")
      } else {
        output_dir_r(result$output_dir)
        last_run_time_r(Sys.time())
        all_files <- c(result$rds, file.path("export", result$csv))
        produced_files_r(all_files)
        if (!is.na(hip_count) && hip_count == 0) {
          outcome_msg_r("Run completed: 0 HIP episodes found. Placeholder outputs written (0 rows).")
          log_app("Run completed: 0 HIP episodes found.", outputDir = result$output_dir, append_cb = append_log)
          shiny::showNotification("0 HIP episodes found. Outputs are 0-row placeholders.", type = "message", duration = 5)
        } else if (!is.na(hip_count) && hip_count > 0) {
          outcome_msg_r(sprintf("Run completed: %d HIP episodes found.", hip_count))
          log_app(sprintf("Run completed: %d HIP episodes found.", hip_count), outputDir = result$output_dir, append_cb = append_log)
          shiny::showNotification(sprintf("Run completed: %d HIP episodes found.", hip_count), type = "message")
        } else {
          outcome_msg_r("Run completed; HIP episode count could not be determined. Outputs ensured.")
          log_app("Run completed; HIP count could not be determined. Outputs ensured.", outputDir = result$output_dir, append_cb = append_log)
          shiny::showNotification("Run completed. Outputs ensured.", type = "message")
        }
      }

      files_after <- character(0)
      if (dir.exists(result$output_dir)) {
        files_after <- list.files(result$output_dir, recursive = TRUE, full.names = FALSE)
        export_sub <- file.path(result$output_dir, "export")
        if (dir.exists(export_sub)) {
          files_after <- c(files_after, file.path("export", list.files(export_sub, recursive = FALSE)))
        }
      }
      new_files_r(setdiff(files_after, files_before))

      if (file.exists(temp_excel)) unlink(temp_excel, force = TRUE)
      if (dir.exists(temp_json_dir)) unlink(temp_json_dir, recursive = TRUE, force = TRUE)
    })

    output$status <- shiny::renderText({
      out <- output_dir_r()
      t <- last_run_time_r()
      if (is.null(out)) {
        return("No run yet. Set output directory and click Run.")
      }
      lines <- c(
        paste("Last output dir:", out),
        if (!is.null(t)) paste("Last run:", format(t, "%Y-%m-%d %H:%M:%S"))
      )
      paste(lines, collapse = "\n")
    })

    output$outcome_ui <- shiny::renderUI({
      msg <- outcome_msg_r()
      if (is.null(msg) || identical(msg, "")) return(NULL)
      shiny::tags$div(
        class = "alert alert-info",
        role = "alert",
        shiny::tags$strong("Outcome:"),
        shiny::tags$p(msg, style = "margin-top: 0.25rem; margin-bottom: 0;")
      )
    })

    output$error_ui <- shiny::renderUI({
      err <- error_msg_r()
      if (is.null(err) || identical(err, "")) return(NULL)
      shiny::tags$div(
        class = "alert alert-danger",
        role = "alert",
        shiny::tags$strong("Error:"),
        shiny::tags$pre(err, style = "white-space: pre-wrap; margin-top: 0.5rem;")
      )
    })

    output$files_ui <- shiny::renderUI({
      files <- produced_files_r()
      new_f <- new_files_r()
      out <- list()
      if (length(files) > 0) {
        out <- c(out, list(
          shiny::tags$strong("Produced files:"),
          shiny::tags$ul(lapply(files, function(f) shiny::tags$li(shiny::tags$code(f))))
        ))
      }
      if (length(new_f) > 0) {
        out <- c(out, list(
          shiny::tags$strong("New/updated this run:", shiny::tags$code(paste(new_f, collapse = ", ")))
        ))
      }
      if (length(out) == 0) return(NULL)
      shiny::tagList(out)
    })

    output$run_log <- shiny::renderText({
      lines <- run_log_lines_r()
      if (length(lines) == 0) return("(No log yet. Run the algorithm to see messages here and in outputDir/log.txt.)")
      paste(tail(lines, 200L), collapse = "\n")
    })

    output$log_txt_ui <- shiny::renderUI({
      out <- output_dir_r()
      if (is.null(out)) return(NULL)
      log_path <- file.path(out, "log.txt")
      if (!file.exists(log_path)) return(NULL)
      lines <- tryCatch(
        readLines(log_path, warn = FALSE),
        error = function(e) character(0)
      )
      tail_lines <- tail(lines, 200L)
      preview_text <- if (length(tail_lines) > 0) {
        paste(tail_lines, collapse = "\n")
      } else {
        "(empty)"
      }
      shiny::tags$div(
        shiny::tags$small("Log file: ", shiny::tags$code(log_path)),
        shiny::br(),
        shiny::downloadButton(ns("download_log_txt"), "Download log.txt", class = "btn-sm"),
        shiny::tags$details(
          shiny::tags$summary("View full log.txt (last 200 lines)", style = "cursor: pointer; margin-top: 0.5rem;"),
          shiny::tags$pre(
            preview_text,
            style = "font-size: 0.75rem; max-height: 200px; overflow: auto; background: #f8f9fa; padding: 0.5rem; margin-top: 0.25rem; white-space: pre-wrap;"
          )
        )
      )
    })

    output$download_log_txt <- shiny::downloadHandler(
      filename = function() "log.txt",
      content = function(file) {
        out <- output_dir_r()
        if (!is.null(out)) {
          src <- file.path(out, "log.txt")
          if (file.exists(src)) file.copy(src, file, overwrite = TRUE)
        }
      },
      contentType = "text/plain"
    )

    list(
      output_dir = shiny::reactive(output_dir_r()),
      files = produced_files_r
    )
  })
}
