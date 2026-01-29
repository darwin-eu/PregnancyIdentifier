# mod_outputs_browser.R
# Browse and preview RDS or CSV outputs; download selected file.

#' Outputs Browser UI (card layout, file list + preview in one card)
#'
#' @param id Module ID
#' @param title Panel/card title
#' @param mode "rds" or "csv"
#' @return UI elements
#' @export
mod_outputs_browser_ui <- function(id, title, mode = c("rds", "csv")) {
  ns <- shiny::NS(id)
  mode <- match.arg(as.character(mode), c("rds", "csv"))
  bslib::card(
    bslib::card_header(title),
    bslib::card_body(
      shiny::fluidRow(
        shiny::column(4, mod_outputs_browser_sidebar_ui(id, title, mode)),
        shiny::column(8, mod_outputs_browser_main_ui(id, title, mode))
      )
    )
  )
}

#' Outputs Browser sidebar UI (file list, meta, download)
#'
#' @param id Module ID
#' @param title Panel title (for context)
#' @param mode "rds" or "csv"
#' @return UI elements for sidebar
#' @export
mod_outputs_browser_sidebar_ui <- function(id, title, mode = c("rds", "csv")) {
  ns <- shiny::NS(id)
  mode <- match.arg(as.character(mode), c("rds", "csv"))
  shiny::tagList(
    shiny::uiOutput(ns("file_list_ui")),
    shiny::uiOutput(ns("meta_ui")),
    shiny::uiOutput(ns("download_ui"))
  )
}

#' Outputs Browser main UI (preview)
#'
#' @param id Module ID
#' @param title Panel title (for context)
#' @param mode "rds" or "csv"
#' @return UI elements for main content
#' @export
mod_outputs_browser_main_ui <- function(id, title, mode = c("rds", "csv")) {
  ns <- shiny::NS(id)
  mode <- match.arg(as.character(mode), c("rds", "csv"))
  shiny::uiOutput(ns("preview_ui"))
}

#' Outputs Browser Server
#'
#' @param id Module ID
#' @param folder_r Reactive character: top-level output directory (for RDS) or same (for CSV, subfolder "export" is used)
#' @param mode "rds" or "csv"
#' @return Nothing; used for side effects (UI outputs)
#' @export
mod_outputs_browser_server <- function(id, folder_r, mode = c("rds", "csv")) {
  mode <- match.arg(as.character(mode), c("rds", "csv"))
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Resolve folder: RDS = folder_r(), CSV = file.path(folder_r(), "export")
    resolved_folder_r <- shiny::reactive({
      f <- folder_r()
      if (is.null(f) || is.na(f) || f == "") return(NULL)
      if (mode == "csv") {
        f <- file.path(f, "export")
      }
      if (!dir.exists(f)) return(NULL)
      f
    })

    files_r <- shiny::reactive({
      folder <- resolved_folder_r()
      if (is.null(folder)) return(character(0))
      pat <- if (mode == "rds") "\\.rds$" else "\\.csv$"
      list.files(folder, pattern = pat, full.names = TRUE)
    })

    file_names_r <- shiny::reactive({
      f <- files_r()
      if (length(f) == 0) return(character(0))
      basename(f)
    })

    selected_path_r <- shiny::reactive({
      sel <- input$selected_file
      if (is.null(sel) || sel == "") return(NULL)
      folder <- resolved_folder_r()
      if (is.null(folder)) return(NULL)
      path <- file.path(folder, sel)
      if (!file.exists(path)) return(NULL)
      path
    })

    output$file_list_ui <- shiny::renderUI({
      names <- file_names_r()
      if (length(names) == 0) {
        return(shiny::tags$p(
          class = "text-muted",
          if (is.null(resolved_folder_r())) "No output directory set. Run the algorithm first."
          else paste("No", mode, "files in this folder.")
        ))
      }
      shiny::selectInput(
        ns("selected_file"),
        label = "File",
        choices = stats::setNames(names, names),
        selected = names[1],
        width = "100%"
      )
    })

    output$meta_ui <- shiny::renderUI({
      path <- selected_path_r()
      if (is.null(path)) return(NULL)
      info <- file.info(path)
      if (is.na(info$mtime)) return(NULL)
      shiny::tags$div(
        shiny::tags$p(shiny::tags$strong("Size:"), format(info$size, big.mark = ","), "bytes"),
        shiny::tags$p(shiny::tags$strong("Modified:"), format(info$mtime, "%Y-%m-%d %H:%M:%S"))
      )
    })

    output$download_ui <- shiny::renderUI({
      path <- selected_path_r()
      if (is.null(path)) return(NULL)
      shiny::downloadButton(ns("download_file"), "Download", class = "btn-sm")
    })

    output$download_file <- shiny::downloadHandler(
      filename = function() basename(selected_path_r()),
      content = function(file) {
        path <- selected_path_r()
        if (!is.null(path) && file.exists(path)) {
          file.copy(path, file, overwrite = TRUE)
        }
      },
      contentType = if (mode == "rds") "application/octet-stream" else "text/csv"
    )

    # Preview: RDS = readRDS + DT/str; CSV = read_csv + DT (limit 5000). 0-row safe; load failure -> friendly message.
    preview_data_r <- shiny::reactive({
      path <- selected_path_r()
      if (is.null(path)) return(NULL)
      if (mode == "rds") {
        tryCatch(
          readRDS(path),
          error = function(e) {
            list(error = conditionMessage(e), is_error = TRUE)
          }
        )
      } else {
        if (!requireNamespace("readr", quietly = TRUE)) {
          tryCatch(
            utils::read.csv(path, nrows = 5000),
            error = function(e) data.frame(error = conditionMessage(e), is_error = TRUE)
          )
        } else {
          tryCatch(
            readr::read_csv(path, n_max = 5000, show_col_types = FALSE),
            error = function(e) data.frame(error = conditionMessage(e), is_error = TRUE)
          )
        }
      }
    })

    output$preview_ui <- shiny::renderUI({
      path <- selected_path_r()
      if (is.null(path)) {
        return(shiny::tags$p(class = "text-muted", "Select a file to preview."))
      }
      if (mode == "rds") {
        obj <- preview_data_r()
        if (is.null(obj)) return(NULL)
        if (is.list(obj) && isTRUE(obj$is_error)) {
          return(shiny::tags$div(
            shiny::tags$p(class = "text-warning", "Could not load file. It may be missing or corrupted."),
            shiny::tags$pre(if (!is.null(obj$error)) obj$error else "Unknown error", class = "text-danger", style = "font-size: 0.8rem;")
          ))
        }
        if (is.data.frame(obj)) {
          nr <- nrow(obj)
          nc <- ncol(obj)
          dim_text <- sprintf("%d rows, %d cols", nr, nc)
          if (nr == 0) {
            dim_text <- paste(dim_text, "(empty table with headers)")
          }
          return(shiny::tagList(
            shiny::tags$p(shiny::tags$strong("Dimensions:"), dim_text),
            DT::dataTableOutput(ns("preview_table"))
          ))
        }
        if (is.list(obj)) {
          str_out <- capture.output(str(obj, max.level = 3))
          return(shiny::tagList(
            shiny::tags$p(shiny::tags$strong("Structure:")),
            shiny::tags$pre(paste(str_out, collapse = "\n"), style = "font-size: 0.8rem; max-height: 400px; overflow: auto;")
          ))
        }
        shiny::tagList(
          shiny::tags$p(shiny::tags$strong("Class:"), paste(class(obj), collapse = ", ")),
          shiny::tags$pre(paste(capture.output(str(obj)), collapse = "\n"), style = "font-size: 0.8rem; max-height: 300px; overflow: auto;")
        )
      } else {
        df <- preview_data_r()
        if (is.null(df)) return(NULL)
        if (is.data.frame(df) && isTRUE(df$is_error)) {
          return(shiny::tags$div(
            shiny::tags$p(class = "text-warning", "Could not load file."),
            shiny::tags$pre(if (length(df$error) > 0) df$error[1] else "Unknown error", class = "text-danger", style = "font-size: 0.8rem;")
          ))
        }
        if (is.data.frame(df) && "error" %in% names(df) && ncol(df) == 1) {
          return(shiny::tags$pre(df$error[1], class = "text-danger"))
        }
        nr <- nrow(df)
        nc <- ncol(df)
        dim_text <- sprintf("Preview (max 5000 rows): %d rows, %d cols", nr, nc)
        if (nr == 0) dim_text <- paste(dim_text, "(empty with headers)")
        shiny::tagList(
          shiny::tags$p(shiny::tags$strong(dim_text)),
          DT::dataTableOutput(ns("preview_table"))
        )
      }
    })

    output$preview_table <- DT::renderDataTable({
      obj <- preview_data_r()
      if (is.null(obj)) return(NULL)
      if (is.list(obj) && isTRUE(obj$is_error)) return(NULL)
      if (!is.data.frame(obj)) return(NULL)
      if (is.data.frame(obj) && "error" %in% names(obj) && ncol(obj) == 1) return(NULL)
      DT::datatable(obj, options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}

# ---------- Page-level wrappers (nav_panel + layout_sidebar per page) ----------

#' Outputs (RDS) page UI: nav_panel with layout_sidebar
#' @param id Module id (e.g. "outputs_rds")
#' @return bslib::nav_panel for Outputs (RDS)
#' @export
outputs_rds_ui <- function(id) {
  bslib::nav_panel(
    title = "Outputs (RDS)",
    value = "rds",
    bslib::layout_sidebar(
      sidebar = mod_outputs_browser_sidebar_ui(id, title = "Output RDS files", mode = "rds"),
      mod_outputs_browser_main_ui(id, title = "Output RDS files", mode = "rds")
    )
  )
}

#' Outputs (RDS) page Server
#' @param id Module id (e.g. "outputs_rds")
#' @param folder_r Reactive: output directory
#' @export
outputs_rds_server <- function(id, folder_r) {
  mod_outputs_browser_server(id, folder_r = folder_r, mode = "rds")
}

#' Exports (CSV) page UI: nav_panel with layout_sidebar
#' @param id Module id (e.g. "outputs_csv")
#' @return bslib::nav_panel for Exports (CSV)
#' @export
outputs_csv_ui <- function(id) {
  bslib::nav_panel(
    title = "Exports (CSV)",
    value = "csv",
    bslib::layout_sidebar(
      sidebar = mod_outputs_browser_sidebar_ui(id, title = "Exported CSV files", mode = "csv"),
      mod_outputs_browser_main_ui(id, title = "Exported CSV files", mode = "csv")
    )
  )
}

#' Exports (CSV) page Server
#' @param id Module id (e.g. "outputs_csv")
#' @param folder_r Reactive: output directory (export subfolder is used for CSV)
#' @export
outputs_csv_server <- function(id, folder_r) {
  mod_outputs_browser_server(id, folder_r = folder_r, mode = "csv")
}
