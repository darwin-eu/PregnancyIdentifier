# mod_concepts.R
# Browse and display Excel/CSV concept files from inst/concepts.

#' Resolve path to inst/concepts folder
#'
#' Tries project-relative paths first, then system.file when package is loaded.
#' @return Character path to concepts directory, or NULL if not found.
#' @export
concepts_dir <- function() {
  candidates <- c(
    "inst/concepts",
    "../../inst/concepts",
    file.path(dirname(dirname(getwd())), "inst", "concepts")
  )
  if (requireNamespace("PregnancyIdentifier", quietly = TRUE)) {
    pkg_path <- system.file("concepts", package = "PregnancyIdentifier")
    if (nzchar(pkg_path) && dir.exists(pkg_path)) {
      candidates <- c(candidates, pkg_path)
    }
  }
  for (d in candidates) {
    if (dir.exists(d)) return(d)
  }
  NULL
}

#' Concepts browser sidebar UI
#'
#' @param id Module ID
#' @return UI elements for file and sheet selection
#' @export
mod_concepts_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("file_list_ui")),
    shiny::uiOutput(ns("sheet_list_ui")),
    shiny::uiOutput(ns("meta_ui"))
  )
}

#' Concepts browser main UI (table preview)
#'
#' @param id Module ID
#' @return UI elements for data table
#' @export
mod_concepts_main_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("preview_ui"))
}

#' Concepts browser server
#'
#' @param id Module ID
#' @return Nothing; used for side effects
#' @export
mod_concepts_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dir_r <- shiny::reactive(concepts_dir())

    files_r <- shiny::reactive({
      d <- dir_r()
      if (is.null(d)) return(character(0))
      xlsx <- list.files(d, pattern = "\\.xlsx$", full.names = TRUE)
      csv  <- list.files(d, pattern = "\\.csv$", full.names = TRUE)
      c(xlsx, csv)
    })

    file_names_r <- shiny::reactive({
      f <- files_r()
      if (length(f) == 0) return(character(0))
      basename(f)
    })

    selected_path_r <- shiny::reactive({
      sel <- input$selected_file
      if (is.null(sel) || sel == "") return(NULL)
      d <- dir_r()
      if (is.null(d)) return(NULL)
      path <- file.path(d, sel)
      if (!file.exists(path)) return(NULL)
      path
    })

    is_excel_r <- shiny::reactive({
      path <- selected_path_r()
      if (is.null(path)) return(FALSE)
      tolower(tools::file_ext(path)) == "xlsx"
    })

    sheets_r <- shiny::reactive({
      path <- selected_path_r()
      if (is.null(path) || !is_excel_r()) return(character(0))
      if (!requireNamespace("readxl", quietly = TRUE)) return(character(0))
      tryCatch(
        readxl::excel_sheets(path),
        error = function(e) character(0)
      )
    })

    output$file_list_ui <- shiny::renderUI({
      names <- file_names_r()
      if (length(names) == 0) {
        return(shiny::tags$p(
          class = "text-muted",
          if (is.null(dir_r())) "Concepts folder not found (inst/concepts)."
          else "No Excel or CSV files in concepts folder."
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

    output$sheet_list_ui <- shiny::renderUI({
      if (!is_excel_r()) return(NULL)
      sheets <- sheets_r()
      if (length(sheets) == 0) return(NULL)
      if (length(sheets) == 1) {
        return(shiny::tags$p(
          class = "text-muted small",
          "Sheet:", sheets[1]
        ))
      }
      shiny::selectInput(
        ns("selected_sheet"),
        label = "Sheet",
        choices = stats::setNames(sheets, sheets),
        selected = sheets[1],
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

    preview_data_r <- shiny::reactive({
      path <- selected_path_r()
      if (is.null(path)) return(NULL)
      ext <- tolower(tools::file_ext(path))
      if (ext == "csv") {
        tryCatch(
          utils::read.csv(path),
          error = function(e) data.frame(error = conditionMessage(e), is_error = TRUE)
        )
      } else if (ext == "xlsx") {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          return(data.frame(error = "Package 'readxl' is required to view Excel files.", is_error = TRUE))
        }
        sheets <- sheets_r()
        if (length(sheets) == 0) {
          return(data.frame(error = "No sheets in workbook.", is_error = TRUE))
        }
        sheet <- input$selected_sheet
        sheet_to_read <- if (is.null(sheet) || !sheet %in% sheets) sheets[1] else sheet
        tryCatch(
          readxl::read_excel(path, sheet = sheet_to_read),
          error = function(e) data.frame(error = conditionMessage(e), is_error = TRUE)
        )
      } else {
        return(NULL)
      }
    })

    output$preview_ui <- shiny::renderUI({
      path <- selected_path_r()
      if (is.null(path)) {
        return(shiny::tags$p(class = "text-muted", "Select a file to preview."))
      }
      df <- preview_data_r()
      if (is.null(df)) return(NULL)
      if (is.data.frame(df) && isTRUE(df$is_error)) {
        return(shiny::tags$div(
          shiny::tags$p(class = "text-warning", "Could not load file."),
          shiny::tags$pre(if (!is.null(df$error)) df$error else "Unknown error", class = "text-danger", style = "font-size: 0.8rem;")
        ))
      }
      nr <- nrow(df)
      nc <- ncol(df)
      dim_text <- sprintf("%d rows, %d cols", nr, nc)
      if (nr == 0) dim_text <- paste(dim_text, "(empty)")
      shiny::tagList(
        shiny::tags$p(shiny::tags$strong(dim_text)),
        DT::dataTableOutput(ns("preview_table"))
      )
    })

    output$preview_table <- DT::renderDataTable({
      df <- preview_data_r()
      if (is.null(df)) return(NULL)
      if (is.data.frame(df) && isTRUE(df$is_error)) return(NULL)
      DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE))
    })
  })
}

# ---------- Page-level wrappers ----------

#' Concepts page UI: nav_panel with layout_sidebar
#'
#' @param id Module id (e.g. "concepts")
#' @return bslib::nav_panel for Concepts
#' @export
concepts_ui <- function(id) {
  bslib::nav_panel(
    title = "Concepts",
    value = "concepts",
    bslib::layout_sidebar(
      sidebar = mod_concepts_sidebar_ui(id),
      mod_concepts_main_ui(id)
    )
  )
}

#' Concepts page Server
#'
#' @param id Module id (e.g. "concepts")
#' @export
concepts_server <- function(id) {
  mod_concepts_server(id)
}
