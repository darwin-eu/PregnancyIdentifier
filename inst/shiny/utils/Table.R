# Copyright 2024 DARWIN EU®
#
# This file is part of DarwinShinyModules
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @title Table Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Table module that displays `data.frame` like objects as a table that are
#' supported by `DT::renderDT()` and `DT::DTOutput()`.
#'
#' @details
#' The Table module exposes reactive bindings from the datatable object from
#' `DT`, in `bindings` field. These bindings are:
#' - cell_clicked
#' - cells_selected
#' - cell_info
#' - rows_current
#' - rows_all
#' - rows_selected
#' - row_last_clicked
#' - columns_selected
#' - search
#' - search_columns
#' - state
#'
#' These bindings allow you to trigger events with i.e. `shiny::observeEvent()`
#' in another module.
#'
#' For a full description of the exposed bindings, consult the `DT`
#' documentation: https://rstudio.github.io/DT/shiny.html
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' table <- Table$new(data = mtcars)
#'
#' if (interactive()) {
#'   preview(table)
#' }
Table <- R6::R6Class(
  classname = "Table",
  inherit = ShinyModule,

  # Active ----
  active = list(
    ## Reactive ----
    #' @field bindings (`reactiveValues`) Reactive bindings for `DT::datatable`.
    bindings = function() {
      return(private$.bindings)
    },

    ## Non-reactive ----
    #' @field data (`data.frame`) Reactive data, use `shiny::isolate()` to get the non-reactive data.
    data = function(data) {
      if (missing(data)) {
        return(private$.data)
      } else {
        checkmate::assertDataFrame(data)
        private$.data <- data
        self$reactiveValues$data <- data
      }
    },

    #' @field title (`character`) Title of the table.
    title = function(title) {
      if (missing(title)) {
        return(private$.title)
      } else {
        checkmate::assertCharacter(title, len = 1)
        private$.title <- title
      }
    },

    #' @field options (`list(n)`) List of options used by `DT::datatable`.
    options = function() {
      return(private$.options)
    },

    #' @field filter (`character(1)`) Filter option used by `DT::datatable`.
    filter = function() {
      return(private$.filter)
    }
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param data (`data.frame`) Data to plot with, usually a `data.frame`-like object.
    #' @param title (`character(1)`) Title of the table. When set to `NULL`, no title is shown.
    #' @param options (`list`) table options, by default it shows additional items next to
    #' the table like search box, pagination, etc. Only display the table using
    #' list(dom = '')
    #' @param filter (`character`: `"top"`) filter option, it can be either `"none"`, `"bottom"` or `"top"` (default)
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @return `self`
    initialize = function(data, title = "Table", options = list(scrollX = TRUE), filter = "top", ...) {
      super$initialize(...)
      private$.data <- data
      private$.title <- title
      private$.options <- options
      private$.filter <- filter
      return(invisible(self))
    },

    #' @description validate
    #'
    #' @return `self`
    validate = function() {
      super$validate()
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertDataFrame(
        .var.name = "data",
        x = private$.data,
        add = assertions
      )
      checkmate::assertList(
        .var.name = "options",
        x = private$.options,
        add = assertions
      )
      checkmate::assertTRUE(
        .var.name = "filter",
        x = private$.filter %in% c("none", "bottom", "top"),
        add = assertions
      )
      checkmate::reportAssertions(assertions)
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .title = "",
    .options = NULL,
    .filter = NULL,
    .data = NULL,
    .bindings = shiny::reactiveValues(
      cell_clicked = NULL,
      cells_selected = NULL,
      cell_info = NULL,
      rows_current = NULL,
      rows_all = NULL,
      rows_selected = NULL,
      row_last_clicked = NULL,
      columns_selected = NULL,
      search = NULL,
      search_columns = NULL,
      state = NULL
    ),

    ## Methods ----
    .server = function(input, output, session) {
      self$reactiveValues$data <- private$.data
      private$renderTable(output)
      private$downloader(output)
      private$setBindings(input)
    },
    .UI = function() {
      shiny::tagList(
        shiny::h3(private$.title),
        DT::DTOutput(outputId = shiny::NS(private$.namespace, "table")),
        shiny::downloadButton(outputId = shiny::NS(private$.namespace, "dlButton"), label = "csv")
      )
    },
    setBindings = function(input) {
      shiny::observeEvent(eventExpr = input$table_cells_selected, {
        private$.bindings$cells_selected <- input$table_cells_selected
      })

      shiny::observeEvent(eventExpr = input$table_cell_clicked, {
        private$.bindings$cell_clicked <- input$table_cell_clicked
      })

      shiny::observeEvent(eventExpr = input$table_cell_info, {
        private$.bindings$cell_info <- input$table_cell_info
      })

      shiny::observeEvent(eventExpr = input$table_rows_all, {
        private$.bindings$rows_all <- input$table_rows_all
      })

      shiny::observeEvent(eventExpr = input$table_rows_current, {
        private$.bindings$rows_current <- input$table_rows_current
      })

      shiny::observeEvent(eventExpr = input$table_rows_selected, {
        private$.bindings$rows_selected <- input$table_rows_selected
      })

      shiny::observeEvent(eventExpr = input$table_row_last_clicked, {
        private$.bindings$row_last_clicked <- input$table_row_last_clicked
      })

      shiny::observeEvent(eventExpr = input$table_columns_selected, {
        private$.bindings$columns_selected <- input$table_columns_selected
      })

      shiny::observeEvent(eventExpr = input$table_search, {
        private$.bindings$search <- input$table_search
      })

      shiny::observeEvent(eventExpr = input$table_search_columns, {
        private$.bindings$search_columns <- input$table_search_columns
      })

      shiny::observeEvent(eventExpr = input$table_state, {
        private$.bindings$state <- input$table_state
      })
    },
    renderTable = function(output) {
      output$table <- DT::renderDT({
        data <- self$reactiveValues$data
        rowsToColor <- NULL
        if ("overlap" %in% colnames(data)) {
          data$rowId <- as.numeric(rownames(data))
          rowsToColor <- data %>% dplyr::filter(overlap == TRUE) %>% dplyr::pull(rowId)
          data <- data %>% dplyr::select(-rowId)
        }
        result <- DT::datatable(
          data = data,
          filter = private$.filter,
          options = private$.options)
        if (!is.null(rowsToColor)) {
          result <- result %>%
            DT::formatStyle(0, target='row', backgroundColor = DT::styleEqual(c(rowsToColor), c('red')))
        }
        result
      })
    },
    downloader = function(output) {
      output$dlButton <- shiny::downloadHandler(
        filename = private$dlFilename,
        content = private$dlContent
      )
    },
    dlFilename = function() {
      return(sprintf("%s.csv", private$.title))
    },
    dlContent = function(file) {
      utils::write.csv(isolate(self$reactiveValues$data), file)
    }
  )
)
