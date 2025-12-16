# Copyright 2025 DARWIN EU®
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

#' @title Characteristics Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Characteristics module that shows results from the CohortCharacteristics package.
#'
#' @export
#'
Characteristics <- R6::R6Class(
  classname = "Characteristics",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field data (`summarisedResult`) SummarisedResult object
    data = function(data) {
      if (missing(data)) {
        return(private$.data)
      } else {
        # Checks on data
        checkmate::assertClass(data, "summarised_result")
        private$.data <- data
      }
    },

    #' @field pickers (`list`) List of pickers
    pickers = function() {
      return(private$.pickers)
    }
  ),

  # Public ----
  public = list(

    #' @description
    #' Initializer method
    #'
    #' @param data (`summarised_result`) Result object from the `CohortCharacteristics` package.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(data, ...) {
      super$initialize(...)
      private$.data <- data
      #private$.settings <- settings(data)
      private$initPickers()
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .data = NULL,
    .tidyData = NULL,
    .strata = NULL,
    .pickers = NULL,
    .UI = function() {
      shiny::tagList(
        shinydashboard::tabItem(
          tabName = shiny::NS(private$.namespace, "characteristics"),
          private$.pickers[["cdm"]]$UI(),
          private$.pickers[["outcome"]]$UI(),
          private$.pickers[["strata"]]$UI(),
          shiny::tabsetPanel(
            id = shiny::NS(private$.namespace, "tabsetPanel"),
            type = "tabs",
            shiny::tabPanel(
              "Tidy table",
              private$.pickers[["headerColumn"]]$UI(),
              private$.pickers[["groupColumn"]]$UI(),
              private$.pickers[["hideColumn"]]$UI(),
              p(),
              shiny::downloadButton(shiny::NS(private$.namespace, "downloadTidyTable"), "Download table"),
              p(),
              gt::gt_output(shiny::NS(private$.namespace, "tidyTable")) %>% shinycssloaders::withSpinner()
            ),
            shiny::tabPanel(
              "Plot",
              private$.pickers[["variable"]]$UI(),
              shiny::plotOutput(shiny::NS(private$.namespace, "plot"), height = "600px")
            )
          )
        )
      )
    },
    .server = function(input, output, session) {
      for (module in private$.pickers) {
        module$server(input, output, session)
      }

      # Filtered data
      getData <- reactive({
        private$.data %>%
          dplyr::filter(cdm_name %in% private$.pickers[["cdm"]]$inputValues$cdm) %>%
          dplyr::filter(strata_level %in% private$.pickers[["strata"]]$inputValues$strata) %>%
          dplyr::filter(group_level %in% private$.pickers[["outcome"]]$inputValues$outcome)
      })

      # TABLE
      gtTable <- reactive({
        req(getData())
        CohortCharacteristics::tableCharacteristics(result = getData(),
                                                    header = private$.pickers[["headerColumn"]]$inputValues$headerColumn,
                                                    groupColumn = private$.pickers[["groupColumn"]]$inputValues$groupColumn,
                                                    hide = private$.pickers[["hideColumn"]]$inputValues$hideColumn,
                                                    .options = list(style = "darwin"))
      })

      # Tidy table
      output$tidyTable <- gt::render_gt({
        req(gtTable())
        gtTable()
      })

      # Download tidy table
      output$downloadTidyTable <- downloadHandler(
        filename = function() {
          "Characteristics-Table.docx"
        },
        content = function(file) {
          gt::gtsave(gtTable(), file)
        }
      )

      ## Plot
      output$plot <- renderPlot({
        getData() %>%
          dplyr::filter(variable_name == private$.pickers[["variable"]]$inputValues$variable) %>%
          CohortCharacteristics::plotCharacteristics(
            plotType = "boxplot",
            colour = "cohort_name",
            facet = c("cdm_name")
          )
      })

    },
    transformData = function(data) {
      # TODO
      return(data)
    },
    initPickers = function() {
      # cdm
      private$.pickers[["cdm"]] <- InputPanel$new(
        funs = list(cdm = shinyWidgets::pickerInput),
        args = list(cdm = list(
          inputId = "cdm", label = "Database", choices = unique(private$.data$cdm_name), selected = unique(private$.data$cdm_name), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["cdm"]]$parentNamespace <- self$namespace

      # outcome
      outcomes <- unique(private$.data$group_level)
      private$.pickers[["outcome"]] <- InputPanel$new(
        funs = list(outcome = shinyWidgets::pickerInput),
        args = list(outcome = list(
          inputId = "outcome", label = "Outcome", choices = outcomes, selected = outcomes, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["outcome"]]$parentNamespace <- self$namespace

      # strata
      strataChoices <- unique(private$.data$strata_level)
      private$.pickers[["strata"]] <- InputPanel$new(
        funs = list(strata = shinyWidgets::pickerInput),
        args = list(strata = list(
          inputId = "strata", label = "Strata", choices = strataChoices, selected = strataChoices, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["strata"]]$parentNamespace <- self$namespace

      # headerColumn
      headerColumnOptions <- c("cdm_name", "estimate_name")
      private$.pickers[["headerColumn"]] <- InputPanel$new(
        funs = list(headerColumn = shinyWidgets::pickerInput),
        args = list(headerColumn = list(
          inputId = "headerColumn", choices = headerColumnOptions, label = "Header", selected = headerColumnOptions, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["headerColumn"]]$parentNamespace <- self$namespace

      # groupColumn
      groupColumnOptions <- c("cohort_name", "cdm_name")
      private$.pickers[["groupColumn"]] <- InputPanel$new(
        funs = list(groupColumn = shinyWidgets::pickerInput),
        args = list(groupColumn = list(
          inputId = "groupColumn", choices = groupColumnOptions, label = "Group columns", selected = groupColumnOptions[1], multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["groupColumn"]]$parentNamespace <- self$namespace

      # hideColumn
      hideColumnOptions <- c()
      private$.pickers[["hideColumn"]] <- InputPanel$new(
        funs = list(hideColumn = shinyWidgets::pickerInput),
        args = list(hideColumn = list(
          inputId = "hideColumn", choices = hideColumnOptions, label = "Hide columns", selected = hideColumnOptions, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["hideColumn"]]$parentNamespace <- self$namespace

      variableOptions <- setdiff(unique(private$.data$variable_name), c("Number records", "Number subjects"))
      private$.pickers[["variable"]] <- InputPanel$new(
        funs = list(variable = shinyWidgets::pickerInput),
        args = list(variable = list(
          inputId = "variable", choices = variableOptions, label = "Header", selected = variableOptions, multiple = FALSE
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["variable"]]$parentNamespace <- self$namespace
    }
  )
)
