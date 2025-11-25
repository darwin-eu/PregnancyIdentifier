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

#' @title Incidence Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Incidence module that shows incidence results from the IncidencePrevalence package.
#'
#' @export
IncidencePlot <- R6::R6Class(
  classname = "IncidencePlot",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field data (`summarisedResult`) SummarisedResult object from Incidence.
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
    #' @param data (`summarised_result`) Result object from the `IncidencePrevalence` package.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(data, ...) {
      super$initialize(...)
      private$assertInstall("IncidencePrevalence", "1.2.0")
      private$assertInstall("visOmopResults", "1.0.2")
      private$assertIncidenceData(data)
      private$.data <- data
      private$.tidyData <- private$transformData(data)
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
          tabName = shiny::NS(private$.namespace, "incidence"),
          private$.pickers[["cdm"]]$UI(),
          private$.pickers[["outcome"]]$UI(),
          private$.pickers[["denomAgeGroup"]]$UI(),
          private$.pickers[["startDate"]]$UI(),
          p("Plotting options"),
          private$.pickers[["xAxis"]]$UI(),
          private$.pickers[["facet"]]$UI(),
          private$.pickers[["color"]]$UI(),
          private$.pickers[["ribbon"]]$UI(),
          private$.pickers[["confInterval"]]$UI(),
          private$.pickers[["rotateXLabels"]]$UI(),
          plotly::plotlyOutput(
            shiny::NS(private$.namespace, "plot"),
            height = "800px"
          ) %>%
            shinycssloaders::withSpinner(),
          shiny::h4("Download figure"),
          shiny::div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          shiny::div(
            style = "display: inline-block;",
            shiny::textInput(shiny::NS(private$.namespace, "download_height"), "", 10, width = "50px")
          ),
          shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
          shiny::div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          shiny::div(
            style = "display: inline-block;",
            shiny::textInput(shiny::NS(private$.namespace, "download_width"), "", 20, width = "50px")
          ),
          shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
          shiny::div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          shiny::div(
            style = "display: inline-block; margin-right:",
            shiny::textInput(shiny::NS(private$.namespace, "download_dpi"), "", 300, width = "50px")
          ),
          shiny::downloadButton(shiny::NS(private$.namespace, "download_plot"), "Download plot")
        )
      )
    },
    .server = function(input, output, session) {
      for (module in private$.pickers) {
        module$server(input, output, session)
      }

      # Incidence
      getIncidenceEstimates <- reactive({
        result <- private$.tidyData %>%
          dplyr::filter(database %in% private$.pickers[["cdm"]]$inputValues$cdm) %>%
          dplyr::filter(outcome_cohort_name %in% private$.pickers[["outcome"]]$inputValues$outcome) %>%
          dplyr::filter(denominator_age_group %in% private$.pickers[["denomAgeGroup"]]$inputValues$age_group) %>%
          dplyr::filter(incidence_start_date %in% private$.pickers[["startDate"]]$inputValues$year) %>%
          dplyr::mutate(
            person_years = round(suppressWarnings(as.numeric(person_years))),
            person_days = round(suppressWarnings(as.numeric(person_days))),
            n_events = round(suppressWarnings(as.numeric(n_events))),
            incidence_100000_pys = round(suppressWarnings(as.numeric(incidence_100000_pys))),
            incidence_100000_pys_95CI_lower = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_lower))),
            incidence_100000_pys_95CI_upper = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_upper)))
          )
        return(result)
      })

      ### make plot ----
      plotIncidenceEstimates <- reactive({
        table <- getIncidenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))
        class(table) <- c("IncidenceResult", "IncidencePrevalenceResult", class(table))

        plot <- IncidencePrevalence::plotIncidence(
          result = table,
          x = private$.pickers[["xAxis"]]$inputValues$xAxis,
          y = "incidence_100000_pys",
          line = FALSE,
          point = TRUE,
          ribbon = as.logical(private$.pickers[["ribbon"]]$inputValues$ribbon),
          ymin = "incidence_100000_pys_95CI_lower",
          ymax = "incidence_100000_pys_95CI_upper",
          facet = private$.pickers[["facet"]]$inputValues$facet_by,
          colour = private$.pickers[["color"]]$inputValues$color_by
        )
        # remove confidence interval
        if (!as.logical(private$.pickers[["confInterval"]]$inputValues$confInterval)) {
          plot$layers <- plot$layers[2]
          if (as.logical(private$.pickers[["ribbon"]]$inputValues$ribbon)) {
            plot <- plot + ggplot2::geom_line()
          }
        }
        if (as.logical(private$.pickers[["rotateXLabels"]]$inputValues$rotateXLabels)) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1))
        }
        plot
      })

      ### download plot ----
      output$download_plot <- downloadHandler(
        filename = function() {
          "incidenceEstimatesPlot.png"
        },
        content = function(file) {
          ggplot2::ggsave(
            file,
            plotIncidenceEstimates(),
            width = as.numeric(input$download_width),
            height = as.numeric(input$download_height),
            dpi = as.numeric(input$download_dpi),
            units = "cm"
          )
        }
      )
      ### plot ----
      output$plot <- renderPlotly({
        plotIncidenceEstimates()
      })
    },
    assertIncidenceData = function(data) {
      resSettings <- attr(data, "settings")
      if (is.null(resSettings)) {
        stop("Data does not appear to be a result object of `IncidencePrevalence`")
      }
      if (!all(resSettings$result_type %in% c("incidence", "incidence_attrition"))) {
        stop("Cannot assert `Incidence` result")
      }
    },
    transformData = function(data) {
      # set strata
      strataColumn <- unique(settings(data) %>% dplyr::filter(strata != "reason") %>% dplyr::pull(strata))
      private$.strata <- unique(data %>% dplyr::filter(strata_name != "reason") %>% dplyr::pull(strata_level))

      # transform to readable format
      minCellCount <- attr(data, "settings") %>%
        dplyr::pull(min_cell_count) %>%
        unique()
      data <- IncidencePrevalence::asIncidenceResult(data) %>%
        { if (!"analysis_interval" %in% names(.)) dplyr::mutate(., analysis_interval = "overall") else .} %>%
        dplyr::mutate(analysis_min_cell_count = !!minCellCount) %>%
        dplyr::rename(
          database = cdm_name,
          n_events = outcome_count,
          n_persons = denominator_count
        )
      # add strata column
      if (strataColumn == "") {
        data <- data %>% dplyr::mutate(strata = "overall")
      } else {
        data <- data %>% dplyr::rename(strata = strataColumn)
      }
      return(data)
    },
    initPickers = function() {
      # cdm
      private$.pickers[["cdm"]] <- InputPanel$new(
        funs = list(cdm = shinyWidgets::pickerInput),
        args = list(cdm = list(
          inputId = "cdm", label = "Database", choices = unique(private$.tidyData$database), selected = unique(private$.tidyData$database), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["cdm"]]$parentNamespace <- self$namespace

      # outcome
      private$.pickers[["outcome"]] <- InputPanel$new(
        funs = list(outcome = shinyWidgets::pickerInput),
        args = list(outcome = list(
          inputId = "outcome", label = "Outcome", choices = unique(private$.tidyData$outcome_cohort_name), selected = unique(private$.tidyData$outcome_cohort_name), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["outcome"]]$parentNamespace <- self$namespace

      # denominator age group
      private$.pickers[["denomAgeGroup"]] <- InputPanel$new(
        funs = list(age_group = shinyWidgets::pickerInput),
        args = list(age_group = list(
          inputId = "age_group", label = "Age group", choices = unique(private$.tidyData$denominator_age_group), selected = unique(private$.tidyData$denominator_age_group), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomAgeGroup"]]$parentNamespace <- self$namespace

      # start date
      private$.pickers[["startDate"]] <- InputPanel$new(
        funs = list(year = shinyWidgets::pickerInput),
        args = list(year = list(
          inputId = "year", choices = unique(private$.tidyData$incidence_start_date), label = "Year", selected = unique(private$.tidyData$incidence_start_date), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["startDate"]]$parentNamespace <- self$namespace

      # plot pickers
      plotDataChoices <- c(
        "database", "outcome_cohort_name", "strata", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
        "denominator_start_date", "denominator_end_date", "denominator_time_at_risk", "analysis_outcome_washout", "analysis_repeated_events",
        "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"
      )
      # x-axis
      private$.pickers[["xAxis"]] <- InputPanel$new(
        funs = list(xAxis = shinyWidgets::pickerInput),
        args = list(xAxis = list(
          inputId = "xAxis", choices = plotDataChoices, label = "Incidence_start_date", selected = "incidence_start_date", multiple = F,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["xAxis"]]$parentNamespace <- self$namespace

      # facet by
      private$.pickers[["facet"]] <- InputPanel$new(
        funs = list(facet_by = shinyWidgets::pickerInput),
        args = list(facet_by = list(
          inputId = "facet_by", choices = plotDataChoices, label = "Facet by", selected = c("outcome_cohort_name"), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["facet"]]$parentNamespace <- self$namespace

      # color by
      private$.pickers[["color"]] <- InputPanel$new(
        funs = list(color_by = shinyWidgets::pickerInput),
        args = list(color_by = list(
          inputId = "color_by", choices = plotDataChoices, label = "Colour by", selected = c("database"), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["color"]]$parentNamespace <- self$namespace

      # ribbon
      private$.pickers[["ribbon"]] <- InputPanel$new(
        funs = list(ribbon = shinyWidgets::pickerInput),
        args = list(ribbon = list(
          inputId = "ribbon", choices = c(TRUE, FALSE), label = "Ribbon", selected = TRUE, multiple = FALSE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["ribbon"]]$parentNamespace <- self$namespace

      # confidence interval
      private$.pickers[["confInterval"]] <- InputPanel$new(
        funs = list(confInterval = shinyWidgets::pickerInput),
        args = list(confInterval = list(
          inputId = "confInterval", choices = c(TRUE, FALSE), label = "Confidence interval", selected = TRUE, multiple = FALSE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["confInterval"]]$parentNamespace <- self$namespace

      # rotate x-axis labels
      private$.pickers[["rotateXLabels"]] <- InputPanel$new(
        funs = list(rotateXLabels = shinyWidgets::pickerInput),
        args = list(rotateXLabels = list(
          inputId = "rotateXLabels", choices = c(TRUE, FALSE), label = "Rotate x-axis labels", selected = TRUE
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["rotateXLabels"]]$parentNamespace <- self$namespace
    }
  )
)
