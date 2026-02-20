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

#' @title Prevalence Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Prevalence module that shows prevalence results from the IncidencePrevalence package.
#'
#' @export
#'
#' @examples{
#' \donttest{
#'  library(DarwinShinyModules)
#'
#'  if (
#'    require(
#'      "IncidencePrevalence",
#'      character.only = TRUE,
#'      quietly = TRUE,
#'      warn.conflicts = FALSE
#'    )
#'  ) {
#'     prev <- omopgenerics::importSummarisedResult(system.file(
#'       package = "DarwinShinyModules",
#'       "dummyData/IncidencePrevalence/1.2.0/prevalence.csv"
#'     ))
#'
#'     prevMod <- Prevalence$new(data = prev,
#'                               defaults = list(sex = "Both"))
#'
#'     ui <- shiny::fluidPage(
#'       prevMod$UI()
#'     )
#'
#'     server <- function(input, output, session) {
#'       prevMod$server(input, output, session)
#'     }
#'
#'     if (interactive()) {
#'       shiny::shinyApp(ui = ui, server = server)
#'     }
#'   }
#' }
#' }
Prevalence <- R6::R6Class(
  classname = "Prevalence",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field data (`summarisedResult`) SummarisedResult object from Prevalence.
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
    #' @param defaults list of default values for the pickers
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(data, defaults, ...) {
      super$initialize(...)
      private$assertInstall("IncidencePrevalence", "1.2.0")
      private$assertInstall("visOmopResults", "1.0.2")
      private$assertPrevalenceData(data)
      private$.data <- data
      private$.tidyData <- private$transformData(data)
      private$.defaults <- defaults
      private$initPickers()
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .data = NULL,
    .tidyData = NULL,
    .defaults = NULL,
    .strata = NULL,
    .pickers = NULL,
    .UI = function() {
      shiny::tagList(
        shinydashboard::tabItem(
          tabName = shiny::NS(private$.namespace, "prevalence"),
          shiny::h3("Prevalence estimates"),
          private$.pickers[["cdm"]]$UI(),
          private$.pickers[["outcome"]]$UI(),
          private$.pickers[["denomAgeGroup"]]$UI(),
          private$.pickers[["startDate"]]$UI(),
          shiny::tabsetPanel(
            id = shiny::NS(private$.namespace, "tabsetPanel"),
            type = "tabs",
            shiny::tabPanel(
              "Tidy Table",
              private$.pickers[["headerColumn"]]$UI(),
              private$.pickers[["groupColumn"]]$UI(),
              private$.pickers[["settingsColumn"]]$UI(),
              private$.pickers[["hideColumn"]]$UI(),
              p(),
              shiny::downloadButton(shiny::NS(private$.namespace, "downloadTidyTable"), "Download table"),
              p(),
              gt::gt_output(shiny::NS(private$.namespace, "tidyTable")) %>% shinycssloaders::withSpinner()
            ),
            shiny::tabPanel(
              "Plot",
              p("Plotting options"),
              private$.pickers[["xAxis"]]$UI(),
              private$.pickers[["facet"]]$UI(),
              private$.pickers[["color"]]$UI(),
              private$.pickers[["ribbon"]]$UI(),
              private$.pickers[["confInterval"]]$UI(),
              private$.pickers[["rotateXLabels"]]$UI(),
              plotly::plotlyOutput(
                shiny::NS(private$.namespace, "plot"),
                height = "420px"
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
            ),
            shiny::tabPanel(
              "Table",
              shiny::downloadButton(shiny::NS(private$.namespace, "downloadTable"), "Download current estimates"),
              DT::DTOutput(shiny::NS(private$.namespace, "table")) %>% shinycssloaders::withSpinner()
            )
          )
        )
      )
    },
    .server = function(input, output, session) {
      for (module in private$.pickers) {
        module$server(input, output, session)
      }

      # Prevalence
      getPrevalenceEstimates <- shiny::reactive({
        result <- private$.tidyData %>%
          dplyr::filter(database %in% private$.pickers[["cdm"]]$inputValues$cdm) %>%
          dplyr::filter(outcome_cohort_name %in% private$.pickers[["outcome"]]$inputValues$outcome) %>%
          dplyr::filter(denominator_age_group %in% private$.pickers[["denomAgeGroup"]]$inputValues$age_group) %>%
          dplyr::filter(prevalence_start_date %in% private$.pickers[["startDate"]]$inputValues$year) %>%
          dplyr::mutate(
            n_cases = round(suppressWarnings(as.numeric(n_cases))),
            n_population = round(suppressWarnings(as.numeric(n_population))),
            prevalence = round(suppressWarnings(as.numeric(prevalence)), 4),
            prevalence_95CI_lower = round(suppressWarnings(as.numeric(prevalence_95CI_lower)), 4),
            prevalence_95CI_upper = round(suppressWarnings(as.numeric(prevalence_95CI_upper)), 4),
            prevalence_start_date = as.Date(prevalence_start_date)
          )
        return(result)
      })

      # Filtered data
      summarised_result_data <- shiny::reactive({
        private$.data %>%
          dplyr::filter(
            cdm_name %in% private$.pickers[["cdm"]]$inputValues$cdm) %>%
          omopgenerics::filterSettings(
            denominator_age_group %in% private$.pickers[["denomAgeGroup"]]$inputValues$age_group) %>%
          omopgenerics::filterAdditional(prevalence_start_date %in% private$.pickers[["startDate"]]$inputValues$year) %>%
          omopgenerics::filterGroup(outcome_cohort_name %in% private$.pickers[["outcome"]]$inputValues$outcome)
      })

      summarised_gt_table <- shiny::reactive({
        req(summarised_result_data())
        IncidencePrevalence::tablePrevalence(result = summarised_result_data(),
                                             header = private$.pickers[["headerColumn"]]$inputValues$headerColumn,
                                             groupColumn = private$.pickers[["groupColumn"]]$inputValues$groupColumn,
                                             settingsColumn = private$.pickers[["settingColumn"]]$inputValues$settingColumn,
                                             hide = private$.pickers[["hideColumn"]]$inputValues$hideColumn,
                                             .options = list(style = "darwin"))
      })

      # Tidy table
      output$tidyTable <- gt::render_gt({
        req(summarised_gt_table())
        summarised_gt_table()
      })

      # Download tidy table
      output$downloadTidyTable <- downloadHandler(
        filename = function() {
          "Prevalence-Table.docx"
        },
        content = function(file) {
          gt::gtsave(summarised_gt_table(), file)
        }
      )

      ### download table ----
      output$downloadTable <- downloadHandler(
        filename = function() {
          "prevalenceEstimatesTable.csv"
        },
        content = function(file) {
          utils::write.csv(getPrevalenceEstimates(), file)
        }
      )

      ### table estimates ----
      output$table <- DT::renderDT({
        table <- getPrevalenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))

        table <- table %>%
          mutate(`prevalence (%)` = paste0(
            100 * prevalence, " (", 100 * prevalence_95CI_lower, " to ",
            100 * prevalence_95CI_upper, " )"
          )) %>%
          select(database, outcome_cohort_name, strata, denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, denominator_time_at_risk, analysis_type, analysis_complete_database_intervals, analysis_full_contribution, analysis_min_cell_count, analysis_interval, prevalence_start_date, n_cases, n_population, "prevalence (%)")

        DT::datatable(
          table,
          rownames = FALSE,
          extensions = "Buttons",
          options = list(scrollX = TRUE, scrollCollapse = TRUE)
        )
      })

      ### make plot ----
      plotPrevalenceEstimates <- shiny::reactive({
        table <- getPrevalenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))
        class(table) <- c("PrevalenceResult", "IncidencePrevalenceResult", class(table))

        plot <- IncidencePrevalence::plotPrevalence(
          result = table,
          x = private$.pickers[["xAxis"]]$inputValues$xAxis,
          y = "prevalence",
          line = FALSE,
          point = TRUE,
          ribbon = as.logical(private$.pickers[["ribbon"]]$inputValues$ribbon),
          ymin = "prevalence_95CI_lower",
          ymax = "prevalence_95CI_upper",
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
          "prevalenceEstimatesPlot.png"
        },
        content = function(file) {
          ggplot2::ggsave(
            file,
            plotPrevalenceEstimates(),
            width = as.numeric(input$download_width),
            height = as.numeric(input$download_height),
            dpi = as.numeric(input$download_dpi),
            units = "cm"
          )
        }
      )
      ### plot ----
      output$plot <- plotly::renderPlotly({
        plotPrevalenceEstimates()
      })
    },
    assertPrevalenceData = function(data) {
      resSettings <- attr(data, "settings")
      if (is.null(resSettings)) {
        stop("Data does not appear to be a result object of `IncidencePrevalence`")
      }
      if (!all(resSettings$result_type %in% c("prevalence", "prevalence_attrition"))) {
        stop("Cannot assert `Prevalence` result")
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
      data <- IncidencePrevalence::asPrevalenceResult(data) %>%
        { if (!"analysis_interval" %in% names(.)) dplyr::mutate(., analysis_interval = "overall") else .} %>%
        dplyr::mutate(analysis_min_cell_count = !!minCellCount) %>%
        dplyr::rename(
          database = cdm_name,
          n_cases = outcome_count,
          n_population = denominator_count
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
      allDatabases <- unique(private$.tidyData$database)
      selectedDatabases <- allDatabases[1]
      databaseStr <- "database"
      if (databaseStr %in% names(private$.defaults) && all(private$.defaults[[databaseStr]] %in% allDatabases)) {
        selectedDatabases <- private$.defaults[[databaseStr]]
      }
      private$.pickers[["cdm"]] <- InputPanel$new(
        funs = list(cdm = shinyWidgets::pickerInput),
        args = list(cdm = list(
          inputId = "cdm", label = "Database", choices = allDatabases, selected = selectedDatabases, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["cdm"]]$parentNamespace <- self$namespace

      # outcome
      allOutcomes <- unique(private$.tidyData$outcome_cohort_name)
      selectedOutcomes <- allOutcomes[1]
      outcomeStr <- "outcome"
      if (outcomeStr %in% names(private$.defaults) && all(private$.defaults[[outcomeStr]] %in% allOutcomes)) {
        selectedOutcomes <- private$.defaults[[outcomeStr]]
      }
      private$.pickers[["outcome"]] <- InputPanel$new(
        funs = list(outcome = shinyWidgets::pickerInput),
        args = list(outcome = list(
          inputId = "outcome", label = "Outcome", choices = allOutcomes, selected = selectedOutcomes, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["outcome"]]$parentNamespace <- self$namespace

      # denominator age group
      allAgeGroups <- unique(private$.tidyData$denominator_age_group)
      selectedAgeGroups <- allAgeGroups[1]
      ageGroupStr <- "ageGroup"
      if (ageGroupStr %in% names(private$.defaults) && all(private$.defaults[[ageGroupStr]] %in% allAgeGroups)) {
        selectedAgeGroups <- private$.defaults[[ageGroupStr]]
      }
      private$.pickers[["denomAgeGroup"]] <- InputPanel$new(
        funs = list(age_group = shinyWidgets::pickerInput),
        args = list(age_group = list(
          inputId = "age_group", label = "Age group", choices = allAgeGroups, selected = selectedAgeGroups, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomAgeGroup"]]$parentNamespace <- self$namespace

      # start date
      allStartDates <- unique(private$.tidyData$prevalence_start_date)
      selectedStartDate <- allStartDates
      intervalStartDateStr <- "interval_start_date"
      if (intervalStartDateStr %in% names(private$.defaults) && all(private$.defaults[[intervalStartDateStr]] %in% allStartDates)) {
        selectedStartDate <- private$.defaults[[intervalStartDateStr]]
      }
      private$.pickers[["startDate"]] <- InputPanel$new(
        funs = list(year = shinyWidgets::pickerInput),
        args = list(year = list(
          inputId = "year", choices = allStartDates, label = "Year", selected = selectedStartDate, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["startDate"]]$parentNamespace <- self$namespace

      # plot pickers
      plotDataChoices <- c(
        "database", "outcome_cohort_name", "strata", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
        "denominator_start_date", "denominator_end_date", "denominator_time_at_risk", "analysis_complete_database_intervals",
        "analysis_min_cell_count", "analysis_interval", "prevalence_start_date"
      )
      # x-axis
      private$.pickers[["xAxis"]] <- InputPanel$new(
        funs = list(xAxis = shinyWidgets::pickerInput),
        args = list(xAxis = list(
          inputId = "xAxis", choices = plotDataChoices, label = "Prevalence_start_date", selected = "prevalence_start_date", multiple = F,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["xAxis"]]$parentNamespace <- self$namespace

      # facet by
      private$.pickers[["facet"]] <- InputPanel$new(
        funs = list(facet_by = shinyWidgets::pickerInput),
        args = list(facet_by = list(
          inputId = "facet_by", choices = plotDataChoices, label = "Facet by", selected = c("outcome_cohort_name", "database"), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["facet"]]$parentNamespace <- self$namespace

      # color by
      private$.pickers[["color"]] <- InputPanel$new(
        funs = list(color_by = shinyWidgets::pickerInput),
        args = list(color_by = list(
          inputId = "color_by", choices = plotDataChoices, label = "Colour by", selected = c(), multiple = TRUE,
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
          inputId = "rotateXLabels", choices = c(TRUE, FALSE), label = "Rotate x-axis labels", selected = FALSE
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["rotateXLabels"]]$parentNamespace <- self$namespace

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
      groupColumnOptions <- c("outcome_cohort_name", "cdm_name")
      private$.pickers[["groupColumn"]] <- InputPanel$new(
        funs = list(groupColumn = shinyWidgets::pickerInput),
        args = list(groupColumn = list(
          inputId = "groupColumn", choices = groupColumnOptions, label = "Group columns", selected = groupColumnOptions[1], multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["groupColumn"]]$parentNamespace <- self$namespace

      # settingsColumn
      settingColumnOptions <- c("denominator_time_at_risk", "denominator_age_group", "denominator_sex")
      private$.pickers[["settingsColumn"]] <- InputPanel$new(
        funs = list(settingsColumn = shinyWidgets::pickerInput),
        args = list(settingsColumn = list(
          inputId = "settingsColumn", choices = settingColumnOptions, label = "Settings columns", selected = settingColumnOptions, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["settingsColumn"]]$parentNamespace <- self$namespace

      # hideColumn
      hideColumnOptions <- c("denominator_time_at_risk", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "analysis_interval")
      private$.pickers[["hideColumn"]] <- InputPanel$new(
        funs = list(hideColumn = shinyWidgets::pickerInput),
        args = list(hideColumn = list(
          inputId = "hideColumn", choices = hideColumnOptions, label = "Hide columns", selected = hideColumnOptions, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["hideColumn"]]$parentNamespace <- self$namespace
    }
  )
)
