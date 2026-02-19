library(DarwinShinyModules)

TemporalPatternsModule <- R6::R6Class(
  classname = "TemporalPatternsModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, missingData, dp = unique(data$cdm_name), height = "600px") {
      super$initialize()
      private$.data <- data
      private$.missingData <- missingData
      private$.dp <- dp
      private$.height <- height

      private$.missingTable <- Table$new(data = missingData)
      private$.missingTable$parentNamespace <- self$namespace

      # input pickers
      private$.inputPanelCDM <- InputPanel$new(fun = list(cdm_name = shinyWidgets::pickerInput),
                                               args = list(cdm_name = list(
                                                 inputId = "cdm_name", label = "Database",
                                                 choices = private$.dp,
                                                 selected = private$.dp,
                                                 multiple = TRUE,
                                                 options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
                                               growDirection = "horizontal")
      private$.inputPanelCDM$parentNamespace <- self$namespace

      private$.inputPanelPeriod <- InputPanel$new(fun = list(period = shinyWidgets::pickerInput),
                                               args = list(period = list(
                                                 inputId = "period", label = "Time period",
                                                 choices = c("year", "month"),
                                                 selected = "year",
                                                 multiple = FALSE)),
                                               growDirection = "horizontal")
      private$.inputPanelPeriod$parentNamespace <- self$namespace

      columnChoices <- unique(private$.data$column)
      if (length(columnChoices) == 0) columnChoices <- "column"
      private$.columnChoices <- columnChoices
      private$.inputPanelColumn <- InputPanel$new(fun = list(column = shinyWidgets::pickerInput),
                                                  args = list(column = list(
                                                    inputId = "column", label = "Columns",
                                                    choices = columnChoices,
                                                    selected = columnChoices[1:min(4L, length(columnChoices))],
                                                    multiple = TRUE,
                                                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
                                                  growDirection = "horizontal")
      private$.inputPanelColumn$parentNamespace <- self$namespace

      valNum <- suppressWarnings(as.numeric(private$.data$value))
      minVal <- if (length(valNum) > 0 && !all(is.na(valNum))) min(valNum, na.rm = TRUE) else 2000
      maxVal <- if (length(valNum) > 0 && !all(is.na(valNum))) max(valNum, na.rm = TRUE) else 2020
      if (!is.finite(minVal)) minVal <- 2000
      if (!is.finite(maxVal)) maxVal <- 2020
      defaultYear <- min(max(minVal, 2000), maxVal)
      private$.defaultMinYear <- defaultYear
      private$.inputPanelMinYear <- InputPanel$new(fun = list(min_year = shiny::sliderInput),
                                                    args = list(min_year = list(
                                                      inputId = "min_year", label = "Min year",
                                                      min = minVal,
                                                      max = maxVal,
                                                      value = defaultYear)),
                                                    growDirection = "horizontal")
      private$.inputPanelMinYear$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .data = NULL,
    .missingData = NULL,
    .dp = NULL,
    .columnChoices = NULL,
    .defaultMinYear = NULL,
    .height = NULL,
    .missingTable = NULL,
    .inputPanelCDM = NULL,
    .inputPanelPeriod = NULL,
    .inputPanelColumn = NULL,
    .inputPanelMinYear = NULL,

    .UI = function() {
      emptyMsg <- is.null(private$.data) || nrow(private$.data) == 0
      dataTableOut <- shiny::tagList(
        shiny::h4("Data"),
        DT::DTOutput(shiny::NS(private$.namespace, "dataTable"))
      )
      missingTableOut <- shiny::tagList(
        shiny::h4("Missing data"),
        DT::DTOutput(shiny::NS(private$.namespace, "missingDataTable"))
      )
      if (emptyMsg) {
        return(shiny::tagList(
          shiny::p("Results files are empty.", style = "margin: 20px; font-size: 16px; font-weight: bold;"),
          dataTableOut
        ))
      }
      shiny::tagList(
        private$.inputPanelCDM$UI(),
        private$.inputPanelPeriod$UI(),
        private$.inputPanelColumn$UI(),
        private$.inputPanelMinYear$UI(),
        shiny::tabsetPanel(
          id = shiny::NS(private$.namespace, "tabsetPanel"),
          type = "tabs",
          shiny::tabPanel(
            "Data",
            plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>% shinycssloaders::withSpinner(),
            dataTableOut
          ),
          shiny::tabPanel(
            "Missing data",
            plotly::plotlyOutput(shiny::NS(private$.namespace, "missingPlot"), height = private$.height) %>% shinycssloaders::withSpinner(),
            private$.missingTable$UI(),
            missingTableOut
          )
        )
      )
    },

    .server = function(input, output, session) {
      # tables
      private$.missingTable$server(input, output, session)
      # input filters
      private$.inputPanelCDM$server(input, output, session)
      private$.inputPanelPeriod$server(input, output, session)
      private$.inputPanelColumn$server(input, output, session)
      private$.inputPanelMinYear$server(input, output, session)

      getData <- shiny::reactive({
        cdmSel <- private$.inputPanelCDM$inputValues$cdm_name
        if (is.null(cdmSel) || length(cdmSel) == 0) cdmSel <- private$.dp
        periodSel <- private$.inputPanelPeriod$inputValues$period
        if (is.null(periodSel) || length(periodSel) == 0) periodSel <- "year"
        columnSel <- private$.inputPanelColumn$inputValues$column
        if (is.null(columnSel) || length(columnSel) == 0) columnSel <- private$.columnChoices

        data <-  private$.data %>%
          dplyr::filter(.data$cdm_name %in% cdmSel) %>%
          dplyr::filter(.data$period %in% periodSel) %>%
          dplyr::filter(.data$column %in% columnSel)

        if (periodSel == "year") {
          minYear <- private$.inputPanelMinYear$inputValues$min_year
          if (is.null(minYear)) minYear <- private$.defaultMinYear
          if (!is.null(minYear)) {
            data <- data %>%
              dplyr::filter(as.numeric(.data$value) >= minYear)
          }
        }
        return(data)
      })

      getMissingData <- shiny::reactive({
        cdmSel <- private$.inputPanelCDM$inputValues$cdm_name
        if (is.null(cdmSel) || length(cdmSel) == 0) cdmSel <- private$.dp
        periodSel <- private$.inputPanelPeriod$inputValues$period
        if (is.null(periodSel) || length(periodSel) == 0) periodSel <- "year"
        columnSel <- private$.inputPanelColumn$inputValues$column
        if (is.null(columnSel) || length(columnSel) == 0) columnSel <- private$.columnChoices

        data <-  private$.missingData %>%
          dplyr::filter(.data$cdm_name %in% cdmSel) %>%
          dplyr::filter(.data$period %in% periodSel) %>%
          dplyr::filter(.data$column %in% columnSel)

        return(data)
      })

      # handle updates
      shiny::observeEvent(c(private$.inputPanelCDM$inputValues$cdm_name,
                            private$.inputPanelPeriod$inputValues$period,
                            private$.inputPanelMinYear$inputValues$min_year,
                            private$.inputPanelColumn$inputValues$column), {
        private$.missingTable$data <- getMissingData()
        private$.missingTable$server(input, output, session)
      }, ignoreNULL = FALSE)

      ### make plot ----
      createPlot <- shiny::reactive({
        plot <- NULL
        data <- getData()
        if (!is.null(data) && nrow(data) > 0) {
          period <- private$.inputPanelPeriod$inputValues$period
          if (is.null(period) || length(period) == 0) period <- "year"
          # Single capitalized axis label (e.g. "Year", "Month")
          xLabel <- if (length(period) == 0) "Period" else paste0(toupper(substring(period[1], 1, 1)), substring(period[1], 2))
          # Sort so line plot shows correct temporal order
          if (period == "year") {
            data <- data %>% dplyr::arrange(as.numeric(.data$value))
          } else if (period == "month") {
            data <- data %>% dplyr::arrange(match(.data$value, base::month.name))
          }
          plot <- trendsPlot(data = data,
                             xVar = "value",
                             xLabel = xLabel,
                             facetVar = "cdm_name")
        }
        return(plot)
      })

      output$plot <- plotly::renderPlotly({
        p <- createPlot()
        if (is.null(p)) {
          msg <- if (nrow(private$.data) == 0) "Results files are empty." else "No data for selected filters."
          return(emptyPlotlyMessage(msg))
        }
        p
      })

      createMissingPlot <- shiny::reactive({
        plot <- NULL
        data <- getMissingData()
        if (!is.null(data) && nrow(data) > 0) {
          plot <- barPlot(data = data,
                          xVar = "column",
                          yVar = "count",
                          fillVar = "column",
                          facetVar = "cdm_name",
                          rotateAxisText = TRUE,
                          flipCoordinates = FALSE)
        }
        return(plot)
      })

      output$missingPlot <- plotly::renderPlotly({
        p <- createMissingPlot()
        if (is.null(p)) {
          msg <- if (nrow(private$.missingData) == 0) "Results files are empty." else "No data for selected filters."
          return(emptyPlotlyMessage(msg))
        }
        p
      })

      output$dataTable <- DT::renderDT({
        DT::datatable(
          private$.data,
          options = list(scrollX = TRUE, pageLength = 25),
          filter = "top"
        )
      })
      output$missingDataTable <- DT::renderDT({
        DT::datatable(
          private$.missingData,
          options = list(scrollX = TRUE, pageLength = 25),
          filter = "top"
        )
      })
    }
  )
)
