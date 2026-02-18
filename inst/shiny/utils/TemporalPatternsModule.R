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

      private$.table <- Table$new(data = data)
      private$.table$parentNamespace <- self$namespace
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
      private$.inputPanelColumn <- InputPanel$new(fun = list(column = shinyWidgets::pickerInput),
                                                  args = list(column = list(
                                                    inputId = "column", label = "Columns",
                                                    choices = columnChoices,
                                                    selected = columnChoices[1:4],
                                                    multiple = TRUE,
                                                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
                                                  growDirection = "horizontal")
      private$.inputPanelColumn$parentNamespace <- self$namespace

      private$.inputPanelMinYear <- InputPanel$new(fun = list(min_year = shiny::sliderInput),
                                                    args = list(min_year = list(
                                                      inputId = "min_year", label = "Min year",
                                                      min = min(as.numeric(private$.data$value), na.rm = T),
                                                      max = max(as.numeric(private$.data$value), na.rm = T),
                                                      value = 2000)),
                                                    growDirection = "horizontal")
      private$.inputPanelMinYear$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .data = NULL,
    .missingData = NULL,
    .dp = NULL,
    .height = NULL,
    .table = NULL,
    .missingTable = NULL,
    .inputPanelCDM = NULL,
    .inputPanelPeriod = NULL,
    .inputPanelColumn = NULL,
    .inputPanelMinYear = NULL,

    .UI = function() {
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
            plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height),
            private$.table$UI()
          ),
          shiny::tabPanel(
            "Missing data",
            plotly::plotlyOutput(shiny::NS(private$.namespace, "missingPlot"), height = private$.height),
            private$.missingTable$UI()
          )
        )
      )
    },

    .server = function(input, output, session) {
      # tables
      private$.table$server(input, output, session)
      # input filters
      private$.inputPanelCDM$server(input, output, session)
      private$.inputPanelPeriod$server(input, output, session)
      private$.inputPanelColumn$server(input, output, session)
      private$.inputPanelMinYear$server(input, output, session)

      getData <- shiny::reactive({
        data <-  private$.data %>%
          dplyr::filter(.data$cdm_name %in% private$.inputPanelCDM$inputValues$cdm_name) %>%
          dplyr::filter(.data$period %in% private$.inputPanelPeriod$inputValues$period) %>%
          dplyr::filter(.data$column %in% private$.inputPanelColumn$inputValues$column)

        if (private$.inputPanelPeriod$inputValues$period == "year") {
          data <- data %>%
            dplyr::filter(as.numeric(.data$value) >= private$.inputPanelMinYear$inputValues$min_year)
        }
        return(data)
      })

      getMissingData <- shiny::reactive({
        data <-  private$.missingData %>%
          dplyr::filter(.data$cdm_name %in% private$.inputPanelCDM$inputValues$cdm_name) %>%
          dplyr::filter(.data$period %in% private$.inputPanelPeriod$inputValues$period) %>%
          dplyr::filter(.data$column %in% private$.inputPanelColumn$inputValues$column)

        return(data)
      })

      # handle updates
      shiny::observeEvent(c(private$.inputPanelCDM$inputValues$cdm_name,
                            private$.inputPanelPeriod$inputValues$period,
                            private$.inputPanelMinYear$inputValues$min_year,
                            private$.inputPanelColumn$inputValues$column), {

        private$.table$data <- getData()
        private$.table$server(input, output, session)
        private$.missingTable$data <- getMissingData()
        private$.missingTable$server(input, output, session)
      }, ignoreNULL = FALSE)

      ### make plot ----
      createPlot <- shiny::reactive({
        plot <- NULL
        data <- getData()
        if (!is.null(data) && nrow(data) > 0) {
          plot <- trendsPlot(data = data,
                             xVar = "value",
                             xLabel = unique(data$period),
                             facetVar = "cdm_name")
        }
        return(plot)
      })

      output$plot <- plotly::renderPlotly({
        createPlot()
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
        createMissingPlot()
      })
    }
  )
)
