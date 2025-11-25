library(DarwinShinyModules)

PregnancyFrequencyModule <- R6::R6Class(
  classname = "PregnancyFrequencyModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name), height = "600px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.height <- height

      private$.table <- Table$new(data = data)
      private$.table$parentNamespace <- self$namespace

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
    }
  ),

  private = list(
    .data = NULL,
    .dp = NULL,
    .height = NULL,
    .table = NULL,
    .inputPanelCDM = NULL,


    .UI = function() {
      shiny::tagList(
        private$.inputPanelCDM$UI(),
        plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      # tables
      private$.table$server(input, output, session)
      # input filters
      private$.inputPanelCDM$server(input, output, session)

      getData <- reactive({
        data <- NULL
        if ("cdm_name" %in% colnames(private$.data)) {
          data <-  private$.data %>%
            dplyr::filter(.data$cdm_name %in% private$.inputPanelCDM$inputValues$cdm_name)
        } else {
          nameCols <- setdiff(colnames(private$.data), private$.dp)

          data <-  private$.data %>%
            dplyr::select(c(nameCols, private$.inputPanelCDM$inputValues$cdm_name))
        }
        return(data)
      })

      getPlotData <- reactive({
        getData() %>%
          tidyr::pivot_longer(cols = setdiff(colnames(.), c("freq")), names_to = "cdm_name") %>%
          dplyr::mutate(value = as.numeric(value))
      })

      # handle updates
      shiny::observeEvent(private$.inputPanelCDM$inputValues$cdm_name, {
        private$.table$data <- getData()
        private$.table$server(input, output, session)
      }, ignoreNULL = FALSE)

      ### make plot ----
      createPlot <- reactive({
        plot <- NULL
        data <- getPlotData()
        if (!is.null(data) && nrow(data) > 0) {
          plot <- barPlot(data = data,
                          xVar = "freq",
                          yVar = "value",
                          fillVar = "freq",
                          facetVar = "cdm_name",
                          xLabel = "Freq",
                          yLabel = "Count",
                          title = "Pregnancy frequency",
                          rotateAxisText = TRUE,
                          flipCoordinates = FALSE)
        }
        return(plot)
      })

      output$plot <- renderPlotly({
        createPlot()
      })
    }
  )

)
