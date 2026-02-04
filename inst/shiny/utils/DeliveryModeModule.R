library(DarwinShinyModules)

DeliveryModeModule <- R6::R6Class(
  classname = "DeliveryModeModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name), convertDataForPlot = FALSE, label = NULL, title = "",
                          yVar = "pct", fillVar = "cdm_name", position = "stack", height = "600px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.convertDataForPlot <- convertDataForPlot
      private$.label <- label
      private$.title <- title
      private$.yVar <- yVar
      private$.fillVar <- fillVar
      private$.height <- height
      private$.position <- position

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
    .convertDataForPlot = NULL,
    .title = NULL,
    .label = NULL,
    .yVar = NULL,
    .fillVar = NULL,
    .height = NULL,
    .position = NULL,
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
            dplyr::filter(.data$cdm_name %in% private$.inputPanelCDM$inputValues$cdm_name) %>%
            dplyr::arrange(cdm_name) %>%
            dplyr::mutate(cdm_name = factor(cdm_name, levels = rev(private$.dp)))
        } else {
          nameCols <- setdiff(colnames(private$.data), private$.dp)

          data <-  private$.data %>%
            dplyr::select(c(nameCols, private$.inputPanelCDM$inputValues$cdm_name))
        }
        return(data)
      })

      getPlotData <- reactive({
        data <- getData()
        if (private$.convertDataForPlot) {
          data <- data %>%
            tidyr::pivot_longer(cols = setdiff(colnames(.), c("name", "value")), names_to = "cdm_name") %>%
            dplyr::mutate(cdm_name = factor(cdm_name, levels = rev(private$.dp))) %>%
            dplyr::mutate(value = as.numeric(value)) %>%
            dplyr::rename(!!private$.yVar := value)
          if ("name" %in% colnames(data) &&  any(grepl("perc", unique(data$name)))) {
            data <- data %>% dplyr::filter(grepl("perc", name))
          }
        }
        return(data)
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
                          xVar = "cdm_name",
                          yVar = private$.yVar,
                          fillVar = private$.fillVar,
                          facetVar = "final_outcome_category",
                          label = private$.label,
                          position = private$.position,
                          xLabel = "cdm_name",
                          yLabel = private$.yVar,
                          title = private$.title,
                          rotateAxisText = TRUE,
                          flipCoordinates = TRUE)
        }
        return(plot)
      })

      output$plot <- renderPlotly({
        createPlot()
      })
    }
  )
)
