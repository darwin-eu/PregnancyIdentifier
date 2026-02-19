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
        plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>% shinycssloaders::withSpinner(),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      # tables
      private$.table$server(input, output, session)
      # input filters
      private$.inputPanelCDM$server(input, output, session)

      getData <- shiny::reactive({
        data <- NULL
        if ("cdm_name" %in% colnames(private$.data)) {
          data <-  private$.data %>%
            dplyr::filter(.data$cdm_name %in% private$.inputPanelCDM$inputValues$cdm_name)
        } else {
          nameCols <- setdiff(colnames(private$.data), private$.dp)

          data <-  private$.data %>%
            dplyr::select(dplyr::any_of(c(nameCols, private$.inputPanelCDM$inputValues$cdm_name)))
        }
        return(data)
      })

      getPlotData <- shiny::reactive({
        result <- getData() %>%
          tidyr::pivot_longer(cols = setdiff(colnames(.), c("freq")), names_to = "cdm_name") %>%
          dplyr::mutate(value = as.numeric(value))
        totalResult <- result %>%
          dplyr::group_by(cdm_name) %>%
          dplyr::summarise(total = sum(value, na.rm = T))
        result %>% dplyr::left_join(totalResult, by = "cdm_name") %>%
          dplyr::mutate(perc = round(100*(value/total), 2))
      })

      # handle updates
      shiny::observeEvent(private$.inputPanelCDM$inputValues$cdm_name, {
        private$.table$data <- getData()
        private$.table$server(input, output, session)
      }, ignoreNULL = FALSE)

      ### make plot ----
      createPlot <- shiny::reactive({
        plot <- NULL
        data <- getPlotData()
        if (!is.null(data) && nrow(data) > 0) {
          # create custom facet label
          facetVector <- data %>%
            dplyr::filter(freq == 1) %>%
            dplyr::mutate(label = glue::glue("{perc}% first, {round(100-perc, 2)}% multiple pregnancies")) %>%
            dplyr::select(cdm_name, label)

          plot_labeller <- ggplot2::as_labeller(function(x) {
            vapply(x, function(val) {
              perc <- data %>% dplyr::filter(.data$freq == 1, .data$cdm_name == val) %>% dplyr::pull("perc") %>% round(digits = 1)
              perc2 <- round(100 - perc, digits = 1)
              as.character(glue::glue("{val} - 1: {perc}%, ≥1: {perc2}%"))
            }, character(1))
          })

          plot <- barPlot(data = data,
                          xVar = "freq",
                          yVar = "value",
                          fillVar = "freq",
                          facetVar = "cdm_name",
                          labelFunction = plot_labeller,
                          xLabel = "Freq",
                          yLabel = "Count",
                          title = "Pregnancy frequency",
                          rotateAxisText = TRUE,
                          flipCoordinates = FALSE,
                          facetTextSize = 7)
        }
        return(plot)
      })

      output$plot <- plotly::renderPlotly({
        createPlot()
      })
    }
  )

)
