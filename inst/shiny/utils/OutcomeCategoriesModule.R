library(DarwinShinyModules)

OutcomeCategoriesModule <- R6::R6Class(
  classname = "OutcomeCategoriesModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name), height = "420px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.height <- height

      private$.table <- Table$new(data = data, title = "Outcome categories data")
      private$.table$parentNamespace <- self$namespace

      private$.inputPanelCDM <- createDatabasePicker(private$.dp, self$namespace)
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
        shiny::tabsetPanel(
          id = shiny::NS(private$.namespace, "outcomeCategoriesTabs"),
          shiny::tabPanel(
            "Data",
            private$.table$UI()
          ),
          shiny::tabPanel(
            "Plot",
            plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>% shinycssloaders::withSpinner()
          )
        )
      )
    },

    .server = function(input, output, session) {
      # tables
      private$.table$server(input, output, session)
      # input filters
      private$.inputPanelCDM$server(input, output, session)

      getData <- shiny::reactive({
        cdmSel <- getSelectedCdm(private$.inputPanelCDM, private$.dp)
        private$.data %>%
          dplyr::filter(.data$cdm_name %in% cdmSel)
      })

      getPlotData <- shiny::reactive({
        getData()
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
          plot <- barPlot(data = data,
                          xVar = "outcome_category",
                          yVar = "pct",
                          fillVar = "algorithm",
                          facetVar = "cdm_name",
                          rotateAxisText = TRUE,
                          position = "dodge")
        }
        return(plot)
      })

      output$plot <- plotly::renderPlotly({
        p <- createPlot()
        if (is.null(p)) {
          return(emptyPlotlyMessage("No data for selected filters."))
        }
        p
      })
    }
  )

)
