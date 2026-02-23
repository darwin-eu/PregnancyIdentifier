library(DarwinShinyModules)

AgeSummaryModule <- R6::R6Class(
  classname = "AgeSummaryModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name), height = "420px") {
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
        # Data is long format: cdm_name, name, value (and optionally colName, etc.)
        if (!"cdm_name" %in% colnames(private$.data)) return(private$.data)
        private$.data %>%
          dplyr::filter(.data$cdm_name %in% private$.inputPanelCDM$inputValues$cdm_name)
      })

      getPlotData <- shiny::reactive({
        # One row per CDM with min, Q25, median, Q75, max for horizontal boxplot
        ageMetrics <- c("min", "Q25", "median", "Q75", "max")
        d <- getData() %>%
          dplyr::filter(.data$name %in% ageMetrics) %>%
          dplyr::mutate(value = as.numeric(.data$value))
        if (nrow(d) == 0) return(d)
        idCols <- setdiff(colnames(d), c("name", "value"))
        d <- d %>%
          tidyr::pivot_wider(names_from = "name", values_from = "value") %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::slice(1L) %>%
          dplyr::ungroup()
        if (!all(ageMetrics %in% colnames(d))) return(d[0, ])
        d %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(ageMetrics), as.numeric))
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
          # Horizontal boxplot: y = cdm_name (one per CDM), x = age metric (min–max)
          plot <- boxPlot(data = data, horizontal = TRUE)
        }
        return(plot)
      })

      output$plot <- plotly::renderPlotly({
        createPlot()
      })
    }
  )

)
