library(DarwinShinyModules)

EpisodeFrequencyModule <- R6::R6Class(
  classname = "EpisodeFrequencyModule",
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

    # Display names for the episode frequency table (statistics are "episodes per person")
    .nameLabels = c(
      total_episodes = "Total episodes",
      total_individuals = "Total individuals",
      min = "Minimum episodes per person",
      Q25 = "25th percentile (episodes per person)",
      median = "Median episodes per person",
      Q75 = "75th percentile (episodes per person)",
      max = "Maximum episodes per person",
      mean = "Mean episodes per person",
      sd = "Standard deviation (episodes per person)",
      pkg_version = "Package version",
      date_run = "Date run",
      date_export = "Date export"
    ),


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
        getData() %>%
          dplyr::filter(name %in% c("total_episodes", "total_individuals")) %>%
          tidyr::pivot_longer(cols = setdiff(colnames(.), c("name")), names_to = "cdm_name") %>%
          dplyr::mutate(value = as.numeric(value))
      })

      # handle updates: show table with display names for statistics
      shiny::observeEvent(private$.inputPanelCDM$inputValues$cdm_name, {
        tableData <- getData()
        if (nrow(tableData) > 0 && "name" %in% colnames(tableData)) {
          tableData <- tableData %>%
            dplyr::mutate(name = ifelse(
              .data$name %in% names(private$.nameLabels),
              unname(private$.nameLabels[.data$name]),
              .data$name
            ))
        }
        private$.table$data <- tableData
        private$.table$server(input, output, session)
      }, ignoreNULL = FALSE)

      ### make plot ----
      createPlot <- shiny::reactive({
        plot <- NULL
        data <- getPlotData()
        if (!is.null(data) && nrow(data) > 0) {
          plot <- barPlot(data = data,
                          xVar = "name",
                          yVar = "value",
                          fillVar = "name",
                          facetVar = "cdm_name",
                          rotateAxisText = TRUE)
        }
        return(plot)
      })

      output$plot <- plotly::renderPlotly({
        createPlot()
      })
    }
  )

)
