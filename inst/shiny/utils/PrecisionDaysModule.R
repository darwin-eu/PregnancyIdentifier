library(DarwinShinyModules)

PrecisionDaysModule <- R6::R6Class(
  classname = "PrecisionDaysModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name), height = "420px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.height <- height

      # cdm_name filter: multiple selection, default all
      private$.inputPanelCDM <- InputPanel$new(
        fun = list(cdm_name = shinyWidgets::pickerInput),
        args = list(cdm_name = list(
          inputId = "cdm_name",
          label = "Database",
          choices = private$.dp,
          selected = private$.dp,
          multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.inputPanelCDM$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .data = NULL,
    .dp = NULL,
    .height = NULL,
    .inputPanelCDM = NULL,

    .UI = function() {
      shiny::tagList(
        private$.inputPanelCDM$UI(),
        plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>%
          shinycssloaders::withSpinner()
      )
    },

    .server = function(input, output, session) {
      private$.inputPanelCDM$server(input, output, session)

      getData <- shiny::reactive({
        sel <- private$.inputPanelCDM$inputValues$cdm_name
        if (is.null(sel) || length(sel) == 0) sel <- private$.dp
        if (!"cdm_name" %in% colnames(private$.data)) return(private$.data)
        private$.data %>%
          dplyr::filter(.data$cdm_name %in% sel)
      })

      output$plot <- plotly::renderPlotly({
        data <- getData()
        if (is.null(data) || nrow(data) == 0) {
          return(plotly::plot_ly() %>%
            plotly::layout(
              title = list(text = "No precision days data available.", font = list(size = 14)),
              xaxis = list(title = "ESD precision (days)"),
              yaxis = list(title = "Density")
            ))
        }
        # Ensure numeric and sort so each database line is drawn in x order
        data <- data %>%
          dplyr::mutate(
            esd_precision_days = as.numeric(.data$esd_precision_days),
            density = as.numeric(.data$density)
          ) %>%
          dplyr::filter(!is.na(.data$esd_precision_days), !is.na(.data$density)) %>%
          dplyr::arrange(.data$cdm_name, .data$esd_precision_days)

        if (nrow(data) == 0) {
          return(plotly::plot_ly() %>%
            plotly::layout(
              title = list(text = "No valid data for selected filters.", font = list(size = 14)),
              xaxis = list(title = "ESD precision (days)"),
              yaxis = list(title = "Density")
            ))
        }

        # One line per database: use color = ~cdm_name with type = "scatter", mode = "lines"
        plotly::plot_ly(
          data = data,
          x = ~esd_precision_days,
          y = ~density,
          color = ~cdm_name,
          type = "scatter",
          mode = "lines",
          hovertemplate = "ESD precision: %{x}<br>Density: %{y}<extra></extra>"
        ) %>%
          plotly::layout(
            xaxis = list(title = "ESD precision (days)"),
            yaxis = list(title = "Density"),
            showlegend = TRUE,
            legend = list(title = list(text = "Database"))
          )
      })
    }
  )
)
