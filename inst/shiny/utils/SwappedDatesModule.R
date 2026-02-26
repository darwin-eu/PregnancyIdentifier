library(DarwinShinyModules)

#' Module for displaying swapped (reversed) dates data with a bar chart.
#'
#' Shows episodes where start date is after end date per database,
#' with both a grouped bar chart and a formatted data table.
SwappedDatesModule <- R6::R6Class(
  classname = "SwappedDatesModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name), height = "420px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.height <- height

      private$.table <- Table$new(data = data, title = "Swapped dates")
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
          id = shiny::NS(private$.namespace, "swappedDatesTabs"),
          shiny::tabPanel(
            "Plot",
            plotly::plotlyOutput(shiny::NS(private$.namespace, "swappedPlot"), height = private$.height) %>%
              shinycssloaders::withSpinner()
          ),
          shiny::tabPanel(
            "Data",
            private$.table$UI()
          )
        )
      )
    },

    .server = function(input, output, session) {
      private$.table$server(input, output, session)
      private$.inputPanelCDM$server(input, output, session)

      getData <- shiny::reactive({
        cdmSel <- getSelectedCdm(private$.inputPanelCDM, private$.dp)
        d <- private$.data
        if (is.null(d) || nrow(d) == 0) return(d)
        if ("cdm_name" %in% colnames(d)) {
          d <- d %>% dplyr::filter(.data$cdm_name %in% cdmSel)
        }
        d
      })

      shiny::observeEvent(private$.inputPanelCDM$inputValues$cdm_name, {
        private$.table$data <- getData()
      }, ignoreNULL = FALSE)

      output$swappedPlot <- plotly::renderPlotly({
        d <- getData()
        if (is.null(d) || nrow(d) == 0) {
          return(emptyPlotlyMessage("No swapped dates data for selected filters."))
        }

        # Expect columns: cdm_name, metric, n_swapped, total, pct
        metricCol <- if ("metric" %in% colnames(d)) "metric" else NULL
        nCol <- if ("n_swapped" %in% colnames(d)) "n_swapped" else NULL
        pctCol <- if ("pct" %in% colnames(d)) "pct" else NULL

        if (is.null(metricCol) || (is.null(nCol) && is.null(pctCol))) {
          return(emptyPlotlyMessage("Swapped dates data does not have expected columns."))
        }

        # Filter to only the reversal metrics (not totals)
        plotData <- d %>%
          dplyr::filter(!grepl("total|Total", .data[[metricCol]], ignore.case = TRUE))

        if (nrow(plotData) == 0) {
          return(emptyPlotlyMessage("No reversal metrics found in the data."))
        }

        # Use pct if available, otherwise n_swapped
        if (!is.null(pctCol)) {
          plotData <- plotData %>%
            dplyr::mutate(
              y_val = suppressWarnings(as.numeric(.data[[pctCol]])),
              n_val = if (!is.null(nCol)) suppressWarnings(as.numeric(.data[[nCol]])) else NA_real_,
              hover = paste0(
                .data[[metricCol]], "\n",
                .data$cdm_name, "\n",
                if (!is.null(nCol)) paste0("N: ", format(.data$n_val, big.mark = ","), "\n") else "",
                round(.data$y_val, 2), "%"
              )
            )
          yLabel <- "% of Total Episodes"
        } else {
          plotData <- plotData %>%
            dplyr::mutate(
              y_val = suppressWarnings(as.numeric(.data[[nCol]])),
              hover = paste0(
                .data[[metricCol]], "\n",
                .data$cdm_name, "\n",
                "N: ", format(.data$y_val, big.mark = ",")
              )
            )
          yLabel <- "Count"
        }

        plotData <- plotData %>% dplyr::filter(!is.na(.data$y_val))
        if (nrow(plotData) == 0) {
          return(emptyPlotlyMessage("No numeric data available."))
        }

        p <- ggplot2::ggplot(plotData, ggplot2::aes(
          x = .data$cdm_name, y = .data$y_val,
          fill = .data[[metricCol]],
          text = .data$hover
        )) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::scale_fill_manual(values = c(
            "HIP reversed (start after end)" = "#E41A1C",
            "PPS reversed (start after end)" = "#377EB8",
            "ESD (final episode) reversed (start after end)" = "#4DAF4A"
          ), name = "Metric") +
          ggplot2::labs(x = "Database", y = yLabel) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
            legend.position = "bottom"
          )
        plotly::ggplotly(p, tooltip = "text")
      })
    }
  )
)
