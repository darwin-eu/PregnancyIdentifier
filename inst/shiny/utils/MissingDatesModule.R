library(DarwinShinyModules)

#' Module for displaying missing dates data with both a bar chart and data table.
#'
#' Replaces FilterTableModule for the Missing dates tab to add a visual
#' bar chart of missing-date percentages by date field and database.
MissingDatesModule <- R6::R6Class(
  classname = "MissingDatesModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name), height = "420px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.height <- height

      private$.table <- Table$new(data = data, title = "Missing dates")
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
          id = shiny::NS(private$.namespace, "missingDatesTabs"),
          shiny::tabPanel(
            "Plot",
            plotly::plotlyOutput(shiny::NS(private$.namespace, "missingPlot"), height = private$.height) %>%
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

      output$missingPlot <- plotly::renderPlotly({
        d <- getData()
        if (is.null(d) || nrow(d) == 0) {
          return(emptyPlotlyMessage("No missing dates data for selected filters."))
        }

        # Expect columns: cdm_name, "Date field", "Percent missing" (or percent_missing)
        pctCol <- if ("Percent missing" %in% colnames(d)) "Percent missing"
                  else if ("% Missing" %in% colnames(d)) "% Missing"
                  else if ("percent_missing" %in% colnames(d)) "percent_missing"
                  else NULL
        dateCol <- if ("Date field" %in% colnames(d)) "Date field"
                   else if ("name" %in% colnames(d)) "name"
                   else NULL
        nCol <- if ("N missing" %in% colnames(d)) "N missing"
                else if ("n_missing" %in% colnames(d)) "n_missing"
                else NULL

        if (is.null(pctCol) || is.null(dateCol)) {
          return(emptyPlotlyMessage("Missing dates data does not have expected columns."))
        }

        plotData <- d %>%
          dplyr::mutate(
            date_field = as.character(.data[[dateCol]]),
            pct_missing = suppressWarnings(as.numeric(.data[[pctCol]]))
          ) %>%
          dplyr::filter(!is.na(.data$pct_missing))

        if (nrow(plotData) == 0) {
          return(emptyPlotlyMessage("No numeric missing-date percentages available."))
        }

        # Build hover text
        if (!is.null(nCol)) {
          plotData <- plotData %>%
            dplyr::mutate(hover = paste0(
              .data$date_field, "\n",
              .data$cdm_name, "\n",
              "N missing: ", format(suppressWarnings(as.numeric(.data[[nCol]])), big.mark = ","), "\n",
              round(.data$pct_missing, 2), "% missing"
            ))
        } else {
          plotData <- plotData %>%
            dplyr::mutate(hover = paste0(
              .data$date_field, "\n",
              .data$cdm_name, "\n",
              round(.data$pct_missing, 2), "% missing"
            ))
        }

        # Grouped bar chart: x = date field, y = % missing, fill = database
        p <- ggplot2::ggplot(plotData, ggplot2::aes(
          x = .data$date_field, y = .data$pct_missing,
          fill = .data$cdm_name,
          text = .data$hover
        )) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::labs(x = "Date Field", y = "% Missing", fill = "Database") +
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
