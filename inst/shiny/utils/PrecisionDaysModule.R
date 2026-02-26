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

      private$.inputPanelCDM <- createDatabasePicker(private$.dp, self$namespace)
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
        shiny::tabsetPanel(
          id = shiny::NS(private$.namespace, "precisionTabs"),
          shiny::tabPanel(
            "Plot",
            plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>%
              shinycssloaders::withSpinner()
          ),
          shiny::tabPanel(
            "Summary",
            DT::DTOutput(shiny::NS(private$.namespace, "summaryTable")) %>%
              shinycssloaders::withSpinner()
          )
        )
      )
    },

    .server = function(input, output, session) {
      private$.inputPanelCDM$server(input, output, session)

      getData <- shiny::reactive({
        sel <- getSelectedCdm(private$.inputPanelCDM, private$.dp)
        if (!"cdm_name" %in% colnames(private$.data)) return(private$.data)
        private$.data %>%
          dplyr::filter(.data$cdm_name %in% sel)
      })

      # Summary table: descriptive statistics per database
      output$summaryTable <- DT::renderDT({
        data <- getData()
        if (is.null(data) || nrow(data) == 0) {
          return(DT::datatable(
            data.frame(Message = "No precision days data available."),
            options = list(dom = "t"), rownames = FALSE
          ))
        }

        data <- data %>%
          dplyr::mutate(esd_precision_days = suppressWarnings(as.numeric(.data$esd_precision_days))) %>%
          dplyr::filter(!is.na(.data$esd_precision_days))

        if (nrow(data) == 0 || !"cdm_name" %in% colnames(data)) {
          return(DT::datatable(
            data.frame(Message = "No valid numeric data."),
            options = list(dom = "t"), rownames = FALSE
          ))
        }

        # If we have pre-computed density (not raw observations), compute weighted stats
        hasDensity <- "density" %in% colnames(data) && any(!is.na(suppressWarnings(as.numeric(data$density))))
        if (hasDensity) {
          summary_df <- data %>%
            dplyr::mutate(density = suppressWarnings(as.numeric(.data$density))) %>%
            dplyr::filter(!is.na(.data$density)) %>%
            dplyr::group_by(.data$cdm_name) %>%
            dplyr::summarise(
              `N Points` = dplyr::n(),
              `Min` = round(min(.data$esd_precision_days), 1),
              `Median` = round(stats::weighted.mean(.data$esd_precision_days, .data$density), 1),
              `Max` = round(max(.data$esd_precision_days), 1),
              `Peak Density At` = round(.data$esd_precision_days[which.max(.data$density)], 1),
              .groups = "drop"
            ) %>%
            dplyr::rename(Database = .data$cdm_name)
        } else {
          summary_df <- data %>%
            dplyr::group_by(.data$cdm_name) %>%
            dplyr::summarise(
              N = dplyr::n(),
              Min = round(min(.data$esd_precision_days), 1),
              `Q1 (25th)` = round(stats::quantile(.data$esd_precision_days, 0.25), 1),
              Median = round(stats::median(.data$esd_precision_days), 1),
              `Q3 (75th)` = round(stats::quantile(.data$esd_precision_days, 0.75), 1),
              Max = round(max(.data$esd_precision_days), 1),
              Mean = round(mean(.data$esd_precision_days), 1),
              `Std Dev` = round(stats::sd(.data$esd_precision_days), 1),
              .groups = "drop"
            ) %>%
            dplyr::rename(Database = .data$cdm_name)
        }

        DT::datatable(
          summary_df,
          options = list(dom = "t", paging = FALSE, scrollX = TRUE),
          filter = "none",
          rownames = FALSE
        ) %>%
          DT::formatRound(
            columns = setdiff(colnames(summary_df), "Database"),
            digits = 1
          )
      })

      output$plot <- plotly::renderPlotly({
        data <- getData()
        if (is.null(data) || nrow(data) == 0) {
          return(emptyPlotlyMessage("No precision days data available."))
        }
        # Ensure numeric
        hasDensityCol <- "density" %in% colnames(data)
        data <- data %>%
          dplyr::mutate(
            esd_precision_days = as.numeric(.data$esd_precision_days),
            density = if (hasDensityCol) as.numeric(.data$density) else NA_real_
          ) %>%
          dplyr::filter(!is.na(.data$esd_precision_days))
        if (!"cdm_name" %in% colnames(data)) {
          return(emptyPlotlyMessage("No valid data for selected filters."))
        }

        # If we have pre-computed density, use it; otherwise compute per database
        hasDensity <- "density" %in% colnames(data) && any(!is.na(data$density))
        if (hasDensity) {
          plotData <- data %>%
            dplyr::filter(!is.na(.data$density)) %>%
            dplyr::arrange(.data$cdm_name, .data$esd_precision_days)
        } else {
          plotData <- data %>%
            dplyr::filter(!is.na(.data$esd_precision_days)) %>%
            dplyr::group_by(.data$cdm_name) %>%
            dplyr::group_modify(function(g, ...) {
              x <- g$esd_precision_days
              if (length(x) < 2L) {
                if (length(x) == 0L) return(tibble::tibble(esd_precision_days = numeric(0), density = numeric(0)))
                return(tibble::tibble(esd_precision_days = x, density = 1))
              }
              d <- stats::density(x)
              tibble::tibble(esd_precision_days = d$x, density = d$y)
            }) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(.data$cdm_name, .data$esd_precision_days)
        }

        if (nrow(plotData) == 0) {
          return(emptyPlotlyMessage("No valid data for selected filters."))
        }

        plotly::plot_ly(
          data = plotData,
          x = ~esd_precision_days,
          y = ~density,
          color = ~cdm_name,
          type = "scatter",
          mode = "lines",
          hovertemplate = "Precision: %{x:.0f} days<br>Density: %{y:.4f}<extra></extra>"
        ) %>%
          plotly::layout(
            xaxis = list(title = "ESD Precision (days)"),
            yaxis = list(title = "Density"),
            showlegend = TRUE,
            legend = list(title = list(text = "Database"))
          )
      })
    }
  )
)
