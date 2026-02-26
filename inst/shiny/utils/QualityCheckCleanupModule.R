# Copyright (c) 2024-2025 DARWIN EU
# Quality check cleanup: transposed gt table + horizontal bar chart.

library(DarwinShinyModules)

QualityCheckCleanupModule <- R6::R6Class(
  classname = "QualityCheckCleanupModule",
  inherit = ShinyModule,

  public = list(
    initialize = function(data, dp = unique(data$cdm_name)) {
      super$initialize()
      private$.data <- data
      private$.dp <- dp

      private$.inputPanelCDM <- createDatabasePicker(private$.dp, self$namespace)
    }
  ),

  private = list(
    .data = NULL,
    .dp = NULL,
    .inputPanelCDM = NULL,

    .UI = function() {
      shiny::tagList(
        private$.inputPanelCDM$UI(),
        shiny::tabsetPanel(
          id = shiny::NS(private$.namespace, "qcTabs"),
          shiny::tabPanel(
            "Plot",
            plotly::plotlyOutput(shiny::NS(private$.namespace, "qcPlot"), height = "420px") %>%
              shinycssloaders::withSpinner()
          ),
          shiny::tabPanel(
            "Data",
            gt::gt_output(shiny::NS(private$.namespace, "qualityTable")) %>%
              shinycssloaders::withSpinner()
          )
        )
      )
    },

    .server = function(input, output, session) {
      private$.inputPanelCDM$server(input, output, session)

      tableData <- shiny::reactive({
        sel <- getSelectedCdm(private$.inputPanelCDM, private$.dp)
        d <- private$.data
        if (!is.data.frame(d) || nrow(d) == 0 || !"cdm_name" %in% colnames(d)) {
          return(NULL)
        }
        d <- d %>% dplyr::filter(.data$cdm_name %in% sel)
        metric_cols <- setdiff(colnames(d), "cdm_name")
        if (length(metric_cols) == 0) return(NULL)
        # Apply human-readable metric labels
        long <- d %>%
          tidyr::pivot_longer(
            cols = dplyr::all_of(metric_cols),
            names_to = "Metric",
            values_to = "value"
          ) %>%
          dplyr::mutate(Metric = ifelse(
            .data$Metric %in% names(PRETTY_NAMES),
            PRETTY_NAMES[.data$Metric],
            .data$Metric
          ))
        wide <- long %>%
          tidyr::pivot_wider(names_from = "cdm_name", values_from = "value")
        wide
      })

      output$qualityTable <- gt::render_gt({
        d <- tableData()
        if (is.null(d) || nrow(d) == 0) {
          return(gt::gt(dplyr::tibble(Message = "No quality check cleanup data for selected database(s).")))
        }
        gt::gt(d) %>%
          gt::tab_options(
            table.font.size = gt::px(13),
            data_row.padding = gt::px(8),
            column_labels.font.weight = "bold"
          ) %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = gt::cells_body(columns = "Metric")
          )
      })

      # Plot: horizontal grouped bar chart of cleanup metrics per database
      output$qcPlot <- plotly::renderPlotly({
        sel <- getSelectedCdm(private$.inputPanelCDM, private$.dp)
        d <- private$.data
        if (!is.data.frame(d) || nrow(d) == 0 || !"cdm_name" %in% colnames(d)) {
          return(emptyPlotlyMessage("No quality check cleanup data."))
        }
        d <- d %>% dplyr::filter(.data$cdm_name %in% sel)

        # Select key count metrics for the bar chart
        countCols <- intersect(
          c("n_records_overlapping", "n_records_too_long", "n_records_after_cleanup"),
          colnames(d)
        )
        if (length(countCols) == 0) {
          return(emptyPlotlyMessage("No count metrics available for plotting."))
        }

        plotData <- d %>%
          dplyr::select(dplyr::all_of(c("cdm_name", countCols))) %>%
          tidyr::pivot_longer(-"cdm_name", names_to = "metric", values_to = "value") %>%
          dplyr::mutate(
            value = suppressWarnings(as.numeric(.data$value)),
            metric = ifelse(
              .data$metric %in% names(PRETTY_NAMES),
              PRETTY_NAMES[.data$metric],
              .data$metric
            ),
            metric = factor(.data$metric, levels = rev(unique(.data$metric)))
          )

        p <- ggplot2::ggplot(plotData, ggplot2::aes(
          x = .data$metric, y = .data$value,
          fill = .data$cdm_name,
          text = paste0(.data$cdm_name, "\n",
                        .data$metric, ": ",
                        format(.data$value, big.mark = ","))
        )) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::coord_flip() +
          ggplot2::labs(x = NULL, y = "Count", fill = "Database") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            legend.position = "bottom",
            axis.text.y = ggplot2::element_text(size = 11)
          )
        plotly::ggplotly(p, tooltip = "text")
      })
    }
  )
)
