# Copyright (c) 2024–2025 DARWIN EU®
# Quality check cleanup table: transposed (Metric in rows, databases in columns), rendered with gt.

library(DarwinShinyModules)

#' R6 module for the quality check cleanup table: transposed layout (Metric, then one column per CDM), rendered with gt.
QualityCheckCleanupModule <- R6::R6Class(
  classname = "QualityCheckCleanupModule",
  inherit = ShinyModule,

  public = list(
    initialize = function(data, dp = unique(data$cdm_name)) {
      super$initialize()
      private$.data <- data
      private$.dp <- dp

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
    .inputPanelCDM = NULL,

    .UI = function() {
      shiny::tagList(
        private$.inputPanelCDM$UI(),
        gt::gt_output(shiny::NS(private$.namespace, "qualityTable")) %>% shinycssloaders::withSpinner()
      )
    },

    .server = function(input, output, session) {
      tableData <- shiny::reactive({
        sel <- private$.inputPanelCDM$inputValues$cdm_name
        if (is.null(sel) || length(sel) == 0) sel <- private$.dp
        d <- private$.data
        if (!is.data.frame(d) || nrow(d) == 0 || !"cdm_name" %in% colnames(d)) {
          return(NULL)
        }
        d <- d %>% dplyr::filter(.data$cdm_name %in% sel)
        metric_cols <- setdiff(colnames(d), "cdm_name")
        if (length(metric_cols) == 0) return(NULL)
        # Transpose: long by cdm_name, then wide so Metric is first column, CDMs are columns
        long <- d %>%
          tidyr::pivot_longer(
            cols = dplyr::all_of(metric_cols),
            names_to = "Metric",
            values_to = "value"
          )
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
            table.font.size = gt::px(12),
            data_row.padding = gt::px(6)
          )
      })

      private$.inputPanelCDM$server(input, output, session)
    }
  )
)
