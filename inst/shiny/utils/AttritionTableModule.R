library(DarwinShinyModules)

AttritionTableModule <- R6::R6Class(
  classname = "AttritionTableModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name)) {
      super$initialize()
      # Add step_number (ordering of steps) per database
      if ("cdm_name" %in% colnames(data)) {
        private$.data <- data %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::mutate(step_number = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::relocate("step_number", .before = 1L)
      } else {
        private$.data <- data %>%
          dplyr::mutate(step_number = dplyr::row_number()) %>%
          dplyr::relocate("step_number", .before = 1L)
      }
      private$.dp <- dp

      # Single-select database filter (one CDM at a time)
      private$.inputPanelCDM <- InputPanel$new(
        fun = list(cdm_name = shinyWidgets::pickerInput),
        args = list(cdm_name = list(
          inputId = "cdm_name",
          label = "Database",
          choices = private$.dp,
          selected = private$.dp[1],
          multiple = FALSE,
          options = list(size = 10)
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
        gt::gt_output(shiny::NS(private$.namespace, "attritionTable")) %>% shinycssloaders::withSpinner()
      )
    },

    .server = function(input, output, session) {
      # Table data: one CDM only, no cdm_name column
      tableData <- shiny::reactive({
        cdmSel <- private$.inputPanelCDM$inputValues$cdm_name
        if (is.null(cdmSel) || length(cdmSel) == 0) cdmSel <- private$.dp[1]
        d <- private$.data
        if ("cdm_name" %in% colnames(d)) {
          d <- d %>% dplyr::filter(.data$cdm_name %in% cdmSel)
          d <- d %>% dplyr::select(-"cdm_name")
        }
        d
      })

      output$attritionTable <- gt::render_gt({
        req(tableData())
        d <- tableData()
        if (is.null(d) || nrow(d) == 0) {
          return(gt::gt(dplyr::tibble(Message = "No data for selected database.")))
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
