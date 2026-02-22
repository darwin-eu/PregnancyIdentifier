library(DarwinShinyModules)

#' R6 module for PET comparison formatted table: visOmopResults::visOmopTable on
#' the summarised result with a Database filter.
PetComparisonVisTableModule <- R6::R6Class(
  classname = "PetComparisonVisTableModule",
  inherit = ShinyModule,

  public = list(

    #' @param result Summarised result from PET comparison (e.g. imported with
    #'   omopgenerics::importSummarisedResult from pet_comparison_summarised_result.csv).
    initialize = function(result) {
      super$initialize()
      private$.result <- result
      private$.cdm_names <- unique(as.data.frame(result)$cdm_name)
      private$.inputPanelCDM <- InputPanel$new(
        fun = list(cdm_name = shinyWidgets::pickerInput),
        args = list(cdm_name = list(
          inputId = "cdm_name",
          label = "Database",
          choices = private$.cdm_names,
          selected = private$.cdm_names,
          multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.inputPanelCDM$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .result = NULL,
    .cdm_names = NULL,
    .inputPanelCDM = NULL,

    .UI = function() {
      ns <- private$.namespace
      shiny::tagList(
        private$.inputPanelCDM$UI(),
        shiny::br(),
        gt::gt_output(ns("visTable")) %>% shinycssloaders::withSpinner()
      )
    },

    .server = function(input, output, session) {
      ns <- session$ns
      private$.inputPanelCDM$server(input, output, session)

      filteredResult <- shiny::reactive({
        sel <- private$.inputPanelCDM$inputValues$cdm_name
        if (is.null(sel) || length(sel) == 0) sel <- private$.cdm_names
        res <- private$.result %>%
          dplyr::filter(.data$cdm_name %in% sel)
        if (!inherits(res, "summarised_result") && inherits(private$.result, "summarised_result")) {
          res <- omopgenerics::newSummarisedResult(
            tibble::as_tibble(res),
            omopgenerics::settings(private$.result)
          )
        }
        res
      })

      output$visTable <- gt::render_gt({
        req(filteredResult())
        sr <- filteredResult()
        if (nrow(as.data.frame(sr)) == 0) {
          return(gt::gt(dplyr::tibble(Message = "No data for selected database(s).")))
        }
        visOmopResults::visOmopTable(
          result = sr,
          header = c("cdm_name", "variable_name", "variable_level"),
          rename = c("Database" = "cdm_name"),
          type = "gt"
        )
      })
    }
  )
)
