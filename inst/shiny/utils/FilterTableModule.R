library(DarwinShinyModules)

FilterTableModule <- R6::R6Class(
  classname = "FilterTableModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name)) {
      super$initialize()
      private$.data <- data
      private$.dp <- dp

      private$.table <- Table$new(data = data)
      private$.table$parentNamespace <- self$namespace

      private$.inputPanelCDM <- createDatabasePicker(private$.dp, self$namespace)
    }
  ),

  private = list(
    .data = NULL,
    .dp = NULL,
    .table = NULL,
    .inputPanelCDM = NULL,


    .UI = function() {
      shiny::tagList(
        private$.inputPanelCDM$UI(),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      # handle updates
      shiny::observeEvent(private$.inputPanelCDM$inputValues$cdm_name, {
        cdmSel <- getSelectedCdm(private$.inputPanelCDM, private$.dp)
        data <- private$.data %>%
          dplyr::filter(.data$cdm_name %in% cdmSel)

        private$.table$data <- data
        private$.table$server(input, output, session)
      }, ignoreNULL = FALSE)

      # tables
      private$.table$server(input, output, session)
      # input filters
      private$.inputPanelCDM$server(input, output, session)
    }
  )

)
