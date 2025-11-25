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

        if ("cdm_name" %in% colnames(private$.data)) {
          data <-  private$.data %>%
            dplyr::filter(.data$cdm_name %in% private$.inputPanelCDM$inputValues$cdm_name)
        } else {
          nameCols <- setdiff(colnames(private$.data), private$.dp)

          data <-  private$.data %>%
            dplyr::select(c(nameCols, private$.inputPanelCDM$inputValues$cdm_name))
        }

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
