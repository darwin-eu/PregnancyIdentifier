library(DarwinShinyModules)

DatabaseInfoModule <- R6::R6Class(
  classname = "DatabaseInfoModule",
  inherit = ShinyModule,
  
  public = list(
    
    initialize = function(data) {
      super$initialize()
      private$.data <- data
      
      private$.table <- Table$new(data = data)
      private$.table$parentNamespace <- self$namespace
      
      # input pickers
      private$.inputPanelCDM <- InputPanel$new(fun = list(database = shinyWidgets::pickerInput), 
                                               args = list(database = list(
                                                 inputId = "database", label = "Database", 
                                                 choices = unique(private$.data$database), 
                                                 selected = unique(private$.data$database), 
                                                 multiple = TRUE,
                                                 options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))), 
                                               growDirection = "horizontal")
      private$.inputPanelCDM$parentNamespace <- self$namespace
    }
  ),
  
  private = list(
    .data = NULL,
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
      shiny::observeEvent(private$.inputPanelCDM$inputValues$database, {
        data <-  private$.data %>%
          dplyr::filter(.data$database %in% private$.inputPanelCDM$inputValues$database)
        
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