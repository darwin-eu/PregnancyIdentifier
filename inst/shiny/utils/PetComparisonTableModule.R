library(DarwinShinyModules)

#' R6 module for a single PET comparison table: DT table with CSV download.
#' Used as a subtab under the top-level "PET comparison" tab.
PetComparisonTableModule <- R6::R6Class(
  classname = "PetComparisonTableModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, title = "Table") {
      super$initialize()
      private$.data <- data
      private$.title <- title
      private$.table <- Table$new(
        data = data,
        title = title,
        options = list(scrollX = TRUE, pageLength = 25)
      )
      private$.table$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .data = NULL,
    .title = NULL,
    .table = NULL,

    .UI = function() {
      private$.table$UI()
    },

    .server = function(input, output, session) {
      private$.table$server(input, output, session)
    }
  )
)
