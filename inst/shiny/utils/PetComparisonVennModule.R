library(DarwinShinyModules)

#' Build Venn diagram data for amVennDiagram5 from counts.
#' Left set = PET episodes (pet_only + both), right set = Algorithm episodes (algorithm_only + both).
#' Uses dummy IDs so that intersection size is exactly n_both.
#' @param both Count in both.
#' @param pet_only Count PET only.
#' @param algorithm_only Count algorithm only.
#' @return List suitable for \code{amVennDiagram5::amVennDiagram}.
pet_venn_data_from_counts <- function(both, pet_only, algorithm_only) {
  both <- max(0L, as.integer(both))
  pet_only <- max(0L, as.integer(pet_only))
  algorithm_only <- max(0L, as.integer(algorithm_only))
  n_pet <- pet_only + both
  n_alg <- algorithm_only + both
  if (n_pet == 0 && n_alg == 0) {
    return(amVennDiagram5::makeVennData(list(
      "Pregnancy episode in PET" = integer(0),
      "Pregnancy episode (algorithm)" = integer(0)
    )))
  }
  # PET = 1 .. (pet_only + both), Algorithm = (pet_only+1) .. (pet_only + both + algorithm_only)
  pet_ids <- if (n_pet > 0) seq_len(n_pet) else integer(0)
  alg_ids <- if (n_alg > 0) (pet_only + 1L):(pet_only + both + algorithm_only) else integer(0)
  amVennDiagram5::makeVennData(list(
    "Pregnancy episode in PET" = pet_ids,
    "Pregnancy episode (algorithm)" = alg_ids
  ))
}

#' R6 module for PET comparison Venn diagram (amVennDiagram5).
#' One two-circle diagram per database: left = PET episodes, right = algorithm episodes, overlap = both.
PetComparisonVennModule <- R6::R6Class(
  classname = "PetComparisonVennModule",
  inherit = ShinyModule,

  public = list(

    #' @param data Data frame with columns \code{cdm_name}, \code{both}, \code{pet_only}, \code{algorithm_only} (counts).
    initialize = function(data) {
      super$initialize()
      private$.data <- data
      private$.cdm_names <- unique(data$cdm_name)
    }
  ),

  private = list(
    .data = NULL,
    .cdm_names = NULL,

    .UI = function() {
      ns <- private$.namespace
      db_choices <- stats::setNames(private$.cdm_names, private$.cdm_names)
      shiny::fluidPage(
        if (length(private$.cdm_names) > 1) {
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::selectInput(
                inputId = ns("database"),
                label = "Database",
                choices = db_choices,
                selected = db_choices[1]
              )
            )
          )
        },
        shiny::fluidRow(
          shiny::column(
            width = 12,
            amVennDiagram5::amVennDiagramOutput(ns("venn"), width = "100%", height = "450px") %>%
              shinycssloaders::withSpinner()
          )
        )
      )
    },

    .server = function(input, output, session) {
      ns <- session$ns
      data <- private$.data

      chosen_db <- shiny::reactive({
        if (length(private$.cdm_names) == 1) private$.cdm_names[1] else input$database
      })

      venn_data <- shiny::reactive({
        db <- chosen_db()
        row <- data %>% dplyr::filter(.data$cdm_name == .env$db)
        if (nrow(row) == 0) return(NULL)
        pet_venn_data_from_counts(
          row$both[1],
          row$pet_only[1],
          row$algorithm_only[1]
        )
      })

      output$venn <- amVennDiagram5::renderAmVennDiagram({
        vd <- venn_data()
        if (is.null(vd)) return(NULL)
        amVennDiagram5::amVennDiagram(vd, theme = "default", legendPosition = "bottom")
      })
    }
  )
)
