library(DarwinShinyModules)

#' Overview landing page module.
#'
#' Shows a summary of loaded databases, episode counts, and data availability
#' so users can quickly see what data is loaded before drilling into detail tabs.
OverviewModule <- R6::R6Class(
  classname = "OverviewModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(dbinfo, allDP, episodeCounts = NULL, dataSummary = NULL) {
      super$initialize()
      private$.dbinfo <- dbinfo
      private$.allDP <- allDP
      private$.episodeCounts <- episodeCounts
      private$.dataSummary <- dataSummary
    }
  ),

  private = list(
    .dbinfo = NULL,
    .allDP = NULL,
    .episodeCounts = NULL,
    .dataSummary = NULL,

    .UI = function() {
      shiny::tagList(
        shiny::div(
          style = "max-width: 960px; margin: 0 auto; padding: 20px;",
          shiny::h2("PregnancyIdentifier Results Overview"),
          shiny::hr(),

          # Key metrics row
          shiny::fluidRow(
            shiny::column(4,
              shiny::div(
                class = "overview-card",
                style = "background: #f0f7ff; border-radius: 8px; padding: 20px; text-align: center; margin-bottom: 15px;",
                shiny::h1(style = "color: #2c3e50; margin: 0;", length(private$.allDP)),
                shiny::p(style = "color: #7f8c8d; margin: 5px 0 0 0;", "Databases loaded")
              )
            ),
            shiny::column(4,
              shiny::div(
                class = "overview-card",
                style = "background: #f0fff0; border-radius: 8px; padding: 20px; text-align: center; margin-bottom: 15px;",
                shiny::h1(style = "color: #2c3e50; margin: 0;",
                  if (!is.null(private$.episodeCounts) && nrow(private$.episodeCounts) > 0) {
                    format(sum(suppressWarnings(as.numeric(private$.episodeCounts$total)), na.rm = TRUE), big.mark = ",")
                  } else {
                    "\u2014"
                  }
                ),
                shiny::p(style = "color: #7f8c8d; margin: 5px 0 0 0;", "Total episodes")
              )
            ),
            shiny::column(4,
              shiny::div(
                class = "overview-card",
                style = "background: #fff8f0; border-radius: 8px; padding: 20px; text-align: center; margin-bottom: 15px;",
                shiny::h1(style = "color: #2c3e50; margin: 0;",
                  as.character(utils::packageVersion("DarwinShinyModules"))
                ),
                shiny::p(style = "color: #7f8c8d; margin: 5px 0 0 0;", "DarwinShinyModules version")
              )
            )
          ),

          # Databases list
          shiny::h3("Databases"),
          shiny::div(
            style = "margin-bottom: 20px;",
            shiny::tags$ul(
              lapply(private$.allDP, function(dp) shiny::tags$li(shiny::strong(dp)))
            )
          ),

          # Data availability table
          if (!is.null(private$.dataSummary) && nrow(private$.dataSummary) > 0) {
            shiny::tagList(
              shiny::h3("Data availability"),
              shiny::p(style = "color: #555;", "Which result tables have data for each database."),
              DT::DTOutput(outputId = shiny::NS(private$.namespace, "dataSummary"))
            )
          },

          shiny::hr(),
          shiny::p(
            style = "color: #888; font-size: 0.9em;",
            sprintf("App deployed on %s.", Sys.Date())
          )
        )
      )
    },

    .server = function(input, output, session) {
      if (!is.null(private$.dataSummary) && nrow(private$.dataSummary) > 0) {
        output$dataSummary <- DT::renderDT({
          DT::datatable(
            private$.dataSummary,
            options = list(dom = "t", paging = FALSE, scrollX = TRUE),
            filter = "none",
            rownames = FALSE
          )
        })
      }
    }
  )
)

#' Build a data-availability summary table.
#'
#' Checks which global variables have data for each database in allDP.
#' Returns a data.frame with databases as rows and result tables as columns,
#' with checkmarks or dashes indicating availability.
#'
#' @param allDP Character vector of database names.
#' @return A data.frame for display in the Overview tab.
buildDataSummary <- function(allDP) {
  tableSpecs <- list(
    "Episode frequency"     = "episodeFrequency",
    "Pregnancy frequency"   = "pregnancyFrequency",
    "Gestational weeks"     = "gestationalWeeks",
    "Outcome categories"    = "outcomeCategoriesCount",
    "Delivery mode"         = "deliveryModeSummary",
    "Attrition"             = "attrition_episodes",
    "Concept check"         = "conceptCheck",
    "Precision days"        = "precisionDays",
    "Quality check cleanup" = "qualityCheckCleanup",
    "Age summary"           = "ageSummary"
  )

  rows <- lapply(allDP, function(dp) {
    available <- vapply(names(tableSpecs), function(label) {
      varName <- tableSpecs[[label]]
      if (!exists(varName, envir = .GlobalEnv)) return("\u2014")
      tbl <- get(varName, envir = .GlobalEnv)
      if (!is.data.frame(tbl) || nrow(tbl) == 0) return("\u2014")
      if ("cdm_name" %in% colnames(tbl) && dp %in% tbl$cdm_name) "\u2713" else "\u2014"
    }, character(1))
    c(Database = dp, available)
  })
  do.call(rbind, lapply(rows, function(r) as.data.frame(t(r), stringsAsFactors = FALSE)))
}
