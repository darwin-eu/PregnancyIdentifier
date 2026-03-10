# 27-overview.R - Overview/landing page module (standard Shiny module)
# Shows key metrics from the loaded data.

overviewUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      style = "max-width: 960px; margin: 0 auto; padding: 20px;",
      h2("PregnancyIdentifier Results Overview"),
      hr(),
      uiOutput(ns("summary")) %>% withSpinner(),
      hr(),
      h3("Data availability"),
      p(style = "color: #555;", "Which result tables have data for each database."),
      DT::DTOutput(ns("dataSummary")) %>% withSpinner(),
      downloadButton(ns("download_summary_csv"), "Download table (.csv)"),
      hr(),
      p(
        style = "color: #888; font-size: 0.9em;",
        sprintf("App deployed on %s.", Sys.Date())
      )
    )
  )
}

overviewServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$summary <- renderUI({
      nDatabases <- length(allDP)

      # Try to get total episodes
      nEpisodes <- tryCatch({
        if (exists("episodeFrequency") && is.data.frame(episodeFrequency) && nrow(episodeFrequency) > 0) {
          if ("name" %in% colnames(episodeFrequency) && any(episodeFrequency$name == "total_episodes")) {
            vals <- episodeFrequency %>%
              dplyr::filter(.data$name == "total_episodes") %>%
              dplyr::select(-"name")
            total <- sum(suppressWarnings(as.numeric(unlist(vals))), na.rm = TRUE)
            if (total > 0) format(total, big.mark = ",") else "\u2014"
          } else {
            "\u2014"
          }
        } else {
          "\u2014"
        }
      }, error = function(e) "\u2014")

      # Data sources description
      dataSources <- tryCatch({
        if (exists("zipFiles") && length(zipFiles) > 0) {
          paste(length(zipFiles), "zip files")
        } else if (exists("subfolders") && length(subfolders) > 0) {
          paste(length(subfolders), "folders")
        } else {
          "\u2014"
        }
      }, error = function(e) "\u2014")

      tagList(
        fluidRow(
          column(4,
            div(
              class = "overview-card",
              style = "background: #f0f7ff; border-radius: 8px; padding: 20px; text-align: center; margin-bottom: 15px;",
              h1(style = "color: #2c3e50; margin: 0;", nDatabases),
              p(style = "color: #7f8c8d; margin: 5px 0 0 0;", "Databases loaded")
            )
          ),
          column(4,
            div(
              class = "overview-card",
              style = "background: #f0fff0; border-radius: 8px; padding: 20px; text-align: center; margin-bottom: 15px;",
              h1(style = "color: #2c3e50; margin: 0;", nEpisodes),
              p(style = "color: #7f8c8d; margin: 5px 0 0 0;", "Total episodes")
            )
          ),
          column(4,
            div(
              class = "overview-card",
              style = "background: #fff8f0; border-radius: 8px; padding: 20px; text-align: center; margin-bottom: 15px;",
              h1(style = "color: #2c3e50; margin: 0;", dataSources),
              p(style = "color: #7f8c8d; margin: 5px 0 0 0;", "Data sources")
            )
          )
        ),
        h3("Databases"),
        div(
          style = "margin-bottom: 20px;",
          tags$ul(
            lapply(allDP, function(dp) tags$li(strong(dp)))
          )
        )
      )
    })

    summaryDfReactive <- reactive({
      buildDataAvailabilitySummary(allDP)
    })
    output$dataSummary <- DT::renderDT({
      summaryDf <- summaryDfReactive()
      if (is.null(summaryDf) || nrow(summaryDf) == 0) {
        return(DT::datatable(
          data.frame(Message = "No data summary available."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      DT::datatable(
        summaryDf,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        filter = "none",
        rownames = FALSE
      )
    })
    output$download_summary_csv <- downloadHandler(
      filename = function() { "data_availability_summary.csv" },
      content = function(file) {
        d <- summaryDfReactive()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}

#' Build a data-availability summary table.
#'
#' Checks which global variables have data for each database in allDP.
#' Returns a data.frame with databases as rows and result tables as columns,
#' with checkmarks or dashes indicating availability.
#'
#' @param allDP Character vector of database names.
#' @return A data.frame for display in the Overview tab.
buildDataAvailabilitySummary <- function(allDP) {
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
    "Age summary"           = "ageSummary",
    "Incidence"             = "incidence",
    "Prevalence"            = "prevalence",
    "PET comparison"        = "petComparisonSummarisedResult"
  )
  # Optional (legacy) PET comparison tables
  if (exists("petComparisonSpec", envir = .GlobalEnv)) {
    spec <- get("petComparisonSpec", envir = .GlobalEnv)
    for (varName in names(spec)) {
      tableSpecs[[spec[[varName]]]] <- varName
    }
  }

  rows <- lapply(allDP, function(dp) {
    available <- vapply(names(tableSpecs), function(label) {
      varName <- tableSpecs[[label]]
      if (!exists(varName, envir = .GlobalEnv)) return("\u2014")
      tbl <- get(varName, envir = .GlobalEnv)
      if (!is.data.frame(tbl) || nrow(tbl) == 0) return("\u2014")
      if ("cdm_name" %in% colnames(tbl) && dp %in% tbl$cdm_name) return("\u2713")
      # Tables without cdm_name (e.g. legacy PET): show as available when table has data
      if (!("cdm_name" %in% colnames(tbl))) return("\u2713")
      "\u2014"
    }, character(1))
    c(Database = dp, available)
  })
  do.call(rbind, lapply(rows, function(r) as.data.frame(t(r), stringsAsFactors = FALSE)))
}
