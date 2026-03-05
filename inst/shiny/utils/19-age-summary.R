# 19-age-summary.R - Age summary module (standard Shiny module)
# Data: ageSummaryRaw (columns: cdm_name, colName, plus min/Q25/median/Q75/max/mean/sd)
# Optional final_outcome_category column.

ageSummaryUI <- function(id) {
  ns <- NS(id)

  hasOutcome <- exists("ageSummaryRaw") && is.data.frame(ageSummaryRaw) &&
    "final_outcome_category" %in% colnames(ageSummaryRaw)

  outcomeChoices <- if (hasOutcome) sort(unique(as.character(ageSummaryRaw$final_outcome_category))) else character(0)
  tagList(
    div(class = "tab-help-text",
        "Distribution of maternal age at pregnancy start by outcome group, faceted by database."),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database",
                           choices = allDP, selected = allDP,
                           multiple = TRUE, options = opt)),
      if (hasOutcome) column(3, pickerInput(ns("outcome"), "Outcome group",
                                            choices = outcomeChoices, selected = outcomeChoices,
                                            multiple = TRUE, options = opt))
    ),
    plotly::plotlyOutput(ns("plot"), height = "420px") %>% withSpinner(),
    h4("Data"),
    downloadButton(ns("download_table_csv"), "Download table (.csv)"),
    DT::DTOutput(ns("table")) %>% withSpinner()
  )
}

ageSummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    hasOutcome <- "final_outcome_category" %in% colnames(ageSummaryRaw)

    getData <- reactive({
      data <- ageSummaryRaw
      if (is.null(data) || nrow(data) == 0) return(data.frame())

      # Filter by CDM
      data <- filterByCdm(data, input$cdm, allDP)

      # Filter by outcome if available
      if (hasOutcome) {
        outcomeSel <- input$outcome
        if (!is.null(outcomeSel) && length(outcomeSel) > 0) {
          data <- data %>% dplyr::filter(.data$final_outcome_category %in% outcomeSel)
        }
      }

      data
    })

    getPlotData <- reactive({
      ageMetrics <- c("min", "Q25", "median", "Q75", "max")
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(data.frame())

      # Add a fallback group column if no outcome column
      if (!hasOutcome) {
        if ("colName" %in% colnames(data)) {
          # Use colName as the grouping variable
        } else {
          data <- data %>% dplyr::mutate(colName = "Overall")
        }
      }

      # Ensure numeric
      for (col in ageMetrics) {
        if (col %in% colnames(data)) data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
      }
      data %>%
        dplyr::filter(dplyr::if_all(dplyr::all_of(intersect(ageMetrics, colnames(data))), ~ !is.na(.)))
    })

    output$plot <- plotly::renderPlotly({
      data <- getPlotData()
      if (is.null(data) || nrow(data) == 0) {
        return(emptyPlotlyMessage("No age summary data for selected filters."))
      }

      ageMetrics <- c("min", "Q25", "median", "Q75", "max")
      if (!all(ageMetrics %in% colnames(data))) {
        return(emptyPlotlyMessage("Age summary columns (min, Q25, median, Q75, max) not found."))
      }

      # Determine the group column for y-axis within each subplot
      groupCol <- if (hasOutcome && "final_outcome_category" %in% colnames(data)) {
        "final_outcome_category"
      } else if ("colName" %in% colnames(data)) {
        "colName"
      } else {
        data$group__ <- "Overall"
        "group__"
      }

      # One subplot per database: horizontal boxplot with y = outcome group
      databases <- sort(unique(as.character(data$cdm_name)))
      plots <- lapply(databases, function(db) {
        dbData <- data %>% dplyr::filter(.data$cdm_name == db)
        groupLevels <- rev(sort(unique(as.character(dbData[[groupCol]]))))
        dbData[[groupCol]] <- factor(dbData[[groupCol]], levels = groupLevels)

        p <- plotly::plot_ly()
        for (i in seq_along(groupLevels)) {
          grp <- groupLevels[i]
          row <- dbData %>% dplyr::filter(.data[[groupCol]] == grp)
          if (nrow(row) == 0) next
          p <- p %>%
            plotly::add_trace(
              type = "box",
              y = grp,
              q1 = row$Q25[1],
              median = row$median[1],
              q3 = row$Q75[1],
              lowerfence = row$min[1],
              upperfence = row$max[1],
              orientation = "h",
              boxpoints = FALSE,
              name = db,
              legendgroup = db,
              showlegend = (i == 1L),
              hoverinfo = "text",
              text = paste0(
                grp, "<br>",
                "Median: ", round(row$median[1], 1), "<br>",
                "IQR: ", round(row$Q25[1], 1), " - ", round(row$Q75[1], 1), "<br>",
                "Range: ", round(row$min[1], 1), " - ", round(row$max[1], 1)
              )
            )
        }
        p %>%
          plotly::layout(
            xaxis = list(title = "Age"),
            yaxis = list(title = "", categoryorder = "array", categoryarray = groupLevels),
            title = list(text = db, font = list(size = 12)),
            margin = list(t = 40, b = 40)
          )
      })

      n <- length(plots)
      if (n == 0) return(emptyPlotlyMessage("No valid data for plotting."))
      if (n == 1) return(plots[[1]])

      plotly::subplot(plots, nrows = n, shareX = TRUE, titleY = TRUE, margin = 0.05) %>%
        plotly::layout(
          title = list(text = "Age at pregnancy start by outcome group", font = list(size = 14))
        )
    })

    output$table <- DT::renderDT({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(renderPrettyDT(data.frame()))
      renderPrettyDT(data)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "age_summary.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
