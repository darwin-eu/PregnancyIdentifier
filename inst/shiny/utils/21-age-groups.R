# 21-age-groups.R - Age distribution groups module (standard Shiny module)
# Data: ageSummaryGroups (columns: cdm_name, colName, age column, n, pct, total)

ageGroupsUI <- function(id) {
  ns <- NS(id)

  # Determine the age column (second column or first non-metadata column)
  ageColLabel <- "Age"
  if (exists("ageSummaryGroups") && is.data.frame(ageSummaryGroups) && ncol(ageSummaryGroups) > 1) {
    ageCol <- setdiff(
      colnames(ageSummaryGroups),
      c("colName", "n", "total", "pct", "cdm_name", "date_run", "date_export", "pkg_version")
    )[1]
    if (!is.na(ageCol) && ageCol %in% names(PRETTY_NAMES)) {
      ageColLabel <- PRETTY_NAMES[ageCol]
    }
  }

  # colName choices
  colNameChoices <- character(0)
  if (exists("ageSummaryGroups") && is.data.frame(ageSummaryGroups) &&
      "colName" %in% colnames(ageSummaryGroups)) {
    colNameChoices <- unique(ageSummaryGroups$colName)
    colNameChoices <- colNameChoices[!is.na(colNameChoices) & nzchar(colNameChoices)]
  }

  tagList(
    div(class = "tab-help-text",
        "Counts and percentages of pregnancies by age group, with filters for database and outcome type."),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database",
                           choices = allDP, selected = allDP,
                           multiple = TRUE, options = opt)),
      column(3, pickerInput(ns("colName"), "Outcome type",
                           choices = colNameChoices, selected = colNameChoices,
                           multiple = TRUE, options = opt)),
      column(3, radioButtons(ns("yAxis"), "Y axis",
                            choices = c("Count (n)" = "n", "Percent (%)" = "pct"),
                            selected = "n", inline = TRUE))
    ),
    plotly::plotlyOutput(ns("plot"), height = "420px") %>% withSpinner(),
    h4("Download figure"),
    fluidRow(
      column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
      column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
      column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
    ),
    downloadButton(ns("download_plot"), "Download plot (PNG)"),
    h4("Data"),
    downloadButton(ns("download_table_csv"), "Download table (.csv)"),
    DT::DTOutput(ns("table")) %>% withSpinner()
  )
}

ageGroupsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Determine the age column
    ageCol <- reactive({
      data <- ageSummaryGroups
      if (is.null(data) || ncol(data) < 2) return(NULL)
      col <- setdiff(
        colnames(data),
        c("colName", "n", "total", "pct", "cdm_name", "date_run", "date_export", "pkg_version")
      )[1]
      if (is.na(col)) return(NULL)
      col
    })

    getData <- reactive({
      data <- ageSummaryGroups
      if (is.null(data) || nrow(data) == 0) return(data.frame())

      # Filter by CDM
      data <- filterByCdm(data, input$cdm, allDP)

      # Filter by colName
      if ("colName" %in% colnames(data)) {
        colNameSel <- input$colName
        if (!is.null(colNameSel) && length(colNameSel) > 0) {
          data <- data %>% dplyr::filter(.data$colName %in% colNameSel)
        }
      }

      data
    })

    output$plot <- plotly::renderPlotly({
      data <- getData()
      ac <- ageCol()
      if (is.null(data) || nrow(data) == 0 || is.null(ac) || !ac %in% colnames(data)) {
        return(emptyPlotlyMessage("No data for selected filters."))
      }

      yChoice <- input$yAxis
      if (is.null(yChoice) || !yChoice %in% c("n", "pct")) yChoice <- "n"

      # Prepare plot data
      plotData <- data %>%
        dplyr::mutate(
          n = suppressWarnings(as.numeric(.data$n)),
          pct = suppressWarnings(as.numeric(.data$pct)),
          age_num = suppressWarnings(as.numeric(.data[[ac]]))
        ) %>%
        dplyr::filter(!is.na(.data$age_num))

      if (nrow(plotData) == 0) return(emptyPlotlyMessage("No numeric age data available."))

      yLab <- if (yChoice == "pct") "Percent" else "Count (n)"

      # Aggregate by age and cdm_name
      hasMultiDb <- "cdm_name" %in% colnames(plotData) && dplyr::n_distinct(plotData$cdm_name) > 1

      if (hasMultiDb) {
        dPlot <- plotData %>%
          dplyr::group_by(.data$age_num, .data$cdm_name) %>%
          dplyr::summarise(
            n = sum(.data$n, na.rm = TRUE),
            pct = mean(.data$pct, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        dPlot <- plotData %>%
          dplyr::group_by(.data$age_num) %>%
          dplyr::summarise(
            n = sum(.data$n, na.rm = TRUE),
            pct = mean(.data$pct, na.rm = TRUE),
            .groups = "drop"
          )
      }

      dPlot$y_plot <- dPlot[[yChoice]]

      if (hasMultiDb) {
        p <- plotly::plot_ly() %>%
          plotly::layout(
            barmode = "overlay",
            bargap = 0,
            xaxis = list(title = "Age (years)", type = "linear", dtick = 5),
            yaxis = list(title = yLab),
            showlegend = TRUE
          )
        for (db in unique(dPlot$cdm_name)) {
          dDb <- dplyr::filter(dPlot, .data$cdm_name == !!db)
          p <- p %>%
            plotly::add_trace(
              data = dDb,
              x = ~age_num,
              y = ~y_plot,
              type = "bar",
              name = db,
              opacity = 0.7,
              hovertemplate = paste0("Age: %{x}<br>", yLab, ": %{y:.2f}<extra></extra>")
            )
        }
      } else {
        p <- plotly::plot_ly(
          data = dPlot,
          x = ~age_num,
          y = ~y_plot,
          type = "bar",
          hovertemplate = paste0("Age: %{x}<br>", yLab, ": %{y:.2f}<extra></extra>")
        ) %>%
          plotly::layout(
            xaxis = list(title = "Age (years)", type = "linear", dtick = 5),
            yaxis = list(title = yLab),
            showlegend = FALSE,
            bargap = 0
          )
      }
      p
    })

    output$table <- DT::renderDT({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(renderPrettyDT(data.frame()))
      renderPrettyDT(data)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "age_groups.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
