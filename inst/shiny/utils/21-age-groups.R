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
                            selected = "n", inline = TRUE)),
      column(2, numericInput(ns("max_age"), "Max age for plot", value = 55, min = 1, max = 120, step = 1))
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
    DT::DTOutput(ns("table")) %>% withSpinner(),
    downloadButton(ns("download_table_csv"), "Download table (.csv)")
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

    plot_ggplot <- reactive({
      data <- getData()
      ac <- ageCol()
      if (is.null(data) || nrow(data) == 0 || is.null(ac) || !ac %in% colnames(data)) return(NULL)

      yChoice <- input$yAxis
      if (is.null(yChoice) || !yChoice %in% c("n", "pct")) yChoice <- "n"

      plotData <- data %>%
        dplyr::mutate(
          n = suppressWarnings(as.numeric(.data$n)),
          pct = suppressWarnings(as.numeric(.data$pct)),
          age_num = suppressWarnings(as.numeric(.data[[ac]]))
        ) %>%
        dplyr::filter(!is.na(.data$age_num))

      if (nrow(plotData) == 0) return(NULL)

      yLab <- if (yChoice == "pct") "Percent" else "Count (n)"
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

      # Cap at max age for legibility
      max_age <- input$max_age
      if (!is.null(max_age) && is.finite(max_age)) {
        dPlot <- dPlot %>% dplyr::filter(.data$age_num <= max_age)
      }
      if (nrow(dPlot) == 0) return(NULL)

      x_max <- if (!is.null(max_age) && is.finite(max_age)) max_age else max(100, max(dPlot$age_num, na.rm = TRUE), na.rm = TRUE)

      p <- ggplot2::ggplot(dPlot, ggplot2::aes(x = .data$age_num, y = .data$y_plot))
      if (hasMultiDb) {
        p <- p +
          ggplot2::geom_col(ggplot2::aes(fill = .data$cdm_name), position = "dodge", alpha = 0.8) +
          ggplot2::labs(x = "Age (years)", y = yLab, fill = "Database")
      } else {
        p <- p +
          ggplot2::geom_col(fill = "#377EB8") +
          ggplot2::labs(x = "Age (years)", y = yLab)
      }
      p + ggplot2::scale_x_continuous(breaks = seq(0, x_max, 5), limits = c(0, x_max)) +
        ggplot2::theme_minimal()
    })

    output$plot <- plotly::renderPlotly({
      p <- plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data for selected filters."))
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "ageGroupsPlot.png" },
      content = function(file) {
        p <- plot_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = as.numeric(input$download_width),
            height = as.numeric(input$download_height),
            dpi = as.numeric(input$download_dpi),
            units = "cm"
          )
        }
      }
    )

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
