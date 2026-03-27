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
                                            multiple = TRUE, options = opt)),
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

ageSummaryServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePickerInput(session, "cdm", choices = rv$allDP, selected = rv$allDP)
    })

    observe({
      data <- rv$ageSummaryRaw
      if (!is.null(data) && is.data.frame(data) && "final_outcome_category" %in% colnames(data)) {
        outcomeChoices <- sort(unique(as.character(data$final_outcome_category)))
        updatePickerInput(session, "outcome", choices = outcomeChoices, selected = outcomeChoices)
      }
    })

    hasOutcome <- reactive({
      "final_outcome_category" %in% colnames(rv$ageSummaryRaw)
    })

    getData <- reactive({
      data <- rv$ageSummaryRaw
      if (is.null(data) || nrow(data) == 0) return(data.frame())

      # Filter by CDM
      data <- filterByCdm(data, input$cdm, rv$allDP)

      # Filter by outcome if available
      if (hasOutcome()) {
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

      # Add a fallback group column if no outcome column; expose as "group" for boxplot-precomputed
      if (!hasOutcome()) {
        if ("colName" %in% colnames(data)) {
          data$group <- data$colName
        } else {
          data <- data %>% dplyr::mutate(colName = "Overall", group = "Overall")
        }
      } else if ("final_outcome_category" %in% colnames(data)) {
        data$group <- data$final_outcome_category
      }

      # Ensure numeric
      for (col in ageMetrics) {
        if (col %in% colnames(data)) data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
      }
      data <- data %>%
        dplyr::filter(dplyr::if_all(dplyr::all_of(intersect(ageMetrics, colnames(data))), ~ !is.na(.)))

      # Cap boxplot stats at max age to avoid skewed plots
      max_age <- input$max_age
      if (!is.null(max_age) && is.finite(max_age)) {
        for (col in ageMetrics) {
          if (col %in% colnames(data)) data[[col]] <- pmin(data[[col]], max_age)
        }
        # Enforce order so boxplot remains valid (ymin <= lower <= middle <= upper <= ymax)
        data <- data %>% dplyr::rowwise() %>% dplyr::mutate(
          min = sort(c(.data$min, .data$Q25, .data$median, .data$Q75, .data$max))[1],
          Q25 = sort(c(.data$min, .data$Q25, .data$median, .data$Q75, .data$max))[2],
          median = sort(c(.data$min, .data$Q25, .data$median, .data$Q75, .data$max))[3],
          Q75 = sort(c(.data$min, .data$Q25, .data$median, .data$Q75, .data$max))[4],
          max = sort(c(.data$min, .data$Q25, .data$median, .data$Q75, .data$max))[5]
        ) %>% dplyr::ungroup()
      }
      data
    })

    plot_ggplot <- reactive({
      data <- getPlotData()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      if (!all(c("min", "Q25", "median", "Q75", "max") %in% colnames(data))) return(NULL)
      if (!"group" %in% colnames(data)) return(NULL)

      data <- data %>%
        dplyr::mutate(group = factor(.data$group, levels = rev(sort(unique(as.character(.data$group))))))

      make_ggplot_boxplot_precomputed(
        data,
        facet = ggplot2::vars(.data$cdm_name),
        title = "Age at pregnancy start by outcome group",
        xlab = NULL,
        ylab = "Age",
        category_order = levels(data$group),
        horizontal = TRUE
      )
    })

    output$plot <- plotly::renderPlotly({
      pd <- getPlotData()
      if (is.null(pd) || nrow(pd) == 0 || !"group" %in% colnames(pd)) {
        return(emptyPlotlyMessage("No age summary data for selected filters."))
      }
      pd <- pd %>%
        dplyr::mutate(group = factor(.data$group, levels = rev(sort(unique(as.character(.data$group))))))
      make_plotly_boxplot_precomputed(
        pd,
        facet = "cdm_name",
        title = "Age at pregnancy start by outcome group",
        xlab = NULL,
        ylab = "Age",
        category_order = levels(pd$group),
        horizontal = TRUE
      )
    })

    output$download_plot <- downloadHandler(
      filename = function() { "ageSummaryPlot.png" },
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
      renderPrettyDT(data, numDigits = 1)
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
