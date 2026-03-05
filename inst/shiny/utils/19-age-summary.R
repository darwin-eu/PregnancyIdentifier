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

    plot_ggplot <- reactive({
      data <- getPlotData()
      if (is.null(data) || nrow(data) == 0) return(NULL)

      ageMetrics <- c("min", "Q25", "median", "Q75", "max")
      if (!all(ageMetrics %in% colnames(data))) return(NULL)

      groupCol <- if (hasOutcome && "final_outcome_category" %in% colnames(data)) {
        "final_outcome_category"
      } else if ("colName" %in% colnames(data)) {
        "colName"
      } else {
        data$group__ <- "Overall"
        "group__"
      }

      data[[groupCol]] <- factor(
        data[[groupCol]],
        levels = rev(sort(unique(as.character(data[[groupCol]]))))
      )

      ggplot2::ggplot(data, ggplot2::aes(
        x = .data[[groupCol]],
        ymin = .data$min,
        lower = .data$Q25,
        middle = .data$median,
        upper = .data$Q75,
        ymax = .data$max
      )) +
        ggplot2::geom_boxplot(stat = "identity") +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(ggplot2::vars(.data$cdm_name), scales = "free_y") +
        ggplot2::labs(x = NULL, y = "Age", title = "Age at pregnancy start by outcome group") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5),
          axis.text.y = ggplot2::element_text(size = 9)
        )
    })

    output$plot <- plotly::renderPlotly({
      p <- plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No age summary data for selected filters."))
      plotly::ggplotly(p)
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
