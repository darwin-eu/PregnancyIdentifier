# 25-quality-check-cleanup.R - Quality check cleanup module (standard Shiny module)
# Data: qualityCheckCleanup. CDM picker. Shows gt table (transposed by database)
# and horizontal grouped bar chart.

qualityCheckCleanupUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text",
        "Summary of episode quality characterization and cleanup. Includes counts of episodes flagged as overlapping, too long, and records after cleanup."),
    div(
      class = "well",
      p(
        strong("Note on overlap counts:"),
        " The overlap count here uses an all-pairs check: every episode is compared against every other",
        " episode for the same person, and any pair where start_a < end_b and end_a > start_b is flagged",
        " as overlapping. This is more comprehensive than the ", strong("Pregnancy Overlap"), " tab,",
        " which only checks each episode against its immediately preceding episode (sequential/lag-based).",
        " As a result, the overlap count reported here may be higher than what is shown in the Pregnancy Overlap tab."
      )
    ),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database",
                           choices = character(0), selected = character(0),
                           multiple = TRUE, options = opt))
    ),
    tabsetPanel(
      tabPanel("Plot",
               plotly::plotlyOutput(ns("qcPlot"), height = "420px") %>% withSpinner(),
               h4("Download figure"),
               fluidRow(
                 column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
                 column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
                 column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
               ),
               downloadButton(ns("download_plot"), "Download plot (PNG)")),
      tabPanel("Data",
               gt::gt_output(ns("qualityTable")) %>% withSpinner(),
               downloadButton(ns("download_table_docx"), "Download table (.docx)"))
    )
  )
}

qualityCheckCleanupServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    dp <- if ("cdm_name" %in% colnames(qualityCheckCleanup)) {
      unique(c(allDP, qualityCheckCleanup$cdm_name))
    } else {
      allDP
    }

    observe({
      updatePickerInput(session, "cdm", choices = dp, selected = dp)
    })

    tableData <- reactive({
      sel <- input$cdm
      if (is.null(sel) || length(sel) == 0) sel <- dp
      d <- qualityCheckCleanup
      if (!is.data.frame(d) || nrow(d) == 0 || !"cdm_name" %in% colnames(d)) {
        return(NULL)
      }
      d <- d %>% dplyr::filter(.data$cdm_name %in% sel)
      metric_cols <- setdiff(colnames(d), "cdm_name")
      if (length(metric_cols) == 0) return(NULL)

      # Transpose: metrics become rows, cdm_names become columns
      long <- d %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(metric_cols),
          names_to = "Metric",
          values_to = "value"
        ) %>%
        dplyr::mutate(Metric = ifelse(
          .data$Metric %in% names(PRETTY_NAMES),
          PRETTY_NAMES[.data$Metric],
          .data$Metric
        ))
      wide <- long %>%
        tidyr::pivot_wider(names_from = "cdm_name", values_from = "value")
      wide
    })

    table_gt <- reactive({
      d <- tableData()
      if (is.null(d) || nrow(d) == 0) {
        return(gt::gt(data.frame(Message = "No quality check cleanup data for selected database(s).")))
      }
      gt::gt(d) %>%
        gt::tab_options(
          table.font.size = gt::px(13),
          data_row.padding = gt::px(8),
          column_labels.font.weight = "bold"
        ) %>%
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_body(columns = "Metric")
        )
    })
    output$qualityTable <- gt::render_gt({
      table_gt()
    })
    output$download_table_docx <- downloadHandler(
      filename = function() { "quality_check_cleanup_table.docx" },
      content = function(file) {
        tbl <- table_gt()
        if (!is.null(tbl)) gt::gtsave(tbl, file)
      }
    )

    qc_plot_ggplot <- reactive({
      sel <- input$cdm
      if (is.null(sel) || length(sel) == 0) sel <- dp
      d <- qualityCheckCleanup
      if (!is.data.frame(d) || nrow(d) == 0 || !"cdm_name" %in% colnames(d)) return(NULL)
      d <- d %>% dplyr::filter(.data$cdm_name %in% sel)

      countCols <- intersect(
        c("n_records_overlapping", "n_records_too_long", "n_records_after_cleanup"),
        colnames(d)
      )
      if (length(countCols) == 0) return(NULL)

      plotData <- d %>%
        dplyr::select(dplyr::all_of(c("cdm_name", countCols))) %>%
        tidyr::pivot_longer(-"cdm_name", names_to = "metric", values_to = "value") %>%
        dplyr::mutate(
          value = suppressWarnings(as.numeric(.data$value)),
          metric = ifelse(
            .data$metric %in% names(PRETTY_NAMES),
            PRETTY_NAMES[.data$metric],
            .data$metric
          ),
          metric = factor(.data$metric, levels = rev(unique(.data$metric)))
        )

      ggplot2::ggplot(plotData, ggplot2::aes(
        x = .data$metric, y = .data$value,
        fill = .data$cdm_name,
        text = paste0(.data$cdm_name, "\n",
                      .data$metric, ": ",
                      format(.data$value, big.mark = ","))
      )) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Count", fill = "Database") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "bottom",
          axis.text.y = ggplot2::element_text(size = 11)
        )
    })

    output$qcPlot <- plotly::renderPlotly({
      p <- qc_plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No quality check cleanup data."))
      plotly::ggplotly(p, tooltip = "text")
    })

    output$download_plot <- downloadHandler(
      filename = function() { "qualityCheckCleanupPlot.png" },
      content = function(file) {
        p <- qc_plot_ggplot()
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
  })
}
