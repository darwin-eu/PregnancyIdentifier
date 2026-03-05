# 08-pregnancy-overlap.R - Pregnancy overlap module (standard Shiny module)

pregnancyOverlapUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "tab-help-text",
        "Count of pregnancy episodes by overlap status (overlapping vs non-overlapping). Used for data quality (overlaps may indicate algorithm or data issues)."),
    div(
      class = "well",
      p(
        strong("What the overlap values mean:"),
        tags$ul(
          tags$li(
            strong("Overlapping:"),
            " This pregnancy episode overlaps in time with at least one other episode for the same person. May indicate a data or algorithm issue."
          ),
          tags$li(
            strong("Non-overlapping:"),
            " This pregnancy episode does not overlap with any other episode for the same person."
          ),
          tags$li(
            strong("Single episode (N/A):"),
            " The person has only one pregnancy episode, so overlap is not defined."
          )
        )
      )
    ),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("cdm"), "Database",
                                          choices = allDP, selected = allDP,
                                          multiple = TRUE, options = opt))
    ),
    tabsetPanel(
      id = ns("overlapTabs"),
      tabPanel(
        "Plot",
        plotly::plotlyOutput(ns("overlapPlot"), height = "420px") %>%
          shinycssloaders::withSpinner(),
        h4("Download figure"),
        fluidRow(
          column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
          column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
          column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
        ),
        downloadButton(ns("download_plot"), "Download plot (PNG)")
      ),
      tabPanel(
        "Data",
        downloadButton(ns("download_table_csv"), "Download table (.csv)"),
        DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner()
      )
    )
  )
}

pregnancyOverlapServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    getData <- reactive({
      if (is.null(pregnancyOverlapCounts) || nrow(pregnancyOverlapCounts) == 0) {
        return(data.frame(
          cdm_name = character(0),
          overlap_status = character(0),
          n = integer(0),
          total = integer(0),
          pct = numeric(0)
        ))
      }
      filterByCdm(pregnancyOverlapCounts, input$cdm, allDP)
    })

    overlap_plot_ggplot <- reactive({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)

      plotData <- d %>%
        dplyr::group_by(.data$cdm_name, .data$overlap) %>%
        dplyr::summarise(N = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
        dplyr::group_by(.data$cdm_name) %>%
        dplyr::mutate(pct = round(100 * .data$N / sum(.data$N), 1)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(overlap_status = dplyr::case_when(
          .data$overlap == TRUE  ~ "Overlapping",
          .data$overlap == FALSE ~ "Non-overlapping",
          is.na(.data$overlap)   ~ "Single episode (N/A)",
          TRUE                   ~ as.character(.data$overlap)
        )) %>%
        dplyr::mutate(overlap_status = factor(
          .data$overlap_status,
          levels = c("Overlapping", "Non-overlapping", "Single episode (N/A)")
        ))

      overlapColours <- c(
        "Overlapping"          = "#E41A1C",
        "Non-overlapping"      = "#4DAF4A",
        "Single episode (N/A)" = "#999999"
      )

      ggplot2::ggplot(plotData, ggplot2::aes(
        x = .data$cdm_name, y = .data$pct,
        fill = .data$overlap_status,
        text = paste0(.data$overlap_status, "\n",
                      "N: ", format(.data$N, big.mark = ","), "\n",
                      round(.data$pct, 1), "%")
      )) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::scale_fill_manual(values = overlapColours, name = "Overlap Status") +
        ggplot2::labs(x = "Database", y = "Percentage (%)") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
    })

    output$overlapPlot <- plotly::renderPlotly({
      p <- overlap_plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No pregnancy overlap data for selected filters."))
      plotly::ggplotly(p, tooltip = "text")
    })

    output$download_plot <- downloadHandler(
      filename = function() { "pregnancyOverlapPlot.png" },
      content = function(file) {
        p <- overlap_plot_ggplot()
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
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(renderPrettyDT(data.frame()))
      renderPrettyDT(d)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "pregnancy_overlap.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
