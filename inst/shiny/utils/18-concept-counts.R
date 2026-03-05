# 18-concept-counts.R - Concept counts module (standard Shiny module)
# Parameterized module: pass the data reactive or data frame via the `data` argument.

conceptCountsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database", choices = character(0), selected = character(0), multiple = TRUE, options = opt))
    ),
    tabsetPanel(
      tabPanel("Plot",
               fluidRow(
                 column(3, sliderInput(ns("topN"), "Top N concepts", min = 5, max = 50, value = 20, step = 5))
               ),
               plotlyOutput(ns("plot"), height = "500px") %>% withSpinner(),
               h4("Download figure"),
               fluidRow(
                 column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
                 column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
                 column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
               ),
               downloadButton(ns("download_plot"), "Download plot (PNG)")
      ),
      tabPanel("Data",
               downloadButton(ns("download_table_csv"), "Download table (.csv)"),
               DT::DTOutput(ns("table")) %>% withSpinner())
    )
  )
}

conceptCountsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Resolve data: accept reactive or static data frame
    resolveData <- reactive({
      if (is.reactive(data)) data() else data
    })

    # Initialize CDM picker from data
    observe({
      d <- resolveData()
      if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return()
      if ("cdm_name" %in% colnames(d)) {
        cdmChoices <- sort(unique(d$cdm_name))
        updatePickerInput(session, "cdm", choices = cdmChoices, selected = cdmChoices)
      }
    })

    # Coerce numeric columns and filter by CDM
    getData <- reactive({
      d <- resolveData()
      if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(NULL)

      # Coerce numeric-looking columns
      numericCols <- c("record_count", "person_count", "concept_id",
                       "esd_concept_id", "pps_concept_id",
                       "n_after_min", "n_prior_max", "n_in_span", "n_at_midpoint",
                       "p_after_min", "p_prior_max", "p_in_span", "p_at_midpoint", "p_concept")
      for (col in intersect(numericCols, colnames(d))) {
        d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
      }

      if ("cdm_name" %in% colnames(d)) {
        cdmSel <- input$cdm
        if (!is.null(cdmSel) && length(cdmSel) > 0) {
          d <- d %>% dplyr::filter(.data$cdm_name %in% cdmSel)
        }
      }
      d
    })

    concept_plot_ggplot <- reactive({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)

      nameCol <- intersect(colnames(d), c("concept_name", "esd_concept_name", "pps_concept_name"))[1]
      countCol <- intersect(colnames(d), c("record_count", "person_count"))[1]

      if (is.na(nameCol) || is.na(countCol)) return(NULL)

      topN <- input$topN
      if (is.null(topN)) topN <- 20

      plotData <- d %>%
        dplyr::mutate(
          concept = as.character(.data[[nameCol]]),
          count = suppressWarnings(as.numeric(.data[[countCol]]))
        ) %>%
        dplyr::filter(!is.na(.data$count)) %>%
        dplyr::group_by(.data$concept) %>%
        dplyr::summarise(count = sum(.data$count, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(.data$count)) %>%
        dplyr::slice_head(n = topN) %>%
        dplyr::mutate(
          concept = factor(.data$concept, levels = rev(.data$concept)),
          label = paste0(.data$concept, "\n", format(.data$count, big.mark = ","))
        )

      if (nrow(plotData) == 0) return(NULL)

      ggplot2::ggplot(plotData, ggplot2::aes(
        x = .data$concept, y = .data$count,
        text = .data$label
      )) +
        ggplot2::geom_bar(stat = "identity", fill = "#377EB8") +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = NULL,
          y = if (countCol == "person_count") "Person Count" else "Record Count"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9))
    })

    output$plot <- plotly::renderPlotly({
      p <- concept_plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No concept count data available."))
      plotly::ggplotly(p, tooltip = "text")
    })

    output$download_plot <- downloadHandler(
      filename = function() { "conceptCountsPlot.png" },
      content = function(file) {
        p <- concept_plot_ggplot()
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

    # Table
    output$table <- DT::renderDT({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) {
        return(renderPrettyDT(tibble::tibble()))
      }
      renderPrettyDT(d)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "concept_counts.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
