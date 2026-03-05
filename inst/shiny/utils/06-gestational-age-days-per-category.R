# 06-gestational-age-days-per-category.R - Gestational age days per category module (standard Shiny module)
# ggplot2 boxplots with pre-computed quartiles, rendered with plotly::ggplotly(), with PNG download.

gestationalAgeDaysPerCategoryUI <- function(id) {

  ns <- NS(id)

  allOutcomes <- if (is.data.frame(gestationalAgeDaysPerCategorySummary) && nrow(gestationalAgeDaysPerCategorySummary) > 0) {
    unique(as.character(gestationalAgeDaysPerCategorySummary$final_outcome_category))
  } else {
    character(0)
  }

  emptyMsg <- is.null(gestationalAgeDaysPerCategorySummary) || nrow(gestationalAgeDaysPerCategorySummary) == 0

  dataTableOut <- tagList(
    h4("Data"),
    downloadButton(ns("download_table_csv"), "Download table (.csv)"),
    DT::DTOutput(ns("dataTable")) %>% shinycssloaders::withSpinner()
  )

  if (emptyMsg) {
    return(tagList(
      p("Results files are empty.", style = "margin: 20px; font-size: 16px; font-weight: bold;"),
      dataTableOut
    ))
  }

  tagList(
    div(class = "tab-help-text",
        "Gestational duration by outcome type. Used to check that outcome-specific durations (e.g. live birth vs miscarriage) are plausible."),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("cdm"), "Database",
                                          choices = allDP, selected = allDP,
                                          multiple = TRUE, options = opt)),
      column(3, shinyWidgets::pickerInput(ns("outcome"), "Outcome",
                                          choices = allOutcomes,
                                          selected = allOutcomes,
                                          multiple = TRUE, options = opt)),
      column(3, checkboxInput(ns("iqrOnly"), "IQR only", value = TRUE))
    ),
    plotly::plotlyOutput(ns("plot"), height = "420px") %>% shinycssloaders::withSpinner(),
    h4("Download figure"),
    fluidRow(
      column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
      column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
      column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
    ),
    downloadButton(ns("download_plot"), "Download plot (PNG)"),
    p(),
    dataTableOut
  )
}

gestationalAgeDaysPerCategoryServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    getData <- reactive({
      if (!is.data.frame(gestationalAgeDaysPerCategorySummary) || nrow(gestationalAgeDaysPerCategorySummary) == 0) {
        return(data.frame())
      }

      data <- filterByCdm(gestationalAgeDaysPerCategorySummary, input$cdm, allDP)

      outcomeSel <- input$outcome
      if (is.null(outcomeSel) || length(outcomeSel) == 0) {
        outcomeSel <- unique(as.character(gestationalAgeDaysPerCategorySummary$final_outcome_category))
      }
      data %>%
        dplyr::filter(.data$final_outcome_category %in% outcomeSel)
    })

    plot_ggplot <- reactive({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(NULL)

      iqrOnly <- input$iqrOnly
      if (is.null(iqrOnly)) iqrOnly <- TRUE

      outcomeLevels <- c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG")
      plot_data <- data %>%
        dplyr::group_by(.data$cdm_name, .data$final_outcome_category) %>%
        dplyr::slice(1L) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          final_outcome_category = factor(.data$final_outcome_category, levels = outcomeLevels),
          min = as.numeric(.data$min),
          Q25 = as.numeric(.data$Q25),
          median = as.numeric(.data$median),
          Q75 = as.numeric(.data$Q75),
          max = as.numeric(.data$max),
          ymin = if (iqrOnly) .data$Q25 else .data$min,
          ymax = if (iqrOnly) .data$Q75 else .data$max
        )

      if (nrow(plot_data) == 0) return(NULL)

      ggplot2::ggplot(plot_data, ggplot2::aes(
        x = .data$final_outcome_category,
        ymin = .data$ymin,
        lower = .data$Q25,
        middle = .data$median,
        upper = .data$Q75,
        ymax = .data$ymax,
        fill = .data$final_outcome_category
      )) +
        ggplot2::geom_boxplot(stat = "identity") +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(ggplot2::vars(.data$cdm_name), scales = "free_y") +
        ggplot2::labs(
          x = "Final outcome category",
          y = "Gestational age (days)",
          fill = "Outcome category"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 9),
          legend.position = "bottom"
        )
    })

    output$plot <- plotly::renderPlotly({
      p <- plot_ggplot()
      if (is.null(p)) {
        msg <- if (!is.data.frame(gestationalAgeDaysPerCategorySummary) || nrow(gestationalAgeDaysPerCategorySummary) == 0) {
          "Results files are empty."
        } else {
          "No data for selected filters."
        }
        return(emptyPlotlyMessage(msg))
      }
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "gestationalAgeDaysPerCategoryPlot.png" },
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

    output$dataTable <- DT::renderDT({
      renderPrettyDT(gestationalAgeDaysPerCategorySummary)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "gestational_age_days_per_category.csv" },
      content = function(file) {
        d <- gestationalAgeDaysPerCategorySummary
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
