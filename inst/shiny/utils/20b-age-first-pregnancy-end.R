# 20b-age-first-pregnancy-end.R - Age at first pregnancy end module (standard Shiny module)
# Data: ageSummaryFirstPregnancyEnd (columns: cdm_name, final_outcome_category, year, min, Q25, median, Q75, max, mean, sd)

ageFirstPregnancyEndUI <- function(id) {
  ns <- NS(id)

  allOutcomes <- if (is.data.frame(ageSummaryFirstPregnancyEnd) && nrow(ageSummaryFirstPregnancyEnd) > 0 &&
                     "final_outcome_category" %in% colnames(ageSummaryFirstPregnancyEnd)) {
    unique(as.character(ageSummaryFirstPregnancyEnd$final_outcome_category))
  } else {
    "overall"
  }

  allYears <- if (is.data.frame(ageSummaryFirstPregnancyEnd) && nrow(ageSummaryFirstPregnancyEnd) > 0 &&
                  "year" %in% colnames(ageSummaryFirstPregnancyEnd)) {
    sort(unique(as.character(ageSummaryFirstPregnancyEnd$year)))
  } else {
    "overall"
  }

  tagList(
    div(class = "tab-help-text", "Summary of maternal age at first pregnancy end, by outcome category."),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("outcome"), "Outcome",
                                          choices = allOutcomes,
                                          selected = allOutcomes,
                                          multiple = TRUE, options = opt)),
      column(3, shinyWidgets::pickerInput(ns("year"), "Year",
                                          choices = allYears,
                                          selected = "overall",
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
    DT::DTOutput(ns("table")) %>% withSpinner(),
    downloadButton(ns("download_table_csv"), "Download table (.csv)")
  )
}

ageFirstPregnancyEndServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    getData <- reactive({
      data <- ageSummaryFirstPregnancyEnd
      if (is.null(data) || nrow(data) == 0) return(data.frame())

      # backwards compat: if no outcome column, treat as overall
      if (!"final_outcome_category" %in% colnames(data)) {
        data$final_outcome_category <- "overall"
      }
      # backwards compat: if no year column, treat as overall
      if (!"year" %in% colnames(data)) {
        data$year <- "overall"
      }

      outcomeSel <- input$outcome
      if (!is.null(outcomeSel) && length(outcomeSel) > 0) {
        data <- data %>%
          dplyr::filter(.data$final_outcome_category %in% outcomeSel)
      }

      yearSel <- input$year
      if (!is.null(yearSel) && length(yearSel) > 0) {
        data <- data %>%
          dplyr::filter(.data$year %in% yearSel)
      }

      data
    })

    plot_ggplot <- reactive({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(NULL)

      ageMetrics <- c("min", "Q25", "median", "Q75", "max")
      for (col in ageMetrics) {
        if (col %in% colnames(data)) data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
      }
      data <- data %>%
        dplyr::filter(dplyr::if_all(dplyr::all_of(intersect(ageMetrics, colnames(data))), ~ !is.na(.)))
      if (nrow(data) == 0 || !all(ageMetrics %in% colnames(data))) return(NULL)

      outcomeLevels <- c("overall", "ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG", "LB or PREG")
      data <- data %>%
        dplyr::mutate(
          cdm_name = factor(.data$cdm_name, levels = rev(sort(unique(as.character(.data$cdm_name))))),
          final_outcome_category = factor(.data$final_outcome_category,
                                          levels = intersect(outcomeLevels, unique(as.character(.data$final_outcome_category))))
        )

      make_ggplot_boxplot_precomputed(
        data,
        x = "cdm_name",
        fill = "final_outcome_category",
        title = NULL,
        xlab = NULL,
        ylab = "Age at first pregnancy (end)",
        category_order = levels(data$cdm_name),
        horizontal = TRUE,
        position = ggplot2::position_dodge(width = 0.8)
      ) +
        ggplot2::scale_fill_brewer(palette = "Set2", name = "Outcome")
    })

    output$plot <- plotly::renderPlotly({
      pd <- getData()
      if (is.null(pd) || nrow(pd) == 0) return(emptyPlotlyMessage("No data available."))
      for (col in c("min", "Q25", "median", "Q75", "max")) {
        if (col %in% colnames(pd)) pd[[col]] <- suppressWarnings(as.numeric(pd[[col]]))
      }
      pd <- pd %>%
        dplyr::filter(dplyr::if_all(dplyr::all_of(intersect(c("min", "Q25", "median", "Q75", "max"), colnames(pd))), ~ !is.na(.)))
      if (nrow(pd) == 0) return(emptyPlotlyMessage("No data available."))
      outcomeLevels <- c("overall", "ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG", "LB or PREG")
      pd <- pd %>%
        dplyr::mutate(
          cdm_name = factor(.data$cdm_name, levels = rev(sort(unique(as.character(.data$cdm_name))))),
          final_outcome_category = factor(.data$final_outcome_category,
                                          levels = intersect(outcomeLevels, unique(as.character(.data$final_outcome_category))))
        )
      make_plotly_boxplot_precomputed(
        pd,
        x = "cdm_name",
        fill = "final_outcome_category",
        title = NULL,
        xlab = NULL,
        ylab = "Age at first pregnancy (end)",
        category_order = levels(pd$cdm_name),
        colors = "Set2",
        horizontal = TRUE
      )
    })

    output$download_plot <- downloadHandler(
      filename = function() { "ageFirstPregnancyEndPlot.png" },
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
      renderPrettyDT(getData())
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "age_first_pregnancy_end.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
