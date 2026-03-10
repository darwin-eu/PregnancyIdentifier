# 06-gestational-age-days-per-category.R - Gestational age days per category module (standard Shiny module)
# Boxplots from pre-computed table stats. Interactive: plotly native (ggplotly does not support geom_boxplot stat="identity"). PNG: ggplot2.

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
    DT::DTOutput(ns("dataTable")) %>% shinycssloaders::withSpinner(),
    downloadButton(ns("download_table_csv"), "Download table (.csv)")
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

    # Shared plot data: table stats (min, Q25, median, Q75, max) → boxplot bounds; IQR only = no min/max whiskers
    # Uses canonical names (lower, middle, upper, ymin, ymax) and final_outcome_category as category for boxplot-precomputed helper.
    getPlotData <- reactive({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      req_cols <- c("min", "Q25", "median", "Q75", "max")
      if (!all(req_cols %in% colnames(data))) return(NULL)
      iqrOnly <- isTRUE(input$iqrOnly)
      outcomeLevels <- c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG")
      data %>%
        dplyr::group_by(.data$cdm_name, .data$final_outcome_category) %>%
        dplyr::slice(1L) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          final_outcome_category = factor(.data$final_outcome_category, levels = outcomeLevels),
          lower = suppressWarnings(as.numeric(.data$Q25)),
          middle = suppressWarnings(as.numeric(.data$median)),
          upper = suppressWarnings(as.numeric(.data$Q75)),
          ymin = if (iqrOnly) .data$lower else suppressWarnings(as.numeric(.data$min)),
          ymax = if (iqrOnly) .data$upper else suppressWarnings(as.numeric(.data$max))
        ) %>%
        tidyr::drop_na(lower, middle, upper, ymin, ymax)
    })

    # Interactive: native Plotly from precomputed stats (boxplot-precomputed helper)
    output$plot <- plotly::renderPlotly({
      pd <- getPlotData()
      if (is.null(pd) || nrow(pd) == 0) {
        msg <- if (!is.data.frame(gestationalAgeDaysPerCategorySummary) || nrow(gestationalAgeDaysPerCategorySummary) == 0) {
          "Results files are empty."
        } else {
          "No data for selected filters."
        }
        return(emptyPlotlyMessage(msg))
      }
      make_plotly_boxplot_precomputed(
        pd,
        x = "final_outcome_category",
        facet = "cdm_name",
        title = NULL,
        xlab = "Final outcome category",
        ylab = "Gestational age (days)",
        show_outliers = FALSE,
        category_order = c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG"),
        colors = "Set2",
        horizontal = TRUE
      )
    })

    # PNG download: ggplot from same precomputed data (boxplot-precomputed helper)
    plot_ggplot <- reactive({
      pd <- getPlotData()
      if (is.null(pd) || nrow(pd) == 0) return(NULL)
      make_ggplot_boxplot_precomputed(
        pd,
        x = "final_outcome_category",
        fill = "final_outcome_category",
        facet = ggplot2::vars(.data$cdm_name),
        title = NULL,
        xlab = "Final outcome category",
        ylab = "Gestational age (days)",
        show_outliers = FALSE,
        category_order = c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG"),
        horizontal = TRUE
      ) +
        ggplot2::scale_fill_brewer(palette = "Set2", name = "Outcome category")
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
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(renderPrettyDT(data.frame()))
      if ("person_count" %in% colnames(d)) d <- d %>% dplyr::select(-"person_count")
      renderPrettyDT(d)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "gestational_age_days_per_category.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) {
          if ("person_count" %in% colnames(d)) d <- d %>% dplyr::select(-"person_count")
          readr::write_csv(d, file)
        }
      }
    )
  })
}
