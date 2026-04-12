# 05b-gestational-age-plausibility.R - GW plausibility analysis (binary: ≤44 vs >44 weeks)

gestationalAgePlausibilityUI <- function(id) {
  ns <- NS(id)

  hasOutcome <- "final_outcome_category" %in% colnames(gestationalWeeksPlausibility)
  outcomeChoices <- if (hasOutcome) sort(c(unique(as.character(gestationalWeeksPlausibility$final_outcome_category)), "Overall")) else character(0)

  tagList(
    div(class = "tab-help-text",
        "Binary plausibility check for gestational age: episodes with gestational duration \u226444 weeks are classified as ",
        tags$b("plausible"), ", those >44 weeks as ", tags$b("implausible"),
        ". Percentages are calculated per database", if (hasOutcome) " and outcome category", "."),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("cdm"), "Database",
                                          choices = allDP, selected = allDP,
                                          multiple = TRUE, options = opt)),
      column(3, shinyWidgets::pickerInput(ns("yVar"), "Y value",
                                          choices = c("n", "pct"),
                                          selected = "pct",
                                          multiple = FALSE)),
      if (hasOutcome) column(3, shinyWidgets::pickerInput(ns("outcome"), "Outcome",
                                                          choices = outcomeChoices,
                                                          selected = "Overall",
                                                          multiple = TRUE, options = opt))
    ),
    plotly::plotlyOutput(ns("plot"), height = "420px") %>% shinycssloaders::withSpinner(),
    h4("Download figure"),
    fluidRow(
      column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
      column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
      column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
    ),
    downloadButton(ns("download_plot"), "Download plot (PNG)"),
    h4("Data"),
    DT::DTOutput(ns("dataTable")) %>% shinycssloaders::withSpinner(),
    downloadButton(ns("download_table_csv"), "Download table (.csv)"),
    hr(),
    h3(">52 weeks check"),
    div(class = "tab-help-text",
        "Episodes with gestational duration >52 weeks (>364 days)."),
    h4("Summary by database"),
    DT::DTOutput(ns("over52SummaryTable")) %>% shinycssloaders::withSpinner(),
    downloadButton(ns("download_over52_summary_csv"), "Download summary (.csv)"),
    if (hasOutcome) tagList(
      h4("By database and outcome category"),
      DT::DTOutput(ns("over52DetailTable")) %>% shinycssloaders::withSpinner(),
      downloadButton(ns("download_over52_detail_csv"), "Download detail (.csv)")
    )
  )
}

gestationalAgePlausibilityServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePickerInput(session, "cdm", choices = rv$allDP, selected = rv$allDP)
    })

    hasOutcome <- reactive({
      "final_outcome_category" %in% colnames(rv$gestationalWeeksPlausibility)
    })

    outcomeChoices <- reactive({
      if (hasOutcome()) {
        sort(c(unique(as.character(rv$gestationalWeeksPlausibility$final_outcome_category)), "Overall"))
      } else {
        NULL
      }
    })

    observe({
      oc <- outcomeChoices()
      if (!is.null(oc) && length(oc) > 0) {
        updatePickerInput(session, "outcome", choices = oc, selected = "Overall")
      }
    })

    getData <- reactive({
      data <- rv$gestationalWeeksPlausibility

      if (is.null(data) || nrow(data) == 0) return(data.frame())

      # Filter by CDM
      data <- filterByCdm(data, input$cdm, rv$allDP)

      # Add "Overall" group when outcome column exists
      if (hasOutcome() && "final_outcome_category" %in% colnames(data) && nrow(data) > 0) {
        overall <- data %>%
          dplyr::mutate(n = as.numeric(.data$n)) %>%
          dplyr::group_by(.data$cdm_name, .data$plausibility) %>%
          dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::mutate(pct = round(100 * .data$n / sum(.data$n, na.rm = TRUE), 1)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(final_outcome_category = "Overall", .after = "cdm_name")
        data <- dplyr::bind_rows(data, overall)
      }

      # Filter by outcome selection
      if (hasOutcome()) {
        outcomeSel <- input$outcome
        if (is.null(outcomeSel) || length(outcomeSel) == 0) outcomeSel <- outcomeChoices()
        data <- data %>%
          dplyr::filter(.data$final_outcome_category %in% outcomeSel)
      }

      data
    })

    plot_ggplot <- reactive({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(NULL)

      yVar <- input$yVar
      if (is.null(yVar) || length(yVar) == 0) yVar <- "pct"

      plotArgs <- list(
        data = data,
        xVar = "plausibility",
        yVar = yVar,
        facetVar = "cdm_name",
        label = "n",
        xLabel = "Gestational duration plausibility",
        yLabel = if (yVar == "pct") "Percentage (%)" else yVar,
        title = "Gestational duration plausibility",
        rotateAxisText = FALSE,
        return_ggplot = TRUE
      )

      if (hasOutcome() && "final_outcome_category" %in% colnames(data)) {
        plotArgs$fillVar <- "final_outcome_category"
      }

      do.call(barPlot, plotArgs)
    })

    output$plot <- plotly::renderPlotly({
      p <- plot_ggplot()
      if (is.null(p)) {
        msg <- if (nrow(rv$gestationalWeeksPlausibility) == 0) "Results files are empty." else "No data for selected filters."
        return(emptyPlotlyMessage(msg))
      }
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "gestationalAgePlausibilityPlot.png" },
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
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(renderPrettyDT(data.frame()))
      renderPrettyDT(data)
    })

    output$download_table_csv <- downloadHandler(
      filename = function() { "gestational_age_plausibility.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )

    # >52 weeks tables
    getOver52Filtered <- reactive({
      data <- rv$gestationalWeeksOver52
      if (is.null(data) || nrow(data) == 0) return(data.frame())
      data <- filterByCdm(data, input$cdm, rv$allDP)
      data %>% dplyr::filter(.data$over_52_weeks == ">52 weeks")
    })

    # Summary: aggregated by database only
    getOver52Summary <- reactive({
      data <- getOver52Filtered()
      if (is.null(data) || nrow(data) == 0) return(data.frame())
      if ("final_outcome_category" %in% colnames(data)) {
        data <- data %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop")
        # Recalculate pct from the full over52 data
        totals <- rv$gestationalWeeksOver52 %>%
          filterByCdm(input$cdm, rv$allDP) %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::summarise(total = sum(.data$n, na.rm = TRUE), .groups = "drop")
        data <- data %>%
          dplyr::left_join(totals, by = "cdm_name") %>%
          dplyr::mutate(pct = round(100 * .data$n / .data$total, 1)) %>%
          dplyr::select(-"total")
      }
      data %>%
        dplyr::select(dplyr::any_of(c("cdm_name", "n", "pct"))) %>%
        dplyr::rename(`Database` = cdm_name, `N (>52 weeks)` = n, `% of episodes` = pct)
    })

    # Detail: by database and outcome category
    getOver52Detail <- reactive({
      data <- getOver52Filtered()
      if (is.null(data) || nrow(data) == 0) return(data.frame())
      data %>%
        dplyr::select(dplyr::any_of(c("cdm_name", "final_outcome_category", "n", "pct"))) %>%
        dplyr::rename(`Database` = cdm_name, `N (>52 weeks)` = n, `% of episodes` = pct)
    })

    output$over52SummaryTable <- DT::renderDT({
      data <- getOver52Summary()
      if (is.null(data) || nrow(data) == 0) return(renderPrettyDT(data.frame()))
      renderPrettyDT(data)
    })

    output$download_over52_summary_csv <- downloadHandler(
      filename = function() { "over_52_weeks_summary.csv" },
      content = function(file) {
        d <- getOver52Summary()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )

    output$over52DetailTable <- DT::renderDT({
      data <- getOver52Detail()
      if (is.null(data) || nrow(data) == 0) return(renderPrettyDT(data.frame()))
      renderPrettyDT(data)
    })

    output$download_over52_detail_csv <- downloadHandler(
      filename = function() { "over_52_weeks_by_outcome.csv" },
      content = function(file) {
        d <- getOver52Detail()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
