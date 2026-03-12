# 05-gestational-age-binned.R - Gestational age binned module (standard Shiny module)

gestationalAgeBinnedUI <- function(id) {
  ns <- NS(id)

  hasOutcome <- "final_outcome_category" %in% colnames(gestationalWeeksBinned)
  outcomeChoices <- if (hasOutcome) sort(c(unique(as.character(gestationalWeeksBinned$final_outcome_category)), "Overall")) else character(0)
  tagList(
    div(class = "tab-help-text",
        "Gestational age (rounded to integer weeks) in non-overlapping bands: <12, 12-27, 28-31, 32-36, 37-38, 39-41, 42-43, 44-49, \u226550 weeks. Used for outcome bands and cross-site comparison."),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("cdm"), "Database",
                                          choices = allDP, selected = allDP,
                                          multiple = TRUE, options = opt)),
      column(3, shinyWidgets::pickerInput(ns("yVar"), "Y value",
                                          choices = c("n", "pct", "log(n)"),
                                          selected = "n",
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
    downloadButton(ns("download_table_csv"), "Download table (.csv)")
  )
}

gestationalAgeBinnedServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    hasOutcome <- "final_outcome_category" %in% colnames(gestationalWeeksBinned)
    outcomeChoices <- if (hasOutcome) sort(c(unique(as.character(gestationalWeeksBinned$final_outcome_category)), "Overall")) else NULL

    getData <- reactive({
      data <- gestationalWeeksBinned

      if (is.null(data) || nrow(data) == 0) return(data.frame())

      # Filter by CDM
      data <- filterByCdm(data, input$cdm, allDP)

      # Add "Overall" group when outcome column exists
      if (hasOutcome && "final_outcome_category" %in% colnames(data) && nrow(data) > 0) {
        overall <- data %>%
          dplyr::mutate(n = as.numeric(.data$n)) %>%
          dplyr::group_by(.data$cdm_name, .data$gestational_weeks) %>%
          dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::mutate(pct = round(100 * .data$n / sum(.data$n, na.rm = TRUE), 1)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(final_outcome_category = "Overall", .after = "cdm_name")
        data <- dplyr::bind_rows(data, overall)
      }

      # Filter by outcome selection (after Overall is added)
      if (hasOutcome) {
        outcomeSel <- input$outcome
        if (is.null(outcomeSel) || length(outcomeSel) == 0) outcomeSel <- outcomeChoices
        data <- data %>%
          dplyr::filter(.data$final_outcome_category %in% outcomeSel)
      }

      data
    })

    plot_ggplot <- reactive({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(NULL)

      yVar <- input$yVar
      if (is.null(yVar) || length(yVar) == 0) yVar <- "n"

      if (yVar == "log(n)" && "n" %in% colnames(data)) {
        data <- data %>% dplyr::mutate(`log(n)` = log1p(as.numeric(.data$n)))
      }

      plotArgs <- list(
        data = data,
        xVar = "gestational_weeks",
        yVar = yVar,
        facetVar = "cdm_name",
        label = "n",
        xLabel = "Gestational age band",
        yLabel = yVar,
        title = "Gestational duration",
        rotateAxisText = TRUE,
        return_ggplot = TRUE
      )

      if (hasOutcome && "final_outcome_category" %in% colnames(data)) {
        plotArgs$fillVar <- "final_outcome_category"
      }

      do.call(barPlot, plotArgs)
    })

    output$plot <- plotly::renderPlotly({
      p <- plot_ggplot()
      if (is.null(p)) {
        msg <- if (nrow(gestationalWeeksBinned) == 0) "Results files are empty." else "No data for selected filters."
        return(emptyPlotlyMessage(msg))
      }
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "gestationalAgeBinnedPlot.png" },
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
      filename = function() { "gestational_age_binned.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
