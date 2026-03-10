# 04-gestational-age.R - Gestational age (weekly) module (standard Shiny module)

gestationalAgeUI <- function(id) {
  ns <- NS(id)

  hasOutcome <- "final_outcome_category" %in% colnames(gestationalWeeksSummary)
  outcomeChoices <- if (hasOutcome) sort(c(unique(as.character(gestationalWeeksSummary$final_outcome_category)), "Overall")) else character(0)
  tagList(
    div(class = "tab-help-text",
        "Distribution of gestational age by week. Used for gestational-age histograms, preterm/term summaries, and cross-site comparison."),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("cdm"), "Database",
                                          choices = allDP, selected = allDP,
                                          multiple = TRUE, options = opt)),
      column(2, shinyWidgets::pickerInput(ns("yVar"), "Y value",
                                          choices = c("n", "pct", "log(n)"),
                                          selected = "log(n)",
                                          multiple = FALSE)),
      if (hasOutcome) column(3, shinyWidgets::pickerInput(ns("outcome"), "Outcome",
                                                          choices = outcomeChoices,
                                                          selected = "Overall",
                                                          multiple = TRUE, options = opt)),
      column(2, numericInput(ns("minWeeks"), "Min weeks", value = 0))
    ),
    fluidRow(
      column(2, numericInput(ns("maxWeeks"), "Max weeks", value = 50))
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

gestationalAgeServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    hasOutcome <- "final_outcome_category" %in% colnames(gestationalWeeksSummary)
    outcomeChoices <- if (hasOutcome) sort(c(unique(as.character(gestationalWeeksSummary$final_outcome_category)), "Overall")) else NULL

    getData <- reactive({
      data <- gestationalWeeksSummary

      if (is.null(data) || nrow(data) == 0) return(data.frame())

      # Apply min/max weeks filter
      minW <- input$minWeeks
      if (is.null(minW)) minW <- 0
      maxW <- input$maxWeeks
      if (is.null(maxW)) maxW <- 50

      data <- data %>%
        dplyr::mutate(gestational_weeks = as.numeric(.data$gestational_weeks)) %>%
        dplyr::filter(.data$gestational_weeks >= minW, .data$gestational_weeks <= maxW)

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
      if (is.null(yVar) || length(yVar) == 0) yVar <- "log(n)"

      if (yVar == "log(n)" && "n" %in% colnames(data)) {
        data <- data %>% dplyr::mutate(`log(n)` = log1p(as.numeric(.data$n)))
      }

      plot_labeller <- NULL
      if (!is.null(gestationalAgeDaysCounts) && is.data.frame(gestationalAgeDaysCounts) && nrow(gestationalAgeDaysCounts) > 0) {
        cdmCols <- setdiff(colnames(gestationalAgeDaysCounts), "name")
        if (length(cdmCols) > 0) {
          plot_labeller <- ggplot2::as_labeller(function(x) {
            vapply(x, function(val) {
              if (!val %in% cdmCols) return(val)
              dpData <- gestationalAgeDaysCounts %>% dplyr::select(dplyr::all_of(c("name", val)))
              less1day <- dpData %>% dplyr::filter(.data$name == "less_1day") %>% dplyr::pull(2)
              over308 <- dpData %>% dplyr::filter(.data$name == "over_308days") %>% dplyr::pull(2)
              as.character(glue::glue("{val} - <1d: {less1day}, >308d: {over308}"))
            }, character(1))
          })
        }
      }

      plotArgs <- list(
        data = data,
        xVar = "gestational_weeks",
        yVar = yVar,
        facetVar = "cdm_name",
        labelFunction = plot_labeller,
        label = "n",
        xLabel = "Weeks",
        yLabel = yVar,
        title = "Gestational duration",
        rotateAxisText = TRUE,
        verticalLinesPos = c(0, 44),
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
        msg <- if (nrow(gestationalWeeksSummary) == 0) "Results files are empty." else "No data for selected filters."
        return(emptyPlotlyMessage(msg))
      }
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "gestationalAgePlot.png" },
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
      filename = function() { "gestational_age.csv" },
      content = function(file) {
        d <- getData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
