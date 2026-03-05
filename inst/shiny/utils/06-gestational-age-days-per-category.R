# 06-gestational-age-days-per-category.R - Gestational age days per category module (standard Shiny module)
# Native plotly boxplot with pre-computed quartiles (ggplotly does not support geom_boxplot(stat="identity")).

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

    output$plot <- plotly::renderPlotly({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) {
        msg <- if (!is.data.frame(gestationalAgeDaysPerCategorySummary) || nrow(gestationalAgeDaysPerCategorySummary) == 0) {
          "Results files are empty."
        } else {
          "No data for selected filters."
        }
        return(emptyPlotlyMessage(msg))
      }

      iqrOnly <- input$iqrOnly
      if (is.null(iqrOnly)) iqrOnly <- TRUE

      facetBy <- "cdm_name"
      fill_var <- "final_outcome_category"

      outcomeLevels <- c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG")
      has_count_cols <- all(c("person_count", "episode_count") %in% colnames(data))

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
          lowerfence = if (iqrOnly) .data$Q25 else .data$min,
          upperfence = if (iqrOnly) .data$Q75 else .data$max
        )

      if (has_count_cols) {
        plot_data <- plot_data %>%
          dplyr::mutate(
            person_count = suppressWarnings(as.integer(.data$person_count)),
            episode_count = suppressWarnings(as.integer(.data$episode_count))
          )
      }

      if (nrow(plot_data) == 0) return(emptyPlotlyMessage("No data for selected filters."))

      n_cats <- length(unique(plot_data[[fill_var]]))
      pal <- scales::hue_pal()(max(1L, n_cats))
      if (n_cats >= 1) names(pal) <- sort(unique(as.character(plot_data[[fill_var]])))

      facet_levels <- sort(unique(as.character(plot_data[[facetBy]])))

      plot_list <- lapply(facet_levels, function(facet_val) {
        pd <- plot_data %>% dplyr::filter(.data[[facetBy]] == .env$facet_val)
        p <- plotly::plot_ly()

        for (i in seq_len(nrow(pd))) {
          row <- pd[i, ]
          cat_val <- as.character(row$final_outcome_category)
          clr <- pal[as.character(row[[fill_var]])]
          if (is.na(clr)) clr <- "gray"

          hover_lines <- c(
            paste0("Outcome: ", cat_val),
            paste0("Median: ", round(row$median, 1), " days"),
            paste0("IQR: ", round(row$Q25, 1), " - ", round(row$Q75, 1))
          )
          if (has_count_cols && "person_count" %in% colnames(row) && "episode_count" %in% colnames(row)) {
            hover_lines <- c(hover_lines,
              paste0("Person count: ", format(row$person_count, big.mark = ",")),
              paste0("Episode count: ", format(row$episode_count, big.mark = ","))
            )
          }

          trace_args <- list(
            type = "box",
            q1 = row$Q25,
            median = row$median,
            q3 = row$Q75,
            lowerfence = row$lowerfence,
            upperfence = row$upperfence,
            name = cat_val,
            line = list(color = clr),
            fillcolor = clr,
            showlegend = (facet_val == facet_levels[1]),
            hoverinfo = "text",
            text = paste(hover_lines, collapse = "<br>")
          )
          trace_args$y <- cat_val
          trace_args$orientation <- "h"
          p <- do.call(plotly::add_trace, c(list(p), trace_args))
        }

        p <- plotly::layout(
          p,
          title = list(text = facet_val, font = list(size = 12)),
          xaxis = list(title = "Gestational age (days)", tickangle = 0),
          yaxis = list(title = "Final outcome category"),
          showlegend = (facet_val == facet_levels[1]),
          legend = list(title = list(text = "Outcome category"))
        )
        p
      })

      if (length(plot_list) == 1) {
        out <- plot_list[[1]]
      } else {
        nrows <- ceiling(sqrt(length(plot_list)))
        out <- do.call(plotly::subplot, c(plot_list, list(
          nrows = nrows,
          margin = 0.05,
          shareX = TRUE,
          shareY = TRUE,
          titleX = TRUE,
          titleY = TRUE
        )))
      }
      out
    })

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
