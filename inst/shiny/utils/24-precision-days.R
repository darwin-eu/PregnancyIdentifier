# 24-precision-days.R - Precision days module (standard Shiny module)
# Data: precisionDays (columns: cdm_name, esd_precision_days, optionally density)

precisionDaysUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text",
        "Distribution of ESD start-date precision (in days). Lower values indicate more precise inferred start dates."),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database",
                           choices = character(0), selected = character(0),
                           multiple = TRUE, options = opt))
    ),
    tabsetPanel(
      tabPanel("Plot",
               plotly::plotlyOutput(ns("plot"), height = "420px") %>% withSpinner(),
               h4("Download figure"),
               fluidRow(
                 column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
                 column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
                 column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
               ),
               downloadButton(ns("download_plot"), "Download plot (PNG)")),
      tabPanel("Summary",
               downloadButton(ns("download_summary_csv"), "Download table (.csv)"),
               DT::DTOutput(ns("summaryTable")) %>% withSpinner())
    )
  )
}

precisionDaysServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    dp <- if ("cdm_name" %in% colnames(precisionDays)) {
      unique(c(allDP, precisionDays$cdm_name))
    } else {
      allDP
    }

    observe({
      updatePickerInput(session, "cdm", choices = dp, selected = dp)
    })

    getData <- reactive({
      data <- precisionDays
      if (!"cdm_name" %in% colnames(data)) return(data)
      filterByCdm(data, input$cdm, dp)
    })

    # Summary table: descriptive statistics per database
    summaryTableData <- reactive({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      data <- data %>%
        dplyr::mutate(esd_precision_days = suppressWarnings(as.numeric(.data$esd_precision_days))) %>%
        dplyr::filter(!is.na(.data$esd_precision_days))
      if (nrow(data) == 0 || !"cdm_name" %in% colnames(data)) return(NULL)
      hasDensity <- "density" %in% colnames(data) &&
        any(!is.na(suppressWarnings(as.numeric(data$density))))
      if (hasDensity) {
        data %>%
          dplyr::mutate(density = suppressWarnings(as.numeric(.data$density))) %>%
          dplyr::filter(!is.na(.data$density)) %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::summarise(
            `N Points` = dplyr::n(),
            `Min` = round(min(.data$esd_precision_days), 1),
            `Weighted Mean` = round(stats::weighted.mean(.data$esd_precision_days, .data$density), 1),
            `Max` = round(max(.data$esd_precision_days), 1),
            `Peak Density At` = round(.data$esd_precision_days[which.max(.data$density)], 1),
            .groups = "drop"
          ) %>%
          dplyr::rename(Database = .data$cdm_name)
      } else {
        data %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::summarise(
            N = dplyr::n(),
            Min = round(min(.data$esd_precision_days), 1),
            `Q1 (25th)` = round(stats::quantile(.data$esd_precision_days, 0.25), 1),
            Median = round(stats::median(.data$esd_precision_days), 1),
            `Q3 (75th)` = round(stats::quantile(.data$esd_precision_days, 0.75), 1),
            Max = round(max(.data$esd_precision_days), 1),
            Mean = round(mean(.data$esd_precision_days), 1),
            `Std Dev` = round(stats::sd(.data$esd_precision_days), 1),
            .groups = "drop"
          ) %>%
          dplyr::rename(Database = .data$cdm_name)
      }
    })
    output$summaryTable <- DT::renderDT({
      summary_df <- summaryTableData()
      if (is.null(summary_df) || nrow(summary_df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No precision days data available."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      DT::datatable(
        summary_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        filter = "none",
        rownames = FALSE
      ) %>%
        DT::formatRound(
          columns = setdiff(colnames(summary_df), "Database"),
          digits = 1
        )
    })
    output$download_summary_csv <- downloadHandler(
      filename = function() { "precision_days_summary.csv" },
      content = function(file) {
        d <- summaryTableData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )

    precision_plot_ggplot <- reactive({
      data <- getData()
      if (is.null(data) || nrow(data) == 0) return(NULL)

      hasDensityCol <- "density" %in% colnames(data)
      data <- data %>%
        dplyr::mutate(
          esd_precision_days = suppressWarnings(as.numeric(.data$esd_precision_days)),
          density = if (hasDensityCol) suppressWarnings(as.numeric(.data$density)) else NA_real_
        ) %>%
        dplyr::filter(!is.na(.data$esd_precision_days))

      if (!"cdm_name" %in% colnames(data)) return(NULL)

      hasDensity <- "density" %in% colnames(data) && any(!is.na(data$density))
      if (hasDensity) {
        plotData <- data %>%
          dplyr::filter(!is.na(.data$density)) %>%
          dplyr::arrange(.data$cdm_name, .data$esd_precision_days)
      } else {
        plotData <- data %>%
          dplyr::filter(!is.na(.data$esd_precision_days)) %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::group_modify(function(g, ...) {
            x <- g$esd_precision_days
            if (length(x) < 2L) {
              if (length(x) == 0L) return(tibble::tibble(esd_precision_days = numeric(0), density = numeric(0)))
              return(tibble::tibble(esd_precision_days = x, density = 1))
            }
            d <- stats::density(x)
            tibble::tibble(esd_precision_days = d$x, density = d$y)
          }) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(.data$cdm_name, .data$esd_precision_days)
      }

      if (nrow(plotData) == 0) return(NULL)

      ggplot2::ggplot(plotData, ggplot2::aes(
        x = .data$esd_precision_days,
        y = .data$density,
        colour = .data$cdm_name
      )) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::labs(x = "ESD Precision (days)", y = "Density", colour = "Database") +
        ggplot2::theme_minimal()
    })

    output$plot <- plotly::renderPlotly({
      p <- precision_plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No precision days data available."))
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "precisionDaysPlot.png" },
      content = function(file) {
        p <- precision_plot_ggplot()
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
