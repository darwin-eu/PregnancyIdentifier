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
               fluidRow(
                 column(3, numericInput(ns("max_days_plot"), "Max days for plot", value = 50, min = 1, step = 1))
               ),
               plotly::plotlyOutput(ns("plot"), height = "420px") %>% withSpinner(),
               h4("Download figure"),
               fluidRow(
                 column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
                 column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
                 column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
               ),
               downloadButton(ns("download_plot"), "Download plot (PNG)")),
      tabPanel("Summary",
               h4("Episodes assessed"),
               p("Number of episodes for which precision (ESD start-date) could be assessed."),
               DT::DTOutput(ns("denominatorsTable")) %>% withSpinner(),
               br(),
               h4("Precision statistics"),
               DT::DTOutput(ns("summaryTable")) %>% withSpinner(),
               downloadButton(ns("download_summary_csv"), "Download table (.csv)"))
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

    # Denominators: total episodes, episodes with precision-days, % with precision-days (and GW timing if present)
    denominatorsTableData <- reactive({
      denom <- precisionDaysDenominators
      if (is.null(denom) || nrow(denom) == 0 || !"cdm_name" %in% colnames(denom)) return(NULL)
      denom <- filterByCdm(denom, input$cdm, dp)
      if (nrow(denom) == 0) return(NULL)
      out <- denom %>%
        dplyr::mutate(
          total_episodes = suppressWarnings(as.integer(.data$total_episodes)),
          episodes_with_precision_days = suppressWarnings(as.integer(.data$episodes_with_precision_days)),
          pct_with_precision_days = suppressWarnings(as.numeric(.data$pct_with_precision_days))
        ) %>%
        dplyr::select("cdm_name", "total_episodes", "episodes_with_precision_days", "pct_with_precision_days", dplyr::any_of(c("episodes_with_gw_timing", "pct_with_gw_timing")))
      if ("episodes_with_gw_timing" %in% colnames(out)) {
        out <- out %>% dplyr::mutate(
          episodes_with_gw_timing = suppressWarnings(as.integer(.data$episodes_with_gw_timing)),
          pct_with_gw_timing = suppressWarnings(as.numeric(.data$pct_with_gw_timing))
        )
      }
      out <- out %>%
        dplyr::rename(
          Database = .data$cdm_name,
          `Total episodes` = .data$total_episodes,
          `Episodes with precision-days` = .data$episodes_with_precision_days,
          `% with precision-days` = .data$pct_with_precision_days
        )
      if ("episodes_with_gw_timing" %in% colnames(out)) {
        out <- out %>% dplyr::rename(
          `Episodes with GW timing` = .data$episodes_with_gw_timing,
          `% with GW timing` = .data$pct_with_gw_timing
        )
      }
      out
    })
    output$denominatorsTable <- DT::renderDT({
      d <- denominatorsTableData()
      if (is.null(d) || nrow(d) == 0) {
        return(DT::datatable(
          data.frame(Message = "No episode denominators available. Re-export with the latest PregnancyIdentifier to get total episodes and episodes with precision-days."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      DT::datatable(
        d,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        filter = "none",
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = intersect(c("% with precision-days", "% with GW timing"), colnames(d)), digits = 1)
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

      max_days <- if (is.numeric(input$max_days_plot) && input$max_days_plot > 0) {
        input$max_days_plot
      } else {
        50
      }
      ggplot2::ggplot(plotData, ggplot2::aes(
        x = .data$esd_precision_days,
        y = .data$density,
        colour = .data$cdm_name
      )) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::scale_x_continuous(limits = c(0, max_days)) +
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
