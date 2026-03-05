# 16-prevalence.R - Prevalence module (table, plot, data)
# Structure matches 14-incidence.R: Table of estimates (gt), Plot of estimates (ggplot2 + plotly), Data (DT).

# ---- Pre-extract picker choices at file load time ----
if (exists("prevalence") && !is.null(prevalence) && is.data.frame(prevalence) && nrow(prevalence) > 0) {
  .prev_cdm <- sort(unique(prevalence$cdm_name))
  .prev_outcomes <- tryCatch(
    prevalence |> visOmopResults::splitGroup() |> dplyr::pull(outcome_cohort_name) |> unique() |> sort(),
    error = function(e) character(0)
  )
  .prev_settings <- tryCatch(omopgenerics::settings(prevalence), error = function(e) NULL)
  .prev_ageGroups <- if (!is.null(.prev_settings) && "denominator_age_group" %in% colnames(.prev_settings)) {
    sort(unique(.prev_settings$denominator_age_group))
  } else character(0)
  .prev_startDates <- tryCatch({
    prevalence |>
      visOmopResults::splitAdditional() |>
      dplyr::distinct(.data$prevalence_start_date) |>
      dplyr::filter(!is.na(.data$prevalence_start_date)) |>
      dplyr::arrange(.data$prevalence_start_date) |>
      dplyr::pull(.data$prevalence_start_date)
  }, error = function(e) character(0))
} else {
  .prev_cdm <- character(0)
  .prev_outcomes <- character(0)
  .prev_ageGroups <- character(0)
  .prev_startDates <- character(0)
}

# ---- UI ----
prevalenceUI <- function(id) {
  ns <- NS(id)
  cdm_choices <- if (exists("prevalence") && !is.null(prevalence) && nrow(prevalence) > 0) .prev_cdm else character(0)
  age_choices <- if (exists("prevalence") && !is.null(prevalence)) .prev_ageGroups else character(0)
  tagList(
    div(class = "tab-help-text", "Prevalence estimates and confidence intervals."),
    fluidRow(
      h3("Prevalence estimates"),
      p("Prevalence estimates are shown below, please select configuration to filter them:"),
      fluidRow(
        column(3, pickerInput(ns("cdm"), "CDM name", choices = cdm_choices, selected = cdm_choices, multiple = TRUE, options = opt)),
        column(3, pickerInput(ns("outcome"), "Outcome", choices = .prev_outcomes, selected = .prev_outcomes, multiple = TRUE, options = opt)),
        column(3, pickerInput(ns("age_group"), "Age group", choices = age_choices, selected = "0 to 150", multiple = TRUE, options = opt)),
        column(3, pickerInput(ns("start_date"), "Start date", choices = .prev_startDates, selected = .prev_startDates, multiple = TRUE, options = opt))
      ),
      tabsetPanel(
        id = ns("tabsetPanel"),
        type = "tabs",
        tabPanel(
          "Table of estimates",
          gt::gt_output(ns("report_table")) %>% withSpinner(type = 6),
          h4("Download table"),
          downloadButton(ns("download_table"), "Download table (.docx)")
        ),
        tabPanel(
          "Plot of estimates",
          p("Plotting options"),
          fluidRow(
            column(3, selectInput(ns("plot_facet"), "Facet by",
                                  choices = c("cdm_name", "denominator_age_group", "denominator_sex", "outcome_cohort_name", "None"))),
            column(3, selectInput(ns("plot_colour"), "Colour by",
                                  choices = c("denominator_age_group", "cdm_name", "denominator_sex", "outcome_cohort_name", "None"),
                                  selected = "outcome_cohort_name")),
            column(3, selectInput(ns("plot_ribbon"), "Ribbon", choices = c(FALSE, TRUE), selected = "FALSE")),
            column(3, checkboxInput(ns("conf_interval"), "Show CI", value = FALSE))
          ),
          fluidRow(
            column(3, checkboxInput(ns("rotate_x_labels"), "Rotate X labels", value = FALSE))
          ),
          plotlyOutput(ns("prevalence_plot"), height = "600px") %>% withSpinner(type = 6),
          h4("Download figure"),
          fluidRow(
            column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
            column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
            column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
          ),
          downloadButton(ns("download_plot"), "Download plot (PNG)")
        ),
        tabPanel(
          "Data",
          downloadButton(ns("download_data_csv"), "Download data (.csv)"),
          DT::DTOutput(ns("dataTable")) %>% withSpinner(type = 6)
        )
      )
    )
  )
}

# ---- Server ----
prevalenceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filtered summarised_result
    filtered_data <- reactive({
      req(input$cdm, input$age_group)
      d <- prevalence %>%
        dplyr::filter(cdm_name %in% input$cdm) %>%
        visOmopResults::filterSettings(denominator_age_group %in% input$age_group) %>%
        visOmopResults::filterGroup(outcome_cohort_name %in% input$outcome)
      if (!is.null(input$start_date) && length(input$start_date) > 0) {
        d <- d %>% visOmopResults::filterAdditional(prevalence_start_date %in% input$start_date)
      }
      d
    })

    # Table of estimates (gt)
    table_gt <- reactive({
      d <- filtered_data()
      validate(need(nrow(d) > 0, "No results for selected filters"))
      tryCatch({
        IncidencePrevalence::tablePrevalence(
          d,
          type = "gt",
          header = c("estimate_name"),
          groupColumn = c("cdm_name", "outcome_cohort_name"),
          settingsColumn = c("denominator_age_group", "denominator_sex"),
          hide = c("denominator_cohort_name"),
          .options = list(style = "darwin")
        )
      }, error = function(e) {
        gt::gt(data.frame(Message = paste("Could not render table:", e$message)))
      })
    })

    output$report_table <- gt::render_gt({
      table_gt()
    })

    output$download_table <- downloadHandler(
      filename = function() { "prevalenceEstimatesTable.docx" },
      content = function(file) {
        tryCatch({
          gt_tbl <- table_gt()
          gt::gtsave(gt_tbl, file)
        }, error = function(e) NULL)
      }
    )

    # Tidy data for plot and Data tab
    get_filtered_tidy <- reactive({
      d <- filtered_data()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      tryCatch({
        resSettings <- omopgenerics::settings(d)
        strataCol <- ""
        if ("strata" %in% colnames(resSettings)) {
          strataCol <- unique(resSettings %>% dplyr::filter(.data$strata != "reason") %>% dplyr::pull(.data$strata))
        }
        minCellCount <- if ("min_cell_count" %in% colnames(resSettings)) unique(resSettings$min_cell_count) else 5L
        tidy <- IncidencePrevalence::asPrevalenceResult(d)
        if (!"analysis_interval" %in% names(tidy)) tidy <- tidy %>% dplyr::mutate(analysis_interval = "overall")
        tidy <- tidy %>%
          dplyr::mutate(analysis_min_cell_count = !!minCellCount) %>%
          dplyr::rename(database = .data$cdm_name, n_cases = .data$outcome_count, n_population = .data$denominator_count)
        if (length(strataCol) == 0 || all(strataCol == "")) {
          tidy <- tidy %>% dplyr::mutate(strata = "overall")
        } else if (strataCol[1] %in% colnames(tidy)) {
          tidy <- tidy %>% dplyr::rename(strata = !!strataCol[1])
        } else {
          tidy <- tidy %>% dplyr::mutate(strata = "overall")
        }
        tidy %>%
          dplyr::mutate(
            prevalence = suppressWarnings(as.numeric(.data$prevalence)),
            prevalence_95CI_lower = suppressWarnings(as.numeric(.data$prevalence_95CI_lower)),
            prevalence_95CI_upper = suppressWarnings(as.numeric(.data$prevalence_95CI_upper)),
            prevalence_start_date = as.Date(.data$prevalence_start_date)
          )
      }, error = function(e) NULL)
    })

    # Plot (ggplot2: prevalence + geom_ribbon/line/point)
    plot_prevalence <- reactive({
      td <- get_filtered_tidy()
      if (is.null(td) || nrow(td) == 0) return(NULL)
      validate(need(nrow(td) > 0, "No results for selected inputs"))

      x_var <- "prevalence_start_date"
      if (!x_var %in% colnames(td)) return(NULL)
      facetVal <- if (input$plot_facet != "None") input$plot_facet else NULL
      colourVal <- if (input$plot_colour != "None") input$plot_colour else NULL

      tryCatch({
        aes_x <- ggplot2::aes(x = .data[[x_var]], y = .data$prevalence)
        if (!is.null(colourVal) && colourVal %in% colnames(td)) {
          aes_x <- ggplot2::aes(
            x = .data[[x_var]],
            y = .data$prevalence,
            colour = .data[[colourVal]],
            group = .data[[colourVal]]
          )
        }

        p <- ggplot2::ggplot(td, aes_x) + ggplot2::ylim(0, NA)

        if (as.logical(input$plot_ribbon) && isTRUE(input$conf_interval) &&
            "prevalence_95CI_lower" %in% colnames(td) && "prevalence_95CI_upper" %in% colnames(td)) {
          rib_aes <- ggplot2::aes(ymin = .data$prevalence_95CI_lower, ymax = .data$prevalence_95CI_upper)
          if (!is.null(colourVal) && colourVal %in% colnames(td)) {
            rib_aes <- ggplot2::aes(ymin = .data$prevalence_95CI_lower, ymax = .data$prevalence_95CI_upper, fill = .data[[colourVal]])
          }
          p <- p + ggplot2::geom_ribbon(rib_aes, alpha = 0.2, linewidth = 0)
        }
        if (as.logical(input$plot_ribbon) && !isTRUE(input$conf_interval)) {
          p <- p + ggplot2::geom_line(linewidth = 0.8)
        }
        p <- p +
          ggplot2::geom_point(size = 2) +
          ggplot2::labs(x = "Prevalence start date", y = "Prevalence") +
          ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
          ggplot2::ggtitle("Prevalence estimates") +
          ggplot2::guides(fill = "none")
        if (!is.null(facetVal) && facetVal %in% colnames(td)) {
          p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facetVal]]), scales = "free_y")
        }
        if (isTRUE(input$rotate_x_labels)) {
          p <- p + ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5),
            axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 7),
            axis.text.y = ggplot2::element_text(size = 8)
          )
        } else {
          p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), axis.text.y = ggplot2::element_text(size = 8))
        }
        p
      }, error = function(e) NULL)
    })

    output$prevalence_plot <- plotly::renderPlotly({
      p <- plot_prevalence()
      if (is.null(p)) return(emptyPlotlyMessage("No results for selected inputs."))
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "prevalenceEstimatesPlot.png" },
      content = function(file) {
        p <- plot_prevalence()
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

    # Data tab: reactive for table data (used by DT and CSV download)
    dataTableData <- reactive({
      td <- get_filtered_tidy()
      if (is.null(td) || nrow(td) == 0) return(NULL)
      displayData <- td
      if (all(c("prevalence", "prevalence_95CI_lower", "prevalence_95CI_upper") %in% colnames(displayData))) {
        displayData <- displayData %>%
          dplyr::mutate(`prevalence (%)` = paste0(
            round(100 * .data$prevalence, 2), " (",
            round(100 * .data$prevalence_95CI_lower, 2), " to ",
            round(100 * .data$prevalence_95CI_upper, 2), ")"
          ))
      }
      selectCols <- intersect(
        c("database", "outcome_cohort_name", "strata", "denominator_age_group", "denominator_sex",
          "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date",
          "denominator_time_at_risk", "analysis_interval", "prevalence_start_date",
          "n_cases", "n_population", "prevalence (%)"),
        colnames(displayData)
      )
      displayData %>%
        dplyr::select(dplyr::all_of(selectCols)) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.character), as.character))
    })

    output$dataTable <- DT::renderDT({
      d <- dataTableData()
      if (is.null(d) || nrow(d) == 0) {
        return(DT::datatable(data.frame(Message = "No data for selected filters."), rownames = FALSE))
      }
      d <- d %>% dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))
      DT::datatable(d, rownames = FALSE, filter = "top", options = list(scrollX = TRUE, scrollCollapse = TRUE))
    })

    output$download_data_csv <- downloadHandler(
      filename = function() { "prevalence_data.csv" },
      content = function(file) {
        d <- dataTableData()
        if (!is.null(d) && nrow(d) > 0) {
          readr::write_csv(d, file)
        }
      }
    )
  })
}

# ---- Minimal app for testing the prevalence module ----
# Usage (from inst/shiny): source("global.R"); runPrevalenceModuleApp()
runPrevalenceModuleApp <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Prevalence module – test"),
      prevalenceUI("prevalence")
    ),
    server = function(input, output, session) {
      prevalenceServer("prevalence")
    }
  )
}
runPrevalenceModuleApp()

