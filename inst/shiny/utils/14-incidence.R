# 14-incidence.R - Incidence module (table, plot, attrition, population rates)
# Single module: Table of estimates (gt), Plot of estimates (ggplot2 + plotly, per 1,000 pys), Attrition (gt), Population rates (DT).

# ---- Pre-extract picker choices at file load time ----
if (exists("incidence") && !is.null(incidence) && is.data.frame(incidence) && nrow(incidence) > 0) {
  .inc_cdm <- sort(unique(incidence$cdm_name))
  .inc_outcomes <- tryCatch(
    incidence |> visOmopResults::splitGroup() |> dplyr::pull(outcome_cohort_name) |> unique() |> sort(),
    error = function(e) character(0)
  )
  .inc_settings <- tryCatch(omopgenerics::settings(incidence), error = function(e) NULL)
  .inc_ageGroups <- if (!is.null(.inc_settings) && "denominator_age_group" %in% colnames(.inc_settings)) {
    sort(unique(.inc_settings$denominator_age_group))
  } else character(0)
  .inc_sex <- if (!is.null(.inc_settings) && "denominator_sex" %in% colnames(.inc_settings)) {
    sort(unique(.inc_settings$denominator_sex))
  } else character(0)
  .inc_years <- tryCatch({
    incidence |>
      visOmopResults::splitAdditional() |>
      dplyr::distinct(.data$incidence_start_date) |>
      dplyr::filter(.data$incidence_start_date != "overall") |>
      dplyr::mutate(year = stringr::str_extract(.data$incidence_start_date, "^[0-9]+")) |>
      dplyr::filter(!is.na(year)) |>
      dplyr::distinct(year) |>
      dplyr::arrange(year) |>
      dplyr::pull(year)
  }, error = function(e) character(0))
} else {
  .inc_cdm <- character(0)
  .inc_outcomes <- character(0)
  .inc_ageGroups <- character(0)
  .inc_sex <- character(0)
  .inc_years <- character(0)
}

# ---- UI ----
incidenceUI <- function(id) {
  ns <- NS(id)
  cdm_choices <- if (exists("incidence") && !is.null(incidence) && nrow(incidence) > 0) .inc_cdm else character(0)
  age_choices <- if (exists("incidence") && !is.null(incidence)) .inc_ageGroups else character(0)
  sex_choices <- if (exists("incidence") && !is.null(incidence)) .inc_sex else character(0)
  tagList(
    div(class = "tab-help-text", "Incidence estimates and confidence intervals."),
    fluidRow(
      h3("Incidence estimates"),
      p("Incidence estimates are shown below, please select configuration to filter them:"),
      fluidRow(
        column(3, pickerInput(ns("cdm"), "CDM name", choices = cdm_choices, selected = cdm_choices, multiple = TRUE, options = opt)),
        column(3, pickerInput(ns("outcome"), "Outcome", choices = .inc_outcomes, selected = .inc_outcomes, multiple = TRUE, options = opt)),
        column(3, pickerInput(ns("age_group"), "Age group", choices = age_choices, selected = "0 to 150", multiple = TRUE, options = opt)),
        column(3, pickerInput(ns("sex"), "Sex", choices = sex_choices, selected = sex_choices, multiple = TRUE, options = opt))
      ),
      fluidRow(
        column(3, pickerInput(ns("interval"), "Interval", choices = c("years", "overall"), selected = "years", multiple = FALSE, options = opt)),
        column(3, pickerInput(ns("years"), "Years", choices = .inc_years, selected = .inc_years, multiple = TRUE, options = opt))
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
            column(3, selectInput(ns("plot_ribbon"), "Ribbon", choices = c(FALSE, TRUE))),
            column(3, checkboxInput(ns("rotate_x_labels"), "Rotate X labels", value = TRUE))
          ),
          plotlyOutput(ns("incidence_plot"), height = "600px") %>% withSpinner(type = 6),
          h4("Download figure"),
          fluidRow(
            column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
            column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
            column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
          ),
          downloadButton(ns("download_plot"), "Download plot (PNG)")
        ),
        tabPanel(
          "Attrition",
          gt::gt_output(ns("attrition_table")) %>% withSpinner(type = 6),
          h4("Download attrition table"),
          downloadButton(ns("download_attrition_table"), "Download attrition table (.docx)")
        )
      )
    )
  )
}

# ---- Server ----
incidenceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filtered summarised_result
    filtered_data <- reactive({
      req(input$cdm, input$age_group, input$sex, input$interval)
      d <- incidence %>%
        dplyr::filter(cdm_name %in% input$cdm) |>
        visOmopResults::filterSettings(
          denominator_age_group %in% input$age_group,
          denominator_sex %in% input$sex
        ) %>%
        visOmopResults::filterGroup(outcome_cohort_name %in% input$outcome)

      if (input$interval == "years") {
        d <- d %>%
          visOmopResults::filterAdditional(analysis_interval %in% input$interval) %>%
          visOmopResults::filterAdditional(
            stringr::str_extract(incidence_start_date, "^[0-9]+") %in% input$years
          )
      } else {
        d <- d %>%
          visOmopResults::filterAdditional(analysis_interval %in% input$interval)
      }
      d
    })

    # Table of estimates (gt)
    table_gt <- reactive({
      d <- filtered_data()
      validate(need(nrow(d) > 0, "No results for selected filters"))
      tryCatch({
        IncidencePrevalence::tableIncidence(
          d,
          type = "gt",
          header = c("estimate_name"),
          groupColumn = c("cdm_name", "outcome_cohort_name"),
          settingsColumn = c("denominator_age_group", "denominator_sex"),
          hide = c("denominator_cohort_name")
        )
      }, error = function(e) {
        gt::gt(data.frame(Message = paste("Could not render table:", e$message)))
      })
    })

    output$report_table <- gt::render_gt({
      table_gt()
    })

    output$download_table <- downloadHandler(
      filename = function() { "incidenceEstimatesTable.docx" },
      content = function(file) {
        tryCatch({
          gt_tbl <- table_gt()
          gt::gtsave(gt_tbl, file)
        }, error = function(e) NULL)
      }
    )

    # Attrition table (gt)
    attrition_gt <- reactive({
      req(input$cdm, input$age_group, input$sex)
      d <- incidence %>%
        dplyr::filter(cdm_name %in% input$cdm) |>
        visOmopResults::filterSettings(
          denominator_age_group %in% input$age_group,
          denominator_sex %in% input$sex
        )
      tryCatch({
        IncidencePrevalence::tableIncidenceAttrition(
          d,
          type = "gt",
          header = c("variable_name"),
          groupColumn = c("cdm_name"),
          settingsColumn = c("denominator_age_group", "denominator_sex", "denominator_days_prior_observation"),
          hide = c("denominator_cohort_name", "estimate_name", "reason_id", "variable_level")
        )
      }, error = function(e) {
        gt::gt(data.frame(Message = paste("Could not render attrition:", e$message)))
      })
    })

    output$attrition_table <- gt::render_gt({
      attrition_gt()
    })

    output$download_attrition_table <- downloadHandler(
      filename = function() { "incidenceAttritionTable.docx" },
      content = function(file) {
        tryCatch({
          gt_tbl <- attrition_gt()
          gt::gtsave(gt_tbl, file)
        }, error = function(e) NULL)
      }
    )

    # Plot (ggplot2: asIncidenceResult + geom_ribbon/line/point, per 1,000 person-years)
    plot_incidence <- reactive({
      d <- filtered_data()
      validate(need(nrow(d) > 0, "No results for selected inputs"))

      facetVal <- if (input$plot_facet != "None") input$plot_facet else NULL
      colourVal <- if (input$plot_colour != "None") input$plot_colour else NULL

      tryCatch({
        td <- IncidencePrevalence::asIncidenceResult(d)
        td <- td %>%
          dplyr::mutate(
            incidence_100000_pys = suppressWarnings(as.numeric(.data$incidence_100000_pys)),
            incidence_100000_pys_95CI_lower = suppressWarnings(as.numeric(.data$incidence_100000_pys_95CI_lower)),
            incidence_100000_pys_95CI_upper = suppressWarnings(as.numeric(.data$incidence_100000_pys_95CI_upper))
          ) %>%
          dplyr::filter(!is.na(.data$incidence_100000_pys))
        if (nrow(td) == 0) return(NULL)

        x_var <- "incidence_start_date"
        y_var <- "incidence_100000_pys"
        ymin_var <- "incidence_100000_pys_95CI_lower"
        ymax_var <- "incidence_100000_pys_95CI_upper"

        aes_x <- ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
        if (!is.null(colourVal) && colourVal %in% colnames(td)) {
          aes_x <- ggplot2::aes(
            x = .data[[x_var]],
            y = .data[[y_var]],
            colour = .data[[colourVal]],
            group = .data[[colourVal]]
          )
        }

        p <- ggplot2::ggplot(td, aes_x) +
          ggplot2::ylim(0, NA)
        if (as.logical(input$plot_ribbon) && ymin_var %in% colnames(td) && ymax_var %in% colnames(td)) {
          rib_aes <- ggplot2::aes(ymin = .data[[ymin_var]], ymax = .data[[ymax_var]])
          if (!is.null(colourVal) && colourVal %in% colnames(td)) {
            rib_aes <- ggplot2::aes(ymin = .data[[ymin_var]], ymax = .data[[ymax_var]], fill = .data[[colourVal]])
          }
          p <- p + ggplot2::geom_ribbon(rib_aes, alpha = 0.2, linewidth = 0)
        }
        p <- p +
          ggplot2::geom_line(linewidth = 0.8) +
          ggplot2::geom_point(size = 2) +
          ggplot2::labs(x = "Incidence start date", y = "Incidence (100,000 person-years)")
        if (!is.null(facetVal) && facetVal %in% colnames(td)) {
          p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facetVal]]), scales = "free_y")
        }
        p <- p +
          ggplot2::scale_y_continuous(
            labels = function(x) round(x / 100, 1),
            name = "Incidence (per 1,000 person-years)"
          ) +
          ggplot2::ggtitle("Incidence estimates") +
          ggplot2::guides(fill = "none")
        if (isTRUE(input$rotate_x_labels)) {
          p <- p + ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5),
            axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 7),
            axis.text.y = ggplot2::element_text(size = 8)
          )
        } else {
          p <- p + ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5),
            axis.text.x = ggplot2::element_text(size = 7),
            axis.text.y = ggplot2::element_text(size = 8)
          )
        }
        p
      }, error = function(e) NULL)
    })

    output$incidence_plot <- plotly::renderPlotly({
      if (input$interval == "years") {
        p <- plot_incidence()
        if (is.null(p)) return(emptyPlotlyMessage("No results for selected inputs."))
        plotly::ggplotly(p)
      } else {
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "No plot available when interval = 'overall'", size = 6) +
          ggplot2::theme_void()
        plotly::ggplotly(p)
      }
    })

    output$download_plot <- downloadHandler(
      filename = function() { "incidenceEstimatesPlot.png" },
      content = function(file) {
        p <- plot_incidence()
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

# ---- Minimal app for testing the incidence module ----
# Usage (from package root or inst/shiny):
#   source("global.R"); runIncidenceModuleApp()
runIncidenceModuleApp <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Incidence module – test"),
      incidenceUI("incidence")
    ),
    server = function(input, output, session) {
      incidenceServer("incidence")
    }
  )
}

# runIncidenceModuleApp()
