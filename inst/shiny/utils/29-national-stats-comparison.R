# 29-national-stats-comparison.R
# National statistics comparison module - UI and Server
# Data preparation functions are in 00-national-stats-data.R

opt <- list(`live-search` = TRUE, `actions-box` = TRUE,
            `selected-text-format` = "count > 2",
            `count-selected-text` = "{0} selected")

# ---- UI ----
nationalStatsComparisonUI <- function(id) {
  ns <- NS(id)

  tagList(
    h3("National Statistics Comparison"),
    p("Compare algorithm results with reference values from national statistics as validation.",
      "Each reference value is matched with its counterpart from algorithm results."),

    # ---- Shared filters ----
    fluidRow(
      column(3, pickerInput(ns("sel_db"), "Data source / Country",
                            choices = character(0), selected = character(0),
                            multiple = TRUE, options = opt)),
      column(2, pickerInput(ns("sel_metric"), "Metric",
                            choices = character(0), selected = character(0),
                            multiple = TRUE, options = opt)),
      column(2, pickerInput(ns("sel_year"), "Year",
                            choices = character(0), selected = character(0),
                            multiple = TRUE, options = opt)),
      column(2, pickerInput(ns("sel_lb_def"), "Live birth definition",
                            choices = c("LB" = "LB", "LB+DELIV" = "LB+DELIV"),
                            selected = "LB", multiple = TRUE,
                            options = list(`actions-box` = TRUE))),
      column(3, uiOutput(ns("country_display")))
    ),
    fluidRow(
      column(3, checkboxInput(ns("show_num_denom"), "Show numerator/denominator", value = FALSE))
    ),

    # ---- Sub-tabs ----
    tabsetPanel(
      id = ns("tabs"),
      type = "tabs",

      # == Tab 0: Overview ==
      tabPanel(
        "Overview",
        br(),
        p("This module compares pregnancy algorithm results against published national",
          "statistics to validate that observed rates and distributions are plausible.",
          "Each metric from our database results is matched to a corresponding reference",
          "value from external sources."),
        h4("Comparisons and data sources"),
        tags$table(
          class = "table table-bordered table-striped",
          style = "margin-top: 10px;",
          tags$thead(
            tags$tr(
              tags$th("Metric", style = "width: 25%;"),
              tags$th("Source for reference data")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(tags$strong("Birth rate")),
              tags$td(
                tags$strong("EUROSTAT"), " (e.g., 2014, 2023):", tags$br(),
                "Crude birth rate: expressed as the ratio of the number of live births",
                " during the year to the average population in that year"
              )
            ),
            tags$tr(
              tags$td(tags$strong("Maternal age at birth")),
              tags$td(
                tags$strong("EUROSTAT"), " (e.g., 2014, 2023):", tags$br(),
                "Mean age of woman at birth of first child"
              )
            ),
            tags$tr(
              tags$td(tags$strong("Pregnancy outcome rates")),
              tags$td(
                tags$strong("EUROSTAT"), " (e.g., 2014, 2023):", tags$br(),
                "Number of live births per year", tags$br(), tags$br(),
                tags$strong("Euro-Peristat"), " (e.g., 2015, 2019):", tags$br(),
                "Foetal mortality rate: foetal deaths at or after 24-weeks threshold",
                tags$sup("*"), " (per 1000 total births)"
              )
            ),
            tags$tr(
              tags$td(tags$strong("Gestational duration distribution")),
              tags$td(
                tags$strong("Euro-Peristat"), " (e.g., 2015, 2019):", tags$br(),
                "Number of live births at each completed gestational age bin",
                " (<32, 32-36, 37-38, 39-41, and \u226542 weeks)"
              )
            ),
            tags$tr(
              tags$td(tags$strong("Mode of delivery")),
              tags$td(
                tags$strong("Euro-Peristat"), " (e.g., 2015, 2019):", tags$br(),
                "Mode of delivery rate (% vaginal and c-section deliveries)"
              )
            )
          )
        ),
        tags$p(
          style = "font-size: 0.9em; color: #666; margin-top: 8px;",
          tags$sup("*"), "For the purpose of this validation, we only consider",
          " foetal losses at or after 24 weeks."
        ),
        tags$div(
          style = "margin-top: 12px; padding: 10px 14px; background: #fff8e1; border-left: 4px solid #ffc107; border-radius: 4px; font-size: 0.9em;",
          tags$strong("Note: "),
          "Expected live birth counts are calculated as national total live births",
          " multiplied by the database's population coverage percentage.",
          " The following databases are hospital-based or patient cohort databases",
          " for which expected live birth counts are not estimable (shown as NA):",
          tags$ul(
            style = "margin-top: 4px; margin-bottom: 0;",
            lapply(.DB_SKIP_LB_COUNT, function(db) {
              country <- .resolve_db_country(db)
              label <- if (is.na(country)) db else paste0(db, " (", country, ")")
              tags$li(label)
            })
          )
        )
      ),

      # == Tab 1: Tables ==
      tabPanel(
        "Tables",
        br(),
        tabsetPanel(
          id = ns("table_tabs"),

          # -- Main comparison table (stacked, multi-DB) --
          tabPanel(
            "Comparison Table",
            br(),
            p("Stacked comparison table across selected databases.",
              "Includes birth rate, maternal age, live births, foetal mortality,",
              "gestational duration distribution, and delivery mode."),
            gt::gt_output(ns("main_table")) %>% withSpinner(type = 6),
            downloadButton(ns("download_main_table"), "Download table (.docx)")
          ),

          # -- Difference overview (tile view) --
          tabPanel(
            "Difference Overview",
            br(),
            p("Percentage difference for each indicator across databases.",
              "Color coding: ",
              tags$span("< 5%", style = "background-color:#d4edda; padding:2px 6px; border-radius:3px;"), " ",
              tags$span("5-10%", style = "background-color:#fff3cd; padding:2px 6px; border-radius:3px;"), " ",
              tags$span("10-50%", style = "background-color:#ffe0b2; padding:2px 6px; border-radius:3px;"), " ",
              tags$span("> 50%", style = "background-color:#f8d7da; padding:2px 6px; border-radius:3px;")),
            gt::gt_output(ns("diffov_table")) %>% withSpinner(type = 6),
            downloadButton(ns("download_diffov"), "Download table (.docx)")
          )
        )
      ),

      # == Tab 2: Plots ==
      tabPanel(
        "Plots",
        br(),
        p("All comparison plots. When multiple databases are selected, plots are faceted by database/country."),
        tabsetPanel(
          id = ns("plot_tabs"),

          tabPanel(
            "Gestational Duration",
            br(),
            uiOutput(ns("gest_plot_ui")),
            fluidRow(
              column(3, textInput(ns("gest_plot_height"), "Height (cm)", value = "14")),
              column(3, textInput(ns("gest_plot_width"), "Width (cm)", value = "26")),
              column(3, textInput(ns("gest_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            downloadButton(ns("download_gest_plot"), "Download plot (PNG)")
          ),
          tabPanel(
            "Delivery Mode",
            br(),
            uiOutput(ns("dm_plot_ui")),
            fluidRow(
              column(3, textInput(ns("dm_plot_height"), "Height (cm)", value = "14")),
              column(3, textInput(ns("dm_plot_width"), "Width (cm)", value = "26")),
              column(3, textInput(ns("dm_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            downloadButton(ns("download_dm_plot"), "Download plot (PNG)")
          ),
          tabPanel(
            "Live Births",
            br(),
            uiOutput(ns("lb_plot_ui")),
            fluidRow(
              column(3, textInput(ns("lb_plot_height"), "Height (cm)", value = "14")),
              column(3, textInput(ns("lb_plot_width"), "Width (cm)", value = "26")),
              column(3, textInput(ns("lb_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            downloadButton(ns("download_lb_plot"), "Download plot (PNG)")
          ),
          tabPanel(
            "Maternal Age",
            br(),
            uiOutput(ns("ma_plot_ui")),
            fluidRow(
              column(3, textInput(ns("ma_plot_height"), "Height (cm)", value = "14")),
              column(3, textInput(ns("ma_plot_width"), "Width (cm)", value = "26")),
              column(3, textInput(ns("ma_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            downloadButton(ns("download_ma_plot"), "Download plot (PNG)")
          ),
          tabPanel(
            "Foetal Mortality",
            br(),
            uiOutput(ns("fm_plot_ui")),
            fluidRow(
              column(3, textInput(ns("fm_plot_height"), "Height (cm)", value = "14")),
              column(3, textInput(ns("fm_plot_width"), "Width (cm)", value = "26")),
              column(3, textInput(ns("fm_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            downloadButton(ns("download_fm_plot"), "Download plot (PNG)")
          )
        )
      ),

      # == Tab 3: Raw Data ==
      tabPanel(
        "Raw Data",
        br(),
        DT::DTOutput(ns("raw_table")) %>% withSpinner(type = 6),
        downloadButton(ns("download_raw"), "Download raw data (.csv)")
      )
    )
  )
}


# Compute plot height in pixels based on number of facets (300px per row of facets, ncol=3)
.facet_plot_height <- function(n_facets, px_per_row = 350, ncol = 3, min_px = 450) {
  n_rows <- ceiling(n_facets / ncol)
  max(min_px, n_rows * px_per_row)
}

# ---- Server ----
nationalStatsComparisonServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: load national stats once
    natl_data <- reactive({ .load_national_stats() })

    # ---- Initialize picker inputs ----
    observe({
      natl <- natl_data()
      if (is.null(natl)) return()
      mapped_dbs <- .get_mapped_databases(rv$allDP)

      if (length(mapped_dbs) > 0) {
        # Build display labels: "db_name (Country)"
        labels <- vapply(mapped_dbs, function(db) {
          country <- .resolve_db_country(db)
          if (is.na(country)) db else paste0(db, " (", country, ")")
        }, character(1))
        choices <- stats::setNames(mapped_dbs, labels)
        updatePickerInput(session, "sel_db",
                          choices = choices,
                          selected = mapped_dbs)
      }
    })

    # Reactive: derive lb_categories from user selection
    lb_categories <- reactive({
      sel <- input$sel_lb_def
      if (is.null(sel) || length(sel) == 0) return("LB")
      if (identical(sel, "LB")) "LB" else c("LB", "DELIV")
    })

    # Reactive: are both LB definitions selected?
    both_lb_defs <- reactive({
      sel <- input$sel_lb_def
      !is.null(sel) && length(sel) == 2
    })

    # Reactive: compute comparison across all mapped DBs
    all_db_data <- reactive({
      natl <- natl_data()
      if (is.null(natl)) return(tibble::tibble())

      if (both_lb_defs()) {
        # Run separately for each definition and stack with a label column
        d_lb <- .build_all_db_comparison(natl, lb_categories = "LB") %>%
          dplyr::mutate(live_birth_definition = "LB")
        d_lb_deliv <- .build_all_db_comparison(natl, lb_categories = c("LB", "DELIV")) %>%
          dplyr::mutate(live_birth_definition = "LB+DELIV")
        dplyr::bind_rows(d_lb, d_lb_deliv)
      } else {
        .build_all_db_comparison(natl, lb_categories = lb_categories())
      }
    })

    # Update metric picker when data is available
    observe({
      d <- all_db_data()
      if (is.null(d) || nrow(d) == 0) return()
      d <- d %>%
        dplyr::mutate(
          metric_label = dplyr::if_else(
            is.na(.data$level) | !nzchar(.data$level),
            paste0(.data$indicator, " | ", .data$variable),
            paste0(.data$indicator, " | ", .data$variable, " [", .data$level, "]")
          )
        )
      metrics <- sort(unique(d$metric_label))
      updatePickerInput(session, "sel_metric",
                        choices = metrics, selected = metrics)

      years <- sort(unique(na.omit(d$year)))
      year_choices <- as.character(years)
      updatePickerInput(session, "sel_year",
                        choices = year_choices, selected = year_choices)
    })

    # Show mapped countries for selected DBs
    output$country_display <- renderUI({
      dbs <- input$sel_db
      if (is.null(dbs) || length(dbs) == 0) return(NULL)
      countries <- unique(na.omit(.resolve_db_country(dbs)))
      if (length(countries) == 0) {
        tags$div(class = "alert alert-warning", "No country mappings found.")
      } else {
        tags$div(
          tags$strong("Reference countries: "),
          tags$span(paste(countries, collapse = ", "),
                    style = "font-size: 1.1em; color: #337ab7;")
        )
      }
    })

    # ---- Shared filtered data ----
    filtered_data <- reactive({
      d <- all_db_data()
      if (is.null(d) || nrow(d) == 0) return(tibble::tibble())

      sel_dbs <- input$sel_db
      sel_metrics <- input$sel_metric
      sel_years <- input$sel_year
      if (is.null(sel_dbs) || length(sel_dbs) == 0) return(tibble::tibble())
      if (is.null(sel_metrics) || length(sel_metrics) == 0) return(tibble::tibble())

      d %>%
        dplyr::filter(.data$cdm_name %in% sel_dbs) %>%
        dplyr::filter(is.na(.data$year) | as.character(.data$year) %in% sel_years) %>%
        dplyr::mutate(
          metric_label = dplyr::if_else(
            is.na(.data$level) | !nzchar(.data$level),
            paste0(.data$indicator, " | ", .data$variable),
            paste0(.data$indicator, " | ", .data$variable, " [", .data$level, "]")
          )
        ) %>%
        dplyr::filter(.data$metric_label %in% sel_metrics)
    })


    # ======================================================================
    # TAB 1a: Main Comparison Table
    # ======================================================================
    main_gt <- reactive({
      d <- filtered_data()
      validate(need(nrow(d) > 0, "No comparison data available. Check database and metric selections."))

      # Symbols for missing data reasons (used in footnotes)
      # \u2020 = dagger, \u2021 = double dagger
      sym_no_ref  <- "\u2020"   # no reference data for this year/metric
      sym_no_algo <- "\u2021"   # no algorithm data for this year/metric

      d_display <- d %>%
        dplyr::mutate(
          external_fmt = dplyr::case_when(
            is.na(.data$external_value) & !is.na(.data$internal_value) ~ sym_no_ref,
            is.na(.data$external_value) ~ sym_no_ref,
            TRUE ~ format(.data$external_value, big.mark = ",", scientific = FALSE, trim = TRUE)
          ),
          internal_fmt = dplyr::case_when(
            is.na(.data$internal_value) & !is.na(.data$external_value) ~ sym_no_algo,
            is.na(.data$internal_value) ~ sym_no_algo,
            TRUE ~ format(.data$internal_value, big.mark = ",", scientific = FALSE, trim = TRUE)
          ),
          numerator_fmt = dplyr::case_when(
            !is.na(.data$numerator_breakdown) ~ .data$numerator_breakdown,
            is.na(.data$internal_numerator) ~ "-",
            TRUE ~ format(.data$internal_numerator, big.mark = ",", scientific = FALSE, trim = TRUE)
          ),
          denominator_fmt = dplyr::if_else(
            is.na(.data$internal_denominator), "-",
            format(.data$internal_denominator, big.mark = ",", scientific = FALSE, trim = TRUE)
          ),
          difference = dplyr::case_when(
            is.na(.data$external_value) | is.na(.data$internal_value) ~ "-",
            .data$external_value != 0 ~ paste0(
              format(round(.data$diff_abs, 1), big.mark = ",", trim = TRUE),
              " (", ifelse(.data$diff_abs >= 0, "+", ""),
              round(.data$diff_pct, 1), "%)"
            ),
            TRUE ~ format(round(.data$diff_abs, 1), big.mark = ",", trim = TRUE)
          ),
          diff_pct_val = .data$diff_pct,
          year = dplyr::if_else(is.na(.data$year), "-", as.character(.data$year)),
          expected_coverage = vapply(.data$cdm_name, function(db) {
            cov <- .resolve_db_coverage(db)
            if (is.na(cov)) "-" else paste0(cov, "%")
          }, character(1))
        ) %>%
        dplyr::select(
          "cdm_name", "country", "expected_coverage",
          dplyr::any_of("live_birth_definition"),
          "indicator", "variable", "level", "year",
          "external_fmt", "internal_fmt", "numerator_fmt", "denominator_fmt",
          "difference", "diff_pct_val"
        ) %>%
        dplyr::mutate(
          .level_order = factor(
            dplyr::if_else(is.na(.data$level) | !nzchar(.data$level), "(none)", .data$level),
            levels = c("<32", "32-36", "37-38", "39-41", "\u226542",
                       "Vaginal", "C-section", "(none)")
          )
        ) %>%
        dplyr::arrange(.data$cdm_name, .data$variable, .data$.level_order, .data$year) %>%
        dplyr::select(-".level_order")

      show_lb_col <- both_lb_defs() && "live_birth_definition" %in% names(d_display)

      show_num_denom <- isTRUE(input$show_num_denom)

      tbl <- gt::gt(d_display) %>%
        gt::tab_header(
          title = "National Statistics Comparison",
          subtitle = "Stacked comparison across selected databases"
        )

      # Build column labels
      col_labels <- list(
        cdm_name = "Database",
        country = "Country",
        expected_coverage = "Expected % Pop. Covered",
        indicator = "Indicator",
        variable = "Variable",
        level = "Level",
        year = "Year",
        external_fmt = "Reference",
        internal_fmt = "Algorithm",
        numerator_fmt = paste0("Numerator (", paste(lb_categories(), collapse = " + "), ")"),
        denominator_fmt = "Denominator",
        difference = "Difference"
      )
      if (show_lb_col) {
        col_labels[["live_birth_definition"]] <- "Live Birth Definition"
      }
      hide_cols <- c("diff_pct_val", "expected_coverage", "indicator")
      if (!show_num_denom) {
        hide_cols <- c(hide_cols, "numerator_fmt", "denominator_fmt")
      }
      tbl <- tbl %>%
        gt::cols_label(.list = col_labels) %>%
        gt::cols_hide(columns = hide_cols) %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#f0f7ff"),
          locations = gt::cells_body(columns = "external_fmt")
        ) %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#fff0f0"),
          locations = gt::cells_body(columns = "internal_fmt")
        ) %>%
        gt::sub_missing(missing_text = "-") %>%
        gt::tab_options(table.font.size = "small") %>%
        gt::cols_width(
          "variable" ~ gt::px(220),
          "cdm_name" ~ gt::px(120)
        ) %>%
        gt::tab_footnote("308 days (44 weeks) is counted as 43 weeks in algorithm results.") %>%
        gt::tab_footnote("Expected live birth counts are calculated as national total multiplied by database population coverage %.") %>%
        gt::tab_footnote(paste0(sym_no_ref, " No reference data available for this year/metric.")) %>%
        gt::tab_footnote(paste0(sym_no_algo, " No algorithm data available for this year/metric (database may not cover this period)."))

      for (i in seq_len(nrow(d_display))) {
        bg <- .diff_color(d_display$diff_pct_val[i])
        tbl <- tbl %>%
          gt::tab_style(
            style = gt::cell_fill(color = bg),
            locations = gt::cells_body(columns = "difference", rows = i)
          )
      }

      tbl
    })

    output$main_table <- gt::render_gt({ main_gt() })

    output$download_main_table <- downloadHandler(
      filename = function() "national_stats_comparison.docx",
      content = function(file) {
        tryCatch(gt::gtsave(main_gt(), file), error = function(e) NULL)
      }
    )


    # ======================================================================
    # TAB 1b: Difference Overview
    # ======================================================================
    diffov_gt <- reactive({
      d <- filtered_data()
      validate(need(nrow(d) > 0, "No data available."))

      d_pivot <- d %>%
        dplyr::mutate(
          row_label = dplyr::case_when(
            is.na(.data$level) | !nzchar(.data$level) ~
              paste0(.data$variable, " (", dplyr::if_else(is.na(.data$year), "-", as.character(.data$year)), ")"),
            TRUE ~
              paste0(.data$variable, " [", .data$level, "] (", dplyr::if_else(is.na(.data$year), "-", as.character(.data$year)), ")")
          ),
          diff_label = dplyr::case_when(
            is.na(.data$diff_pct) ~ "-",
            TRUE ~ paste0(ifelse(.data$diff_pct >= 0, "+", ""), round(.data$diff_pct, 1), "%")
          )
        )

      wide_label <- d_pivot %>%
        dplyr::select("indicator", "row_label", "cdm_name", "diff_label") %>%
        tidyr::pivot_wider(names_from = "cdm_name", values_from = "diff_label") %>%
        dplyr::arrange(.data$indicator, .data$row_label)

      wide_pct <- d_pivot %>%
        dplyr::select("indicator", "row_label", "cdm_name", "diff_pct") %>%
        tidyr::pivot_wider(names_from = "cdm_name", values_from = "diff_pct",
                           names_prefix = "pct_") %>%
        dplyr::arrange(.data$indicator, .data$row_label)

      db_cols <- setdiff(colnames(wide_label), c("indicator", "row_label"))
      pct_cols <- paste0("pct_", db_cols)

      display <- wide_label %>%
        dplyr::left_join(wide_pct, by = c("indicator", "row_label"))

      tbl <- gt::gt(display, groupname_col = "indicator") %>%
        gt::tab_header(
          title = "Difference Overview Across Databases",
          subtitle = "Percentage difference (Algorithm vs Reference) per indicator and database"
        ) %>%
        gt::cols_label(row_label = "Metric") %>%
        gt::cols_hide(dplyr::all_of(pct_cols)) %>%
        gt::sub_missing(missing_text = "-") %>%
        gt::tab_options(
          table.font.size = "small",
          row_group.font.weight = "bold"
        ) %>%
        gt::cols_width("row_label" ~ gt::px(300))

      for (db in db_cols) {
        pct_col <- paste0("pct_", db)
        if (pct_col %in% colnames(display)) {
          for (i in seq_len(nrow(display))) {
            pct_val <- display[[pct_col]][i]
            bg <- .diff_color(pct_val)
            tbl <- tbl %>%
              gt::tab_style(
                style = gt::cell_fill(color = bg),
                locations = gt::cells_body(columns = db, rows = i)
              )
          }
        }
      }

      tbl
    })

    output$diffov_table <- gt::render_gt({ diffov_gt() })

    output$download_diffov <- downloadHandler(
      filename = function() "difference_overview_all_databases.docx",
      content = function(file) {
        tryCatch(gt::gtsave(diffov_gt(), file), error = function(e) NULL)
      }
    )


    # ======================================================================
    # TAB 2: Plots (all faceted by database/country when multiple selected)
    # ======================================================================

    # Helper: selected DB-country pairs
    selected_pairs <- reactive({
      dbs <- req(input$sel_db)
      purrr::map_dfr(dbs, function(db) {
        country <- .resolve_db_country(db)
        if (is.na(country)) return(tibble::tibble())
        tibble::tibble(db = db, country = country)
      })
    })

    # ---- Gestational Duration Plot ----
    gest_plot_data <- reactive({
      pairs <- selected_pairs()
      natl <- natl_data()
      if (is.null(natl) || nrow(pairs) == 0) return(tibble::tibble())

      purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
        d <- .build_gest_pct_comparison(natl, pairs$db[i], pairs$country[i], lb_categories = lb_categories())
        if (nrow(d) > 0) d$facet_label <- paste0(pairs$db[i], " (", pairs$country[i], ")")
        d
      })
    })

    gest_ggplot <- reactive({
      d <- gest_plot_data()
      validate(need(nrow(d) > 0, "No gestational duration data to plot."))

      d <- d %>%
        dplyr::mutate(bin = factor(.data$bin, levels = c("<32", "32-36", "37-41", ">=42")))

      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$label, y = .data$pct, fill = .data$bin)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(x = NULL, y = "Percentage (%)", fill = "Gestational\nAge (weeks)",
                      title = "Gestational Duration: Algorithms vs. Reference",
                      caption = "Note: 308 days (44 weeks) is counted as 43 weeks in algorithm results.") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_brewer(palette = "Set2")

      if (length(unique(d$facet_label)) > 1) {
        p <- p + ggplot2::facet_wrap(~ facet_label, scales = "free_x")
      }
      p
    })

    output$gest_plot_ui <- renderUI({
      d <- gest_plot_data()
      n <- length(unique(d$facet_label))
      h <- .facet_plot_height(n)
      plotlyOutput(ns("gest_plot"), height = paste0(h, "px")) %>% withSpinner(type = 6)
    })

    output$gest_plot <- plotly::renderPlotly({
      p <- gest_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_gest_plot <- downloadHandler(
      filename = function() "gestational_duration_comparison.png",
      content = function(file) {
        p <- gest_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p,
                          width = as.numeric(input$gest_plot_width),
                          height = as.numeric(input$gest_plot_height),
                          dpi = as.numeric(input$gest_plot_dpi), units = "cm")
        }
      }
    )

    # ---- Delivery Mode Plot ----
    dm_plot_data <- reactive({
      pairs <- selected_pairs()
      natl <- natl_data()
      if (is.null(natl) || nrow(pairs) == 0) return(tibble::tibble())

      purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
        d <- .build_dm_pct_comparison(natl, pairs$db[i], pairs$country[i], lb_categories = lb_categories())
        if (nrow(d) > 0) d$facet_label <- paste0(pairs$db[i], " (", pairs$country[i], ")")
        d
      })
    })

    dm_ggplot <- reactive({
      d <- dm_plot_data()
      validate(need(nrow(d) > 0, "No delivery mode data to plot."))

      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$label, y = .data$pct, fill = .data$mode)) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::labs(x = NULL, y = "Percentage (%)", fill = "Mode",
                      title = "Delivery Mode: Algorithms vs. Reference") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_manual(values = c("Vaginal" = "#4DAF4A", "Cesarean" = "#E41A1C"))

      if (length(unique(d$facet_label)) > 1) {
        p <- p + ggplot2::facet_wrap(~ facet_label, scales = "free_x")
      }
      p
    })

    output$dm_plot_ui <- renderUI({
      d <- dm_plot_data()
      n <- length(unique(d$facet_label))
      h <- .facet_plot_height(n)
      plotlyOutput(ns("dm_plot"), height = paste0(h, "px")) %>% withSpinner(type = 6)
    })

    output$dm_plot <- plotly::renderPlotly({
      p <- dm_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_dm_plot <- downloadHandler(
      filename = function() "delivery_mode_comparison.png",
      content = function(file) {
        p <- dm_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p,
                          width = as.numeric(input$dm_plot_width),
                          height = as.numeric(input$dm_plot_height),
                          dpi = as.numeric(input$dm_plot_dpi), units = "cm")
        }
      }
    )

    # ---- Live Births Plot ----
    lb_plot_data <- reactive({
      pairs <- selected_pairs()
      natl <- natl_data()
      if (is.null(natl) || nrow(pairs) == 0) return(tibble::tibble())

      purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
        pair <- list(db = pairs$db[i], country = pairs$country[i])

        coverage_pct <- .resolve_db_coverage(pair$db)

        natl_lb <- tibble::tibble()
        if (!is.na(coverage_pct)) {
          natl_lb <- natl %>%
            dplyr::filter(
              grepl("Number of live births", .data$variable),
              .data$country == pair$country,
              !is.na(.data$value)
            ) %>%
            dplyr::transmute(
              source_label = paste0(pair$country, " (Expected, ", coverage_pct, "%)"),
              year = .data$year,
              count = round(.data$value * coverage_pct / 100),
              source = "Expected",
              facet_label = paste0(pair$db, " (", pair$country, ")")
            )
        }

        int_lb <- tibble::tibble()
        if (exists("incidence")) {
          include_deliv <- "DELIV" %in% lb_categories()
          lb_counts <- .extract_lb_counts_from_incidence(incidence, pair$db, include_deliv = include_deliv)
          if (nrow(lb_counts) > 0) {
            int_lb <- lb_counts %>%
              dplyr::filter(.data$year %in% natl_lb$year) %>%
              dplyr::transmute(
                source_label = paste0(pair$db, " (Algorithm)"),
                year = .data$year,
                count = .data$numerator,
                source = "Algorithm",
                facet_label = paste0(pair$db, " (", pair$country, ")")
              )
          }
        }

        dplyr::bind_rows(natl_lb, int_lb)
      })
    })

    lb_ggplot <- reactive({
      d <- lb_plot_data()
      validate(need(nrow(d) > 0, "No live births data."))

      p <- ggplot2::ggplot(d, ggplot2::aes(x = as.factor(.data$year), y = .data$count,
                                             fill = .data$source)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(x = "Year", y = "Count", fill = "Source",
                      title = "Live Births: Algorithms vs. Reference",
                      caption = "Expected = national total * database population coverage %") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       plot.title = ggplot2::element_text(hjust = 0.5, size = 11)) +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_fill_manual(values = c("Expected" = "#377EB8", "Algorithm" = "#E41A1C"))

      if (length(unique(d$facet_label)) > 1) {
        p <- p + ggplot2::facet_wrap(~ facet_label, scales = "free_y")
      }
      p
    })

    output$lb_plot_ui <- renderUI({
      d <- lb_plot_data()
      n <- length(unique(d$facet_label))
      h <- .facet_plot_height(n)
      plotlyOutput(ns("lb_plot"), height = paste0(h, "px")) %>% withSpinner(type = 6)
    })

    output$lb_plot <- plotly::renderPlotly({
      p <- lb_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_lb_plot <- downloadHandler(
      filename = function() "live_births_comparison.png",
      content = function(file) {
        p <- lb_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p,
                          width = as.numeric(input$lb_plot_width),
                          height = as.numeric(input$lb_plot_height),
                          dpi = as.numeric(input$lb_plot_dpi), units = "cm")
        }
      }
    )

    # ---- Maternal Age Plot ----
    ma_plot_data <- reactive({
      pairs <- selected_pairs()
      natl <- natl_data()
      if (is.null(natl) || nrow(pairs) == 0) return(tibble::tibble())

      purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
        pair <- list(db = pairs$db[i], country = pairs$country[i])

        natl_ma <- natl %>%
          dplyr::filter(
            grepl("Mean age", .data$variable),
            .data$country == pair$country,
            !is.na(.data$value)
          ) %>%
          dplyr::transmute(
            source_label = paste0(pair$country, " (Reference)"),
            year = .data$year,
            value = .data$value,
            source = "Reference",
            facet_label = paste0(pair$db, " (", pair$country, ")")
          )

        int_ma <- tibble::tibble()
        if (exists("ageSummaryFirstPregnancy")) {
          afp <- ageSummaryFirstPregnancy
          if (!is.null(afp) && nrow(afp) > 0 && "year" %in% colnames(afp)) {
            int_ma <- afp %>%
              dplyr::filter(
                tolower(.data$cdm_name) == tolower(pair$db) |
                  grepl(paste0("^", tolower(sub("_v[0-9]+$", "", pair$db))),
                        tolower(.data$cdm_name)),
                .data$final_outcome_category == "overall",
                .data$year != "overall"
              ) %>%
              dplyr::mutate(
                year = suppressWarnings(as.integer(.data$year)),
                value = round(suppressWarnings(as.numeric(.data$mean)), 1)
              ) %>%
              dplyr::filter(!is.na(.data$year),
                            .data$year %in% natl_ma$year) %>%
              dplyr::transmute(
                source_label = paste0(pair$db, " (Algorithm)"),
                year = .data$year,
                value = .data$value,
                source = "Algorithm",
                facet_label = paste0(pair$db, " (", pair$country, ")")
              )
          }
        }

        dplyr::bind_rows(natl_ma, int_ma)
      })
    })

    ma_ggplot <- reactive({
      d <- ma_plot_data()
      validate(need(nrow(d) > 0, "No maternal age data."))

      p <- ggplot2::ggplot(d, ggplot2::aes(x = as.factor(.data$year), y = .data$value,
                                             fill = .data$source)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::geom_text(ggplot2::aes(label = .data$value),
                           position = ggplot2::position_dodge(width = 0.9),
                           vjust = -0.5, size = 3.5) +
        ggplot2::labs(x = "Year", y = "Mean age (years)", fill = "Source",
                      title = "Mean Maternal Age at First Child",
                      caption = paste0("Reference: Mean age at birth of first child\n",
                                       "Algorithm result: Mean age at pregnancy start (first pregnancy)")) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       plot.title = ggplot2::element_text(hjust = 0.5, size = 11)) +
        ggplot2::scale_fill_manual(values = c("Reference" = "#377EB8", "Algorithm" = "#E41A1C")) +
        ggplot2::coord_cartesian(ylim = c(25, 35))

      if (length(unique(d$facet_label)) > 1) {
        p <- p + ggplot2::facet_wrap(~ facet_label, scales = "free_x")
      }
      p
    })

    output$ma_plot_ui <- renderUI({
      d <- ma_plot_data()
      n <- length(unique(d$facet_label))
      h <- .facet_plot_height(n)
      plotlyOutput(ns("ma_plot"), height = paste0(h, "px")) %>% withSpinner(type = 6)
    })

    output$ma_plot <- plotly::renderPlotly({
      p <- ma_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_ma_plot <- downloadHandler(
      filename = function() "maternal_age_comparison.png",
      content = function(file) {
        p <- ma_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p,
                          width = as.numeric(input$ma_plot_width),
                          height = as.numeric(input$ma_plot_height),
                          dpi = as.numeric(input$ma_plot_dpi), units = "cm")
        }
      }
    )

    # ---- Foetal Mortality Plot ----
    fm_plot_data <- reactive({
      pairs <- selected_pairs()
      natl <- natl_data()
      if (is.null(natl) || nrow(pairs) == 0) return(tibble::tibble())

      purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
        pair <- list(db = pairs$db[i], country = pairs$country[i])

        natl_fm <- natl %>%
          dplyr::filter(
            grepl("Foetal mortality", .data$variable),
            .data$country == pair$country,
            !is.na(.data$value)
          ) %>%
          dplyr::transmute(
            source_label = paste0(pair$country, " (Reference, ", .data$year, ")"),
            year = .data$year,
            value = .data$value,
            source = "Reference",
            facet_label = paste0(pair$db, " (", pair$country, ")")
          )

        int_fm <- tibble::tibble()
        include_deliv <- "DELIV" %in% lb_categories()
        if (exists("prevalence", envir = .GlobalEnv)) {
          prev <- get("prevalence", envir = .GlobalEnv)
          fm_sb <- .extract_counts_from_prevalence_by_cohort(prev, pair$db, "^hipps_sb$")
          fm_denom <- .extract_counts_from_prevalence(prev, pair$db, include_deliv = include_deliv)
          if (nrow(fm_sb) > 0 && nrow(fm_denom) > 0) {
            fm_joined <- fm_sb %>%
              dplyr::select("year", sb_count = "numerator") %>%
              dplyr::inner_join(
                fm_denom %>% dplyr::select("year", lb_count = "numerator"),
                by = "year"
              ) %>%
              dplyr::mutate(total = .data$sb_count + .data$lb_count) %>%
              dplyr::filter(.data$total > 0) %>%
              dplyr::mutate(value = round(1000 * .data$sb_count / .data$total, 1))
            if (nrow(fm_joined) > 0) {
              int_fm <- fm_joined %>%
                dplyr::transmute(
                  source_label = paste0(pair$db, " (Algorithm, ", .data$year, ")"),
                  year = .data$year,
                  value = .data$value,
                  source = "Algorithm",
                  facet_label = paste0(pair$db, " (", pair$country, ")")
                )
            }
          }
        }

        dplyr::bind_rows(natl_fm, int_fm)
      })
    })

    fm_ggplot <- reactive({
      d <- fm_plot_data()
      validate(need(nrow(d) > 0, "No foetal mortality data."))

      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$source_label, y = .data$value,
                                             fill = .data$source)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_text(ggplot2::aes(label = .data$value),
                           vjust = -0.5, size = 3.5) +
        ggplot2::labs(x = NULL, y = "Rate (per 1,000 total births)", fill = "Source",
                      title = "Foetal Mortality Rate",
                      caption = "Algorithm result rate is computed as SB/(SB+LB)*1000 across all years") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       plot.title = ggplot2::element_text(hjust = 0.5, size = 11)) +
        ggplot2::scale_fill_manual(values = c("Reference" = "#377EB8", "Algorithm" = "#E41A1C"))

      if (length(unique(d$facet_label)) > 1) {
        p <- p + ggplot2::facet_wrap(~ facet_label, scales = "free_x")
      }
      p
    })

    output$fm_plot_ui <- renderUI({
      d <- fm_plot_data()
      n <- length(unique(d$facet_label))
      h <- .facet_plot_height(n)
      plotlyOutput(ns("fm_plot"), height = paste0(h, "px")) %>% withSpinner(type = 6)
    })

    output$fm_plot <- plotly::renderPlotly({
      p <- fm_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_fm_plot <- downloadHandler(
      filename = function() "foetal_mortality_comparison.png",
      content = function(file) {
        p <- fm_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p,
                          width = as.numeric(input$fm_plot_width),
                          height = as.numeric(input$fm_plot_height),
                          dpi = as.numeric(input$fm_plot_dpi), units = "cm")
        }
      }
    )


    # ======================================================================
    # TAB 3: Raw Data
    # ======================================================================
    output$raw_table <- DT::renderDT({
      natl <- natl_data()
      validate(need(!is.null(natl) && nrow(natl) > 0, "No national statistics data available."))

      # Filter to selected countries
      sel_dbs <- input$sel_db
      if (!is.null(sel_dbs) && length(sel_dbs) > 0) {
        countries <- unique(na.omit(.resolve_db_country(sel_dbs)))
        if (length(countries) > 0) {
          natl <- natl %>% dplyr::filter(.data$country %in% countries)
        }
      }

      DT::datatable(
        natl %>% dplyr::mutate(value = round(.data$value, 2)),
        filter = "top",
        options = list(scrollX = TRUE, pageLength = 25),
        colnames = c("Indicator", "Variable", "Level", "Year", "Country", "Value")
      )
    })

    output$download_raw <- downloadHandler(
      filename = function() "national_statistics_raw.csv",
      content = function(file) {
        natl <- natl_data()
        if (!is.null(natl) && nrow(natl) > 0) readr::write_csv(natl, file)
      }
    )

  })
}
