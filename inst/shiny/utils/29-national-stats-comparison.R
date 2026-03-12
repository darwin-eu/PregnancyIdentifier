# 29-national-stats-comparison.R - National statistics comparison module
# Compares PregnancyIdentifier results with published national statistics
# (birth rates, gestational duration, delivery mode, maternal age, live births).

# ---- Load and parse national statistics at source time ----
.load_national_stats <- function() {
  csvPath <- file.path(getwd(), "National_Statistics_Obj2_v2.csv")
  if (!file.exists(csvPath)) {
    csvPath <- system.file("shiny", "National_Statistics_Obj2_v2.csv",
                           package = "PregnancyIdentifier")
  }
  if (!file.exists(csvPath) || !nzchar(csvPath)) return(NULL)

  raw <- readr::read_csv(csvPath, col_types = readr::cols(.default = "c"),
                          show_col_types = FALSE)
  colnames(raw) <- trimws(colnames(raw))
  country_cols <- setdiff(colnames(raw), c("Indicators", "variable", "level", "Year"))

  raw %>%
    tidyr::pivot_longer(cols = dplyr::all_of(country_cols),
                         names_to = "country", values_to = "value_raw") %>%
    dplyr::rename(indicator = "Indicators", year = "Year") %>%
    dplyr::mutate(
      year = suppressWarnings(as.integer(.data$year)),
      value = .parse_eu_number(.data$value_raw)
    ) %>%
    dplyr::select("indicator", "variable", "level", "year", "country", "value")
}

.parse_eu_number <- function(x) {
  vapply(x, function(val) {
    if (is.na(val) || !nzchar(trimws(val))) return(NA_real_)
    val <- trimws(val)
    val <- gsub(",", ".", val)
    dots <- gregexpr("\\.", val)[[1]]
    n_dots <- sum(dots > 0)
    if (n_dots == 1L) {
      parts <- strsplit(val, "\\.")[[1]]
      if (nchar(parts[2]) == 3L && grepl("^[0-9]+$", parts[2]))
        val <- gsub("\\.", "", val)
    } else if (n_dots > 1L) {
      val <- gsub("\\.", "", val)
    }
    suppressWarnings(as.numeric(val))
  }, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
}

# ---- Helper: map app gestational bins to national stats bins ----
.map_gest_bins <- function(gestationalWeeksBinned, natl) {
  natl_gest <- natl %>%
    dplyr::filter(.data$indicator == "Gestational duration distribution",
                  !is.na(.data$value)) %>%
    dplyr::select("level", "year", "country", "value") %>%
    dplyr::rename(bin = "level")

  if (nrow(natl_gest) == 0) return(list(natl = tibble::tibble(), app = tibble::tibble()))

  # Merge national 37-38 + 39-41 into 37-41 so we can compare with app
  natl_gest <- natl_gest %>%
    dplyr::mutate(bin = dplyr::case_when(
      .data$bin %in% c("37-38", "39-41") ~ "37-41",
      .data$bin == "\u226542"             ~ ">=42",
      TRUE                               ~ .data$bin
    )) %>%
    dplyr::group_by(.data$country, .data$year, .data$bin) %>%
    dplyr::summarise(n = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(.data$country, .data$year) %>%
    dplyr::mutate(pct = round(100 * .data$n / sum(.data$n, na.rm = TRUE), 1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(source = "National Statistics")

  if (is.null(gestationalWeeksBinned) || nrow(gestationalWeeksBinned) == 0) {
    return(list(natl = natl_gest, app = tibble::tibble()))
  }

  app_bin_map <- dplyr::tribble(
    ~app_bin,  ~natl_bin,
    "<12",     "<32",
    "12-27",   "<32",
    "28-31",   "<32",
    "32-36",   "32-36",
    "37-41",   "37-41",
    "42-43",   ">=42",
    "44-49",   ">=42",
    ">=50",    ">=42"
  )

  app_data <- gestationalWeeksBinned %>%
    dplyr::mutate(gestational_weeks = as.character(.data$gestational_weeks)) %>%
    dplyr::left_join(app_bin_map, by = c("gestational_weeks" = "app_bin"))

  if ("final_outcome_category" %in% colnames(app_data)) {
    app_data <- app_data %>%
      dplyr::group_by(.data$cdm_name, .data$natl_bin) %>%
      dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop")
  } else {
    app_data <- app_data %>%
      dplyr::group_by(.data$cdm_name, .data$natl_bin) %>%
      dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop")
  }

  app_data <- app_data %>%
    dplyr::group_by(.data$cdm_name) %>%
    dplyr::mutate(pct = round(100 * .data$n / sum(.data$n, na.rm = TRUE), 1)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(bin = "natl_bin", country = "cdm_name") %>%
    dplyr::mutate(source = "Database", year = NA_integer_)

  list(natl = natl_gest, app = app_data)
}

# ---- Helper: map delivery mode ----
.map_delivery_mode <- function(deliveryModeSummary, natl) {
  natl_dm <- natl %>%
    dplyr::filter(.data$indicator == "Mode of delivery", !is.na(.data$value)) %>%
    dplyr::select("level", "year", "country", "value") %>%
    dplyr::rename(mode = "level", pct = "value") %>%
    dplyr::mutate(
      mode = dplyr::case_when(
        tolower(.data$mode) == "vaginal"   ~ "Vaginal",
        tolower(.data$mode) == "c-section" ~ "Cesarean",
        TRUE                               ~ .data$mode
      ),
      source = "National Statistics"
    )

  if (is.null(deliveryModeSummary) || nrow(deliveryModeSummary) == 0) {
    return(list(natl = natl_dm, app = tibble::tibble()))
  }

  app_dm <- deliveryModeSummary %>%
    dplyr::mutate(
      n = suppressWarnings(as.numeric(.data$n)),
      pct = suppressWarnings(as.numeric(.data$pct))
    )

  if ("n_known" %in% colnames(app_dm)) {
    app_dm <- app_dm %>%
      dplyr::mutate(n_known = suppressWarnings(as.numeric(.data$n_known))) %>%
      dplyr::group_by(.data$cdm_name, .data$mode) %>%
      dplyr::summarise(n = sum(.data$n, na.rm = TRUE),
                        n_known = sum(.data$n_known, na.rm = TRUE),
                        .groups = "drop") %>%
      dplyr::mutate(pct = round(100 * .data$n / .data$n_known, 1))
  } else {
    app_dm <- app_dm %>%
      dplyr::group_by(.data$cdm_name, .data$mode) %>%
      dplyr::summarise(pct = mean(.data$pct, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(pct = round(.data$pct, 1))
  }

  app_dm <- app_dm %>%
    dplyr::rename(country = "cdm_name") %>%
    dplyr::mutate(
      source = "Database",
      year = NA_integer_,
      mode = dplyr::case_when(
        tolower(.data$mode) == "vaginal"   ~ "Vaginal",
        tolower(.data$mode) == "cesarean"  ~ "Cesarean",
        TRUE                               ~ .data$mode
      )
    ) %>%
    dplyr::select("country", "year", "mode", "pct", "source")

  list(natl = natl_dm, app = app_dm)
}

# ---- Helper: extract birth rate / live births / maternal age / foetal mortality ----
.extract_natl_summary <- function(natl) {
  natl %>%
    dplyr::filter(.data$indicator %in% c(
      "Birth rate", "Maternal age at birth", "Pregnancy outcome rates"
    )) %>%
    dplyr::select("indicator", "variable", "level", "year", "country", "value")
}


# ---- UI ----
nationalStatsComparisonUI <- function(id) {
  ns <- NS(id)

  tagList(
    h3("National Statistics Comparison"),
    p("Compare pregnancy-episode summaries from the databases with published national statistics across European countries."),
    tabsetPanel(
      id = ns("tabs"),
      type = "tabs",

      # -- Tab 1: Overview summary table --
      tabPanel(
        "Overview",
        br(),
        p("Summary of national statistics indicators. Select countries and years to display."),
        fluidRow(
          column(4, pickerInput(ns("overview_countries"), "Countries",
                                choices = character(0), selected = character(0),
                                multiple = TRUE, options = opt)),
          column(4, pickerInput(ns("overview_years"), "Years",
                                choices = character(0), selected = character(0),
                                multiple = TRUE, options = opt))
        ),
        gt::gt_output(ns("overview_table")) %>% withSpinner(type = 6),
        downloadButton(ns("download_overview"), "Download table (.docx)")
      ),

      # -- Tab 2: Gestational duration comparison --
      tabPanel(
        "Gestational Duration",
        br(),
        p("Distribution of live births by gestational age bin: database results vs. national statistics."),
        fluidRow(
          column(3, pickerInput(ns("gest_databases"), "Databases",
                                choices = character(0), selected = character(0),
                                multiple = TRUE, options = opt)),
          column(3, pickerInput(ns("gest_countries"), "National stats countries",
                                choices = character(0), selected = character(0),
                                multiple = TRUE, options = opt)),
          column(3, pickerInput(ns("gest_years"), "National stats years",
                                choices = character(0), selected = character(0),
                                multiple = TRUE, options = opt))
        ),
        tabsetPanel(
          tabPanel(
            "Table",
            gt::gt_output(ns("gest_table")) %>% withSpinner(type = 6),
            downloadButton(ns("download_gest_table"), "Download table (.docx)")
          ),
          tabPanel(
            "Plot",
            fluidRow(
              column(3, textInput(ns("gest_plot_height"), "Height (cm)", value = "12")),
              column(3, textInput(ns("gest_plot_width"), "Width (cm)", value = "24")),
              column(3, textInput(ns("gest_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            plotlyOutput(ns("gest_plot"), height = "600px") %>% withSpinner(type = 6),
            downloadButton(ns("download_gest_plot"), "Download plot (PNG)")
          )
        )
      ),

      # -- Tab 3: Delivery mode comparison --
      tabPanel(
        "Delivery Mode",
        br(),
        p("Cesarean vs. vaginal delivery rates: database results vs. national statistics."),
        fluidRow(
          column(3, pickerInput(ns("dm_databases"), "Databases",
                                choices = character(0), selected = character(0),
                                multiple = TRUE, options = opt)),
          column(3, pickerInput(ns("dm_countries"), "National stats countries",
                                choices = character(0), selected = character(0),
                                multiple = TRUE, options = opt)),
          column(3, pickerInput(ns("dm_years"), "National stats years",
                                choices = character(0), selected = character(0),
                                multiple = TRUE, options = opt))
        ),
        tabsetPanel(
          tabPanel(
            "Table",
            gt::gt_output(ns("dm_table")) %>% withSpinner(type = 6),
            downloadButton(ns("download_dm_table"), "Download table (.docx)")
          ),
          tabPanel(
            "Plot",
            fluidRow(
              column(3, textInput(ns("dm_plot_height"), "Height (cm)", value = "10")),
              column(3, textInput(ns("dm_plot_width"), "Width (cm)", value = "20")),
              column(3, textInput(ns("dm_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            plotlyOutput(ns("dm_plot"), height = "500px") %>% withSpinner(type = 6),
            downloadButton(ns("download_dm_plot"), "Download plot (PNG)")
          )
        )
      ),

      # -- Tab 4: Birth rates / Live births --
      tabPanel(
        "Birth Rates & Live Births",
        br(),
        p("National statistics for crude birth rates and live births per year across countries."),
        fluidRow(
          column(4, pickerInput(ns("br_countries"), "Countries",
                                choices = character(0), selected = character(0),
                                multiple = TRUE, options = opt)),
          column(4, pickerInput(ns("br_years"), "Years",
                                choices = character(0), selected = character(0),
                                multiple = TRUE, options = opt))
        ),
        tabsetPanel(
          tabPanel(
            "Birth Rate",
            plotlyOutput(ns("birth_rate_plot"), height = "500px") %>% withSpinner(type = 6),
            fluidRow(
              column(3, textInput(ns("br_plot_height"), "Height (cm)", value = "10")),
              column(3, textInput(ns("br_plot_width"), "Width (cm)", value = "20")),
              column(3, textInput(ns("br_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            downloadButton(ns("download_br_plot"), "Download plot (PNG)")
          ),
          tabPanel(
            "Live Births",
            plotlyOutput(ns("live_births_plot"), height = "500px") %>% withSpinner(type = 6),
            fluidRow(
              column(3, textInput(ns("lb_plot_height"), "Height (cm)", value = "10")),
              column(3, textInput(ns("lb_plot_width"), "Width (cm)", value = "20")),
              column(3, textInput(ns("lb_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            downloadButton(ns("download_lb_plot"), "Download plot (PNG)")
          ),
          tabPanel(
            "Foetal Mortality",
            plotlyOutput(ns("foetal_mort_plot"), height = "500px") %>% withSpinner(type = 6),
            fluidRow(
              column(3, textInput(ns("fm_plot_height"), "Height (cm)", value = "10")),
              column(3, textInput(ns("fm_plot_width"), "Width (cm)", value = "20")),
              column(3, textInput(ns("fm_plot_dpi"), "Resolution (dpi)", value = "300"))
            ),
            downloadButton(ns("download_fm_plot"), "Download plot (PNG)")
          )
        )
      ),

      # -- Tab 5: Raw national statistics data --
      tabPanel(
        "Raw Data",
        br(),
        DT::DTOutput(ns("raw_table")) %>% withSpinner(type = 6),
        downloadButton(ns("download_raw"), "Download raw data (.csv)")
      )
    )
  )
}


# ---- Server ----
nationalStatsComparisonServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: load national stats once
    natl_data <- reactive({
      .load_national_stats()
    })

    # Reactive: gestational duration comparison data
    gest_comparison <- reactive({
      natl <- natl_data()
      if (is.null(natl)) return(list(natl = tibble::tibble(), app = tibble::tibble()))
      gwb <- if (exists("gestationalWeeksBinned")) gestationalWeeksBinned else NULL
      .map_gest_bins(gwb, natl)
    })

    # Reactive: delivery mode comparison data
    dm_comparison <- reactive({
      natl <- natl_data()
      if (is.null(natl)) return(list(natl = tibble::tibble(), app = tibble::tibble()))
      dms <- if (exists("deliveryModeSummary")) deliveryModeSummary else NULL
      .map_delivery_mode(dms, natl)
    })

    # Reactive: summary indicators
    natl_summary <- reactive({
      natl <- natl_data()
      if (is.null(natl)) return(tibble::tibble())
      .extract_natl_summary(natl)
    })

    # ---- Update picker inputs when data is available ----
    observe({
      natl <- natl_data()
      if (is.null(natl)) return()
      countries <- sort(unique(natl$country))
      years <- sort(unique(natl$year[!is.na(natl$year)]))
      databases <- if (exists("allDP")) allDP else character(0)

      # Overview
      updatePickerInput(session, "overview_countries", choices = countries, selected = countries)
      updatePickerInput(session, "overview_years", choices = as.character(years), selected = as.character(years))

      # Gestational
      updatePickerInput(session, "gest_databases", choices = databases, selected = databases)
      updatePickerInput(session, "gest_countries", choices = countries, selected = countries[1:min(3, length(countries))])
      updatePickerInput(session, "gest_years", choices = as.character(years), selected = as.character(max(years)))

      # Delivery mode
      updatePickerInput(session, "dm_databases", choices = databases, selected = databases)
      updatePickerInput(session, "dm_countries", choices = countries, selected = countries[1:min(3, length(countries))])
      updatePickerInput(session, "dm_years", choices = as.character(years), selected = as.character(max(years)))

      # Birth rates
      updatePickerInput(session, "br_countries", choices = countries, selected = countries)
      updatePickerInput(session, "br_years", choices = as.character(years), selected = as.character(years))
    })


    # ========== TAB 1: Overview ==========
    overview_gt <- reactive({
      d <- natl_summary()
      validate(need(nrow(d) > 0, "No national statistics data available."))
      req(input$overview_countries, input$overview_years)

      d <- d %>%
        dplyr::filter(
          .data$country %in% input$overview_countries,
          as.character(.data$year) %in% input$overview_years
        )
      validate(need(nrow(d) > 0, "No data for selected filters."))

      # Pivot wider: one column per country
      d_wide <- d %>%
        dplyr::mutate(value = dplyr::if_else(
          is.na(.data$value), NA_character_,
          format(.data$value, big.mark = ",", scientific = FALSE, trim = TRUE)
        )) %>%
        tidyr::pivot_wider(
          id_cols = c("indicator", "variable", "level", "year"),
          names_from = "country",
          values_from = "value"
        ) %>%
        dplyr::arrange(.data$indicator, .data$variable, .data$level, .data$year)

      gt::gt(d_wide) %>%
        gt::tab_header(title = "National Statistics Overview") %>%
        gt::cols_label(
          indicator = "Indicator",
          variable = "Variable",
          level = "Level",
          year = "Year"
        ) %>%
        gt::sub_missing(missing_text = "-") %>%
        gt::tab_options(table.font.size = "small")
    })

    output$overview_table <- gt::render_gt({ overview_gt() })

    output$download_overview <- downloadHandler(
      filename = function() "national_stats_overview.docx",
      content = function(file) {
        tryCatch(gt::gtsave(overview_gt(), file), error = function(e) NULL)
      }
    )


    # ========== TAB 2: Gestational Duration ==========
    gest_filtered <- reactive({
      gc <- gest_comparison()
      req(input$gest_countries, input$gest_years)
      databases <- input$gest_databases

      natl_filt <- gc$natl %>%
        dplyr::filter(
          .data$country %in% input$gest_countries,
          as.character(.data$year) %in% input$gest_years
        )

      app_filt <- gc$app
      if (nrow(app_filt) > 0 && length(databases) > 0) {
        app_filt <- app_filt %>% dplyr::filter(.data$country %in% databases)
      }

      dplyr::bind_rows(natl_filt, app_filt)
    })

    gest_gt_reactive <- reactive({
      d <- gest_filtered()
      validate(need(nrow(d) > 0, "No gestational duration data available."))

      # Create a label column for display
      d <- d %>%
        dplyr::mutate(
          label = dplyr::if_else(
            .data$source == "National Statistics",
            paste0(.data$country, " (", .data$year, ")"),
            paste0(.data$country, " [DB]")
          )
        )

      d_wide <- d %>%
        dplyr::select("label", "bin", "pct") %>%
        tidyr::pivot_wider(names_from = "bin", values_from = "pct") %>%
        dplyr::arrange(.data$label)

      bin_order <- c("<32", "32-36", "37-41", ">=42")
      present_bins <- intersect(bin_order, colnames(d_wide))

      gt::gt(d_wide) %>%
        gt::tab_header(title = "Gestational Duration Distribution (%)",
                       subtitle = "Percentage of live births per gestational-age bin") %>%
        gt::cols_label(label = "Source") %>%
        gt::fmt_number(columns = dplyr::all_of(present_bins), decimals = 1) %>%
        gt::sub_missing(missing_text = "-") %>%
        gt::tab_options(table.font.size = "small") %>%
        gt::tab_spanner(label = "Gestational Age (weeks)", columns = dplyr::all_of(present_bins))
    })

    output$gest_table <- gt::render_gt({ gest_gt_reactive() })

    output$download_gest_table <- downloadHandler(
      filename = function() "gestational_duration_comparison.docx",
      content = function(file) {
        tryCatch(gt::gtsave(gest_gt_reactive(), file), error = function(e) NULL)
      }
    )

    gest_ggplot <- reactive({
      d <- gest_filtered()
      validate(need(nrow(d) > 0, "No gestational duration data to plot."))

      d <- d %>%
        dplyr::mutate(
          label = dplyr::if_else(
            .data$source == "National Statistics",
            paste0(.data$country, " (", .data$year, ")"),
            paste0(.data$country, " [DB]")
          ),
          bin = factor(.data$bin, levels = c("<32", "32-36", "37-41", ">=42"))
        )

      ggplot2::ggplot(d, ggplot2::aes(x = .data$label, y = .data$pct, fill = .data$bin)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(x = NULL, y = "Percentage (%)", fill = "Gestational\nAge (weeks)",
                      title = "Gestational Duration: Database vs. National Statistics") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_brewer(palette = "Set2")
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


    # ========== TAB 3: Delivery Mode ==========
    dm_filtered <- reactive({
      dc <- dm_comparison()
      req(input$dm_countries, input$dm_years)
      databases <- input$dm_databases

      natl_filt <- dc$natl %>%
        dplyr::filter(
          .data$country %in% input$dm_countries,
          as.character(.data$year) %in% input$dm_years
        )

      app_filt <- dc$app
      if (nrow(app_filt) > 0 && length(databases) > 0) {
        app_filt <- app_filt %>% dplyr::filter(.data$country %in% databases)
      }

      dplyr::bind_rows(natl_filt, app_filt)
    })

    dm_gt_reactive <- reactive({
      d <- dm_filtered()
      validate(need(nrow(d) > 0, "No delivery mode data available."))

      d <- d %>%
        dplyr::mutate(
          label = dplyr::if_else(
            .data$source == "National Statistics",
            paste0(.data$country, " (", .data$year, ")"),
            paste0(.data$country, " [DB]")
          )
        )

      d_wide <- d %>%
        dplyr::select("label", "mode", "pct") %>%
        tidyr::pivot_wider(names_from = "mode", values_from = "pct") %>%
        dplyr::arrange(.data$label)

      gt::gt(d_wide) %>%
        gt::tab_header(title = "Delivery Mode Distribution (%)",
                       subtitle = "Vaginal vs. Cesarean delivery rates") %>%
        gt::cols_label(label = "Source") %>%
        gt::fmt_number(columns = dplyr::any_of(c("Vaginal", "Cesarean")), decimals = 1) %>%
        gt::sub_missing(missing_text = "-") %>%
        gt::tab_options(table.font.size = "small")
    })

    output$dm_table <- gt::render_gt({ dm_gt_reactive() })

    output$download_dm_table <- downloadHandler(
      filename = function() "delivery_mode_comparison.docx",
      content = function(file) {
        tryCatch(gt::gtsave(dm_gt_reactive(), file), error = function(e) NULL)
      }
    )

    dm_ggplot <- reactive({
      d <- dm_filtered()
      validate(need(nrow(d) > 0, "No delivery mode data to plot."))

      d <- d %>%
        dplyr::mutate(
          label = dplyr::if_else(
            .data$source == "National Statistics",
            paste0(.data$country, " (", .data$year, ")"),
            paste0(.data$country, " [DB]")
          )
        )

      ggplot2::ggplot(d, ggplot2::aes(x = .data$label, y = .data$pct, fill = .data$mode)) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::labs(x = NULL, y = "Percentage (%)", fill = "Mode",
                      title = "Delivery Mode: Database vs. National Statistics") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_manual(values = c("Vaginal" = "#4DAF4A", "Cesarean" = "#E41A1C"))
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


    # ========== TAB 4: Birth Rates & Live Births ==========
    birth_rate_data <- reactive({
      d <- natl_summary()
      req(input$br_countries, input$br_years)
      d %>%
        dplyr::filter(
          .data$variable == "Crude birth rate (per 1000 person)",
          .data$country %in% input$br_countries,
          as.character(.data$year) %in% input$br_years
        )
    })

    br_ggplot <- reactive({
      d <- birth_rate_data()
      validate(need(nrow(d) > 0, "No birth rate data."))
      ggplot2::ggplot(d, ggplot2::aes(x = .data$country, y = .data$value,
                                       fill = as.factor(.data$year))) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(x = NULL, y = "Crude birth rate (per 1,000 persons)", fill = "Year",
                      title = "Crude Birth Rate by Country") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_brewer(palette = "Set1")
    })

    output$birth_rate_plot <- plotly::renderPlotly({
      p <- br_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_br_plot <- downloadHandler(
      filename = function() "birth_rate_plot.png",
      content = function(file) {
        p <- br_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p,
                          width = as.numeric(input$br_plot_width),
                          height = as.numeric(input$br_plot_height),
                          dpi = as.numeric(input$br_plot_dpi), units = "cm")
        }
      }
    )

    live_births_data <- reactive({
      d <- natl_summary()
      req(input$br_countries, input$br_years)
      d %>%
        dplyr::filter(
          grepl("Number of live births", .data$variable),
          .data$country %in% input$br_countries,
          as.character(.data$year) %in% input$br_years
        )
    })

    lb_ggplot <- reactive({
      d <- live_births_data()
      validate(need(nrow(d) > 0, "No live births data."))
      ggplot2::ggplot(d, ggplot2::aes(x = .data$country, y = .data$value,
                                       fill = as.factor(.data$year))) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(x = NULL, y = "Number of live births", fill = "Year",
                      title = "Live Births per Year by Country") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_fill_brewer(palette = "Set1")
    })

    output$live_births_plot <- plotly::renderPlotly({
      p <- lb_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_lb_plot <- downloadHandler(
      filename = function() "live_births_plot.png",
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

    foetal_mort_data <- reactive({
      d <- natl_summary()
      req(input$br_countries, input$br_years)
      d %>%
        dplyr::filter(
          grepl("Foetal mortality", .data$variable),
          .data$country %in% input$br_countries,
          as.character(.data$year) %in% input$br_years
        )
    })

    fm_ggplot <- reactive({
      d <- foetal_mort_data()
      validate(need(nrow(d) > 0, "No foetal mortality data."))
      ggplot2::ggplot(d, ggplot2::aes(x = .data$country, y = .data$value,
                                       fill = as.factor(.data$year))) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(x = NULL, y = "Foetal mortality rate (per 1,000 total births)", fill = "Year",
                      title = "Foetal Mortality Rate by Country") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_brewer(palette = "Set1")
    })

    output$foetal_mort_plot <- plotly::renderPlotly({
      p <- fm_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No data."))
      plotly::ggplotly(p)
    })

    output$download_fm_plot <- downloadHandler(
      filename = function() "foetal_mortality_plot.png",
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


    # ========== TAB 5: Raw Data ==========
    output$raw_table <- DT::renderDT({
      natl <- natl_data()
      validate(need(!is.null(natl) && nrow(natl) > 0, "No national statistics data available."))
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
