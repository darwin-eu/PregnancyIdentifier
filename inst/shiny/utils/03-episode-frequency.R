# 03-episode-frequency.R - Episode frequency & pregnancy frequency modules (standard Shiny modules)

# ---- Episode Frequency Module ----

episodeFrequencyUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text",
        "Total pregnancy episodes and total individuals. Used as the main denominator for rates and site-level summaries."),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("cdm"), "Database",
                                          choices = allDP, selected = allDP,
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
    DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner(),
    downloadButton(ns("download_table_csv"), "Download table (.csv)")
  )
}

episodeFrequencyServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePickerInput(session, "cdm", choices = rv$allDP, selected = rv$allDP)
    })

    nameLabels <- c(
      total_episodes = "Total episodes",
      total_individuals = "Total individuals",
      min = "Minimum episodes per person",
      Q25 = "25th percentile (episodes per person)",
      median = "Median episodes per person",
      Q75 = "75th percentile (episodes per person)",
      max = "Maximum episodes per person",
      mean = "Mean episodes per person",
      sd = "Standard deviation (episodes per person)"
    )

    getData <- reactive({
      if (!is.data.frame(rv$episodeFrequency) || nrow(rv$episodeFrequency) == 0) {
        return(data.frame())
      }
      if (!"cdm_name" %in% colnames(rv$episodeFrequency)) {
        return(rv$episodeFrequency)
      }
      filterByCdm(rv$episodeFrequency, input$cdm, rv$allDP)
    })

    getPlotData <- reactive({
      data <- getData()
      if (!is.data.frame(data) || nrow(data) == 0 || !"name" %in% colnames(data)) {
        return(data.frame())
      }
      data %>%
        dplyr::filter(.data$name %in% c("total_episodes", "total_individuals")) %>%
        tidyr::pivot_longer(cols = setdiff(colnames(.), c("name")),
                            names_to = "cdm_name") %>%
        dplyr::mutate(
          value = as.numeric(value),
          name = dplyr::case_when(
            .data$name == "total_episodes" ~ "Total Episodes",
            .data$name == "total_individuals" ~ "Total Individuals",
            TRUE ~ .data$name
          )
        )
    })

    plot_ggplot <- reactive({
      plotData <- getPlotData()
      if (is.null(plotData) || nrow(plotData) == 0) return(NULL)
      barPlot(data = plotData,
              xVar = "name",
              yVar = "value",
              fillVar = "name",
              facetVar = "cdm_name",
              rotateAxisText = TRUE,
              return_ggplot = TRUE)
    })

    output$plot <- plotly::renderPlotly({
      p <- plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No episode frequency data available."))
      plotly::ggplotly(p)
    })

    output$download_plot <- downloadHandler(
      filename = function() { "episodeFrequencyPlot.png" },
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

    tableDataDisplay <- reactive({
      tableData <- getData()
      if (is.null(tableData) || nrow(tableData) == 0) return(data.frame())
      if ("name" %in% colnames(tableData)) {
        tableData <- tableData %>%
          dplyr::mutate(name = ifelse(
            .data$name %in% names(nameLabels),
            unname(nameLabels[.data$name]),
            .data$name
          ))
      }
      tableData
    })
    output$table <- DT::renderDT({
      renderPrettyDT(tableDataDisplay())
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "episode_frequency.csv" },
      content = function(file) {
        d <- tableDataDisplay()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}

# ---- Pregnancy Frequency Module ----

pregnancyFrequencyUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text",
        "Distribution of pregnancy count per person (parity-like). Used to describe repeat pregnancies and check for implausible multiplicity."),
    fluidRow(
      column(3, shinyWidgets::pickerInput(ns("cdm"), "Database",
                                          choices = allDP, selected = allDP,
                                          multiple = TRUE, options = opt))
    ),
    DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner(),
    downloadButton(ns("download_table_csv"), "Download table (.csv)")
  )
}

pregnancyFrequencyServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observe({
      updatePickerInput(session, "cdm", choices = rv$allDP, selected = rv$allDP)
    })
    tableData <- reactive({
      d <- filterByCdm(rv$pregnancyFrequency, input$cdm, rv$allDP)
      if (!is.data.frame(d) || nrow(d) == 0) return(d)
      if (!"cdm_name" %in% colnames(d) || !"number_individuals" %in% colnames(d)) return(d)
      d %>%
        tidyr::pivot_wider(
          names_from = "cdm_name",
          values_from = "number_individuals",
          values_fn = dplyr::first
        )
    })
    output$table <- DT::renderDT({
      renderPrettyDT(tableData())
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "pregnancy_frequency.csv" },
      content = function(file) {
        d <- tableData()
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}
