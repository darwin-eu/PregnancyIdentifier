# Minimal Shiny demo: precomputed boxplots (Plotly + ggplot2 download)
# Run from inst/shiny: shiny::runApp("extras/boxplot_precomputed_demo")
# Or: setwd("inst/shiny"); shiny::runApp("extras/boxplot_precomputed_demo")

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)

# Source the reusable module (assumes run from inst/shiny or project root)
if (file.exists("utils/boxplot-precomputed.R")) {
  source("utils/boxplot-precomputed.R")
} else if (file.exists("inst/shiny/utils/boxplot-precomputed.R")) {
  source("inst/shiny/utils/boxplot-precomputed.R")
} else {
  stop("Could not find utils/boxplot-precomputed.R. Run from inst/shiny or project root.")
}

# -----------------------------------------------------------------------------
# Example mock precomputed data (canonical + alias column names)
# -----------------------------------------------------------------------------
mock_precomputed_boxplot_data <- function() {
  # One row per box: group (or x), ymin, lower, middle, upper, ymax; optional n, tooltip, outliers
  tibble::tibble(
    group = rep(c("A", "B", "C"), each = 2),
    cdm_name = rep(c("DB1", "DB2"), 3),
    min = c(10, 12, 20, 22, 30, 32),
    Q25 = c(25, 26, 40, 42, 55, 56),
    median = c(35, 36, 50, 52, 70, 72),
    Q75 = c(45, 46, 60, 62, 85, 86),
    max = c(80, 82, 95, 98, 120, 122),
    n = c(100L, 150L, 200L, 180L, 90L, 110L),
    outliers = list(
      c(5, 85),
      c(8),
      numeric(0),
      c(18, 99),
      c(28, 125),
      c(25)
    )
  )
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Precomputed boxplot demo"),
  sidebarLayout(
    sidebarPanel(
      p("Filter by group. Same data drives both Plotly and download."),
      checkboxGroupInput(
        "groups",
        "Groups",
        choices = c("A", "B", "C"),
        selected = c("A", "B", "C")
      ),
      hr(),
      h4("Download figure"),
      numericInput("fig_width", "Width (cm)", value = 20, min = 5, max = 50),
      numericInput("fig_height", "Height (cm)", value = 10, min = 5, max = 30),
      numericInput("fig_dpi", "DPI", value = 300, min = 72, max = 600),
      downloadButton("download_png", "Download PNG"),
      downloadButton("download_pdf", "Download PDF")
    ),
    mainPanel(
      plotly::plotlyOutput("plot", height = "420px"),
      p("Interactive Plotly above; download uses ggplot2 with the same precomputed data.")
    )
  )
)

# -----------------------------------------------------------------------------
# Server: one reactive dataset, Plotly for UI and ggplot for download
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  # Raw data (in real app this would be from loadFile/global)
  raw_data <- mock_precomputed_boxplot_data()

  # Single reactive: filtered precomputed boxplot data (alias columns: min, Q25, median, Q75, max, group)
  plot_data <- reactive({
    req(input$groups)
    raw_data %>%
      dplyr::filter(.data$group %in% input$groups) %>%
      dplyr::mutate(
        # Normalize to canonical names for modules that expect them, or pass as-is (module accepts aliases)
        group = factor(.data$group, levels = c("A", "B", "C"))
      )
  })

  # Interactive Plotly (native; no ggplotly)
  output$plot <- plotly::renderPlotly({
    pd <- plot_data()
    if (is.null(pd) || nrow(pd) == 0) {
      return(plotly::plot_ly() %>%
               plotly::add_annotations(
                 text = "No data for selected filters.",
                 x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                 showarrow = FALSE
               ) %>%
               plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }
    make_plotly_boxplot_precomputed(
      pd,
      fill = NULL,
      facet = "cdm_name",
      title = "Precomputed boxplots by group",
      xlab = "Group",
      ylab = "Value",
      show_outliers = TRUE,
      ref_hline = c(50),
      category_order = c("A", "B", "C"),
      horizontal = TRUE
    )
  })

  # Static ggplot for download (same data, same options)
  plot_ggplot <- reactive({
    pd <- plot_data()
    if (is.null(pd) || nrow(pd) == 0) return(NULL)
    make_ggplot_boxplot_precomputed(
      pd,
      fill = NULL,
      facet = ggplot2::vars(.data$cdm_name),
      title = "Precomputed boxplots by group",
      xlab = "Group",
      ylab = "Value",
      show_outliers = TRUE,
      ref_hline = 50,
      category_order = c("A", "B", "C"),
      horizontal = TRUE
    )
  })

  output$download_png <- downloadHandler(
    filename = function() "precomputed_boxplot.png",
    content = function(file) {
      p <- plot_ggplot()
      if (!is.null(p)) {
        ggplot2::ggsave(
          filename = file,
          plot = p,
          width = input$fig_width,
          height = input$fig_height,
          dpi = input$fig_dpi,
          units = "cm"
        )
      }
    }
  )

  output$download_pdf <- downloadHandler(
    filename = function() "precomputed_boxplot.pdf",
    content = function(file) {
      p <- plot_ggplot()
      if (!is.null(p)) {
        ggplot2::ggsave(
          filename = file,
          plot = p,
          width = input$fig_width,
          height = input$fig_height,
          device = "pdf",
          units = "cm"
        )
      }
    }
  )
}

shinyApp(ui, server)
