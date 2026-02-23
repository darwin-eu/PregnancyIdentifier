library(DarwinShinyModules)

AgeSummaryFirstPregnancyModule <- R6::R6Class(
  classname = "AgeSummaryFirstPregnancyModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name), height = "420px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.height <- height

      private$.table <- Table$new(data = data, title = "age_summary_first_pregnancy.csv", options = list(scrollX = TRUE, pageLength = 25))
      private$.table$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .data = NULL,
    .dp = NULL,
    .height = NULL,
    .table = NULL,

    .UI = function() {
      shiny::tagList(
        plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>% shinycssloaders::withSpinner(),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      private$.table$server(input, output, session)

      getPlotData <- shiny::reactive({
        ageMetrics <- c("min", "Q25", "median", "Q75", "max")
        d <- private$.data
        if (is.null(d) || nrow(d) == 0 || !"cdm_name" %in% colnames(d)) return(d)
        for (col in ageMetrics) {
          if (col %in% colnames(d)) d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
        }
        d %>%
          dplyr::filter(dplyr::if_all(dplyr::all_of(intersect(ageMetrics, colnames(d))), ~ !is.na(.)))
      })

      output$plot <- plotly::renderPlotly({
        data <- getPlotData()
        if (is.null(data) || nrow(data) == 0) {
          return(emptyPlotlyMessage("No data available."))
        }
        ageMetrics <- c("min", "Q25", "median", "Q75", "max")
        if (!all(ageMetrics %in% colnames(data))) {
          return(emptyPlotlyMessage("Age summary columns (min, Q25, median, Q75, max) not found."))
        }
        # One row per cdm_name: horizontal boxplot
        data <- data %>%
          dplyr::mutate(cdm_name = factor(.data$cdm_name, levels = rev(sort(unique(as.character(.data$cdm_name))))))
        plotly::plot_ly(data = data, y = ~ cdm_name) %>%
          plotly::add_boxplot(
            lowerfence = ~ min,
            q1 = ~ Q25,
            median = ~ median,
            q3 = ~ Q75,
            upperfence = ~ max,
            orientation = "h",
            boxpoints = FALSE
          ) %>%
          plotly::layout(
            xaxis = list(title = "Age at first pregnancy (end)"),
            yaxis = list(title = "", categoryorder = "array", categoryarray = levels(data$cdm_name)),
            margin = list(t = 40, b = 40)
          )
      })
    }
  )
)
