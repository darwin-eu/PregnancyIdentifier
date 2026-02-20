library(DarwinShinyModules)

AgeSummaryCsvModule <- R6::R6Class(
  classname = "AgeSummaryCsvModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name), height = "420px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.height <- height
      private$.hasOutcome <- "final_outcome_category" %in% colnames(data)

      private$.table <- Table$new(data = data, title = "age_summary.csv", options = list(scrollX = TRUE, pageLength = 25))
      private$.table$parentNamespace <- self$namespace

      # Database picker
      private$.inputPanelCDM <- InputPanel$new(
        fun = list(cdm_name = shinyWidgets::pickerInput),
        args = list(cdm_name = list(
          inputId = "cdm_name", label = "Database",
          choices = private$.dp,
          selected = private$.dp,
          multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        growDirection = "horizontal"
      )
      private$.inputPanelCDM$parentNamespace <- self$namespace

      # Outcome group picker (only when column exists)
      if (private$.hasOutcome) {
        private$.outcomeChoices <- sort(unique(as.character(data$final_outcome_category)))
        private$.inputPanelOutcome <- InputPanel$new(
          fun = list(final_outcome_category = shinyWidgets::pickerInput),
          args = list(final_outcome_category = list(
            inputId = "final_outcome_category", label = "Outcome group",
            choices = private$.outcomeChoices,
            selected = private$.outcomeChoices,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
          growDirection = "horizontal"
        )
        private$.inputPanelOutcome$parentNamespace <- self$namespace
      } else {
        private$.inputPanelOutcome <- NULL
      }
    }
  ),

  private = list(
    .data = NULL,
    .dp = NULL,
    .height = NULL,
    .hasOutcome = NULL,
    .outcomeChoices = NULL,
    .table = NULL,
    .inputPanelCDM = NULL,
    .inputPanelOutcome = NULL,

    .UI = function() {
      uiElements <- list(
        private$.inputPanelCDM$UI()
      )
      if (!is.null(private$.inputPanelOutcome)) {
        uiElements <- c(uiElements, list(private$.inputPanelOutcome$UI()))
      }
      uiElements <- c(
        uiElements,
        list(
          plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>% shinycssloaders::withSpinner(),
          private$.table$UI()
        )
      )
      do.call(shiny::tagList, uiElements)
    },

    .server = function(input, output, session) {
      private$.table$server(input, output, session)
      private$.inputPanelCDM$server(input, output, session)
      if (!is.null(private$.inputPanelOutcome)) {
        private$.inputPanelOutcome$server(input, output, session)
      }

      getData <- shiny::reactive({
        d <- private$.data
        if (!"cdm_name" %in% colnames(d)) return(d)
        cdmSel <- private$.inputPanelCDM$inputValues$cdm_name
        if (is.null(cdmSel) || length(cdmSel) == 0) cdmSel <- private$.dp
        d <- d %>% dplyr::filter(.data$cdm_name %in% cdmSel)
        if (private$.hasOutcome && !is.null(private$.inputPanelOutcome)) {
          outcomeSel <- private$.inputPanelOutcome$inputValues$final_outcome_category
          if (!is.null(outcomeSel) && length(outcomeSel) > 0) {
            d <- d %>% dplyr::filter(.data$final_outcome_category %in% outcomeSel)
          }
        }
        d
      })

      getPlotData <- shiny::reactive({
        ageMetrics <- c("min", "Q25", "median", "Q75", "max")
        d <- getData()
        if (nrow(d) == 0) return(d)
        if (!private$.hasOutcome) {
          d <- d %>% dplyr::mutate(final_outcome_category = "Overall", .after = "cdm_name")
        }
        # Ensure numeric and one row per (cdm_name, outcome)
        for (col in ageMetrics) {
          if (col %in% colnames(d)) d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
        }
        d %>%
          dplyr::filter(dplyr::if_all(dplyr::all_of(intersect(ageMetrics, colnames(d))), ~ !is.na(.)))
      })

      shiny::observe({
        filtered <- getData()
        private$.table$data <- filtered
        private$.table$server(input, output, session)
      })

      createPlot <- shiny::reactive({
        data <- getPlotData()
        if (is.null(data) || nrow(data) == 0) {
          return(emptyPlotlyMessage("No data for selected filters."))
        }
        ageMetrics <- c("min", "Q25", "median", "Q75", "max")
        if (!all(ageMetrics %in% colnames(data))) {
          return(emptyPlotlyMessage("Age summary columns (min, Q25, median, Q75, max) not found."))
        }
        # One subplot per database: horizontal boxplot with y = outcome group
        databases <- unique(data$cdm_name)
        plots <- lapply(databases, function(db) {
          dbData <- data %>% dplyr::filter(.data$cdm_name == db)
          dbData <- dbData %>%
            dplyr::mutate(final_outcome_category = factor(.data$final_outcome_category, levels = rev(sort(unique(as.character(.data$final_outcome_category))))))
          p <- plotly::plot_ly(data = dbData, y = ~ final_outcome_category) %>%
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
              xaxis = list(title = "Age"),
              yaxis = list(title = "", categoryorder = "array", categoryarray = levels(dbData$final_outcome_category)),
              title = list(text = db, font = list(size = 12)),
              margin = list(t = 40, b = 40)
            )
          p
        })
        n <- length(plots)
        if (n == 1) {
          return(plots[[1]])
        }
        plotly::subplot(plots, nrows = n, shareX = TRUE, titleY = TRUE, margin = 0.05) %>%
          plotly::layout(
            title = list(text = "Age at pregnancy start by outcome group", font = list(size = 14))
          )
      })

      output$plot <- plotly::renderPlotly({
        createPlot()
      })
    }
  )
)
