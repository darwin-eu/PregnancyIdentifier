library(DarwinShinyModules)

GestationalAgeModule <- R6::R6Class(
  classname = "GestationalAgeModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, daysData = NULL, dp = unique(data$cdm_name), maxWeeksFilter = TRUE, height = "900px") {
      super$initialize()
      private$.data <- data
      private$.daysData <- daysData
      private$.dp <- dp
      private$.maxWeeksFilter <- maxWeeksFilter
      private$.height <- height
      private$.hasOutcome <- "final_outcome_category" %in% colnames(data)
      if (private$.hasOutcome) {
        private$.outcomeChoices <- sort(unique(as.character(data$final_outcome_category)))
      }

      # input pickers
      private$.inputPanelCDM <- InputPanel$new(fun = list(cdm_name = shinyWidgets::pickerInput),
                                               args = list(cdm_name = list(
                                                 inputId = "cdm_name", label = "Database",
                                                 choices = private$.dp,
                                                 selected = private$.dp,
                                                 multiple = TRUE,
                                                 options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
                                               growDirection = "horizontal")
      private$.inputPanelCDM$parentNamespace <- self$namespace

      outputChoices <- c("pct", "n", "log(n)")
      selectedOutputChoice <- ifelse(private$.maxWeeksFilter, "log(n)", "pct")
      private$.inputPanelOutput <- InputPanel$new(fun = list(output = shinyWidgets::pickerInput),
                                               args = list(output = list(
                                                 inputId = "output", label = "Y value",
                                                 choices = outputChoices,
                                                 selected = selectedOutputChoice,
                                                 multiple = FALSE)),
                                               growDirection = "horizontal")
      private$.inputPanelOutput$parentNamespace <- self$namespace

      if (private$.hasOutcome) {
        private$.inputPanelOutcome <- InputPanel$new(fun = list(final_outcome_category = shinyWidgets::pickerInput),
                                                    args = list(final_outcome_category = list(
                                                      inputId = "final_outcome_category", label = "Outcome",
                                                      choices = private$.outcomeChoices,
                                                      selected = private$.outcomeChoices,
                                                      multiple = TRUE,
                                                      options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
                                                    growDirection = "horizontal")
        private$.inputPanelOutcome$parentNamespace <- self$namespace
      } else {
        private$.inputPanelOutcome <- NULL
      }

      if (private$.maxWeeksFilter) {
        private$.inputPanelMaxWeeks <- InputPanel$new(fun = list(max_weeks = shiny::numericInput),
                                                 args = list(max_weeks = list(
                                                   inputId = "max_weeks",
                                                   label = "Max weeks",
                                                   value = 50)),
                                                 growDirection = "horizontal")
        private$.inputPanelMaxWeeks$parentNamespace <- self$namespace
      }
    }
  ),

  private = list(
    .data = NULL,
    .daysData = NULL,
    .dp = NULL,
    .height = NULL,
    .maxWeeksFilter = NULL,
    .hasOutcome = FALSE,
    .outcomeChoices = NULL,
    .inputPanelCDM = NULL,
    .inputPanelOutput = NULL,
    .inputPanelOutcome = NULL,
    .inputPanelMaxWeeks = NULL,


    .UI = function() {
      emptyMsg <- is.null(private$.data) || nrow(private$.data) == 0
      dataTableOut <- shiny::tagList(
        shiny::h4("Data"),
        DT::DTOutput(shiny::NS(private$.namespace, "dataTable"))
      )
      if (emptyMsg) {
        return(shiny::tagList(
          shiny::p("Results files are empty.", style = "margin: 20px; font-size: 16px; font-weight: bold;"),
          dataTableOut
        ))
      }
      controls <- list(
        private$.inputPanelCDM$UI(),
        private$.inputPanelOutput$UI()
      )
      if (private$.hasOutcome && !is.null(private$.inputPanelOutcome)) {
        controls <- c(controls, list(private$.inputPanelOutcome$UI()))
      }
      if (private$.maxWeeksFilter) {
        controls <- c(controls, list(private$.inputPanelMaxWeeks$UI()))
      }
      result <- shiny::tagList(
        controls,
        plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>% shinycssloaders::withSpinner(),
        dataTableOut
      )
    },

    .server = function(input, output, session) {
      # input filters
      private$.inputPanelCDM$server(input, output, session)
      private$.inputPanelOutput$server(input, output, session)
      if (!is.null(private$.inputPanelOutcome)) {
        private$.inputPanelOutcome$server(input, output, session)
      }
      if (!is.null(private$.inputPanelMaxWeeks)) {
        private$.inputPanelMaxWeeks$server(input, output, session)
      }

      getData <- shiny::reactive({
        data <- private$.data
        cdmSel <- private$.inputPanelCDM$inputValues$cdm_name
        if (is.null(cdmSel) || length(cdmSel) == 0) cdmSel <- private$.dp

        if (private$.maxWeeksFilter) {
          maxWeeks <- private$.inputPanelMaxWeeks$inputValues$max_weeks
          if (is.null(maxWeeks)) maxWeeks <- 50
          data <- private$.data %>%
            dplyr::mutate(gestational_weeks = as.numeric(gestational_weeks)) %>%
            dplyr::filter(gestational_weeks <= maxWeeks)
        }

        if ("cdm_name" %in% colnames(data)) {
          data <-  data %>%
            dplyr::filter(.data$cdm_name %in% cdmSel)
        } else {
          nameCols <- setdiff(colnames(data), private$.dp)
          data <-  data %>%
            dplyr::select(dplyr::any_of(c(nameCols, cdmSel)))
        }
        if (private$.hasOutcome && !is.null(private$.inputPanelOutcome)) {
          outcomeSel <- private$.inputPanelOutcome$inputValues$final_outcome_category
          if (is.null(outcomeSel) || length(outcomeSel) == 0) outcomeSel <- private$.outcomeChoices
          if (!is.null(outcomeSel) && length(outcomeSel) > 0) {
            data <- data %>%
              dplyr::filter(.data$final_outcome_category %in% outcomeSel)
          }
        }
        return(data)
      })

      getPlotData <- shiny::reactive({
        data <- getData()
        # barPlot expects a column matching the y-axis choice; data has n and pct but not "log(n)"
        if (!is.null(data) && nrow(data) > 0 && "n" %in% colnames(data)) {
          data <- data %>%
            dplyr::mutate(`log(n)` = log1p(.data$n))
        }
        data
      })

      getLabelData <- shiny::reactive({
        private$.daysData
      })

      ### make plot ----
      createPlot <- shiny::reactive({
        plot <- NULL
        data <- getPlotData()
        labelData <- getLabelData()
        if (!is.null(data) && nrow(data) > 0) {

          plot_labeller <- NULL
          if (private$.maxWeeksFilter && !is.null(labelData) && nrow(labelData) > 0) {
            cdmCols <- setdiff(colnames(labelData), "name")
            if (length(cdmCols) > 0) {
              plot_labeller <- ggplot2::as_labeller(function(x) {
                vapply(x, function(val) {
                  if (!val %in% cdmCols) return(val)
                  dpData <- labelData %>% dplyr::select(dplyr::all_of(c("name", val)))
                  less1day <- dpData %>% dplyr::filter(.data$name == "less_1day") %>% dplyr::pull(2)
                  over308 <- dpData %>% dplyr::filter(.data$name == "over_308days") %>% dplyr::pull(2)
                  as.character(glue::glue("{val} - <1d: {less1day}, >308d: {over308}"))
                }, character(1))
              })
            }
          }

          yVar <- private$.inputPanelOutput$inputValues$output
          if (is.null(yVar) || length(yVar) == 0) yVar <- if (private$.maxWeeksFilter) "log(n)" else "pct"
          # Binned data has n and pct only; compute log(n) when selected
          if (yVar == "log(n)" && !"log(n)" %in% colnames(data) && "n" %in% colnames(data)) {
            data <- data %>% dplyr::mutate(`log(n)` = log1p(as.numeric(.data$n)))
          }

          plotArgs <- list(
            data = data,
            xVar = "gestational_weeks",
            yVar = yVar,
            facetVar = "cdm_name",
            labelFunction = plot_labeller,
            label = "n",
            xLabel = if (private$.maxWeeksFilter) "Weeks" else "Gestational age band",
            yLabel = yVar,
            title = "Gestational duration",
            rotateAxisText = TRUE,
            verticalLinesPos = if (private$.maxWeeksFilter) c(0, 44) else NULL
          )
          if (private$.hasOutcome && "final_outcome_category" %in% colnames(data)) {
            plotArgs$fillVar <- "final_outcome_category"
          }
          plot <- do.call(barPlot, plotArgs)
        }
        return(plot)
      })

      output$plot <- plotly::renderPlotly({
        p <- createPlot()
        if (is.null(p)) {
          msg <- if (nrow(private$.data) == 0) "Results files are empty." else "No data for selected filters."
          return(emptyPlotlyMessage(msg))
        }
        p
      })

      output$dataTable <- DT::renderDT({
        DT::datatable(
          private$.data,
          options = list(scrollX = TRUE, pageLength = 25),
          filter = "top"
        )
      })
    }
  )
)
