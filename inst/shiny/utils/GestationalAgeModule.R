library(DarwinShinyModules)

GestationalAgeModule <- R6::R6Class(
  classname = "GestationalAgeModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, daysData = NULL, dp = unique(data$cdm_name), maxWeeksFilter = TRUE, height = "600px") {
      super$initialize()
      private$.data <- data
      private$.daysData <- daysData
      private$.dp <- dp
      private$.maxWeeksFilter <- maxWeeksFilter
      private$.height <- height

      private$.table <- Table$new(data = data)
      private$.table$parentNamespace <- self$namespace

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

      if (private$.maxWeeksFilter) {
        private$.inputPanelMaxWeeks <- InputPanel$new(fun = list(max_weeks = shiny::sliderInput),
                                                 args = list(max_weeks = list(
                                                   inputId = "max_weeks", label = "Max weeks",
                                                   min = 2,
                                                   max = max(data$gestational_weeks, na.rm = T),
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
    .table = NULL,
    .maxWeeksFilter = NULL,
    .inputPanelCDM = NULL,
    .inputPanelOutput = NULL,
    .inputPanelMaxWeeks = NULL,


    .UI = function() {
      if (private$.maxWeeksFilter) {
        result <- shiny::tagList(
          private$.inputPanelCDM$UI(),
          private$.inputPanelOutput$UI(),
          private$.inputPanelMaxWeeks$UI(),
          plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height),
          private$.table$UI()
        )
      } else {
        result <- shiny::tagList(
          private$.inputPanelCDM$UI(),
          private$.inputPanelOutput$UI(),
          plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height),
          private$.table$UI()
        )
      }
    },

    .server = function(input, output, session) {
      # tables
      private$.table$server(input, output, session)
      # input filters
      private$.inputPanelCDM$server(input, output, session)
      private$.inputPanelOutput$server(input, output, session)
      if (!is.null(private$.inputPanelMaxWeeks)) {
        private$.inputPanelMaxWeeks$server(input, output, session)
      }

      getData <- reactive({
        data <- private$.data
        if (private$.maxWeeksFilter) {
          data <- private$.data %>%
            dplyr::mutate(gestational_weeks = as.numeric(gestational_weeks)) %>%
            dplyr::filter(gestational_weeks <= private$.inputPanelMaxWeeks$inputValues$max_weeks)
        }

        if ("cdm_name" %in% colnames(data)) {
          data <-  data %>%
            dplyr::filter(.data$cdm_name %in% private$.inputPanelCDM$inputValues$cdm_name)
        } else {
          nameCols <- setdiff(colnames(data), private$.dp)
          data <-  data %>%
            dplyr::select(c(nameCols, private$.inputPanelCDM$inputValues$cdm_name))
        }
        return(data)
      })

      getPlotData <- reactive({
        getData()
      })

      getLabelData <- reactive({
        private$.daysData
      })

      # handle updates
      shiny::observeEvent(c(private$.inputPanelCDM$inputValues$cdm_name,
                            private$.inputPanelMaxWeeks$inputValues$max_weeks), {
        private$.table$data <- getData()
        private$.table$server(input, output, session)
      }, ignoreNULL = FALSE)

      ### make plot ----
      createPlot <- reactive({
        plot <- NULL
        data <- getPlotData()
        labelData <- getLabelData()
        if (!is.null(data) && nrow(data) > 0) {

          plot_labeller <- NULL
          if (private$.maxWeeksFilter) {
            plot_labeller <- function(variable, value) {
              dpData <- labelData %>% dplyr::select(dplyr::all_of(c("name", value)))
              less1day <- dpData %>% dplyr::filter(name == "less_1day") %>% dplyr::pull(value)
              over308 <- dpData %>% dplyr::filter(name == "over_308days") %>% dplyr::pull(value)
              result <- glue::glue("{value} - <1d: {less1day}, >308d: {over308}")
              return(result)
            }
          }

          plot <- barPlot(data = data,
                          xVar = "gestational_weeks",
                          yVar = private$.inputPanelOutput$inputValues$output,
                          facetVar = "cdm_name",
                          labelFunction = plot_labeller,
                          label = "n",
                          xLabel = "Weeks",
                          yLabel = private$.inputPanelOutput$inputValues$output,
                          title = "Gestational duration",
                          rotateAxisText = TRUE,
                          verticalLinesPos = c(0, 44))
        }
        return(plot)
      })

      output$plot <- renderPlotly({
        createPlot()
      })
    }
  )
)
