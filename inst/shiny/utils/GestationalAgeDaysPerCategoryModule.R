library(DarwinShinyModules)

GestationalAgeDaysPerCategoryModule <- R6::R6Class(
  classname = "GestationalAgeDaysPerCategoryModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, plotData = NULL, dp = unique(data$cdm_name), height = "600px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.height <- height
      # input pickers
      private$.cdmInputPanel <- InputPanel$new(fun = list(cdm_name = shinyWidgets::pickerInput),
                                               args = list(cdm_name = list(
                                                 inputId = "cdm_name", label = "Database",
                                                 choices = private$.dp,
                                                 selected = private$.dp,
                                                 multiple = TRUE,
                                                 options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
                                               growDirection = "horizontal")
      private$.cdmInputPanel$parentNamespace <- self$namespace

      allOutcomes <- unique(private$.data$final_outcome_category)
      private$.outcomeInputPanel <- InputPanel$new(fun = list(outcome = shinyWidgets::pickerInput),
                                               args = list(outcome = list(
                                                 inputId = "outcome", label = "Outcome",
                                                 choices = allOutcomes,
                                                 selected = allOutcomes,
                                                 multiple = TRUE,
                                                 options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
                                               growDirection = "horizontal")
      private$.outcomeInputPanel$parentNamespace <- self$namespace

      plotDataChoices <- c("cdm_name", "final_outcome_category")
      # facet by
      private$.facetByInputPanel <- InputPanel$new(
        fun = list(facet_by = shinyWidgets::pickerInput),
        args = list(facet_by = list(
          inputId = "facet_by", choices = plotDataChoices, label = "Facet by", selected = c("cdm_name"), multiple = FALSE
        )),
        growDirection = "horizontal"
      )
      private$.facetByInputPanel$parentNamespace <- self$namespace

      # color by
      private$.colorByInputPanel <- InputPanel$new(
        fun = list(color_by = shinyWidgets::pickerInput),
        args = list(color_by = list(
          inputId = "color_by", choices = plotDataChoices, label = "Colour by", selected = c("final_outcome_category"), multiple = FALSE
        )),
        growDirection = "horizontal"
      )
      private$.colorByInputPanel$parentNamespace <- self$namespace

      private$.maxInputPanel <- InputPanel$new(fun = list(max = shiny::checkboxInput),
                                                           args = list(max = list(
                                                             inputId = "max",
                                                             value = TRUE,
                                                             label = "IQR only"
                                                           )),
                                                           growDirection = "horizontal")
      private$.maxInputPanel$parentNamespace <- self$namespace

      private$.flipCoordinatesInputPanel <- InputPanel$new(fun = list(flip = shiny::checkboxInput),
                                                            args = list(flip = list(
                                                              inputId = "flip",
                                                              value = TRUE,
                                                              label = "Flip coordinates"
                                                            )),
                                                            growDirection = "horizontal")
      private$.flipCoordinatesInputPanel$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .data = NULL,
    .dp = NULL,
    .cdmInputPanel = NULL,
    .outcomeInputPanel = NULL,
    .facetByInputPanel = NULL,
    .colorByInputPanel = NULL,
    .maxInputPanel = NULL,
    .flipCoordinatesInputPanel = NULL,
    .height = NULL,

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
      shiny::tagList(
        private$.cdmInputPanel$UI(),
        private$.outcomeInputPanel$UI(),
        shiny::p("Plotting options"),
        private$.facetByInputPanel$UI(),
        private$.colorByInputPanel$UI(),
        shiny::HTML("&nbsp;&nbsp;"),
        private$.maxInputPanel$UI(),
        shiny::HTML("&nbsp;&nbsp;"),
        private$.flipCoordinatesInputPanel$UI(),
        shiny::plotOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>% shinycssloaders::withSpinner(),
        shiny::p(),
        dataTableOut
      )
    },

    .server = function(input, output, session) {
      # input filters
      private$.cdmInputPanel$server(input, output, session)
      private$.outcomeInputPanel$server(input, output, session)
      private$.facetByInputPanel$server(input, output, session)
      private$.colorByInputPanel$server(input, output, session)
      private$.maxInputPanel$server(input, output, session)
      private$.flipCoordinatesInputPanel$server(input, output, session)

      getData <- shiny::reactive({
        cdmSel <- private$.cdmInputPanel$inputValues$cdm_name
        outcomeSel <- private$.outcomeInputPanel$inputValues$outcome
        if (is.null(cdmSel) || length(cdmSel) == 0) cdmSel <- private$.dp
        if (is.null(outcomeSel) || length(outcomeSel) == 0) outcomeSel <- unique(as.character(private$.data$final_outcome_category))
        private$.data %>%
          dplyr::filter(.data$cdm_name %in% cdmSel) %>%
          dplyr::filter(.data$final_outcome_category %in% outcomeSel)
      })

      ### make plot ----
      createPlot <- shiny::reactive({
        plot <- NULL
        data <- getData()
        if (!is.null(data) && nrow(data) > 0) {
          colorBy <- private$.colorByInputPanel$inputValues$color_by
          if (is.null(colorBy) || length(colorBy) == 0) colorBy <- "final_outcome_category"
          iqrOnly <- private$.maxInputPanel$inputValues$max
          if (is.null(iqrOnly)) iqrOnly <- TRUE
          ymaxValue <- ifelse(iqrOnly, "Q75", "max")
          yminValue <- ifelse(iqrOnly, "Q25", "min")
          plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[colorBy]], ymin = .data[[yminValue]], lower = .data[["Q25"]], middle = .data[["median"]], upper = .data[["Q75"]], ymax = .data[[ymaxValue]])) +
            ggplot2::geom_boxplot(ggplot2::aes(fill = .data[[colorBy]]), stat = "identity")

          facetBy <- private$.facetByInputPanel$inputValues$facet_by
          if (is.null(facetBy) || length(facetBy) == 0) facetBy <- "cdm_name"
          if (!is.null(facetBy)) {
            plot <- plot +
              ggplot2::facet_wrap(as.formula(paste("~", facetBy)), scales = "free")
          }
          flipCoordinates <- private$.flipCoordinatesInputPanel$inputValues$flip
          if (is.null(flipCoordinates)) flipCoordinates <- TRUE
          if (flipCoordinates) {
            plot <- plot + ggplot2::coord_flip()
          }
        }
        return(plot)
      })

      output$plot <- shiny::renderPlot({
        p <- createPlot()
        if (is.null(p)) {
          msg <- if (nrow(private$.data) == 0) "Results files are empty." else "No data for selected filters."
          p <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg, size = 6) +
            ggplot2::theme_void()
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
