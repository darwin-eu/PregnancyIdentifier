library(DarwinShinyModules)

GestationalAgeDaysPerCategoryModule <- R6::R6Class(
  classname = "GestationalAgeDaysPerCategoryModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, plotData, dp = unique(data$cdm_name), height = "600px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.height <- height
      private$.table <- Table$new(data = data)
      private$.table$parentNamespace <- self$namespace

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
        funs = list(facet_by = shinyWidgets::pickerInput),
        args = list(facet_by = list(
          inputId = "facet_by", choices = plotDataChoices, label = "Facet by", selected = c("cdm_name"), multiple = FALSE
        )),
        growDirection = "horizontal"
      )
      private$.facetByInputPanel$parentNamespace <- self$namespace

      # color by
      private$.colorByInputPanel <- InputPanel$new(
        funs = list(color_by = shinyWidgets::pickerInput),
        args = list(color_by = list(
          inputId = "color_by", choices = plotDataChoices, label = "Colour by", selected = c("final_outcome_category"), multiple = FALSE
        )),
        growDirection = "horizontal"
      )
      private$.colorByInputPanel$parentNamespace <- self$namespace

      private$.maxInputPanel <- InputPanel$new(funs = list(max = shiny::checkboxInput),
                                                           args = list(max = list(
                                                             inputId = "max",
                                                             value = TRUE,
                                                             label = "IQR only"
                                                           )),
                                                           growDirection = "horizontal")
      private$.maxInputPanel$parentNamespace <- self$namespace

      private$.flipCoordinatesInputPanel <- InputPanel$new(funs = list(flip = shiny::checkboxInput),
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
    .table = NULL,
    .cdmInputPanel = NULL,
    .outcomeInputPanel = NULL,
    .facetByInputPanel = NULL,
    .colorByInputPanel = NULL,
    .maxInputPanel = NULL,
    .flipCoordinatesInputPanel = NULL,
    .height = NULL,

    .UI = function() {
      shiny::tagList(
        private$.cdmInputPanel$UI(),
        private$.outcomeInputPanel$UI(),
        p("Plotting options"),
        private$.facetByInputPanel$UI(),
        private$.colorByInputPanel$UI(),
        shiny::HTML("&nbsp;&nbsp;"),
        private$.maxInputPanel$UI(),
        shiny::HTML("&nbsp;&nbsp;"),
        private$.flipCoordinatesInputPanel$UI(),
        shiny::plotOutput(shiny::NS(private$.namespace, "plot"), height = private$.height),
        p(),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      # tables
      private$.table$server(input, output, session)
      # input filters
      private$.cdmInputPanel$server(input, output, session)
      private$.outcomeInputPanel$server(input, output, session)
      private$.facetByInputPanel$server(input, output, session)
      private$.colorByInputPanel$server(input, output, session)
      private$.maxInputPanel$server(input, output, session)
      private$.flipCoordinatesInputPanel$server(input, output, session)

      getData <- reactive({
        private$.data %>%
          dplyr::filter(.data$cdm_name %in% private$.cdmInputPanel$inputValues$cdm_name) %>%
          dplyr::filter(.data$final_outcome_category %in% private$.outcomeInputPanel$inputValues$outcome)
      })

      # handle updates
      shiny::observeEvent(c(private$.cdmInputPanel$inputValues$cdm_name,
                            private$.outcomeInputPanel$inputValues$outcome), {
        data <-  getData()

        # format
        if (nrow(data) > 0) {
          resultList <- lapply(unique(data$cdm_name), FUN = function(name) {
            dbData <- data %>%dplyr::filter(cdm_name == name)
            result <- dbData %>%
              dplyr::select(-c("cdm_name", "colName")) %>%
              tidyr::pivot_longer(cols = setdiff(colnames(.), c("final_outcome_category")), names_to = "name", values_to = unique(dbData$cdm_name))
            return(result)
          })
          resultList <- resultList[order(sapply(resultList, nrow), decreasing = T)]
          result <- purrr::reduce(resultList, dplyr::left_join)
        } else {
          result <- data.frame(final_outcome_category = character(0),
                               name = character(0))
        }

        private$.table$data <- result
        private$.table$server(input, output, session)
      }, ignoreNULL = FALSE)

      ### make plot ----
      createPlot <- reactive({
        plot <- NULL
        data <- getData()
        if (!is.null(data) && nrow(data) > 0) {
          colorBy <- private$.colorByInputPanel$inputValues$color_by
          iqrOnly <- private$.maxInputPanel$inputValues$max
          ymaxValue <- ifelse(iqrOnly, "Q75", "max")
          yminValue <- ifelse(iqrOnly, "Q25", "min")
          plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = colorBy, ymin = yminValue, lower = "Q25", middle = "median", upper = "Q75", ymax = ymaxValue)) +
            ggplot2::geom_boxplot(ggplot2::aes_string(fill = colorBy), stat = "identity")

          facetBy <- private$.facetByInputPanel$inputValues$facet_by
          if (!is.null(facetBy)) {
            plot <- plot +
              ggplot2::facet_wrap(sym(facetBy), scales = "free")
          }
          flipCoordinates <- private$.flipCoordinatesInputPanel$inputValues$flip
          if (flipCoordinates) {
            plot <- plot + ggplot2::coord_flip()
          }
        }
        return(plot)
      })

      output$plot <- renderPlot({
        createPlot()
      })
    }
  )
)
