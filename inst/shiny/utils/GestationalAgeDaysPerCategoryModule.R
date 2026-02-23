library(DarwinShinyModules)
library(ggplot2)

GestationalAgeDaysPerCategoryModule <- R6::R6Class(
  classname = "GestationalAgeDaysPerCategoryModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, plotData = NULL, dp = unique(data$cdm_name), height = "420px") {
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
        plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>% shinycssloaders::withSpinner(),
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
      # ggplot2 boxplot with pre-computed quartiles, then plotly::ggplotly() for one legend per category and facet labels.
      createPlotly <- shiny::reactive({
        data <- getData()
        if (is.null(data) || nrow(data) == 0) return(NULL)

        colorBy <- private$.colorByInputPanel$inputValues$color_by
        if (is.null(colorBy) || length(colorBy) == 0) colorBy <- "final_outcome_category"
        facetBy <- private$.facetByInputPanel$inputValues$facet_by
        if (is.null(facetBy) || length(facetBy) == 0) facetBy <- "cdm_name"
        iqrOnly <- private$.maxInputPanel$inputValues$max
        if (is.null(iqrOnly)) iqrOnly <- TRUE
        flipCoordinates <- private$.flipCoordinatesInputPanel$inputValues$flip
        if (is.null(flipCoordinates)) flipCoordinates <- TRUE

        outcomeLevels <- c("ECT", "AB", "SA", "SB", "DELIV", "LB", "PREG")
        plot_data <- data %>%
          dplyr::group_by(.data$cdm_name, .data$final_outcome_category) %>%
          dplyr::slice(1L) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            final_outcome_category = factor(.data$final_outcome_category, levels = outcomeLevels),
            ymin_ = if (iqrOnly) .data$Q25 else .data$min,
            ymax_ = if (iqrOnly) .data$Q75 else .data$max
          )
        if (nrow(plot_data) == 0) return(NULL)

        # One color per category; use fill so legend has a single entry per category
        fill_var <- colorBy
        n_cats <- length(unique(plot_data[[fill_var]]))
        pal <- scales::hue_pal()(max(1L, n_cats))
        if (n_cats >= 1) names(pal) <- sort(unique(as.character(plot_data[[fill_var]])))

        gg <- ggplot2::ggplot(
          plot_data,
          ggplot2::aes(
            x = .data$final_outcome_category,
            ymin = .data$ymin_,
            lower = .data$Q25,
            middle = .data$median,
            upper = .data$Q75,
            ymax = .data$ymax_,
            fill = .data[[fill_var]]
          )
        ) +
          ggplot2::geom_boxplot(stat = "identity") +
          ggplot2::scale_fill_manual(values = pal, name = if (colorBy == "final_outcome_category") "Outcome category" else "Database") +
          ggplot2::labs(
            x = "Final outcome category",
            y = "Gestational age (days)",
            title = NULL
          ) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            strip.text = ggplot2::element_text(size = 12, face = "bold"),
            legend.position = "right"
          )

        # Facet by selected variable so each subplot shows its label (e.g. cdm_name)
        facet_sym <- rlang::sym(facetBy)
        gg <- gg + ggplot2::facet_wrap(ggplot2::vars(!!facet_sym), labeller = ggplot2::as_labeller(as.character))

        if (flipCoordinates) {
          gg <- gg + ggplot2::coord_flip()
        }

        plotly::ggplotly(gg, tooltip = c("ymin", "lower", "middle", "upper", "ymax"))
      })

      output$plot <- plotly::renderPlotly({
        p <- createPlotly()
        if (is.null(p)) {
          msg <- if (nrow(private$.data) == 0) "Results files are empty." else "No data for selected filters."
          fig <- plotly::plot_ly()
          fig <- plotly::layout(fig, annotations = list(text = msg, xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16)))
          return(fig)
        }
        p
      })

      output$dataTable <- DT::renderDT({
        DT::datatable(
          private$.data %>% dplyr::mutate_if(is.character, as.factor),
          options = list(scrollX = TRUE, pageLength = 25),
          filter = "top"
        )
      })
    }
  )
)
