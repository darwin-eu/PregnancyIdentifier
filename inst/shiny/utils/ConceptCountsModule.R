library(DarwinShinyModules)

#' Module for displaying concept counts with a horizontal bar chart and table.
#'
#' Works for ESD, HIP, and PPS concept count CSVs.
#' Shows a bar chart of top concepts by record count plus a searchable table.
ConceptCountsModule <- R6::R6Class(
  classname = "ConceptCountsModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = if ("cdm_name" %in% colnames(data)) unique(data$cdm_name) else character(0),
                          title = "Concept counts", height = "500px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.title <- title
      private$.height <- height

      private$.table <- Table$new(data = data, title = title)
      private$.table$parentNamespace <- self$namespace

      if (length(dp) > 0) {
        private$.inputPanelCDM <- createDatabasePicker(private$.dp, self$namespace)
      }
    }
  ),

  private = list(
    .data = NULL,
    .dp = NULL,
    .title = NULL,
    .height = NULL,
    .table = NULL,
    .inputPanelCDM = NULL,

    .UI = function() {
      shiny::tagList(
        if (!is.null(private$.inputPanelCDM)) private$.inputPanelCDM$UI(),
        shiny::tabsetPanel(
          id = shiny::NS(private$.namespace, "conceptTabs"),
          shiny::tabPanel(
            "Plot",
            shiny::sliderInput(
              shiny::NS(private$.namespace, "topN"),
              label = "Top N concepts",
              min = 5, max = 50, value = 20, step = 5
            ),
            plotly::plotlyOutput(shiny::NS(private$.namespace, "conceptPlot"), height = private$.height) %>%
              shinycssloaders::withSpinner()
          ),
          shiny::tabPanel(
            "Data",
            private$.table$UI()
          )
        )
      )
    },

    .server = function(input, output, session) {
      private$.table$server(input, output, session)
      if (!is.null(private$.inputPanelCDM)) {
        private$.inputPanelCDM$server(input, output, session)
      }

      getData <- shiny::reactive({
        d <- private$.data
        if (is.null(d) || nrow(d) == 0) return(d)
        if (!is.null(private$.inputPanelCDM) && "cdm_name" %in% colnames(d)) {
          cdmSel <- getSelectedCdm(private$.inputPanelCDM, private$.dp)
          d <- d %>% dplyr::filter(.data$cdm_name %in% cdmSel)
        }
        d
      })

      # Update table on filter change
      if (!is.null(private$.inputPanelCDM)) {
        shiny::observeEvent(private$.inputPanelCDM$inputValues$cdm_name, {
          private$.table$data <- getData()
          private$.table$server(input, output, session)
        }, ignoreNULL = FALSE)
      }

      output$conceptPlot <- plotly::renderPlotly({
        d <- getData()
        if (is.null(d) || nrow(d) == 0) {
          return(emptyPlotlyMessage("No concept count data available."))
        }

        # Find the concept name column (esd_concept_name, pps_concept_name, concept_name, etc.)
        nameCol <- intersect(colnames(d), c("concept_name", "esd_concept_name", "pps_concept_name"))[1]
        countCol <- intersect(colnames(d), c("record_count", "person_count"))[1]

        if (is.na(nameCol) || is.na(countCol)) {
          return(emptyPlotlyMessage("Expected concept_name and record_count columns not found."))
        }

        topN <- input$topN
        if (is.null(topN)) topN <- 20

        plotData <- d %>%
          dplyr::mutate(
            concept = as.character(.data[[nameCol]]),
            count = suppressWarnings(as.numeric(.data[[countCol]]))
          ) %>%
          dplyr::filter(!is.na(.data$count)) %>%
          dplyr::group_by(.data$concept) %>%
          dplyr::summarise(count = sum(.data$count, na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(dplyr::desc(.data$count)) %>%
          dplyr::slice_head(n = topN) %>%
          dplyr::mutate(
            concept = factor(.data$concept, levels = rev(.data$concept)),
            label = paste0(.data$concept, "\n", format(.data$count, big.mark = ","))
          )

        if (nrow(plotData) == 0) {
          return(emptyPlotlyMessage("No numeric count data available."))
        }

        p <- ggplot2::ggplot(plotData, ggplot2::aes(
          x = .data$concept, y = .data$count,
          text = .data$label
        )) +
          ggplot2::geom_bar(stat = "identity", fill = "#377EB8") +
          ggplot2::coord_flip() +
          ggplot2::labs(
            x = NULL,
            y = if (countCol == "person_count") "Person Count" else "Record Count"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9))
        plotly::ggplotly(p, tooltip = "text")
      })
    }
  )
)
