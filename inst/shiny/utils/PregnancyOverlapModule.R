library(DarwinShinyModules)

PregnancyOverlapModule <- R6::R6Class(
  classname = "PregnancyOverlapModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name)) {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.summary <- private$.buildSummary(data)
      private$.table <- Table$new(
        data = private$.summary,
        title = "Pregnancy overlap summary",
        options = list(scrollX = TRUE, dom = "t")
      )
      private$.table$parentNamespace <- self$namespace

      private$.inputPanelCDM <- createDatabasePicker(private$.dp, self$namespace)
    }
  ),

  private = list(
    .data = NULL,
    .dp = NULL,
    .summary = NULL,
    .table = NULL,
    .inputPanelCDM = NULL,

    .buildSummary = function(data) {
      if (is.null(data) || nrow(data) == 0) {
        return(data.frame(
          cdm_name = character(0),
          overlap_status = character(0),
          N = integer(0),
          pct = numeric(0)
        ))
      }
      data %>%
        dplyr::group_by(.data$cdm_name, .data$overlap) %>%
        dplyr::summarise(N = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
        dplyr::group_by(.data$cdm_name) %>%
        dplyr::mutate(pct = round(100 * .data$N / sum(.data$N), 1)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(overlap_status = dplyr::case_when(
          .data$overlap == TRUE ~ "Overlapping",
          .data$overlap == FALSE ~ "Non-overlapping",
          is.na(.data$overlap) ~ "Single episode (N/A)",
          TRUE ~ as.character(.data$overlap)
        )) %>%
        dplyr::select("cdm_name", "overlap_status", "N", "pct")
    },

    .UI = function() {
      shiny::tagList(
        shiny::div(
          class = "well",
          shiny::p(
            strong("What the overlap values mean:"),
            shiny::tags$ul(
              shiny::tags$li(
                strong("Overlapping:"),
                " This pregnancy episode overlaps in time with at least one other episode for the same person. May indicate a data or algorithm issue."
              ),
              shiny::tags$li(
                strong("Non-overlapping:"),
                " This pregnancy episode does not overlap with any other episode for the same person."
              ),
              shiny::tags$li(
                strong("Single episode (N/A):"),
                " The person has only one pregnancy episode, so overlap is not defined."
              )
            )
          )
        ),
        private$.inputPanelCDM$UI(),
        shiny::tabsetPanel(
          id = shiny::NS(private$.namespace, "overlapTabs"),
          shiny::tabPanel(
            "Plot",
            plotly::plotlyOutput(shiny::NS(private$.namespace, "overlapPlot"), height = "420px") %>%
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
      private$.inputPanelCDM$server(input, output, session)

      getData <- shiny::reactive({
        if (is.null(private$.summary) || nrow(private$.summary) == 0) {
          return(private$.summary)
        }
        cdmSel <- getSelectedCdm(private$.inputPanelCDM, private$.dp)
        private$.summary %>%
          dplyr::filter(.data$cdm_name %in% cdmSel)
      })

      shiny::observeEvent(private$.inputPanelCDM$inputValues$cdm_name, {
        private$.table$data <- getData()
        private$.table$server(input, output, session)
      }, ignoreNULL = FALSE)

      output$overlapPlot <- plotly::renderPlotly({
        d <- getData()
        if (is.null(d) || nrow(d) == 0) {
          return(emptyPlotlyMessage("No pregnancy overlap data for selected filters."))
        }
        # Stacked bar chart: x = database, y = %, fill = overlap status
        overlapColours <- c(
          "Overlapping"         = "#E41A1C",
          "Non-overlapping"     = "#4DAF4A",
          "Single episode (N/A)" = "#999999"
        )
        d <- d %>%
          dplyr::mutate(overlap_status = factor(
            .data$overlap_status,
            levels = c("Overlapping", "Non-overlapping", "Single episode (N/A)")
          ))
        p <- ggplot2::ggplot(d, ggplot2::aes(
          x = .data$cdm_name, y = .data$pct,
          fill = .data$overlap_status,
          text = paste0(.data$overlap_status, "\n",
                        "N: ", format(.data$N, big.mark = ","), "\n",
                        round(.data$pct, 1), "%")
        )) +
          ggplot2::geom_bar(stat = "identity", position = "stack") +
          ggplot2::scale_fill_manual(values = overlapColours, name = "Overlap Status") +
          ggplot2::labs(x = "Database", y = "Percentage (%)") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
        plotly::ggplotly(p, tooltip = "text")
      })
    }
  )
)
