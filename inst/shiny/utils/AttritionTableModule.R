library(DarwinShinyModules)

AttritionTableModule <- R6::R6Class(
  classname = "AttritionTableModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name)) {
      super$initialize()
      # Add step_number (ordering of steps) per database
      if ("cdm_name" %in% colnames(data)) {
        private$.data <- data %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::mutate(step_number = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::relocate("step_number", .before = 1L)
      } else {
        private$.data <- data %>%
          dplyr::mutate(step_number = dplyr::row_number()) %>%
          dplyr::relocate("step_number", .before = 1L)
      }
      private$.dp <- dp

      # Single-select database filter (one CDM at a time)
      private$.inputPanelCDM <- createDatabasePicker(private$.dp, self$namespace, multiple = FALSE)
    }
  ),

  private = list(
    .data = NULL,
    .dp = NULL,
    .inputPanelCDM = NULL,

    .UI = function() {
      shiny::tagList(
        private$.inputPanelCDM$UI(),
        shiny::tabsetPanel(
          id = shiny::NS(private$.namespace, "attritionTabs"),
          shiny::tabPanel(
            "Plot",
            plotly::plotlyOutput(shiny::NS(private$.namespace, "attritionPlot"), height = "500px") %>%
              shinycssloaders::withSpinner()
          ),
          shiny::tabPanel(
            "Data",
            gt::gt_output(shiny::NS(private$.namespace, "attritionTable")) %>%
              shinycssloaders::withSpinner()
          )
        )
      )
    },

    .server = function(input, output, session) {
      # Table data: one CDM only, no cdm_name column
      tableData <- shiny::reactive({
        cdmSel <- private$.inputPanelCDM$inputValues$cdm_name
        if (is.null(cdmSel) || length(cdmSel) == 0) cdmSel <- private$.dp[1]
        d <- private$.data
        if ("cdm_name" %in% colnames(d)) {
          d <- d %>% dplyr::filter(.data$cdm_name %in% cdmSel)
          d <- d %>% dplyr::select(-"cdm_name")
        }
        d
      })

      output$attritionTable <- gt::render_gt({
        req(tableData())
        d <- tableData()
        if (is.null(d) || nrow(d) == 0) {
          return(gt::gt(dplyr::tibble(Message = "No data for selected database.")))
        }
        # Apply pretty column names
        displayNames <- colnames(d)
        matched <- displayNames %in% names(PRETTY_NAMES)
        displayNames[matched] <- PRETTY_NAMES[displayNames[matched]]
        colnames(d) <- displayNames

        # Format numeric columns
        numCols <- colnames(d)[vapply(d, function(x) is.numeric(x) || all(!is.na(suppressWarnings(as.numeric(x[!is.na(x)])))), logical(1))]
        for (col in numCols) {
          d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
        }

        tbl <- gt::gt(d) %>%
          gt::tab_options(
            table.font.size = gt::px(13),
            data_row.padding = gt::px(8),
            column_labels.font.weight = "bold"
          )

        # Format numeric columns with commas
        for (col in numCols) {
          tbl <- tbl %>% gt::fmt_number(columns = col, decimals = 0, use_seps = TRUE)
        }

        # Highlight step name column
        if ("Step" %in% colnames(d)) {
          tbl <- tbl %>%
            gt::tab_style(
              style = gt::cell_text(weight = "bold"),
              locations = gt::cells_body(columns = "Step")
            )
        }

        tbl
      })

      # Plot: waterfall/funnel showing record and person counts per step
      output$attritionPlot <- plotly::renderPlotly({
        d <- tableData()
        if (is.null(d) || nrow(d) == 0) {
          return(emptyPlotlyMessage("No attrition data for selected database."))
        }

        # Determine which count columns are available for the waterfall
        recordCol <- if ("post_records" %in% colnames(d)) "post_records" else NULL
        personCol <- if ("post_persons" %in% colnames(d)) "post_persons" else NULL
        stepCol <- if ("step" %in% colnames(d)) "step" else NULL

        if (is.null(stepCol) || (is.null(recordCol) && is.null(personCol))) {
          return(emptyPlotlyMessage("Attrition data does not contain step and post count columns."))
        }

        # Build plot data with records and persons as two series
        plotRows <- list()
        for (i in seq_len(nrow(d))) {
          stepLabel <- d[[stepCol]][i]
          if (!is.null(recordCol)) {
            plotRows[[length(plotRows) + 1L]] <- data.frame(
              step = stepLabel,
              step_num = i,
              series = "Records",
              count = suppressWarnings(as.numeric(d[[recordCol]][i])),
              stringsAsFactors = FALSE
            )
          }
          if (!is.null(personCol)) {
            plotRows[[length(plotRows) + 1L]] <- data.frame(
              step = stepLabel,
              step_num = i,
              series = "Persons",
              count = suppressWarnings(as.numeric(d[[personCol]][i])),
              stringsAsFactors = FALSE
            )
          }
        }
        plotData <- do.call(rbind, plotRows)
        plotData <- plotData %>%
          dplyr::filter(!is.na(.data$count)) %>%
          dplyr::mutate(
            step = factor(.data$step, levels = unique(.data$step)),
            label = paste0(.data$series, ": ", format(.data$count, big.mark = ","))
          )

        if (nrow(plotData) == 0) {
          return(emptyPlotlyMessage("No numeric count data available for plotting."))
        }

        # Grouped bar chart (horizontal): step on y, count on x, grouped by series
        p <- ggplot2::ggplot(plotData, ggplot2::aes(
          x = .data$count, y = .data$step,
          fill = .data$series,
          text = .data$label
        )) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::scale_x_continuous(labels = scales::comma_format()) +
          ggplot2::scale_fill_manual(values = c("Records" = "#377EB8", "Persons" = "#FF7F00")) +
          ggplot2::labs(x = "Count", y = NULL, fill = NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = 10),
            legend.position = "bottom"
          )
        plotly::ggplotly(p, tooltip = "text")
      })

      private$.inputPanelCDM$server(input, output, session)
    }
  )
)
