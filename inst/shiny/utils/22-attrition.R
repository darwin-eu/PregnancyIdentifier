# 22-attrition.R - Attrition module (standard Shiny module)
# Data: passed as parameter (attritionEpisodes or attritionIfCleanup)
# Columns: cdm_name, step_number, step, outcome, prior_records, prior_persons,
#           dropped_records, dropped_persons, post_records, post_persons

attritionUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database",
                           choices = character(0), selected = character(0),
                           multiple = FALSE))
    ),
    tabsetPanel(
      tabPanel("Table",
               gt::gt_output(ns("gtTable")) %>% withSpinner(),
               downloadButton(ns("download_table_docx"), "Download table (.docx)")),
      tabPanel("Plot",
               plotly::plotlyOutput(ns("plot"), height = "600px") %>% withSpinner(),
               h4("Download figure"),
               fluidRow(
                 column(3, textInput(ns("download_height"), "Height (cm)", value = "10")),
                 column(3, textInput(ns("download_width"), "Width (cm)", value = "20")),
                 column(3, textInput(ns("download_dpi"), "Resolution (dpi)", value = "300"))
               ),
               downloadButton(ns("download_plot"), "Download plot (PNG)"))
    )
  )
}

attritionServer <- function(id, rv, dataKey) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePickerInput(session, "cdm", choices = rv$allDP, selected = rv$allDP[1])
    })

    preparedData <- reactive({
      data <- rv[[dataKey]]
      if (is.null(data) || nrow(data) == 0) return(data.frame())
      # Ensure step_number exists
      if (!"step_number" %in% colnames(data) && "cdm_name" %in% colnames(data)) {
        data <- data %>%
          dplyr::group_by(.data$cdm_name) %>%
          dplyr::mutate(step_number = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::relocate("step_number", .before = 1L)
      } else if (!"step_number" %in% colnames(data)) {
        data <- data %>%
          dplyr::mutate(step_number = dplyr::row_number()) %>%
          dplyr::relocate("step_number", .before = 1L)
      }
      data
    })

    getData <- reactive({
      data <- preparedData()
      if (is.null(data) || nrow(data) == 0) return(data.frame())
      dp <- if ("cdm_name" %in% colnames(data)) unique(data$cdm_name) else character(0)
      sel <- input$cdm
      if (is.null(sel) || !nzchar(sel)) sel <- dp[1]
      if (!"cdm_name" %in% colnames(data)) return(data)
      d <- data %>% dplyr::filter(.data$cdm_name == sel)
      d %>% dplyr::select(-"cdm_name")
    })

    table_gt <- reactive({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) {
        return(gt::gt(data.frame(Message = "No attrition data.")))
      }

      # Format numeric columns
      numCols <- c("prior_records", "prior_persons", "dropped_records",
                    "dropped_persons", "post_records", "post_persons")
      for (col in intersect(numCols, colnames(d))) {
        d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
      }

      # Apply pretty column names
      displayNames <- colnames(d)
      matched <- displayNames %in% names(PRETTY_NAMES)
      displayNames[matched] <- PRETTY_NAMES[displayNames[matched]]
      colnames(d) <- displayNames

      # Identify numeric columns for formatting
      numericDisplayCols <- colnames(d)[vapply(d, function(x) {
        is.numeric(x) || all(!is.na(suppressWarnings(as.numeric(x[!is.na(x)]))))
      }, logical(1))]
      for (col in numericDisplayCols) {
        d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
      }

      tbl <- gt::gt(d) %>%
        gt::tab_options(
          table.font.size = gt::px(13),
          data_row.padding = gt::px(8),
          column_labels.font.weight = "bold"
        )

      for (col in numericDisplayCols) {
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
    output$gtTable <- gt::render_gt({
      table_gt()
    })
    output$download_table_docx <- downloadHandler(
      filename = function() { "attrition_table.docx" },
      content = function(file) {
        tbl <- table_gt()
        if (!is.null(tbl)) gt::gtsave(tbl, file)
      }
    )

    attrition_plot_ggplot <- reactive({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)

      recordCol <- if ("post_records" %in% colnames(d)) "post_records" else NULL
      personCol <- if ("post_persons" %in% colnames(d)) "post_persons" else NULL
      stepCol <- if ("step" %in% colnames(d)) "step" else NULL

      if (is.null(stepCol) || (is.null(recordCol) && is.null(personCol))) return(NULL)

      numCols <- c("post_records", "post_persons")
      for (col in intersect(numCols, colnames(d))) {
        d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
      }

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

      if (nrow(plotData) == 0) return(NULL)

      ggplot2::ggplot(plotData, ggplot2::aes(
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
    })

    output$plot <- plotly::renderPlotly({
      p <- attrition_plot_ggplot()
      if (is.null(p)) return(emptyPlotlyMessage("No attrition data."))
      plotly::ggplotly(p, tooltip = "text")
    })

    output$download_plot <- downloadHandler(
      filename = function() { "attritionPlot.png" },
      content = function(file) {
        p <- attrition_plot_ggplot()
        if (!is.null(p)) {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = as.numeric(input$download_width),
            height = as.numeric(input$download_height),
            dpi = as.numeric(input$download_dpi),
            units = "cm"
          )
        }
      }
    )
  })
}
