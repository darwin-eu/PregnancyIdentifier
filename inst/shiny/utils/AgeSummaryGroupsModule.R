library(DarwinShinyModules)

AgeSummaryGroupsModule <- R6::R6Class(
  classname = "AgeSummaryGroupsModule",
  inherit = ShinyModule,

  public = list(

    initialize = function(data, dp = unique(data$cdm_name), height = "420px") {
      super$initialize()
      private$.data <- data
      private$.dp <- dp
      private$.height <- height

      # Age bin column: second column in age_summary_groups (e.g. age_pregnancy_start)
      private$.ageCol <- names(data)[2]
      if (!private$.ageCol %in% names(data) || private$.ageCol %in% c("n", "total", "pct")) {
        private$.ageCol <- setdiff(names(data), c("colName", "n", "total", "pct", "cdm_name", "date_run", "date_export", "pkg_version"))[1]
      }

      private$.table <- Table$new(data = data, title = "age_summary_groups.csv", options = list(scrollX = TRUE, pageLength = 25))
      private$.table$parentNamespace <- self$namespace

      # Database filter
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

      # Outcome type filter (colName: e.g. age_pregnancy_start, PREG, LB)
      colNameChoices <- unique(data$colName)
      colNameChoices <- colNameChoices[!is.na(colNameChoices) & nzchar(colNameChoices)]
      private$.inputPanelColName <- InputPanel$new(
        fun = list(colName = shinyWidgets::pickerInput),
        args = list(colName = list(
          inputId = "colName", label = "Outcome type",
          choices = colNameChoices,
          selected = colNameChoices,
          multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        growDirection = "horizontal"
      )
      private$.inputPanelColName$parentNamespace <- self$namespace

      # Y axis: n or pct
      private$.inputPanelY <- InputPanel$new(
        fun = list(y_axis = shiny::radioButtons),
        args = list(y_axis = list(
          inputId = "y_axis",
          label = "Y axis",
          choices = c("n" = "n", "pct" = "pct"),
          selected = "n",
          inline = TRUE
        )),
        growDirection = "horizontal"
      )
      private$.inputPanelY$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .data = NULL,
    .dp = NULL,
    .height = NULL,
    .ageCol = NULL,
    .table = NULL,
    .inputPanelCDM = NULL,
    .inputPanelColName = NULL,
    .inputPanelY = NULL,

    .UI = function() {
      shiny::tagList(
        private$.inputPanelCDM$UI(),
        private$.inputPanelColName$UI(),
        private$.inputPanelY$UI(),
        plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>% shinycssloaders::withSpinner(),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      private$.table$server(input, output, session)
      private$.inputPanelCDM$server(input, output, session)
      private$.inputPanelColName$server(input, output, session)
      private$.inputPanelY$server(input, output, session)

      getData <- shiny::reactive({
        d <- private$.data
        if ("cdm_name" %in% colnames(d)) {
          d <- dplyr::filter(d, .data$cdm_name %in% private$.inputPanelCDM$inputValues$cdm_name)
        }
        if ("colName" %in% colnames(d)) {
          d <- dplyr::filter(d, .data$colName %in% private$.inputPanelColName$inputValues$colName)
        }
        d
      })

      getPlotData <- shiny::reactive({
        d <- getData()
        if (is.null(d) || nrow(d) == 0 || is.null(private$.ageCol) || !private$.ageCol %in% names(d)) {
          return(data.frame())
        }
        ageCol <- private$.ageCol
        d <- d %>%
          dplyr::mutate(
            n = as.numeric(.data$n),
            pct = as.numeric(.data$pct),
            age_raw = as.character(.data[[ageCol]]),
            age_num = suppressWarnings(as.numeric(.data[[ageCol]]))
          ) %>%
          dplyr::filter(!is.na(.data$age_num))
        if (nrow(d) == 0) return(data.frame())
        d
      })

      shiny::observeEvent(
        list(
          private$.inputPanelCDM$inputValues$cdm_name,
          private$.inputPanelColName$inputValues$colName
        ),
        {
          private$.table$data <- getData()
          private$.table$server(input, output, session)
        },
        ignoreNULL = FALSE
      )

      output$plot <- plotly::renderPlotly({
        d <- getPlotData()
        yChoice <- private$.inputPanelY$inputValues$y_axis
        if (is.null(d) || nrow(d) == 0) {
          return(emptyPlotlyMessage("No data for selected filters."))
        }
        if (!yChoice %in% c("n", "pct")) yChoice <- "n"

        # Aggregate by numeric age (and cdm_name if multiple databases)
        if ("cdm_name" %in% names(d) && dplyr::n_distinct(d$cdm_name) > 1) {
          dPlot <- d %>%
            dplyr::group_by(.data$age_num, .data$cdm_name) %>%
            dplyr::summarise(
              n = sum(.data$n, na.rm = TRUE),
              pct = mean(.data$pct, na.rm = TRUE),
              .groups = "drop"
            )
        } else {
          dPlot <- d %>%
            dplyr::group_by(.data$age_num) %>%
            dplyr::summarise(
              n = sum(.data$n, na.rm = TRUE),
              pct = mean(.data$pct, na.rm = TRUE),
              .groups = "drop"
            )
        }

        yLab <- if (yChoice == "pct") "Percent" else "Count (n)"
        dPlot$y_plot <- dPlot[[yChoice]]

        if ("cdm_name" %in% names(dPlot)) {
          p <- plotly::plot_ly() %>%
            plotly::layout(
              barmode = "overlay",
              bargap = 0,
              xaxis = list(title = "Age (years)", type = "linear", dtick = 5),
              yaxis = list(title = yLab),
              showlegend = TRUE
            )
          for (db in unique(dPlot$cdm_name)) {
            dDb <- dplyr::filter(dPlot, .data$cdm_name == !!db)
            p <- p %>%
              plotly::add_trace(
                data = dDb,
                x = ~age_num,
                y = ~y_plot,
                type = "bar",
                name = db,
                opacity = 0.7,
                hovertemplate = paste0("Age: %{x}<br>", yLab, ": %{y:.2f}<extra></extra>")
              )
          }
        } else {
          p <- plotly::plot_ly(
            data = dPlot,
            x = ~age_num,
            y = ~y_plot,
            type = "bar",
            hovertemplate = paste0("Age: %{x}<br>", yLab, ": %{y:.2f}<extra></extra>")
          ) %>%
            plotly::layout(
              xaxis = list(title = "Age (years)", type = "linear", dtick = 5),
              yaxis = list(title = yLab),
              showlegend = FALSE,
              bargap = 0
            )
        }
        p
      })
    }
  )
)
