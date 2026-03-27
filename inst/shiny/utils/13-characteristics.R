# 13-characteristics.R - Characteristics module (standard Shiny module)

characteristicsUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text", "Cohort characteristics and stratified summaries."),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database", choices = character(0), selected = character(0), multiple = TRUE, options = opt)),
      column(3, pickerInput(ns("outcome"), "Cohort name", choices = character(0), selected = character(0), multiple = TRUE, options = opt)),
      column(3, pickerInput(ns("age_group"), "Age group", choices = character(0), selected = character(0), multiple = TRUE, options = opt)),
      column(3, pickerInput(ns("final_outcome"), "Final outcome category", choices = character(0), selected = character(0), multiple = TRUE, options = opt))
    ),
    fluidRow(
      column(3, pickerInput(ns("strata"), "Strata", choices = character(0), selected = character(0), multiple = TRUE, options = opt)),
      column(3, pickerInput(ns("variable"), "Variable", choices = character(0), selected = character(0), multiple = TRUE, options = opt))
    ),
    tabsetPanel(
      tabPanel("Table",
               gt::gt_output(ns("gtTable")) %>% withSpinner(),
               downloadButton(ns("download_table_docx"), "Download table (.docx)")),
      tabPanel("Plot", plotOutput(ns("plot"), height = "420px") %>% withSpinner())
    )
  )
}

characteristicsServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      updatePickerInput(session, "cdm", choices = rv$allDP, selected = rv$allDP)
    })

    # Initialize pickers from data (guard against missing columns)
    observe({
      if (is.null(rv$characteristics) || !is.data.frame(rv$characteristics) || nrow(rv$characteristics) == 0) return()
      d <- rv$characteristics
      n <- names(d)

      if ("cdm_name" %in% n) {
        cdmChoices <- sort(unique(d$cdm_name))
        updatePickerInput(session, "cdm", choices = cdmChoices, selected = cdmChoices)
      }
      if ("group_level" %in% n) {
        outcomeChoices <- sort(unique(d$group_level))
        updatePickerInput(session, "outcome", choices = outcomeChoices, selected = outcomeChoices)
      }
      if ("strata_level" %in% n) {
        strataChoices <- sort(unique(d$strata_level))
        updatePickerInput(session, "strata", choices = strataChoices, selected = strataChoices)
      }
      if ("variable_name" %in% n) {
        variableChoices <- setdiff(unique(d$variable_name), c("Number records", "Number subjects"))
        if (length(variableChoices) > 0) {
          updatePickerInput(session, "variable", choices = variableChoices, selected = variableChoices[1])
        }
      }
      if ("denominator_age_group" %in% n) {
        ageChoices <- sort(unique(d$denominator_age_group))
        updatePickerInput(session, "age_group", choices = ageChoices, selected = ageChoices)
      } else if ("age_group" %in% n) {
        ageChoices <- sort(unique(d$age_group))
        updatePickerInput(session, "age_group", choices = ageChoices, selected = ageChoices)
      }
      if ("final_outcome_category" %in% n) {
        outcomeCatChoices <- sort(unique(as.character(d$final_outcome_category)))
        updatePickerInput(session, "final_outcome", choices = outcomeCatChoices, selected = outcomeCatChoices)
      }
    })

    # Filtered data (only filter by columns that exist)
    getData <- reactive({
      if (is.null(rv$characteristics) || !is.data.frame(rv$characteristics) || nrow(rv$characteristics) == 0) {
        return(NULL)
      }
      d <- rv$characteristics
      n <- names(d)
      if ("cdm_name" %in% n && !is.null(input$cdm) && length(input$cdm) > 0) {
        d <- d %>% dplyr::filter(.data$cdm_name %in% input$cdm)
      }
      if ("group_level" %in% n && !is.null(input$outcome) && length(input$outcome) > 0) {
        d <- d %>% dplyr::filter(.data$group_level %in% input$outcome)
      }
      if ("strata_level" %in% n && !is.null(input$strata) && length(input$strata) > 0) {
        d <- d %>% dplyr::filter(.data$strata_level %in% input$strata)
      }
      if ("denominator_age_group" %in% n && !is.null(input$age_group) && length(input$age_group) > 0) {
        d <- d %>% dplyr::filter(.data$denominator_age_group %in% input$age_group)
      } else if ("age_group" %in% n && !is.null(input$age_group) && length(input$age_group) > 0) {
        d <- d %>% dplyr::filter(.data$age_group %in% input$age_group)
      }
      if ("final_outcome_category" %in% n && !is.null(input$final_outcome) && length(input$final_outcome) > 0) {
        d <- d %>% dplyr::filter(.data$final_outcome_category %in% input$final_outcome)
      }
      if (is.null(d) || nrow(d) == 0) return(NULL)
      d
    })

    # GT table: use only columns allowed by tableCharacteristics for this result
    table_gt <- reactive({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      tryCatch({
        CohortCharacteristics::tableCharacteristics(
          result = d,
          .options = list(style = "darwin")
        )
      }, error = function(e) {
        gt::gt(data.frame(Message = paste("Could not render table:", e$message)))
      })
    })
    output$gtTable <- gt::render_gt({
      table_gt()
    })
    output$download_table_docx <- downloadHandler(
      filename = function() { "characteristics_table.docx" },
      content = function(file) {
        tbl <- table_gt()
        if (!is.null(tbl)) gt::gtsave(tbl, file)
      }
    )

    # Plot (use only columns that exist in result)
    output$plot <- renderPlot({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      n <- names(d)
      if (!("variable_name" %in% n)) return(NULL)
      varSel <- input$variable
      if (is.null(varSel) || length(varSel) == 0) return(NULL)

      tryCatch({
        plotData <- d %>% dplyr::filter(.data$variable_name %in% varSel)
        if (nrow(plotData) == 0) return(NULL)
        plotCols <- tryCatch(CohortCharacteristics::availablePlotColumns(plotData), error = function(e) character(0))
        colourVar <- if ("group_level" %in% plotCols) "group_level" else if ("cohort_name" %in% plotCols) "cohort_name" else NULL
        facetVar <- if ("cdm_name" %in% plotCols) "cdm_name" else NULL
        if (is.null(colourVar)) return(NULL)
        CohortCharacteristics::plotCharacteristics(
          result = plotData,
          plotType = "boxplot",
          facet = if (length(facetVar) > 0) facetVar else NULL,
          colour = colourVar
        )
      }, error = function(e) {
        NULL
      })
    })
  })
}
