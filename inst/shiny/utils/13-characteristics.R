# 13-characteristics.R - Characteristics module (standard Shiny module)

characteristicsUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-help-text", "Cohort characteristics and stratified summaries."),
    fluidRow(
      column(3, pickerInput(ns("cdm"), "Database", choices = character(0), selected = character(0), multiple = TRUE, options = opt)),
      column(3, pickerInput(ns("outcome"), "Outcome", choices = character(0), selected = character(0), multiple = TRUE, options = opt)),
      column(3, pickerInput(ns("strata"), "Strata", choices = character(0), selected = character(0), multiple = TRUE, options = opt)),
      column(3, pickerInput(ns("variable"), "Variable", choices = character(0), selected = character(0), multiple = TRUE, options = opt))
    ),
    fluidRow(
      column(4, pickerInput(ns("header"), "Header column",
                            choices = c("cdm_name", "estimate_name", "strata_name", "strata_level", "variable_name", "variable_level"),
                            selected = c("cdm_name", "estimate_name"), multiple = TRUE, options = opt)),
      column(4, pickerInput(ns("group"), "Group column",
                            choices = c("cdm_name", "strata_name", "strata_level", "variable_name", "variable_level"),
                            selected = c("strata_name", "strata_level"), multiple = TRUE, options = opt)),
      column(4, pickerInput(ns("hide"), "Hide column",
                            choices = c("cdm_name", "strata_name", "strata_level", "variable_name", "variable_level", "estimate_name"),
                            selected = c("variable_name", "variable_level", "estimate_name"), multiple = TRUE, options = opt))
    ),
    tabsetPanel(
      tabPanel("Table",
               downloadButton(ns("download_table_docx"), "Download table (.docx)"),
               gt::gt_output(ns("gtTable")) %>% withSpinner()),
      tabPanel("Plot", plotOutput(ns("plot"), height = "420px") %>% withSpinner())
    )
  )
}

characteristicsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize pickers from data
    observe({
      if (!exists("characteristics") || is.null(characteristics) || !is.data.frame(characteristics) || nrow(characteristics) == 0) return()
      d <- characteristics

      cdmChoices <- sort(unique(d$cdm_name))
      updatePickerInput(session, "cdm", choices = cdmChoices, selected = cdmChoices)

      outcomeChoices <- sort(unique(d$group_level))
      updatePickerInput(session, "outcome", choices = outcomeChoices, selected = outcomeChoices)

      strataChoices <- sort(unique(d$strata_level))
      updatePickerInput(session, "strata", choices = strataChoices, selected = strataChoices)

      variableChoices <- setdiff(unique(d$variable_name), c("Number records", "Number subjects"))
      updatePickerInput(session, "variable", choices = variableChoices, selected = variableChoices[1])
    })

    # Filtered data
    getData <- reactive({
      if (!exists("characteristics") || is.null(characteristics) || !is.data.frame(characteristics) || nrow(characteristics) == 0) {
        return(NULL)
      }
      d <- characteristics
      if (!is.null(input$cdm) && length(input$cdm) > 0) {
        d <- d %>% dplyr::filter(.data$cdm_name %in% input$cdm)
      }
      if (!is.null(input$outcome) && length(input$outcome) > 0) {
        d <- d %>% dplyr::filter(.data$group_level %in% input$outcome)
      }
      if (!is.null(input$strata) && length(input$strata) > 0) {
        d <- d %>% dplyr::filter(.data$strata_level %in% input$strata)
      }
      if (is.null(d) || nrow(d) == 0) return(NULL)
      d
    })

    # GT table
    table_gt <- reactive({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      tryCatch({
        CohortCharacteristics::tableCharacteristics(
          result = d,
          header = input$header,
          groupColumn = input$group,
          hide = input$hide,
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

    # Plot
    output$plot <- renderPlot({
      d <- getData()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      varSel <- input$variable
      if (is.null(varSel) || length(varSel) == 0) return(NULL)

      tryCatch({
        d %>%
          dplyr::filter(.data$variable_name %in% varSel) %>%
          CohortCharacteristics::plotCharacteristics(
            plotType = "boxplot",
            colour = "cohort_name",
            facet = c("cdm_name")
          )
      }, error = function(e) {
        NULL
      })
    })
  })
}
