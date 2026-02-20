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

      private$.inputPanelCDM <- InputPanel$new(
        fun = list(cdm_name = shinyWidgets::pickerInput),
        args = list(cdm_name = list(
          inputId = "cdm_name",
          label = "Database",
          choices = private$.dp,
          selected = private$.dp,
          multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.inputPanelCDM$parentNamespace <- self$namespace
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
          Percent = numeric(0)
        ))
      }
      data %>%
        dplyr::group_by(.data$cdm_name, .data$overlap) %>%
        dplyr::summarise(N = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
        dplyr::group_by(.data$cdm_name) %>%
        dplyr::mutate(Percent = round(100 * .data$N / sum(.data$N), 1)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(overlap_status = dplyr::case_when(
          .data$overlap == TRUE ~ "Overlapping",
          .data$overlap == FALSE ~ "Non-overlapping",
          is.na(.data$overlap) ~ "Not applicable (single episode)",
          TRUE ~ as.character(.data$overlap)
        )) %>%
        dplyr::select("cdm_name", "overlap_status", "N", "Percent")
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
                strong("Not applicable (single episode):"),
                " The person has only one pregnancy episode, so overlap is not defined."
              )
            )
          )
        ),
        private$.inputPanelCDM$UI(),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      private$.table$server(input, output, session)
      private$.inputPanelCDM$server(input, output, session)

      getData <- shiny::reactive({
        if (is.null(private$.summary) || nrow(private$.summary) == 0) {
          return(private$.summary)
        }
        private$.summary %>%
          dplyr::filter(.data$cdm_name %in% private$.inputPanelCDM$inputValues$cdm_name)
      })

      shiny::observeEvent(private$.inputPanelCDM$inputValues$cdm_name, {
        private$.table$data <- getData()
        private$.table$server(input, output, session)
      }, ignoreNULL = FALSE)
    }
  )
)
