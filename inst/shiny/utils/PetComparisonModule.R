# Copyright (c) 2024–2025 DARWIN EU®
# PET comparison: single module (Overview, Plot, Table, Summarised result).

library(DarwinShinyModules)

# ---- Overview markdown ----
PET_COMPARISON_OVERVIEW_MD <- "
## What is compared

This section compares **pregnancy episodes from the PregnancyIdentifier algorithm** with **pregnancy episodes in the OMOP Pregnancy Extension Table (PET)**. The PET is treated as the **reference (gold standard) for positive episodes** only: there is no negative class (no non-pregnancy episodes), so we can only compute **sensitivity** and **positive predictive value (PPV)**. Specificity and NPV are not defined here.

---

## How the comparison is done

1. **Sources**
   - **PET:** Episodes from the PET table (person, start/end dates, outcome).
   - **Algorithm:** Episodes from the pipeline output (e.g. `final_pregnancy_episodes.rds`).

2. **Matching**
   - Within each person, every PET episode is paired with every algorithm episode whose date range **overlaps by at least a minimum number of days** (e.g. 1 day).
   - **One-to-one assignment:** Among these candidate pairs, a greedy algorithm assigns each PET episode to at most one algorithm episode (and vice versa), choosing pairs by **largest overlap first**. This avoids double-counting and gives consistent counts for Venn and confusion metrics.

3. **Resulting counts**
   - **Both:** PET episode and algorithm episode matched (one-to-one).
   - **PET only:** PET episode with no matched algorithm episode.
   - **Algorithm only:** Algorithm episode with no matched PET episode.

---

## What is calculated

| Metric | Meaning |
|--------|--------|
| **Episode counts** | Total episodes in PET and total from the algorithm (optionally filtered, e.g. gestation 0–308 days, end ≥ start). |
| **Venn counts** | Number of episodes in **both**, **PET only**, and **algorithm only** (from the one-to-one matching). |
| **Sensitivity** | Among all PET episodes, the proportion that have a matched algorithm episode: *TP / (TP + FN)*. |
| **PPV** | Among all algorithm episodes, the proportion that are matched to a PET episode (true positives): *TP / (TP + FP)*, where FP = algorithm-only episodes. |
| **Time overlap** | For each PET (or algorithm) episode, the maximum overlap in days with any algorithm (or PET) episode; summarized (min, quartiles, max). |
| **Confusion 2×2** | Counts for TP, FN, FP; TN is not defined (no gold-standard negatives). |
| **Outcome accuracy** | Among matched pairs, how often the algorithm outcome category agrees with the PET outcome. |
| **Duration** | Distribution of pregnancy duration (days) for PET and algorithm episodes; for matched pairs, duration of PET vs algorithm. |

---

## Person-level comparison

Persons are classified into three groups:
- **Both HIPPS and PET**: Persons who have episodes in both sources.
- **HIPPS only**: Persons who have algorithm episodes but no PET episodes at all.
- **PET only**: Persons who have PET episodes but no algorithm episodes at all.

For each group, the Person-level tab shows:
- Number of distinct persons (Venn diagram)
- Distribution of pregnancies per person (mean, median, min, max, IQR)

---

## Sub-tabs in this section

- **Overview** (this page): Methodology and interpretation.
- **Plot:** Venn diagram by database (PET episodes vs algorithm episodes, overlap = both).
- **Person-level:** Person-level Venn diagram and episodes-per-person summary.
- **Table:** Formatted table of all comparison metrics (visOmopResults), with a **Database** filter.
- **Summarised result:** Raw summarised result table (download as CSV).
"

# ---- Venn helper ----
#' Build Venn diagram data for amVennDiagram5 from counts.
pet_venn_data_from_counts <- function(both, pet_only, algorithm_only) {
  both <- max(0L, as.integer(both))
  pet_only <- max(0L, as.integer(pet_only))
  algorithm_only <- max(0L, as.integer(algorithm_only))
  n_pet <- pet_only + both
  n_alg <- algorithm_only + both
  if (n_pet == 0 && n_alg == 0) {
    return(amVennDiagram5::makeVennData(list(
      "PET" = integer(0),
      "HIPPS" = integer(0)
    )))
  }
  pet_ids <- if (n_pet > 0) seq_len(n_pet) else integer(0)
  alg_ids <- if (n_alg > 0) (pet_only + 1L):(pet_only + both + algorithm_only) else integer(0)
  amVennDiagram5::makeVennData(list(
    "PET" = pet_ids,
    "HIPPS" = alg_ids
  ))
}

#' Extract Venn counts (both, pet_only, algorithm_only) per cdm_name from summarised result.
extract_venn_data_from_sr <- function(sr, variable = "venn_counts", estimate = "n_episodes") {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  vennWide <- tbl %>%
    dplyr::filter(.data$variable_name == variable, .data$estimate_name == estimate) %>%
    dplyr::select("cdm_name", "variable_level", "estimate_value")
  if (nrow(vennWide) == 0) return(NULL)
  pw <- vennWide %>%
    tidyr::pivot_wider(names_from = "variable_level", values_from = "estimate_value")
  for (col in c("both", "pet_only", "algorithm_only")) {
    if (!col %in% names(pw)) pw[[col]] <- NA_character_
  }
  pw %>%
    dplyr::mutate(
      both = suppressWarnings(as.integer(dplyr::coalesce(.data$both, "0"))),
      pet_only = suppressWarnings(as.integer(dplyr::coalesce(.data$pet_only, "0"))),
      algorithm_only = suppressWarnings(as.integer(dplyr::coalesce(.data$algorithm_only, "0")))
    ) %>%
    dplyr::select("cdm_name", "both", "pet_only", "algorithm_only")
}

#' Extract person-level episodes-per-person summary from summarised result.
extract_person_epp_from_sr <- function(sr) {
  tbl <- as.data.frame(sr)
  if (!is.data.frame(tbl) || !"variable_name" %in% names(tbl)) return(NULL)
  epp <- tbl %>%
    dplyr::filter(.data$variable_name == "person_episodes_per_person") %>%
    dplyr::select("cdm_name", "variable_level", "estimate_name", "estimate_value")
  if (nrow(epp) == 0) return(NULL)
  epp %>%
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("mean", "median", "sd", "min", "q25", "q75", "max")),
                                ~ suppressWarnings(as.numeric(.x)))) %>%
    dplyr::mutate(
      group = dplyr::case_when(
        .data$variable_level == "both:algorithm" ~ "Both (HIPPS episodes)",
        .data$variable_level == "both:pet" ~ "Both (PET episodes)",
        .data$variable_level == "algorithm_only" ~ "HIPPS only",
        .data$variable_level == "pet_only" ~ "PET only",
        TRUE ~ .data$variable_level
      )
    )
}

# ---- Main PET comparison module (Overview + Plot + Table + Summarised result) ----
#' Single PET comparison module: Overview, Plot (Venn), Table (visOmopTable), Summarised result (raw).
PetComparisonModule <- R6::R6Class(
  classname = "PetComparisonModule",
  inherit = ShinyModule,

  public = list(
    #' @param result Summarised result from PET comparison (e.g. omopgenerics::importSummarisedResult).
    initialize = function(result) {
      super$initialize()
      private$.result <- result
      private$.venn_data <- extract_venn_data_from_sr(result, "venn_counts", "n_episodes")
      private$.person_venn_data <- extract_venn_data_from_sr(result, "person_venn_counts", "n_persons")
      private$.person_epp_data <- extract_person_epp_from_sr(result)
      tbl <- as.data.frame(result)
      private$.cdm_names <- if (is.data.frame(tbl) && "cdm_name" %in% names(tbl)) unique(tbl$cdm_name) else character(0)
      private$.overview_text <- DarwinShinyModules::Text$new(markdown = PET_COMPARISON_OVERVIEW_MD)
      private$.overview_text$parentNamespace <- self$namespace
      private$.raw_table <- Table$new(
        data = result,
        title = "Summarised result",
        options = list(scrollX = TRUE, pageLength = 25)
      )
      private$.raw_table$parentNamespace <- self$namespace
      private$.input_panel_cdm <- InputPanel$new(
        fun = list(cdm_name = shinyWidgets::pickerInput),
        args = list(cdm_name = list(
          inputId = "cdm_name",
          label = "Database",
          choices = private$.cdm_names,
          selected = private$.cdm_names,
          multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.input_panel_cdm$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .result = NULL,
    .venn_data = NULL,
    .person_venn_data = NULL,
    .person_epp_data = NULL,
    .cdm_names = NULL,
    .overview_text = NULL,
    .raw_table = NULL,
    .input_panel_cdm = NULL,

    .UI = function() {
      ns <- shiny::NS(private$.namespace)
      has_venn <- !is.null(private$.venn_data) && nrow(private$.venn_data) > 0
      db_choices <- stats::setNames(private$.cdm_names, private$.cdm_names)

      overview_ui <- shiny::tagList(
        shiny::tags$style(
          paste(
            ".pet-comparison-overview h2 { margin-top: 1.2em; margin-bottom: 0.5em; font-size: 1.25em; }",
            ".pet-comparison-overview h2:first-child { margin-top: 0; }",
            ".pet-comparison-overview h3 { margin-top: 1em; margin-bottom: 0.4em; font-size: 1.1em; }",
            ".pet-comparison-overview p { margin-bottom: 0.8em; line-height: 1.5; }",
            ".pet-comparison-overview ul { margin-bottom: 0.8em; padding-left: 1.5em; }",
            ".pet-comparison-overview hr { margin: 1.5em 0; border: 0; border-top: 1px solid #ddd; }",
            ".pet-comparison-overview table { border-collapse: collapse; width: 100%; margin: 1em 0; }",
            ".pet-comparison-overview th, .pet-comparison-overview td { border: 1px solid #ddd; padding: 0.5em 0.75em; text-align: left; }",
            ".pet-comparison-overview th { background: #f5f5f5; font-weight: 600; }",
            sep = " "
          )
        ),
        shiny::div(
          class = "pet-comparison-overview",
          style = "max-width: 900px; margin-bottom: 2em; line-height: 1.5; color: #333;",
          private$.overview_text$UI()
        )
      )

      plot_ui <- if (has_venn) {
        shiny::tagList(
          if (length(private$.cdm_names) > 1) {
            shiny::fluidRow(
              shiny::column(12, shiny::selectInput(ns("database"), "Database", choices = db_choices, selected = db_choices[1]))
            )
          },
          shiny::fluidRow(
            shiny::column(12, amVennDiagram5::amVennDiagramOutput(ns("venn"), width = "100%", height = "450px") %>% shinycssloaders::withSpinner())
          )
        )
      } else {
        shiny::p("No Venn data available.")
      }

      table_ui <- shiny::tagList(
        private$.input_panel_cdm$UI(),
        shiny::br(),
        gt::gt_output(ns("visTable")) %>% shinycssloaders::withSpinner()
      )

      summarised_ui <- private$.raw_table$UI()

      # Person-level tab
      has_person_venn <- !is.null(private$.person_venn_data) && nrow(private$.person_venn_data) > 0
      person_ui <- if (has_person_venn) {
        shiny::tagList(
          if (length(private$.cdm_names) > 1) {
            shiny::fluidRow(
              shiny::column(12, shiny::selectInput(ns("person_database"), "Database", choices = db_choices, selected = db_choices[1]))
            )
          },
          shiny::h4("Person-level Venn diagram"),
          shiny::p("Persons grouped by whether they have episodes in both HIPPS and PET, HIPPS only, or PET only."),
          shiny::fluidRow(
            shiny::column(12, amVennDiagram5::amVennDiagramOutput(ns("person_venn"), width = "100%", height = "400px") %>% shinycssloaders::withSpinner())
          ),
          shiny::hr(),
          shiny::h4("Episodes per person by group"),
          DT::DTOutput(ns("person_epp_table")) %>% shinycssloaders::withSpinner()
        )
      } else {
        shiny::p("No person-level comparison data available. Re-run the PET comparison to generate this data.")
      }

      shiny::tabsetPanel(
        id = ns("petTabs"),
        type = "tabs",
        shiny::tabPanel("Overview", overview_ui),
        shiny::tabPanel("Plot", plot_ui),
        shiny::tabPanel("Person-level", person_ui),
        shiny::tabPanel("Table", table_ui),
        shiny::tabPanel("Summarised result", summarised_ui)
      )
    },

    .server = function(input, output, session) {
      ns <- session$ns
      if (is.function(private$.overview_text$server)) {
        private$.overview_text$server(input, output, session)
      }
      private$.raw_table$server(input, output, session)
      private$.input_panel_cdm$server(input, output, session)

      # Plot: Venn
      if (!is.null(private$.venn_data) && nrow(private$.venn_data) > 0) {
        venn_data <- private$.venn_data
        cdm_names <- private$.cdm_names
        chosen_db <- shiny::reactive({
          if (length(cdm_names) == 1) cdm_names[1] else input$database
        })
        venn_plot_data <- shiny::reactive({
          db <- chosen_db()
          row <- venn_data %>% dplyr::filter(.data$cdm_name == .env$db)
          if (nrow(row) == 0) return(NULL)
          pet_venn_data_from_counts(row$both[1], row$pet_only[1], row$algorithm_only[1])
        })
        output$venn <- amVennDiagram5::renderAmVennDiagram({
          vd <- venn_plot_data()
          if (is.null(vd)) return(NULL)
          amVennDiagram5::amVennDiagram(vd, theme = "default", legendPosition = "bottom")
        })
      }

      # Person-level: Venn + EPP table
      if (!is.null(private$.person_venn_data) && nrow(private$.person_venn_data) > 0) {
        person_venn_data <- private$.person_venn_data
        person_epp_data <- private$.person_epp_data
        cdm_names_local <- private$.cdm_names
        chosen_person_db <- shiny::reactive({
          if (length(cdm_names_local) == 1) cdm_names_local[1] else input$person_database
        })
        person_venn_plot_data <- shiny::reactive({
          db <- chosen_person_db()
          row <- person_venn_data %>% dplyr::filter(.data$cdm_name == .env$db)
          if (nrow(row) == 0) return(NULL)
          pet_venn_data_from_counts(row$both[1], row$pet_only[1], row$algorithm_only[1])
        })
        output$person_venn <- amVennDiagram5::renderAmVennDiagram({
          vd <- person_venn_plot_data()
          if (is.null(vd)) return(NULL)
          amVennDiagram5::amVennDiagram(vd, theme = "default", legendPosition = "bottom")
        })
        output$person_epp_table <- DT::renderDT({
          db <- chosen_person_db()
          if (is.null(person_epp_data)) return(NULL)
          tbl <- person_epp_data %>%
            dplyr::filter(.data$cdm_name == .env$db) %>%
            dplyr::select("group", dplyr::any_of(c("mean", "median", "sd", "min", "q25", "q75", "max"))) %>%
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2)))
          DT::datatable(tbl, rownames = FALSE, options = list(dom = "t", pageLength = 10))
        })
      }

      # Table: visOmopTable with database filter
      filtered_result <- shiny::reactive({
        sel <- private$.input_panel_cdm$inputValues$cdm_name
        if (is.null(sel) || length(sel) == 0) sel <- private$.cdm_names
        res <- private$.result %>% dplyr::filter(.data$cdm_name %in% sel)
        if (!inherits(res, "summarised_result") && inherits(private$.result, "summarised_result")) {
          res <- omopgenerics::newSummarisedResult(
            tibble::as_tibble(res),
            omopgenerics::settings(private$.result)
          )
        }
        res
      })
      output$visTable <- gt::render_gt({
        req(filtered_result())
        sr <- filtered_result()
        if (nrow(as.data.frame(sr)) == 0) {
          return(gt::gt(dplyr::tibble(Message = "No data for selected database(s).")))
        }
        visOmopResults::visOmopTable(
          result = sr,
          header = c("cdm_name"),
          groupColumn = c("variable_name", "variable_level"),
          rename = c("Database" = "cdm_name"),
          type = "gt"
        )
      })
    }
  )
)

# ---- Legacy: single-table module (used when multiple PET CSVs are loaded) ----
#' R6 module for a single PET comparison table: DT table with CSV download.
#' Used for legacy multi-CSV PET comparison exports.
PetComparisonTableModule <- R6::R6Class(
  classname = "PetComparisonTableModule",
  inherit = ShinyModule,

  public = list(
    initialize = function(data, title = "Table") {
      super$initialize()
      private$.data <- data
      private$.title <- title
      private$.table <- Table$new(data = data, title = title, options = list(scrollX = TRUE, pageLength = 25))
      private$.table$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .data = NULL,
    .title = NULL,
    .table = NULL,

    .UI = function() {
      private$.table$UI()
    },

    .server = function(input, output, session) {
      private$.table$server(input, output, session)
    }
  )
)
