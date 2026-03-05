# 26-pet-comparison.R - PET comparison module (standard Shiny module)
# Data: petComparisonSummarisedResult (a summarised_result)
# Complex module with Overview/Plot/Person-level/Table/Summarised-result tabs.

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
| **Episode counts** | Total episodes in PET and total from the algorithm (optionally filtered, e.g. gestation 0-308 days, end >= start). |
| **Venn counts** | Number of episodes in **both**, **PET only**, and **algorithm only** (from the one-to-one matching). |
| **Sensitivity** | Among all PET episodes, the proportion that have a matched algorithm episode: *TP / (TP + FN)*. |
| **PPV** | Among all algorithm episodes, the proportion that are matched to a PET episode (true positives): *TP / (TP + FP)*, where FP = algorithm-only episodes. |
| **Time overlap** | For each PET (or algorithm) episode, the maximum overlap in days with any algorithm (or PET) episode; summarized (min, quartiles, max). |
| **Confusion 2x2** | Counts for TP, FN, FP; TN is not defined (no gold-standard negatives). |
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
  take_first <- function(x) { x <- unlist(x, use.names = FALSE); if (length(x) == 0) NA else x[1] }
  pw <- vennWide %>%
    tidyr::pivot_wider(names_from = "variable_level", values_from = "estimate_value", values_fn = take_first)
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
  take_first <- function(x) { x <- unlist(x, use.names = FALSE); if (length(x) == 0) NA else x[1] }
  epp %>%
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value", values_fn = take_first) %>%
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

# ---- Main PET comparison module ----
petComparisonUI <- function(id) {
  ns <- NS(id)

  # Pre-compute data availability for conditional UI
  has_venn <- FALSE
  has_person_venn <- FALSE
  cdm_names <- character(0)

  if (exists("petComparisonSummarisedResult") && !is.null(petComparisonSummarisedResult) &&
      nrow(petComparisonSummarisedResult) > 0) {
    venn_data <- extract_venn_data_from_sr(petComparisonSummarisedResult, "venn_counts", "n_episodes")
    has_venn <- !is.null(venn_data) && nrow(venn_data) > 0
    person_venn_data <- extract_venn_data_from_sr(petComparisonSummarisedResult, "person_venn_counts", "n_persons")
    has_person_venn <- !is.null(person_venn_data) && nrow(person_venn_data) > 0
    tbl <- as.data.frame(petComparisonSummarisedResult)
    if ("cdm_name" %in% names(tbl)) cdm_names <- unique(tbl$cdm_name)
  }

  db_choices <- stats::setNames(cdm_names, cdm_names)

  # Overview tab
  overview_ui <- tagList(
    tags$style(paste(
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
    )),
    div(
      class = "pet-comparison-overview",
      style = "max-width: 900px; margin-bottom: 2em; line-height: 1.5; color: #333;",
      HTML(markdown::markdownToHTML(text = PET_COMPARISON_OVERVIEW_MD, fragment.only = TRUE))
    )
  )

  # Plot tab
  plot_ui <- if (has_venn) {
    tagList(
      if (length(cdm_names) > 1) {
        fluidRow(
          column(3, selectInput(ns("database"), "Database",
                               choices = db_choices, selected = db_choices[1]))
        )
      },
      fluidRow(
        column(12, amVennDiagram5::amVennDiagramOutput(ns("venn"), width = "100%", height = "450px") %>% withSpinner())
      )
    )
  } else {
    p("No Venn data available.")
  }

  # Person-level tab
  person_ui <- if (has_person_venn) {
    tagList(
      if (length(cdm_names) > 1) {
        fluidRow(
          column(3, selectInput(ns("person_database"), "Database",
                               choices = db_choices, selected = db_choices[1]))
        )
      },
      h4("Person-level Venn diagram"),
      p("Persons grouped by whether they have episodes in both HIPPS and PET, HIPPS only, or PET only."),
      fluidRow(
        column(12, amVennDiagram5::amVennDiagramOutput(ns("person_venn"), width = "100%", height = "400px") %>% withSpinner())
      ),
      hr(),
      h4("Episodes per person by group"),
      downloadButton(ns("download_person_epp_csv"), "Download table (.csv)"),
      DT::DTOutput(ns("person_epp_table")) %>% withSpinner()
    )
  } else {
    p("No person-level comparison data available. Re-run the PET comparison to generate this data.")
  }

  # Table tab
  table_ui <- tagList(
    fluidRow(
      column(3, pickerInput(ns("tableCdm"), "Database",
                           choices = cdm_names, selected = cdm_names,
                           multiple = TRUE, options = opt))
    ),
    br(),
    downloadButton(ns("download_vis_docx"), "Download table (.docx)"),
    gt::gt_output(ns("visTable")) %>% withSpinner()
  )

  # Summarised result tab
  summarised_ui <- tagList(
    downloadButton(ns("download_raw_csv"), "Download table (.csv)"),
    DT::DTOutput(ns("rawTable")) %>% withSpinner()
  )

  tabsetPanel(
    id = ns("petTabs"),
    type = "tabs",
    tabPanel("Overview", overview_ui),
    tabPanel("Plot", plot_ui),
    tabPanel("Person-level", person_ui),
    tabPanel("Table", table_ui),
    tabPanel("Summarised result", summarised_ui)
  )
}

petComparisonServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    result <- petComparisonSummarisedResult
    tbl <- as.data.frame(result)
    cdm_names <- if (is.data.frame(tbl) && "cdm_name" %in% names(tbl)) unique(tbl$cdm_name) else character(0)

    venn_data <- extract_venn_data_from_sr(result, "venn_counts", "n_episodes")
    person_venn_data <- extract_venn_data_from_sr(result, "person_venn_counts", "n_persons")
    person_epp_data <- extract_person_epp_from_sr(result)

    # Plot: Venn
    if (!is.null(venn_data) && nrow(venn_data) > 0) {
      chosen_db <- reactive({
        if (length(cdm_names) == 1) cdm_names[1] else input$database
      })
      venn_plot_data <- reactive({
        db <- chosen_db()
        if (is.null(db)) return(NULL)
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
    if (!is.null(person_venn_data) && nrow(person_venn_data) > 0) {
      chosen_person_db <- reactive({
        if (length(cdm_names) == 1) cdm_names[1] else input$person_database
      })
      person_venn_plot_data <- reactive({
        db <- chosen_person_db()
        if (is.null(db)) return(NULL)
        row <- person_venn_data %>% dplyr::filter(.data$cdm_name == .env$db)
        if (nrow(row) == 0) return(NULL)
        pet_venn_data_from_counts(row$both[1], row$pet_only[1], row$algorithm_only[1])
      })
      output$person_venn <- amVennDiagram5::renderAmVennDiagram({
        vd <- person_venn_plot_data()
        if (is.null(vd)) return(NULL)
        amVennDiagram5::amVennDiagram(vd, theme = "default", legendPosition = "bottom")
      })
      personEppTableData <- reactive({
        db <- chosen_person_db()
        if (is.null(person_epp_data) || is.null(db)) return(NULL)
        person_epp_data %>%
          dplyr::filter(.data$cdm_name == .env$db) %>%
          dplyr::select("group", dplyr::any_of(c("mean", "median", "sd", "min", "q25", "q75", "max"))) %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2)))
      })
      output$person_epp_table <- DT::renderDT({
        epp_tbl <- personEppTableData()
        if (is.null(epp_tbl) || nrow(epp_tbl) == 0) return(NULL)
        DT::datatable(epp_tbl, rownames = FALSE, options = list(dom = "t", pageLength = 10))
      })
      output$download_person_epp_csv <- downloadHandler(
        filename = function() { "pet_person_episodes_per_person.csv" },
        content = function(file) {
          d <- personEppTableData()
          if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
        }
      )
    }

    # Table: visOmopTable with database filter
    filtered_result <- reactive({
      sel <- input$tableCdm
      if (is.null(sel) || length(sel) == 0) sel <- cdm_names
      res <- result %>% dplyr::filter(.data$cdm_name %in% sel)
      if (!inherits(res, "summarised_result") && inherits(result, "summarised_result")) {
        res <- tryCatch(
          omopgenerics::newSummarisedResult(
            tibble::as_tibble(res),
            omopgenerics::settings(result)
          ),
          error = function(e) res
        )
      }
      res
    })

    vis_table_gt <- reactive({
      req(filtered_result())
      sr <- filtered_result()
      if (nrow(as.data.frame(sr)) == 0) {
        return(gt::gt(tibble::tibble(Message = "No data for selected database(s).")))
      }
      tryCatch(
        visOmopResults::visOmopTable(
          result = sr,
          header = c("cdm_name"),
          groupColumn = c("variable_name", "variable_level"),
          rename = c("Database" = "cdm_name"),
          type = "gt"
        ),
        error = function(e) {
          gt::gt(tibble::tibble(Message = paste("Error rendering table:", e$message)))
        }
      )
    })
    output$visTable <- gt::render_gt({
      vis_table_gt()
    })
    output$download_vis_docx <- downloadHandler(
      filename = function() { "pet_comparison_table.docx" },
      content = function(file) {
        tbl <- vis_table_gt()
        if (!is.null(tbl)) gt::gtsave(tbl, file)
      }
    )

    # Summarised result: raw data table
    output$rawTable <- DT::renderDT({
      renderPrettyDT(as.data.frame(result))
    })
    output$download_raw_csv <- downloadHandler(
      filename = function() { "pet_comparison_summarised_result.csv" },
      content = function(file) {
        d <- as.data.frame(result)
        if (!is.null(d) && nrow(d) > 0) readr::write_csv(d, file)
      }
    )
  })
}

# ---- Legacy: simple table module for multi-CSV PET comparison ----
petComparisonLegacyUI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download_table_csv"), "Download table (.csv)"),
    DT::DTOutput(ns("table")) %>% withSpinner()
  )
}

petComparisonLegacyServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT({
      renderPrettyDT(data)
    })
    output$download_table_csv <- downloadHandler(
      filename = function() { "pet_comparison.csv" },
      content = function(file) {
        if (!is.null(data) && nrow(data) > 0) readr::write_csv(data, file)
      }
    )
  })
}

# ---- Legacy container: wraps multiple PET comparison tables into tabs ----
petComparisonLegacyContainerUI <- function(id) {
  ns <- NS(id)
  tabs <- list()
  for (varName in names(petComparisonSpec)) {
    if (exists(varName, envir = .GlobalEnv)) {
      data <- get(varName, envir = .GlobalEnv)
      if (!is.null(data) && nrow(data) > 0) {
        displayName <- petComparisonSpec[[varName]]
        tabId <- gsub("[^a-zA-Z0-9]", "_", varName)
        tabs[[displayName]] <- tabPanel(displayName, petComparisonLegacyUI(ns(tabId)))
      }
    }
  }
  if (length(tabs) == 0) return(p("No PET comparison data available."))
  tagList(
    div(class = "tab-help-text", "Compare PregnancyIdentifier episodes with the OMOP Pregnancy Extension Table (PET)."),
    do.call(tabsetPanel, tabs)
  )
}

petComparisonLegacyContainerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    for (varName in names(petComparisonSpec)) {
      if (exists(varName, envir = .GlobalEnv)) {
        data <- get(varName, envir = .GlobalEnv)
        if (!is.null(data) && nrow(data) > 0) {
          tabId <- gsub("[^a-zA-Z0-9]", "_", varName)
          local({
            d <- data
            petComparisonLegacyServer(tabId, d)
          })
        }
      }
    }
  })
}
