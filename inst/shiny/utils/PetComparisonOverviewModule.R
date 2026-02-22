library(DarwinShinyModules)

# Static overview text for the PET comparison tab (markdown).
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

## Sub-tabs in this section

- **Overview** (this page): Methodology and interpretation.
- **Plot:** Venn diagram by database (PET episodes vs algorithm episodes, overlap = both).
- **Table:** Formatted table of all comparison metrics (visOmopResults), with a **Database** filter.
- **Summarised result:** Raw summarised result table (download as CSV).
"

#' R6 module that shows an overview of the PET comparison: what is compared,
#' how matching is done, and what each metric means.
PetComparisonOverviewModule <- R6::R6Class(
  classname = "PetComparisonOverviewModule",
  inherit = ShinyModule,

  public = list(
    initialize = function() {
      super$initialize()
      private$.text <- DarwinShinyModules::Text$new(markdown = PET_COMPARISON_OVERVIEW_MD)
      private$.text$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .text = NULL,

    .UI = function() {
      shiny::tagList(
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
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "pet-comparison-overview",
              style = "max-width: 900px; margin-bottom: 2em; line-height: 1.5; color: #333;",
              private$.text$UI()
            )
          )
        )
      )
    },

    .server = function(input, output, session) {
      if (is.function(private$.text$server)) {
        private$.text$server(input, output, session)
      }
    }
  )
)
