# Comparing algorithm output with the Pregnancy Extension Table (PET)

## Overview

The **Pregnancy Extension Table (PET)** is an OMOP CDM extension that
stores pregnancy episodes (start date, end date, outcome) identified by
a separate process (e.g. chart review or another algorithm). The
function
**[`comparePregnancyIdentifierWithPET()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/comparePregnancyIdentifierWithPET.md)**
compares the episodes produced by the PregnancyIdentifier pipeline to
the PET and writes comparison summaries to CSV files. This vignette
describes how to run the comparison using the mock CDM, how **matching**
is done, and what each output contains.

## How to run the PET comparison

You need:

1.  **Algorithm output:** a directory containing
    `final_pregnancy_episodes.rds` (from
    [`runPregnancyIdentifier()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/runPregnancyIdentifier.md)).
2.  **A PET table in the same CDM:** a table with at least `person_id`,
    `pregnancy_start_date`, `pregnancy_end_date`, and
    `pregnancy_outcome` (concept_id).

Below we run the pipeline with
[`mockPregnancyCdm()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/mockPregnancyCdm.md)
(which includes a PET table `pregnancy_extension` in schema `main`),
then run the comparison.

``` r
library(PregnancyIdentifier)
library(CDMConnector)
library(dplyr)
library(tidyr)
library(knitr)
# Helper: get the results table from a summarised_result (handles list with $results or single table)
sr_results <- function(sr) {
  if (is.list(sr) && "results" %in% names(sr) && is.data.frame(sr$results)) {
    sr$results
  } else {
    as.data.frame(sr)
  }
}
# Helper: extract a wide table for one variable from the summarised result
sr_table <- function(sr, var, level_name = "variable_level") {
  tbl <- sr_results(sr)
  d <- dplyr::filter(tbl, .data$variable_name == .env$var)
  if (nrow(d) == 0) return(NULL)
  d <- dplyr::select(d, "variable_level", "estimate_name", "estimate_value")
  wide <- tidyr::pivot_wider(d, names_from = "estimate_name", values_from = "estimate_value")
  if (level_name != "variable_level") wide <- dplyr::rename(wide, !!level_name := "variable_level")
  wide
}
```

``` r
# Directories: pipeline output (episode data), export (comparison results and log)
td <- tempdir()
if (!dir.exists(td)) dir.create(td, recursive = TRUE, showWarnings = FALSE)
outputDir    <- file.path(td, "pet_vignette_pipeline")
exportFolder    <- file.path(td, "pet_vignette_comparison")
dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
dir.create(exportFolder, recursive = TRUE, showWarnings = FALSE)

# 1) Build mock CDM and run the pipeline (export runs by default to outputDir/export)
cdm <- mockPregnancyCdm()
#> 
#> Download completed!
runPregnancyIdentifier(
  cdm = cdm,
  outputFolder = outputDir,
  outputLogToConsole = FALSE
)
```

``` r
# The mock CDM includes a PET table `pregnancy_extension` in schema `main` with the
# required columns. We run the comparison against it. (Alternatively, you could build
# a PET from the algorithm output as in the `insert-mock-pet` chunk and use
# petTable = "pregnancy_episode".)

# 3) Run the PET comparison (writes summarised result and log to exportFolder; returns nothing)
comparePregnancyIdentifierWithPET(
  cdm = cdm,
  outputFolder = outputDir,
  exportFolder = exportFolder,
  petSchema = "main",
  petTable = "pregnancy_extension",
  minOverlapDays = 1L,
  outputLogToConsole = FALSE
)
# Load the written summarised result for display and programmatic use
res <- omopgenerics::importSummarisedResult(file.path(exportFolder, "pet_comparison_summarised_result.csv"))
```

## How matching is done

Episodes are **matched** by:

1.  **Same person:** only algorithm and PET episodes from the same
    `person_id` are considered.
2.  **Overlapping dates:** for each (algorithm episode, PET episode)
    pair, overlap in days is  
    `max(0, min(alg_end, pet_end) - max(alg_start, pet_start) + 1)`.  
    Pairs with overlap **≥ `minOverlapDays`** (default 1) are
    **candidate pairs**.
3.  **One-to-one assignment:** within each person, candidate pairs are
    sorted by overlap (descending). A greedy algorithm assigns each PET
    episode to at most one algorithm episode and vice versa: it
    repeatedly picks the pair with the largest overlap among those whose
    PET and algorithm indices are not yet used. This avoids
    double-counting and yields consistent Venn and confusion counts.

Optional: if `removeWithinSourceOverlaps = TRUE`, overlapping episodes
within PET and within the algorithm are removed (greedy non-overlapping
by start date, max 400 days) *before* matching, which can reduce
many-to-many pairs.

## Outputs generated

The function writes a **single CSV** in SummarisedResult format to ()
and returns nothing. Re-import it with and display with :

``` r
# Display as a gt table (optional: requires visOmopResults)
if (requireNamespace("visOmopResults", quietly = TRUE)) {
  visOmopResults::visOmopTable(
    result = res,
    header = "cdm_name",
    rename = c("Data source" = "cdm_name"),
    hide =  c("result_id", "group_name", "group_level", "strata_name", "strata_level", "pet_comparison"))
}
```

[TABLE]

The summarised result is in long format: each row has , , , and . The
helper used below extracts and pivots one variable into a wide table for
display. The following sections describe each metric.

### Episode and person counts

``` r
kable(sr_table(res, "episode_counts", "source"), format = "html", caption = "Episode counts: algorithm vs PET")
```

| source    | n_episodes | n_persons |
|:----------|:-----------|:----------|
| algorithm | 35         | 32        |
| pet       | 33         | 25        |

Episode counts: algorithm vs PET

Number of episodes and distinct persons in the algorithm output and in
the PET table.

### Protocol summary

``` r
kable(sr_table(res, "protocol_summary"), format = "html", caption = "Protocol summary (for reporting)")
```

| variable_level | total_pet_episodes | total_algorithm_episodes | total_matched_episodes |
|:---------------|:-------------------|:-------------------------|:-----------------------|
| overall        | 33                 | 35                       | 27                     |

Protocol summary (for reporting)

Totals and number of **matched** episodes (one-to-one pairs).

### Person overlap

``` r
kable(sr_table(res, "person_overlap", "metric"), format = "html", caption = "Person overlap")
```

| metric                | n_persons |
|:----------------------|:----------|
| raw_person_overlap    | 25        |
| cohort_person_overlap | 21        |

Person overlap

- **raw_person_overlap:** distinct persons with at least one PET episode
  and one algorithm episode.
- **cohort_person_overlap:** same, but after filtering both sources to
  gestation 0–308 days and end ≥ start.

### Venn counts (matched / PET-only / algorithm-only)

``` r
kable(sr_table(res, "venn_counts", "category"), format = "html", caption = "Venn counts (one-to-one matching)")
```

| category       | n_episodes | n_pet_matched | n_alg_matched |
|:---------------|:-----------|:--------------|:--------------|
| both           | 27         | 27            | 27            |
| pet_only       | 6          | 27            | 27            |
| algorithm_only | 8          | 27            | 27            |

Venn counts (one-to-one matching)

- **both:** number of matched episode pairs.
- **pet_only:** PET episodes with no matched algorithm episode.
- **algorithm_only:** algorithm episodes with no matched PET episode.

### 2×2 confusion matrix (PET as reference)

``` r
kable(sr_table(res, "confusion_2x2", "cell"), format = "html", caption = "2×2 confusion matrix (PET = reference)")
```

| cell | count |
|:-----|:------|
| TP   | 27    |
| FN   | 6     |
| FP   | 8     |
| TN   | NA    |

2×2 confusion matrix (PET = reference)

- **TP:** PET episode has a matched algorithm episode.
- **FN:** PET episode has no match (algorithm “miss”).
- **FP:** Algorithm episode has no match (algorithm “extra”).
- **TN:** Not defined at episode level (no negative population).

### Sensitivity, PPV, NPV

``` r
kable(sr_table(res, "ppv_sensitivity", "metric"), format = "html", caption = "Sensitivity, specificity, PPV, NPV")
```

| metric      | value             | numerator | denominator |
|:------------|:------------------|:----------|:------------|
| sensitivity | 0.818181818181818 | 27        | 33          |
| ppv         | 0.771428571428571 | 27        | 35          |

Sensitivity, specificity, PPV, NPV

- **Sensitivity** = TP / (TP + FN): fraction of PET episodes that have a
  match.
- **PPV** = TP / (TP + FP): fraction of algorithm episodes that match a
  PET episode.
- Specificity and NPV use TN and are NA at episode level.

### Date differences (matched pairs only)

For each matched pair, date differences are **PET − algorithm**
(positive = PET is later, i.e. algorithm starts/ends too early). Three
measures are reported:

- **Start date difference**: PET start − algorithm start. Positive =
  algorithm starts too early; negative = algorithm starts too late.
- **End date difference**: PET end − algorithm end. Same sign
  convention.
- **Duration difference**: PET duration − algorithm duration. Positive =
  algorithm episodes are shorter; negative = longer.

``` r
dd_summary <- sr_table(res, "date_difference_summary")
if (!is.null(dd_summary) && nrow(dd_summary) > 0) {
  kable(dd_summary, format = "html", caption = "Date difference summary (PET − algorithm, days)")
} else {
  cat("No matched pairs; date differences not computed.\n")
}
```

| variable_level                                | mean              | median | sd               | min   | q25  | q75 | max  | n_matched |
|:----------------------------------------------|:------------------|:-------|:-----------------|:------|:-----|:----|:-----|:----------|
| Start date difference (PET - Algorithm, days) | 54.7407407407407  | 2      | 278.660614173833 | -5    | -1   | 3.5 | 1449 | 27        |
| End date difference (PET - Algorithm, days)   | -44.6296296296296 | 0      | 278.310306637442 | -1420 | -3   | 1.5 | 226  | 27        |
| Duration difference (PET - Algorithm, days)   | -99.3703703703704 | -2     | 388.076434595453 | -1449 | -5.5 | 0   | 227  | 27        |

Date difference summary (PET − algorithm, days)

#### Date differences by outcome

The same start/end/duration differences are also reported stratified by
algorithm outcome category (LB, SB, SA, AB, PREG, etc.), since different
outcomes have very different expected durations.

``` r
dd_by_outcome <- sr_table(res, "date_difference_by_outcome")
if (!is.null(dd_by_outcome) && nrow(dd_by_outcome) > 0) {
  kable(dd_by_outcome, format = "html", caption = "Date differences by algorithm outcome (PET − algorithm, days)")
} else {
  cat("No outcome-stratified date differences available.\n")
}
```

| variable_level                                         | mean              | median | sd                | min   | q25   | q75   | max  | n_matched |
|:-------------------------------------------------------|:------------------|:-------|:------------------|:------|:------|:------|:-----|:----------|
| Start date difference (PET - Algorithm, days) \[AB\]   | 3                 | 3      | NA                | 3     | 3     | 3     | 3    | NA        |
| End date difference (PET - Algorithm, days) \[AB\]     | 1                 | 1      | NA                | 1     | 1     | 1     | 1    | NA        |
| Duration difference (PET - Algorithm, days) \[AB\]     | -2                | -2     | NA                | -2    | -2    | -2    | -2   | NA        |
| Start date difference (PET - Algorithm, days) \[ECT\]  | 0                 | 0      | NA                | 0     | 0     | 0     | 0    | NA        |
| End date difference (PET - Algorithm, days) \[ECT\]    | 0                 | 0      | NA                | 0     | 0     | 0     | 0    | NA        |
| Duration difference (PET - Algorithm, days) \[ECT\]    | 0                 | 0      | NA                | 0     | 0     | 0     | 0    | NA        |
| Start date difference (PET - Algorithm, days) \[LB\]   | -0.25             | -0.5   | 3.15096357144468  | -5    | -1.75 | 3     | 3    | 8         |
| End date difference (PET - Algorithm, days) \[LB\]     | 0.125             | 1.5    | 3.27053949241231  | -5    | -3    | 3     | 3    | 8         |
| Duration difference (PET - Algorithm, days) \[LB\]     | 0.375             | -1.5   | 5.0972681759098   | -6    | -2.5  | 4     | 8    | 8         |
| Start date difference (PET - Algorithm, days) \[PREG\] | 105.142857142857  | 3.5    | 386.800854573644  | -4    | -0.75 | 5     | 1449 | 14        |
| End date difference (PET - Algorithm, days) \[PREG\]   | -85.8571428571429 | -0.5   | 388.721545017046  | -1420 | -2.75 | 0.75  | 226  | 14        |
| Duration difference (PET - Algorithm, days) \[PREG\]   | -191              | -2     | 531.423198011355  | -1449 | -8.5  | -0.25 | 227  | 14        |
| Start date difference (PET - Algorithm, days) \[SA\]   | 1.5               | 1.5    | 0.707106781186548 | 1     | 1.25  | 1.75  | 2    | NA        |
| End date difference (PET - Algorithm, days) \[SA\]     | -2.5              | -2.5   | 2.12132034355964  | -4    | -3.25 | -1.75 | -1   | NA        |
| Duration difference (PET - Algorithm, days) \[SA\]     | -4                | -4     | 1.4142135623731   | -5    | -4.5  | -3.5  | -3   | NA        |
| Start date difference (PET - Algorithm, days) \[SB\]   | 2                 | 2      | NA                | 2     | 2     | 2     | 2    | NA        |
| End date difference (PET - Algorithm, days) \[SB\]     | 0                 | 0      | NA                | 0     | 0     | 0     | 0    | NA        |
| Duration difference (PET - Algorithm, days) \[SB\]     | -2                | -2     | NA                | -2    | -2    | -2    | -2   | NA        |

Date differences by algorithm outcome (PET − algorithm, days)

#### Alignment distribution

The date differences can also be viewed as a **binned distribution**
(histogram), which shows at a glance what fraction of matched episodes
are perfectly aligned (bin = 0), close (within ±7 days), or far off
(±30+ days). The bins are coloured on a red-yellow-green gradient where
green = exact match and red = large discrepancy.

This visualisation is available interactively in the Shiny app’s
**Alignment** tab. It can be filtered by measure (start/end/duration)
and stratified by outcome.

### Outcome confusion (matched pairs)

Cross-tabulation of PET outcome (concept_id) vs algorithm outcome (LB,
SB, AB, SA, etc.) is not included in the summarised result CSV; only
aggregate metrics are exported.

### Outcome accuracy

Among matched pairs with a mappable algorithm outcome (LB/SB/AB/SA/DELIV
→ concept_id), the fraction where PET outcome concept_id equals the
algorithm-mapped concept_id.

``` r
kable(sr_table(res, "outcome_accuracy"), format = "html", caption = "Outcome accuracy (matched pairs)")
```

| variable_level | n_correct | n_total | accuracy |
|:---------------|:----------|:--------|:---------|
| overall        | 12        | 12      | 1        |

Outcome accuracy (matched pairs)

### Outcome by year (same-year pairs)

Among matched pairs in the **same year** (algorithm start year = PET
start year), counts of agreement (e.g. lb_lb, sb_sb) and disagreement
(e.g. lb_sb, sb_lb). Used for same-year outcome cross-tabs.

``` r
kable(sr_table(res, "outcome_by_year"), format = "html", caption = "Outcome by year (same-year pairs)")
```

| variable_level  | overall_equal | overall_diff | lb_lb | lb_miscarriage | lb_ab | lb_sb | lb_unknown | sb_sb | sb_miscarriage | sb_ab | sb_lb | sb_unknown | ab_ab | ab_miscarriage | ab_lb | ab_sb | ab_unknown |
|:----------------|:--------------|:-------------|:------|:---------------|:------|:------|:-----------|:------|:---------------|:------|:------|:-----------|:------|:---------------|:------|:------|:-----------|
| same_year_pairs | 12            | 14           | 8     | NA             | NA    | NA    | NA         | NA    | NA             | NA    | NA    | NA         | NA    | NA             | NA    | NA    | NA         |

Outcome by year (same-year pairs)

### Duration summary (all episodes)

``` r
kable(sr_table(res, "duration_summary", "source"), format = "html", caption = "Pregnancy duration (days) by source")
```

| source    | n   | mean             | median | sd               | min | q25 | q75 | max  |
|:----------|:----|:-----------------|:-------|:-----------------|:----|:----|:----|:-----|
| algorithm | 35  | 328.257142857143 | 266    | 421.504332742866 | 21  | 147 | 293 | 1749 |
| pet       | 33  | 210.454545454545 | 260    | 97.4211511008681 | 15  | 140 | 280 | 377  |

Pregnancy duration (days) by source

Summary statistics of episode length (end − start) for all algorithm
episodes and all PET episodes.

### Duration matched summary

``` r
dm <- sr_table(res, "duration_matched_summary", "source")
if (!is.null(dm) && nrow(dm) > 0) {
  kable(dm, format = "html", caption = "Duration (matched pairs only)")
} else {
  cat("No duration matched summary (no matched pairs).\n")
}
```

| source    | n   | mean            | median | sd               | min | q25   | q75   | max  |
|:----------|:----|:----------------|:-------|:-----------------|:----|:------|:------|:-----|
| algorithm | 27  | 299.37037037037 | 224    | 386.971291734646 | 21  | 143.5 | 280   | 1606 |
| pet       | 27  | 200             | 181    | 102.336173016643 | 15  | 138.5 | 283.5 | 377  |

Duration (matched pairs only)

Duration statistics for the **matched** episodes only (algorithm vs
PET).

------------------------------------------------------------------------

## Files written to `exportFolder`

| File                                   | Content                                                                                                                                                                                                                                                                                                                      |
|----------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `pet_comparison_summarised_result.csv` | All comparison metrics in SummarisedResult format (episode counts, protocol summary, person overlap, Venn counts, time overlap, confusion 2x2, PPV/sensitivity, date-difference summary, outcome accuracy, outcome by year, duration summaries). Includes settings (e.g. , , ) for traceability. Use to read and to display. |
| `log.txt`                              | Run log (appended if file already exists).                                                                                                                                                                                                                                                                                   |

------------------------------------------------------------------------

## Using your own PET table

If your CDM already has a PET table
(e.g. `omop_cmbd.pregnancy_episode`), call:

``` r
comparePregnancyIdentifierWithPET(
  cdm = your_cdm,
  outputFolder = "/path/to/pipeline/output",
  exportFolder = "/path/to/comparison/results",
  petSchema = "omop_cmbd",
  petTable = "pregnancy_episode",
  minOverlapDays = 1L,
  removeWithinSourceOverlaps = FALSE
)
res <- omopgenerics::importSummarisedResult(file.path("/path/to/comparison/results", "pet_comparison_summarised_result.csv"))
```

Ensure the PET table has at least: `person_id`, `pregnancy_start_date`,
`pregnancy_end_date`, `pregnancy_outcome` (concept_id). Gestational
length in days is computed from start and end dates using the database
(via ).

``` r
# Clean up temp dirs (optional; disconnect from cdm when done in your session)
unlink(outputDir, recursive = TRUE)
unlink(exportFolder, recursive = TRUE)
```
