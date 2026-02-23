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
# Directories for pipeline output and comparison output
outputDir    <- file.path(tempdir(), "pet_vignette_pipeline")
outputFolder <- file.path(tempdir(), "pet_vignette_comparison")
dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)

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

# 3) Run the PET comparison (writes summarised result to outputFolder; returns nothing)
comparePregnancyIdentifierWithPET(
  cdm = cdm,
  outputFolder = outputDir,
  petSchema = "main",
  petTable = "pregnancy_extension",
  minOverlapDays = 1L,
  outputLogToConsole = FALSE
)
# Load the written summarised result for display and programmatic use
res <- omopgenerics::importSummarisedResult(file.path(outputDir, "pet_comparison_summarised_result.csv"))
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
| algorithm | 33         | 32        |
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
| overall        | 33                 | 33                       | 26                     |

Protocol summary (for reporting)

Totals and number of **matched** episodes (one-to-one pairs).

### Person overlap

``` r
kable(sr_table(res, "person_overlap", "metric"), format = "html", caption = "Person overlap")
```

| metric                | n_persons |
|:----------------------|:----------|
| raw_person_overlap    | 25        |
| cohort_person_overlap | 20        |

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
| both           | 26         | 26            | 26            |
| pet_only       | 7          | 26            | 26            |
| algorithm_only | 7          | 26            | 26            |

Venn counts (one-to-one matching)

- **both:** number of matched episode pairs.
- **pet_only:** PET episodes with no matched algorithm episode.
- **algorithm_only:** algorithm episodes with no matched PET episode.

### Time overlap summary

``` r
kable(sr_table(res, "time_overlap_summary", "label"), format = "html", caption = "Time overlap (days) per episode")
```

| label                              | min | q25   | median | q75    | max | sd               | n_episodes | n_persons |
|:-----------------------------------|:----|:------|:-------|:-------|:----|:-----------------|:-----------|:----------|
| PET -\> IPE 0 day overlap required | 0   | 57    | 147    | 278    | 378 | 116.763869139232 | 33         | 25        |
| PET -\> IPE 1 day overlap required | 16  | 139   | 165    | 278.75 | 378 | 100.126005269445 | 28         | 25        |
| IPE -\> PET 0 day overlap required | 0   | 54    | 147    | 278    | 378 | 120.9589368089   | 33         | 32        |
| IPE -\> PET 1 day overlap required | 52  | 139.5 | 202.5  | 280.25 | 378 | 96.5070663016644 | 26         | 25        |

Time overlap (days) per episode

For each PET episode, the **maximum** overlap (in days) with any
algorithm episode; and for each algorithm episode, the maximum overlap
with any PET episode. Summaries are shown when requiring **0 day**
overlap (all episodes) and **1 day** overlap (only episodes with at
least 1 day overlap). **PET → IPE** = per-PET max overlap with
algorithm; **IPE → PET** = per-algorithm max overlap with PET. Columns:
min, Q25, median, Q75, max, sd, n_episodes, n_persons.

### 2×2 confusion matrix (PET as reference)

``` r
kable(sr_table(res, "confusion_2x2", "cell"), format = "html", caption = "2×2 confusion matrix (PET = reference)")
```

| cell | count |
|:-----|:------|
| TP   | 26    |
| FN   | 7     |
| FP   | 7     |
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
| sensitivity | 0.787878787878788 | 26        | 33          |
| ppv         | 0.787878787878788 | 26        | 33          |

Sensitivity, specificity, PPV, NPV

- **Sensitivity** = TP / (TP + FN): fraction of PET episodes that have a
  match.
- **PPV** = TP / (TP + FP): fraction of algorithm episodes that match a
  PET episode.
- Specificity and NPV use TN and are NA at episode level.

### Date differences (matched pairs only)

For each matched pair, date differences are **PET − algorithm**
(positive = PET is later). The summarised result may include when there
are matched pairs.

``` r
dd_summary <- sr_table(res, "date_difference_summary")
if (!is.null(dd_summary) && nrow(dd_summary) > 0) {
  kable(dd_summary, format = "html", caption = "Date difference summary (PET − algorithm, days)")
} else {
  cat("No matched pairs; date differences not computed.\n")
}
```

| variable_level  | mean              | median | sd               | min   | q25 | q75 | max  | n_matched |
|:----------------|:------------------|:-------|:-----------------|:------|:----|:----|:-----|:----------|
| start_diff_days | 56.6538461538462  | 1.5    | 283.9983017284   | -5    | -1  | 3   | 1449 | 26        |
| end_diff_days   | -81.2692307692308 | 0      | 304.151483007045 | -1420 | -3  | 1   | 4    | 26        |

Date difference summary (PET − algorithm, days)

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
| same_year_pairs | 12            | 13           | 8     | 0              | 0     | 0     | 0          | 1     | 0              | 0     | 0     | 0          | 1     | 0              | 0     | 0     | 0          |

Outcome by year (same-year pairs)

### Duration summary (all episodes)

``` r
kable(sr_table(res, "duration_summary", "source"), format = "html", caption = "Pregnancy duration (days) by source")
```

| source    | n   | mean             | median | sd               | min | q25 | q75 | max  |
|:----------|:----|:-----------------|:-------|:-----------------|:----|:----|:----|:-----|
| algorithm | 33  | 372.515151515152 | 280    | 433.191652246159 | 56  | 147 | 301 | 1749 |
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

| source    | n   | mean             | median | sd               | min | q25    | q75    | max  |
|:----------|:----|:-----------------|:-------|:-----------------|:----|:-------|:-------|:-----|
| algorithm | 26  | 345.038461538462 | 278.5  | 399.862224349261 | 56  | 147    | 298.5  | 1606 |
| pet       | 26  | 207.115384615385 | 201.5  | 97.3136483431084 | 51  | 139.25 | 285.25 | 377  |

Duration (matched pairs only)

Duration statistics for the **matched** episodes only (algorithm vs
PET).

------------------------------------------------------------------------

## Files written to `outputFolder`

| File                                   | Content                                                                                                                                                                                                                                                                                                                      |
|----------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `pet_comparison_summarised_result.csv` | All comparison metrics in SummarisedResult format (episode counts, protocol summary, person overlap, Venn counts, time overlap, confusion 2x2, PPV/sensitivity, date-difference summary, outcome accuracy, outcome by year, duration summaries). Includes settings (e.g. , , ) for traceability. Use to read and to display. |
| `log.txt`                              | Run log.                                                                                                                                                                                                                                                                                                                     |

------------------------------------------------------------------------

## Using your own PET table

If your CDM already has a PET table
(e.g. `omop_cmbd.pregnancy_episode`), call:

``` r
comparePregnancyIdentifierWithPET(
  cdm = your_cdm,
  outputDir = "/path/to/pipeline/output",
  outputFolder = "/path/to/comparison/results",
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
unlink(outputFolder, recursive = TRUE)
```
