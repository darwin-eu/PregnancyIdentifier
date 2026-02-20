# PregnancyIdentifier pipeline overview

This vignette describes the **PregnancyIdentifier** pipeline, which
implements the HIPPS approach to identify pregnancy episodes in OMOP CDM
data. The pipeline has four main algorithm components, run in sequence:

1.  **HIP** — Outcome-anchored episode builder: identifies pregnancy
    episodes from high-specificity pregnancy outcomes and
    gestational-age evidence. See the [HIP
    vignette](https://darwin-eu-dev.github.io/PregnancyIdentifier/articles/hip.md).
2.  **PPS** — Gestational-timing concept episode builder: constructs
    episodes from pregnancy-related concepts and expected timing
    windows. See the [PPS
    vignette](https://darwin-eu-dev.github.io/PregnancyIdentifier/articles/pps.md).
3.  **Merge** — Combines HIP and PPS episodes into a unified per-person
    timeline (HIPPS) by temporal overlap and best-match deduplication.
    See the [Merge
    vignette](https://darwin-eu-dev.github.io/PregnancyIdentifier/articles/merge.md).
4.  **ESD** — Episode start date refinement: uses gestational timing
    evidence (GW and GR3m) to infer pregnancy start dates and precision.
    See the [ESD
    vignette](https://darwin-eu-dev.github.io/PregnancyIdentifier/articles/esd.md).

## Running the full pipeline

The primary entry point is
[`runPregnancyIdentifier()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/runPregnancyIdentifier.md),
which runs all steps end-to-end and writes intermediate artifacts and
final outputs to `outputDir`.

``` r
library(PregnancyIdentifier)
library(CDMConnector)

# A cdm_reference from CDMConnector (e.g. mock or real CDM)
cdm <- mockPregnancyCdm()

runPregnancyIdentifier(
  cdm                   = cdm,
  outputDir             = "pregnancy_identifier_output",
  startDate             = as.Date("2000-01-01"),
  endDate               = Sys.Date(),
  justGestation         = TRUE,
  minCellCount          = 5L,
  runExport             = FALSE,
  conformToValidation   = FALSE
)
```

### Outputs written to disk

The pipeline writes intermediate RDS artifacts as it proceeds and
finishes with a patient-level episode table plus optional shareable
exports:

| File                           | Description                                                                                                                                                                                                      |
|--------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `runStart.csv`                 | Run timestamp used in exported csv files                                                                                                                                                                         |
| `log.txt`                      | Run log                                                                                                                                                                                                          |
| `hip_episodes.rds`             | HIP episodes (outcome and/or gestation-derived)                                                                                                                                                                  |
| `pps_episodes.rds`             | PPS episodes with inferred outcomes (input to merge)                                                                                                                                                             |
| `pps_concept_counts.csv`       | PPS concept record counts per concept (used by export when `runExport = TRUE`).                                                                                                                                  |
| `pps_gest_timing_episodes.rds` | (Only when `debugMode = TRUE`) Record-level PPS concept rows with episode assignment.                                                                                                                            |
| `pps_min_max_episodes.rds`     | (Only when `debugMode = TRUE`) Episode-level PPS min/max date summaries.                                                                                                                                         |
| `hipps_episodes.rds`           | Merged HIP + PPS episode table (standardized columns)                                                                                                                                                            |
| `final_pregnancy_episodes.rds` | **Final** episode table: one row per episode, with inferred start/end and outcomes                                                                                                                               |
| `esd.rds`                      | (Only when `debugMode = TRUE`) ESD episode-level start inference before merging back to HIPPS.                                                                                                                   |
| `export/`                      | De-identified summary CSVs and ZIP archive when `runExport = TRUE`. See the [Export vignette](https://darwin-eu-dev.github.io/PregnancyIdentifier/articles/export.md) for file names, columns, and analysis use. |

## Pipeline steps in order

1.  **initPregnancies** — Loads pregnancy concept sets into the CDM,
    builds `preg_hip_records` and `preg_pps_records` from condition,
    procedure, observation, and measurement tables within the study
    window.
2.  **runHip** — Builds outcome episodes, then gestation episodes,
    merges and cleans them, and writes `hip_episodes.rds`.
3.  **runPps** — Builds PPS episodes from pregnancy-related concepts,
    assigns outcomes, and writes `pps_episodes.rds`.
4.  **mergeHipps** — Reads `hip_episodes.rds` and `pps_episodes.rds`,
    merges by temporal overlap, deduplicates many-to-many matches, and
    writes `hipps_episodes.rds`.
5.  **runEsd** — Reads `hipps_episodes.rds`, pulls gestational timing
    concepts (GW/GR3m), infers start dates and precision, harmonizes end
    and outcome from HIP vs PPS, and writes
    `final_pregnancy_episodes.rds`. When `conformToValidation = TRUE`,
    episode output is modified to remove overlapping episodes and
    episodes longer than 308 days.
6.  **exportPregnancies** — (Optional, when `runExport = TRUE`) Reads
    `final_pregnancy_episodes.rds` and writes shareable CSVs and a ZIP
    to `file.path(outputDir, "export")`. See the [Export
    vignette](https://darwin-eu-dev.github.io/PregnancyIdentifier/articles/export.md).

## What each step contributes to final start, end, and outcome

The pipeline’s most important analysis variables are
**final_episode_start_date**, **final_episode_end_date**, and
**final_outcome_category**. Here is what each step contributes to them.

| Variable          | Step                | Contribution                                                                                                                                                                                                                                                                                                                                              |
|-------------------|---------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Final start**   | **initPregnancies** | Supplies raw pregnancy-related concepts used later by HIP, PPS, and ESD.                                                                                                                                                                                                                                                                                  |
|                   | **HIP**             | Produces estimated start (e.g. from outcome + term range); used in merge interval and as context for ESD.                                                                                                                                                                                                                                                 |
|                   | **PPS**             | Produces episode bounds (min/max dates) from gestational timing; used in merge interval.                                                                                                                                                                                                                                                                  |
|                   | **Merge**           | Produces **merge_episode_start** / **merge_episode_end**, which define the **evidence window** ESD uses to pull GW/GR3m concepts. Does *not* set the final start.                                                                                                                                                                                         |
|                   | **ESD**             | **Sets final start.** Infers **final_episode_start_date** from GW/GR3m concepts within the merged window; if no start is inferred, uses final end − max_term (Matcho term for the chosen outcome). See [ESD vignette](https://darwin-eu-dev.github.io/PregnancyIdentifier/articles/esd.md).                                                               |
| **Final end**     | **HIP**             | Produces **hip_end_date** (outcome or gestation end).                                                                                                                                                                                                                                                                                                     |
|                   | **PPS**             | Produces **pps_end_date** (outcome or episode max date).                                                                                                                                                                                                                                                                                                  |
|                   | **Merge**           | Puts HIP and PPS on one row per episode; produces **merge_episode_end** (used only as evidence window by ESD). Does *not* set the final end.                                                                                                                                                                                                              |
|                   | **ESD**             | **Sets final end.** Chooses **final_episode_end_date** by picking between **hip_end_date** and **pps_end_date** using harmonization rules (e.g. outcome match, or “take the one that occurs second”). Does *not* use **merge_episode_end** for the reported end. See [ESD vignette](https://darwin-eu-dev.github.io/PregnancyIdentifier/articles/esd.md). |
| **Final outcome** | **HIP**             | Produces **hip_outcome_category**.                                                                                                                                                                                                                                                                                                                        |
|                   | **PPS**             | Produces **pps_outcome_category**.                                                                                                                                                                                                                                                                                                                        |
|                   | **Merge**           | Keeps both **hip_outcome_category** and **pps_outcome_category** on each row. Does *not* choose a single outcome.                                                                                                                                                                                                                                         |
|                   | **ESD**             | **Sets final outcome.** Chooses **final_outcome_category** by picking between **hip_outcome_category** and **pps_outcome_category** using the same rules as for the end date. See [ESD vignette](https://darwin-eu-dev.github.io/PregnancyIdentifier/articles/esd.md).                                                                                    |

So: **final start** comes from ESD (GW/GR3m inference or end −
max_term). **Final end** and **final outcome** come from ESD’s choice
between HIP and PPS; the merge only puts both values on the same row so
ESD can pick one.

## Interpreting the final episode table

The primary analysis dataset is `final_pregnancy_episodes.rds`, with one
row per pregnancy episode. Key fields include:

- **final_episode_start_date** — Best estimate of pregnancy start for
  this episode. Set by ESD: when ESD has gestational timing (GW and/or
  GR3m) within the merged evidence window, the start is inferred from
  that (e.g. concept_date − 7×weeks for GW, or GR3m midpoint). When ESD
  has no inferred start, it is set to **final_episode_end_date −
  max_term** (Matcho term for the chosen outcome, e.g. 301 days for live
  birth).
- **final_episode_end_date** — Best estimate of pregnancy end for this
  episode. **Not** taken from the merged interval
  (**merge_episode_end**). Set by ESD by **picking one of**
  **hip_end_date** or **pps_end_date**: when HIP and PPS agree (same
  outcome and end within 14 days), ESD uses hip_end_date; when one side
  is missing, ESD uses the side present; when they disagree, ESD uses
  rules (e.g. take the one that occurs second, or HIP when timing is
  similar). See the [ESD
  vignette](https://darwin-eu-dev.github.io/PregnancyIdentifier/articles/esd.md)
  for the full logic.
- **final_outcome_category** — Outcome category for this episode
  (e.g. LB, SB, PREG). Set by ESD by **picking one of**
  **hip_outcome_category** or **pps_outcome_category** using the same
  priority rules as for the end date.

Together, **final_episode_start_date**, **final_episode_end_date**, and
**final_outcome_category** are the primary analysis variables: the
pipeline’s final inferred pregnancy interval and outcome.

- **merge_episode_start** — Start of the evidence window for this
  episode from the merge step. It is the minimum of: HIP’s first
  gestation date in the episode, PPS episode min date, and HIP estimated
  start. So it is the earliest date implied by either algorithm for this
  merged episode.
- **merge_episode_end** — End of the evidence window for this episode
  from the merge step. It is the maximum of: PPS episode max date and
  HIP pregnancy end (visit/outcome date). So it is the latest date
  implied by either algorithm for this merged episode.

**merge_episode_start** and **merge_episode_end** describe the span of
raw evidence (HIP + PPS) that was merged for this episode, before ESD
refines start/end into the inferred interval.

- **hip_end_date** — Episode end date from the HIP algorithm: either the
  outcome date (e.g. delivery) or the latest gestation date when there
  is no outcome. **NA** when the episode was found only by PPS (no
  overlapping HIP episode).
- **pps_end_date** — Episode end from the PPS algorithm: the inferred
  outcome date when PPS assigned an outcome (LB, SB, etc.), otherwise
  the PPS episode max date. **NA** when the episode was found only by
  HIP (no overlapping PPS episode).

**hip_end_date** and **pps_end_date** are the end dates produced by each
algorithm; at least one is non-NA when both algorithms contributed to
the episode, and one is NA for one-sided (HIP-only or PPS-only)
episodes.

## An example case

``` r
library(PregnancyIdentifier)
library(CDMConnector)
library(dplyr, warn.conflicts = FALSE)

cdm <- mockPregnancyCdm()
#> 
#> Download completed!

cdm <- cdmSubset(cdm, personId = 24)

cdm %>% 
  cdmFlatten() %>% 
  select(-"type_concept_id", -"domain") %>% 
  arrange(start_date)
#> # Source:     SQL [?? x 6]
#> # Database:   DuckDB 1.4.4 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/Rtmp5eZulR/file241c380e2e95.duckdb]
#> # Ordered by: start_date
#>   person_id observation_concept_id start_date end_date observation_concept_name 
#>       <int>                  <int> <date>     <date>   <chr>                    
#> 1        24                4132434 2023-03-15 NA       Gestation period, 8 weeks
#> 2        24                4094910 2023-01-28 NA       Pregnancy test positive  
#> 3        24                 437611 2023-03-15 NA       Ectopic pregnancy        
#> # ℹ 1 more variable: type_concept_name <chr>
```

``` r
outputDir <- file.path(tempdir(), "pregnancy_output")

invisible(capture.output(
  runPregnancyIdentifier(cdm, outputDir = outputDir)
))

list.files(outputDir)
#>  [1] "attrition.csv"                "esd_concept_counts.csv"      
#>  [3] "final_pregnancy_episodes.rds" "hip_concept_counts.csv"      
#>  [5] "hip_episodes.rds"             "hipps_episodes.rds"          
#>  [7] "log.txt"                      "pps_concept_counts.csv"      
#>  [9] "pps_episodes.rds"             "runStart.csv"
readRDS(file.path(outputDir, "final_pregnancy_episodes.rds"))
#> # A tibble: 1 × 39
#>   person_id merge_episode_number final_episode_start_date final_episode_end_date
#>       <int>                <int> <date>                   <date>                
#> 1        24                    1 2023-01-18               2023-03-15            
#> # ℹ 35 more variables: final_outcome_category <chr>,
#> #   merge_episode_start <date>, merge_episode_end <date>, hip_end_date <date>,
#> #   pps_end_date <date>, hip_outcome_category <chr>,
#> #   pps_outcome_category <chr>, esd_precision_days <dbl>,
#> #   esd_precision_category <chr>, esd_gestational_age_days_calculated <int>,
#> #   esd_gw_flag <dbl>, esd_gr3m_flag <dbl>, esd_outcome_match <dbl>,
#> #   esd_term_duration_flag <dbl>, esd_outcome_concordance_score <dbl>, …
```
