# Compare runPregnancyIdentifier results with the OMOP Pregnancy Extension Table (PET)

Compares algorithm output (from `final_pregnancy_episodes.rds`) to the
PET table and writes comparison summaries to `outputFolder`. Comparisons
include: pregnancy episode and person counts; pregnancy start/end date
differences for linked episodes; Venn counts (both, PET only, algorithm
only); a 2x2 confusion matrix (TP, FN, FP, TN) with sensitivity,
specificity, PPV, and NPV; outcome confusion matrix and accuracy; and
pregnancy duration distributions.

## Usage

``` r
comparePregnancyIdentifierWithPET(
  cdm,
  outputDir,
  outputFolder,
  petSchema,
  petTable,
  minOverlapDays = 1L,
  logger = NULL,
  outputLogToConsole = TRUE
)
```

## Arguments

- cdm:

  A `cdm_reference` (from CDMConnector) with a database connection. The
  PET table is read via `petSchema` and `petTable`.

- outputDir:

  `character(1)`. Directory containing pipeline outputs, in particular
  `final_pregnancy_episodes.rds`.

- outputFolder:

  `character(1)`. Directory where comparison CSVs and optional plots
  will be written. Created if it does not exist.

- petSchema:

  `character(1)`. Schema name of the PET table (e.g. `"omop_cmbd"`).

- petTable:

  `character(1)`. Table name of the pregnancy episode table (e.g.
  `"pregnancy_episode"`). Must contain at least `person_id`,
  `pregnancy_start_date`, `pregnancy_end_date`, and `pregnancy_outcome`
  (concept_id).

- minOverlapDays:

  `integer(1)`. Minimum overlap in days to consider an algorithm episode
  and a PET episode as the same pregnancy (default 1).

- logger:

  Optional [`log4r::logger`](https://rdrr.io/pkg/log4r/man/logger.html).
  If `NULL`, a logger is created via `makeLogger(outputFolder)`.

- outputLogToConsole:

  `logical(1)`. Used only when `logger` is `NULL`. Whether to log to the
  console as well as to the log file.

## Value

Invisibly returns a list with elements `episode_counts`, `venn_counts`,
`confusion_2x2`, `ppv_sensitivity`, `date_differences`,
`outcome_confusion`, `outcome_accuracy`, `duration_summary`, and
`duration_distribution`. `confusion_2x2` is the 2x2 table (TP, FN, FP,
TN; TN is `NA` at episode level). `ppv_sensitivity` contains
sensitivity, specificity, PPV, and NPV (specificity and NPV are `NA`
when TN is not defined). All comparison artifacts are also written to
`outputFolder` as CSVs (and optionally PNGs).
