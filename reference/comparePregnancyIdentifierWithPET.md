# Compare runPregnancyIdentifier results with the OMOP Pregnancy Extension Table (PET)

Compares algorithm output (from `final_pregnancy_episodes.rds`) to the
PET table and writes comparison summaries to `outputFolder`. Comparisons
include: pregnancy episode and person counts; raw and filtered
(gestation 0-308, end \>= start) person overlap; time-overlap summaries
(PET-\>IPE and IPE-\>PET, 0 and 1 day); Venn counts (both, PET only,
algorithm only); a 2x2 confusion matrix (TP, FN, FP, TN) with
sensitivity and PPV (no gold-standard negatives, so specificity and NPV
are not defined); outcome confusion matrix and accuracy; outcome
comparison by year (same-year pairs, cross-tab LB/SB/AB vs PET outcome);
and pregnancy duration distributions.

## Usage

``` r
comparePregnancyIdentifierWithPET(
  cdm,
  outputFolder,
  petSchema,
  petTable,
  minOverlapDays = 1L,
  removeWithinSourceOverlaps = FALSE,
  logger = NULL,
  outputLogToConsole = TRUE
)
```

## Arguments

- cdm:

  A `cdm_reference` (from CDMConnector) with a database connection. The
  PET table is read via `petSchema` and `petTable`.

- outputFolder:

  `character(1)`. Directory containing pipeline outputs
  (`final_pregnancy_episodes.rds`) and where comparison CSVs and
  optional plots will be written. Created if it does not exist.

- petSchema:

  `character(1)`. Schema name of the PET table (e.g. `"omop_cmbd"`).

- petTable:

  `character(1)`. Table name of the pregnancy episode table (e.g.
  `"pregnancy_episode"`). Must contain at least `person_id`,
  `pregnancy_start_date`, `pregnancy_end_date`, and `pregnancy_outcome`
  (concept_id). Gestational length in days is computed from start and
  end dates in the database via
  [`CDMConnector::datediff()`](https://darwin-eu.github.io/CDMConnector/reference/datediff.html);
  the table need not have a `gestational_length_in_day` column.

- minOverlapDays:

  `integer(1)`. Minimum overlap in days to consider an algorithm episode
  and a PET episode as the same pregnancy (default 1).

- removeWithinSourceOverlaps:

  `logical(1)`. If `TRUE`, before matching the code removes overlapping
  episodes within PET and within the algorithm (greedy non-overlapping
  by start date per person), which can reduce many-to-many candidate
  pairs. Default `FALSE`.

- logger:

  Optional [`log4r::logger`](https://rdrr.io/pkg/log4r/man/logger.html).
  If `NULL`, a logger is created via `makeLogger(outputFolder)`.

- outputLogToConsole:

  `logical(1)`. Used only when `logger` is `NULL`. Whether to log to the
  console as well as to the log file.

## Value

Nothing. The summarised result is written to
`file.path(outputFolder, "pet_comparison_summarised_result.csv")`. Use
[`omopgenerics::importSummarisedResult()`](https://darwin-eu.github.io/omopgenerics/reference/importSummarisedResult.html)
to read it and
[`visOmopResults::visTable()`](https://darwin-eu.github.io/visOmopResults/reference/visTable.html)
to display it.
