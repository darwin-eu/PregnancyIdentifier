# Compare PregnancyIdentifier results with national statistics

Reads the bundled National_Statistics_Obj2_v2.csv and compares key
indicators (birth rate, maternal age, gestational duration distribution,
delivery mode) with the corresponding summaries produced by the
pipeline.

## Usage

``` r
compareWithNationalStats(
  incidence = NULL,
  gestationalWeeksBinned = NULL,
  deliveryModeSummary = NULL,
  outcomeCategoriesCount = NULL,
  yearlyTrend = NULL,
  ageSummary = NULL,
  nationalStatsPath = NULL
)
```

## Arguments

- incidence:

  A `summarised_result` incidence object (may be NULL).

- gestationalWeeksBinned:

  Data frame of binned gestational-age counts produced by global.R
  (columns: cdm_name, gestational_weeks, n, pct, and optionally
  final_outcome_category).

- deliveryModeSummary:

  Data frame of delivery-mode percentages (long format with columns:
  cdm_name, final_outcome_category, mode, pct).

- outcomeCategoriesCount:

  Data frame of outcome-category counts (columns: cdm_name,
  outcome_category / final_outcome_category, n, pct).

- yearlyTrend:

  Data frame with yearly episode counts (columns: cdm_name, column,
  value \[year\], count).

- ageSummary:

  Data frame with age summary statistics (columns: cdm_name,
  final_outcome_category, mean, median, etc.).

- nationalStatsPath:

  Path to the national statistics CSV. Defaults to the bundled copy
  inside the installed package.

## Value

A list with components:

- gestational_duration:

  Long data frame comparing gestational-age bin percentages (source =
  "Database" or "National Statistics").

- delivery_mode:

  Long data frame comparing delivery-mode percentages.

- national_stats_raw:

  The parsed national statistics data frame.
