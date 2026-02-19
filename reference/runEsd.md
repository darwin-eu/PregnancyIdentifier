# runEsd

Runs the Episode Start Date (ESD) algorithm to infer pregnancy episode
start dates for merged HIP/PPS episodes.

## Usage

``` r
runEsd(
  cdm,
  outputDir,
  startDate = as.Date("1900-01-01"),
  endDate = Sys.Date(),
  logger,
  debugMode = FALSE,
  conformToValidation = FALSE
)
```

## Arguments

- cdm:

  A CDM reference, must include all necessary OMOP tables and concept
  sets for pregnancy inference algorithms.

- outputDir:

  Character. Path to directory where input and output RDS files reside.

- startDate:

  Earliest episode date to include (as.Date). Default:
  `as.Date("1900-01-01")`.

- endDate:

  Latest episode date to include (as.Date). Default:
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- logger:

  A `log4r` logger object for info/debug messages.

- debugMode:

  (\`logical(1)\`) Should the ESD algorithm write intermediate datasets
  to the outputDir? \`TRUE\` or \`FALSE\` (default)

- conformToValidation:

  (\`logical(1)\`: \`FALSE\`) If \`TRUE\`, modify episodes to conform
  (remove overlaps and length \> 308 days). Validation and logging
  always run.

## Value

Invisibly returns `NULL`. Main result is written as an RDS file
(`final_pregnancy_episodes.rds`) to `outputDir`. The output contains one
row per inferred pregnancy episode. Columns include:
`final_episode_start_date`, `final_episode_end_date`,
`final_outcome_category` (no prefix), and ESD-derived columns with
`esd_` prefix: `esd_precision_days`, `esd_precision_category`,
`esd_gestational_age_days_calculated`, `esd_gw_flag`, `esd_gr3m_flag`,
`esd_outcome_match`, `esd_term_duration_flag`,
`esd_outcome_concordance_score`, `esd_preterm_status_from_calculation`,
plus merge/HIPPS metadata (e.g. `recorded_episode_start`,
`hip_end_date`, `pps_end_date`).

## Details

This function performs the following major steps:

1.  Loads previously merged HIPPS episode table from `outputDir`.

2.  Extracts gestational timing concept evidence (e.g. gestational week,
    trimester concepts) for each episode within the specified date
    range.

3.  Infers episode start and start precision using all available
    gestational timing evidence, with logging.

4.  Merges inferred start dates, timing evidence, and metadata back onto
    the merged episodes table and computes final start/end/outcome
    fields.

5.  Filters episodes to retain only those overlapping the requested
    `startDate`–`endDate` study period, including episodes with missing
    inferred dates.

6.  Writes the resulting cohort of identified pregnancy episodes to an
    RDS file (`final_pregnancy_episodes.rds`) in `outputDir`.
