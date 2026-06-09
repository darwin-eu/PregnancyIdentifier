# Validate episode periods: overlaps within person and max duration

Checks a dataframe of episode periods (start/end dates per person) for
two conditions and logs warnings: (1) number of records that overlap
with another record within the same person; (2) number of records whose
span (end - start in days) exceeds a maximum (default 308 days).

## Usage

``` r
validateEpisodePeriods(
  df,
  personIdCol,
  startDateCol,
  endDateCol,
  logger,
  maxDays = 308
)
```

## Arguments

- df:

  A dataframe with at least person id and start/end date columns.

- personIdCol:

  Character. Name of the person identifier column.

- startDateCol:

  Character. Name of the period start date column.

- endDateCol:

  Character. Name of the period end date column.

- logger:

  A log4r logger object (e.g. from \`makeLogger()\`).

- maxDays:

  Numeric. Threshold in days for the second warning (default 308).

## Value

Invisibly returns \`df\` unchanged.
