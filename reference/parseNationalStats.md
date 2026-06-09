# Parse the national statistics CSV into a tidy long-format data frame.

The CSV uses European number formatting (dots as thousands separators)
for some numeric fields. This function normalises those to standard
numerics and pivots country columns into long format.

## Usage

``` r
parseNationalStats(path)
```

## Arguments

- path:

  Path to the CSV file.

## Value

A tibble with columns: indicator, variable, level, year, country, value.
