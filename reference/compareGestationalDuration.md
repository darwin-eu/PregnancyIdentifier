# Compare gestational-duration bin distributions

Maps the app's gestational-week bins to the national-statistics bins and
returns a combined long-format data frame.

## Usage

``` r
compareGestationalDuration(gestationalWeeksBinned, natl)
```

## Arguments

- gestationalWeeksBinned:

  App data (may be NULL).

- natl:

  Parsed national statistics.

## Value

A tibble.
