# runPps

Runs PPS algorithm

## Usage

``` r
runPps(
  cdm,
  outputDir,
  uploadConceptSets = FALSE,
  startDate = as.Date("1900-01-01"),
  endDate = Sys.Date(),
  logger,
  ...
)
```

## Arguments

- cdm:

  (\`cdm_reference\`)

- outputDir:

  output directory

- uploadConceptSets:

  if concept sets should be uploaded

- startDate:

  (\`Date(1)\`: \`as.Date("1900-01-01"\`) Start date of data to use. By
  default 1900-01-01

- endDate:

  (\`Date(1)\`: \`Sys.Date()\`) End date of data to use. By default
  today.

- logger:

  (\`logger\`) Logger object.

- ...:

  optional parameters

## Value

cdm object
