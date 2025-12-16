# runEsd

Runs ESD

## Usage

``` r
runEsd(
  HIPPS,
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

- HIPPS:

  (\`data.frame\`)

- cdm:

  (\`cdm_reference\`)

- outputDir:

  (\`character(1)\`)

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

  Extra (development) parameters

## Value

\`NULL\`
