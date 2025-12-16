# runHipps

Runs the HIPPS algorithm (HIP, PPS, and ESD) from:
https://github.com/louisahsmith/allofus-pregnancy/ without requiring the
\`allofus\` R package.

## Usage

``` r
runHipps(
  cdm,
  outputDir,
  startDate = as.Date("1900-01-01"),
  endDate = Sys.Date(),
  justGestation = TRUE,
  ...
)
```

## Arguments

- cdm:

  (\`cdm_reference\`) A CDM-Reference object from CDMConnector.

- outputDir:

  (\`character(1)\`) Output directory to write output to.

- startDate:

  (\`Date(1)\`: \`as.Date("1900-01-01"\`) Start date of data to use. By
  default 1900-01-01

- endDate:

  (\`Date(1)\`: \`Sys.Date()\`) End date of data to use. By default
  today.

- justGestation:

  (\`logical(1)\`: \`TRUE\`) Should episodes that only have gestational
  concepts be concidered?

- ...:

  Dev params

## Value

\`NULL\`
