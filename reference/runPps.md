# Run the PPS Algorithm to Identify Pregnancy Episodes

This function executes the Pregnancy Progression Signature (PPS)
algorithm against a Common Data Model (CDM) instance. It inserts the
required PPS concept lookup table, extracts gestational timing evidence
(e.g., gestational weeks, trimesters) across OMOP clinical domains,
assembles person-level gestational timing records, and writes
intermediate episode and summary files to the specified output
directory.

## Usage

``` r
runPps(
  cdm,
  outputDir,
  startDate = as.Date("1900-01-01"),
  endDate = Sys.Date(),
  logger,
  debugMode = FALSE
)
```

## Arguments

- cdm:

  A \`cdm_reference\` object; must include all OMOP tables and structure
  needed for pregnancy concept search.

- outputDir:

  Character. Directory path where intermediate and output RDS files will
  be saved.

- startDate:

  Date (\`Date(1)\`). Earliest clinical date to be considered for
  gestational timing evidence (default: \`"1900-01-01"\`).

- endDate:

  Date (\`Date(1)\`). Latest clinical date to be considered for
  gestational timing evidence (default: \`Sys.Date()\`).

- logger:

  \`log4r\` logger object (required) for emitting information and debug
  messages.

- debugMode:

  (\`Logical\`) Should intermediate datasets be written to the output
  folder for debugging? TRUE or FALSE (default)

## Value

Returns the input \`cdm_reference\` invisibly, possibly modified with
intermediate tables in its environment. Main results are side effects:
RDS files with person-level gestational timing episodes and summary
statistics are written to \`outputDir\`.
