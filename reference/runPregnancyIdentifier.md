# Run PregnancyIdentifier end-to-end (HIP + PPS + merge + ESD + export)

Orchestrates the full PregnancyIdentifier pipeline (adapted from the
HIPPS implementation at
https://github.com/louisahsmith/allofus-pregnancy/) on an OMOP CDM with
\`CDMConnector\`.

## Usage

``` r
runPregnancyIdentifier(
  cdm,
  outputFolder,
  exportFolder = NULL,
  startDate = as.Date("1900-01-01"),
  endDate = Sys.Date(),
  justGestation = TRUE,
  minCellCount = 5L,
  debugMode = FALSE,
  outputLogToConsole = TRUE,
  conformToValidation = FALSE
)
```

## Arguments

- cdm:

  (\`cdm_reference\`) A CDM reference created by \`CDMConnector\`
  pointing to an OMOP CDM instance.

- outputFolder:

  (\`character(1)\`) Directory for pipeline outputs: \*\*person-level
  and episode-level data\*\* (RDS files such as
  \`final_pregnancy_episodes.rds\`, logs, \`runStart.csv\`, concept
  counts, etc.). Created if it does not exist.

- exportFolder:

  (\`character(1)\`) Directory where \*\*shareable aggregated CSV
  files\*\* are written. Required. Defaults to \`file.path(outputFolder,
  "export")\`. These CSVs can be used as input to the Shiny app. With
  \`conformToValidation = "both"\`, results are written to
  \`exportFolder/conform_false\` and \`exportFolder/conform_true\`.

- startDate:

  (\`Date(1)\`) Lower bound for concept/event dates included in the run.
  Default \`1900-01-01\`.

- endDate:

  (\`Date(1)\`) Upper bound for concept/event dates included in the run.
  Default \`Sys.Date()\`.

- justGestation:

  (\`logical(1)\`) If \`TRUE\`, allow episodes consisting only of
  gestational concepts (HIP behavior). Passed through to \`runHip()\`.

- minCellCount:

  (\`integer(1)\`) Minimum cell count used for suppression in exported
  summaries. Passed through to \`exportPregnancies()\`.

- debugMode:

  (\`logical(1)\`) Should extra intermediate datasets be written to the
  outputFolder for debugging? \`TRUE\` or \`FALSE\` (default)

- outputLogToConsole:

  (\`logical(1)\`) If \`TRUE\` (default), log messages are written to
  the console. If \`FALSE\`, only to the log file (e.g. for tests).

- conformToValidation:

  (`logical(1)` or `"both"`) If `TRUE`, after validation modify episode
  output to conform (remove overlapping episodes and episodes longer
  than 308 days). If `FALSE` (default), only validate and log issues. If
  `"both"`, the pipeline runs once without conforming; export is run
  twice and results are written to `exportFolder/conform_false` and
  `exportFolder/conform_true`.

## Value

Invisibly returns \`NULL\`. Side effects: - Adds/updates tables inside
\`cdm\` (e.g., \`cdm\$preg_hip_records\`, concept tables, and
intermediate algorithm tables). - Writes person-level and episode-level
data (RDS, logs) under \`outputFolder\`. - Writes shareable aggregated
CSV files to \`exportFolder\` (or \`exportFolder/conform_false\` and
\`exportFolder/conform_true\` when \`conformToValidation = "both"\`).

## Details

The pipeline performs: 1) cohort initialization (\`initPregnancies()\`):
creates pregnancy-related concept tables and an initial cohort in the
CDM, 2) HIP episode identification (\`runHip()\`): identifies episodes
based on HIP rules, 3) PPS episode identification (\`runPps()\`):
identifies episodes based on PPS rules, 4) merge (\`mergeHipps()\`):
merges HIP and PPS into combined HIPPS episodes, 5) ESD refinement
(\`runEsd()\`): derives inferred pregnancy start/precision and enriches
merged episodes, 6) export (\`exportPregnancies()\`): writes shareable
aggregated CSV files to \`exportFolder\` (with optional small-cell
suppression).
