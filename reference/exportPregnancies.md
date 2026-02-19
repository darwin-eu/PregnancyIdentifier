# Export pregnancy results into shareable summary CSVs

Reads the patient-level pregnancy episode results produced by the
pipeline (from \`outputDir\`; see \`runPregnancyIdentifier()\`),
generates a set of de-identified summary tables (counts, age summaries,
timing distributions, outcome counts, and date completeness checks),
writes them to \`exportDir\`, and creates a ZIP archive of all exported
files.

## Usage

``` r
exportPregnancies(cdm, outputDir, exportDir, minCellCount = 5)
```

## Arguments

- cdm:

  (\`cdm_reference\`) CDM reference used to compute exports that require
  database tables (e.g., \`person\`, \`observation_period\`).

- outputDir:

  (\`character(1)\`) Directory containing pipeline outputs (e.g.,
  \`final_pregnancy_episodes.rds\`, logs, \`pps_concept_counts.csv\`).

- exportDir:

  (\`character(1)\`) Directory where shareable CSVs (and ZIP) will be
  written.

- minCellCount:

  (\`integer(1)\`) Minimum count threshold for suppression of small
  cells (default 5). Values in (0, minCellCount) are replaced with
  \`NA\`.

## Value

Invisibly returns \`NULL\`. Writes CSVs and a ZIP file to \`exportDir\`.

## Details

The export is intended for lightweight QA and sharing across sites.
Small cell counts can be suppressed via \`minCellCount\`.
