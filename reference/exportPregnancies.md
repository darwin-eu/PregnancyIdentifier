# Export pregnancy results into shareable summary CSVs

Reads the patient-level pregnancy episode results produced by the
pipeline (from `outputFolder`; see
[`runPregnancyIdentifier()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/runPregnancyIdentifier.md)),
generates a set of de-identified summary tables (counts, age summaries,
timing distributions, outcome counts, and date completeness checks), and
writes them to `exportDir`.
[`runPregnancyIdentifier()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/runPregnancyIdentifier.md)
runs this step automatically and writes to `exportFolder` (default
`file.path(outputDir, "export")`); use `exportPregnancies()` when you
need to re-export or write to a different directory. Does not create a
ZIP file; use
[`zipExportFolder()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/zipExportFolder.md)
after export (and optionally after writing PET comparison tables to the
same folder) to create an archive.

## Usage

``` r
exportPregnancies(cdm, outputFolder, exportDir, minCellCount = 5, res = NULL)
```

## Arguments

- cdm:

  (\`cdm_reference\`) CDM reference used to compute exports that require
  database tables (e.g., \`person\`, \`observation_period\`).

- outputFolder:

  (\`character(1)\`) Directory containing pipeline outputs (e.g.,
  \`final_pregnancy_episodes.rds\`, logs, \`pps_concept_counts.csv\`).

- exportDir:

  (\`character(1)\`) Directory where shareable CSVs will be written.

- minCellCount:

  (\`integer(1)\`) Minimum count threshold for suppression of small
  cells (default 5). Values in (0, minCellCount) are replaced with
  \`NA\`.

- res:

  Optional data frame of pregnancy episodes. If provided, used instead
  of reading `final_pregnancy_episodes.rds` from `outputFolder`. Used
  when exporting a conformed copy (e.g. `conformToValidation = "both"`).

## Value

Invisibly returns \`NULL\`. Writes CSVs to \`exportDir\`.

## Details

The export is intended for lightweight QA and sharing across sites.
Small cell counts can be suppressed via `minCellCount`.
