# Create a ZIP archive of an export folder

Zips all files in the given export directory into a single ZIP file. Use
this after
[`exportPregnancies()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/exportPregnancies.md)
and, if applicable, after writing PET comparison tables to the same
folder, so the archive includes both shareable CSVs and PET comparison
outputs.

## Usage

``` r
zipExportFolder(exportDir, zipPath = NULL)
```

## Arguments

- exportDir:

  (`character(1)`) Path to the export folder (contents will be zipped).

- zipPath:

  (`character(1)` or `NULL`) Full path for the output ZIP file. If
  `NULL`, the ZIP is created inside `exportDir` with a name like
  `YYYY-MM-DD-version-results.zip` (using today's date and package
  version). Supply `zipPath` for a custom name (e.g. including your CDM
  name).

## Value

Invisibly returns the path to the created ZIP file.
