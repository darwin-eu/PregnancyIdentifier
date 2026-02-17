
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PregnancyIdentifier

[![Tests
(PostgreSQL)](https://github.com/darwin-eu/PregnancyIdentifier/actions/workflows/test-postgres.yaml/badge.svg)](https://github.com/darwin-eu/PregnancyIdentifier/actions/workflows/test-postgres.yaml)
[![Tests
(Snowflake)](https://github.com/darwin-eu/PregnancyIdentifier/actions/workflows/test-snowflake.yaml/badge.svg)](https://github.com/darwin-eu/PregnancyIdentifier/actions/workflows/test-snowflake.yaml)
[![Tests
(Spark)](https://github.com/darwin-eu/PregnancyIdentifier/actions/workflows/test-spark.yaml/badge.svg)](https://github.com/darwin-eu/PregnancyIdentifier/actions/workflows/test-spark.yaml)
[![Tests
(SQLServer)](https://github.com/darwin-eu/PregnancyIdentifier/actions/workflows/test-sqlserver.yaml/badge.svg)](https://github.com/darwin-eu/PregnancyIdentifier/actions/workflows/test-sqlserver.yaml)

Identify pregnancy episodes in OMOP CDM data using the **HIPPS**
algorithm (Smith et al. 2024,
[doi:10.1093/jamia/ocae195](https://doi.org/10.1093/jamia/ocae195)).

------------------------------------------------------------------------

Observational health data rarely has `pregnancy_start` or
`pregnancy_end` variables. More often we get scattered pregnancy-related
events such as live birth, gestational week 12, delivery procedure,
miscarriage, etc. **PregnancyIdentifier** turns pregnancy-related codes
into:

- **One row per pregnancy episode**
- **Inferred start and end dates** (and precision) from gestational
  timing evidence.
- **Standard outcome categories** (LB, SB, AB, SA, ECT, DELIV, PREG) you
  can use in analyses or exports.

The pipeline combines outcome-anchored episodes (HIP), timing-anchored
episodes (PPS), merges them (HIPPS), then refines start dates (ESD)—so
you get a consistent definition of a pregnancy across sites and data
sources.

------------------------------------------------------------------------

## How to use it

**Install** (requires R ≥ 4.1 and CDMConnector):

``` r
# From GitHub (DARWIN EU)
remotes::install_github("darwin-eu/PregnancyIdentifier")
```

**Run the full pipeline** (initializes concepts, runs HIP → PPS → merge
→ ESD, writes outputs):

``` r
library(PregnancyIdentifier)
library(CDMConnector)

cdm <- mockPregnancyCdm()  # or your real cdm_reference

runPregnancyIdentifier(
  cdm       = cdm,
  outputDir = "pregnancy_output",
  startDate = as.Date("2000-01-01"),
  endDate   = Sys.Date()
)
```

**Use the result:**  
`pregnancy_output/final_pregnancy_episodes.rds` is a data frame with one
row per pregnancy episode: `person_id`, `final_episode_start_date`,
`final_episode_end_date`, `final_outcome_category`,
`esd_precision_days`, and other esd\_\* QA/concordance columns. Load it
for cohort definition, export, or further analysis.

Optional: run **export** for de-identified summary CSVs and a ZIP:

``` r
exportPregnancies(cdm, outputDir = "pregnancy_output", exportDir = "pregnancy_export")
```

------------------------------------------------------------------------

## Documentation

- **Vignettes:** [Pipeline
  overview](https://darwin-eu.github.io/PregnancyIdentifier/articles/algorithm.html),
  [HIP](https://darwin-eu.github.io/PregnancyIdentifier/articles/hip.html),
  [PPS](https://darwin-eu.github.io/PregnancyIdentifier/articles/pps.html),
  [Merge](https://darwin-eu.github.io/PregnancyIdentifier/articles/merge.html),
  [ESD](https://darwin-eu.github.io/PregnancyIdentifier/articles/esd.html).
- **Reference:** [pkgdown
  site](https://darwin-eu.github.io/PregnancyIdentifier/).
- **Issues:** [GitHub
  issues](https://github.com/darwin-eu/PregnancyIdentifier/issues).

------------------------------------------------------------------------

## License

Apache 2.0.
