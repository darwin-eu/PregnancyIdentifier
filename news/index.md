# Changelog

## PregnancyIdentifier 3.1.1

### Export changes

- Age at first pregnancy export (`age_summary_first_pregnancy.csv`) now
  includes all episodes, not just live births. Output is stratified by
  `final_outcome_category`: **overall** (any outcome), each individual
  outcome (LB, SB, AB, SA, ECT, DELIV, PREG), and a combined **LB or
  PREG** group.

### Shiny app

- Age at first pregnancy tab now shows an outcome picker and displays
  side-by-side boxplots coloured by outcome category.
  Backwards-compatible with older export files that lack the
  `final_outcome_category` column.

## PregnancyIdentifier 3.1.0

### Algorithm changes

- Restored all 5 PPS source domains (condition_occurrence,
  procedure_occurrence, observation, measurement, visit_occurrence).
  v3.0.0 had dropped observation and visit_occurrence; they are now
  included again as a precaution even though current PPS concepts do not
  map to those domains. (Just to be on the safe side.)

### PET comparison improvements

- Replaced time-overlap summary metric with three directional
  date-difference measures (PET minus algorithm): start date difference,
  end date difference, and duration difference. Reported as
  mean/median/SD/min/Q25/Q75/max.
- Added outcome-stratified date differences (same start/end/duration
  differences broken down by algorithm outcome category: LB, SB, SA, AB,
  etc.).
- Added binned date-difference distributions for episode alignment
  visualisation (bins from “≤ -30 days” through “0” to “≥ 30 days”),
  both overall and stratified by outcome.
- Removed the `time_overlap_summary` metric from new output (overlap
  days was confounded by episode length). Old result files with this
  metric are still supported in the Shiny app.

### Shiny app

- Complete rewrite of the Shiny app: replaced legacy single-file `app.R`
  with modular architecture using `global.R`, `ui.R`, `server.R`, and
  per-tab modules in `utils/`.
- Added new **Alignment** tab to PET comparison: interactive histogram
  showing the distribution of start/end/duration date differences
  between matched PET and algorithm episodes, with colour-coded bins
  (green = exact match, red = large discrepancy). Supports filtering by
  measure and stratification by outcome.
- Added descriptive labels to PET comparison: all variable names in the
  summarised result now use human-readable labels (e.g. “Start date
  difference (PET - Algorithm, days)”). Legacy short labels from older
  result files are automatically remapped for display.
- Added backward-compatible display label remapping so the app works
  with both old (pre-v3.1.0) and new result files.
- Added explanatory notes to the **Pregnancy Overlap** tab
  (sequential/lag-based check) and the **Quality Check Cleanup** tab
  (all-pairs check) explaining why overlap counts may differ between
  them.
- Added **Overview** tab with markdown methodology description for PET
  comparison.
- Added episode and person count denominators to the precision days
  export.
- Various UI improvements: formatted tables, interactive plotly charts,
  download buttons for plots and tables.

### Export changes

- `gestational_duration_counts.csv` now includes `episode_count` and
  `person_count` columns per outcome category.
- Added `precision_days_denominators.csv` to the export folder.

### Documentation

- Updated PET comparison vignette: removed time-overlap section, added
  sections for date differences, date differences by outcome, and
  alignment distributions.
- Added `extras/algorithm_improvements_v2_v3.md` summarising
  episode-count differences between v2 and v3.
- Added `extras/v2_vs_v3_episode_drop.md` with detailed analysis of why
  episode/person counts differ.

## PregnancyIdentifier 3.0.5

- Default output now matches v2 episode counts: ESD filters for
  observation period, start \>= end, and zero-length episodes are no
  longer applied by default. Set `conformToValidation = TRUE` to enable
  cleanup, or use `conformToValidation = "both"` to export both
  versions.
- Added per-record quality flags to `final_pregnancy_episodes.rds`:
  `is_start_outside_observation_period`,
  `is_end_outside_observation_period`, `is_start_gte_end`,
  `is_zero_length`, `is_too_long`, `is_overlapping`. Flags characterize
  episode quality without removing records.
- Enriched `quality_check_cleanup.csv` with flag-based counts (records
  and persons for each quality flag category).
- Extended `attrition_if_cleanup.csv` with observation period and
  zero-length steps when quality flags are present.
- Restored PREG-only concepts in
  [`initPregnancies()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/initPregnancies.md)
  (reverted v3.0.0 filter that dropped PREG-category concepts without
  gestational age values).
- Added person-level summary to PET comparison output.
- Added PET-only HIP/PPS record characterization to PET comparison.
- `fixStartBeforeEnd()` only runs when `conformToValidation = TRUE`. By
  default, episodes with inverted start/end dates are left as-is and
  characterized via the `is_start_gte_end` quality flag.

## PregnancyIdentifier 3.0.4

- Shiny app updates and fixes.
- Added min cell count to PET comparison.

## PregnancyIdentifier 3.0.3

- Shiny app updates
- API changes

## PregnancyIdentifier 3.0.2

- Shiny app updates
- bugfixes
- Improved attrition output
- PET comparison vignette

## PregnancyIdentifier 3.0.1

- Shiny app updates

## PregnancyIdentifier 3.0.0

- Refactored code
- Additional testing
