# Changelog

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
