# PregnancyIdentifier code style

## Naming conventions

- **camelCase** for:
  - Function names (e.g. `runEsd`, `mergeOutcomeAndGestation`, `getGtTiming`)
  - Argument names (e.g. `outputDir`, `startDate`, `personIds`, `episodesWithGestationalTimingInfoDf`)
  - Variable names in R code (e.g. `finalDf`, `timingDf`, `bothVisitIds`, `percMajority`)

- **snake_case** for:
  - CDM table names (e.g. `cdm$preg_hip_records`, `cdm$outcome_episodes_df`, `cdm$matcho_outcome_limits`)
  - Column names in data frames and lazy tbls (e.g. `person_id`, `inferred_episode_start`, `final_outcome_category`)

When building tibbles or lists that define row/column structure, use snake_case for the **names** that become column names (e.g. `list(inferred_start_date = x, precision_days = y)`).
