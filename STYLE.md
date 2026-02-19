# PregnancyIdentifier code style

## Naming conventions

- **camelCase** for:
  - Function names (e.g. `runEsd`, `mergeOutcomeAndGestation`,
    `getGtTiming`)
  - Argument names (e.g. `outputDir`, `startDate`, `personIds`,
    `episodesWithGestationalTimingInfoDf`)
  - Variable names in R code (e.g. `finalDf`, `timingDf`,
    `bothVisitIds`, `percMajority`)
- **snake_case** for:
  - CDM table names (e.g. `cdm$preg_hip_records`,
    `cdm$outcome_episodes_df`, `cdm$matcho_outcome_limits`)
  - Column names in data frames and lazy tbls (e.g. `person_id`,
    `final_episode_start_date`, `final_outcome_category`)

When building tibbles or lists that define row/column structure, use
snake_case for the **names** that become column names
(e.g. `list(inferred_start_date = x, precision_days = y)`).

- **Lowercase** for:
  - Output file names written to `outputDir` (e.g. `hip_episodes.rds`,
    `pps_episodes.rds`, `hipps_episodes.rds`, `esd.rds`,
    `final_pregnancy_episodes.rds`). Use lowercase so behavior is
    consistent on case-sensitive (e.g. Linux, CI) and case-insensitive
    (e.g. macOS) filesystems and to avoid bugs from mismatched casing in
    tests or downstream code.
