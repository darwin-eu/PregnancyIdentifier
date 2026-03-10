# PregnancyIdentifier 3.1.1

## Export changes

* Age at first pregnancy export (`age_summary_first_pregnancy.csv`) now includes
  all episodes, not just live births. Output is stratified by
  `final_outcome_category`: **overall** (any outcome), each individual outcome
  (LB, SB, AB, SA, ECT, DELIV, PREG), and a combined **LB or PREG** group.
  _No expected impact on episode or person counts._

## Shiny app

* Age at first pregnancy tab now shows an outcome picker and displays
  side-by-side boxplots coloured by outcome category. Backwards-compatible with
  older export files that lack the `final_outcome_category` column.
  _No expected impact on episode or person counts._

# PregnancyIdentifier 3.1.0

## Algorithm changes

* Restored all 5 PPS source domains (condition_occurrence, procedure_occurrence,
  observation, measurement, visit_occurrence). v3.0.0 had dropped observation
  and visit_occurrence; they are now included again as a precaution even though
  current PPS concepts do not map to those domains. (Just to be on the safe side.)
  _No expected impact on counts with current concept sets (neutral). All current
  PPS concepts resolve to Condition, Procedure, or Measurement domains.
  Could increase counts in the future if PPS concepts are added that map to
  Observation or Visit domains._

## PET comparison improvements

* Replaced time-overlap summary metric with three directional date-difference
  measures (PET minus algorithm): start date difference, end date difference,
  and duration difference. Reported as mean/median/SD/min/Q25/Q75/max.
* Added outcome-stratified date differences (same start/end/duration
  differences broken down by algorithm outcome category: LB, SB, SA, AB, etc.).
* Added binned date-difference distributions for episode alignment
  visualisation (bins from "≤ -30 days" through "0" to "≥ 30 days"),
  both overall and stratified by outcome.
* Removed the `time_overlap_summary` metric from new output (overlap days was
  confounded by episode length). Old result files with this metric are still
  supported in the Shiny app.

## Shiny app

* Complete rewrite of the Shiny app: replaced legacy single-file `app.R` with
  modular architecture using `global.R`, `ui.R`, `server.R`, and per-tab
  modules in `utils/`.
* Added new **Alignment** tab to PET comparison: interactive histogram showing
  the distribution of start/end/duration date differences between matched PET
  and algorithm episodes, with colour-coded bins (green = exact match,
  red = large discrepancy). Supports filtering by measure and stratification
  by outcome.
* Added descriptive labels to PET comparison: all variable names in the
  summarised result now use human-readable labels (e.g. "Start date difference
  (PET - Algorithm, days)"). Legacy short labels from older result files are
  automatically remapped for display.
* Added backward-compatible display label remapping so the app works with both
  old (pre-v3.1.0) and new result files.
* Added explanatory notes to the **Pregnancy Overlap** tab (sequential/lag-based
  check) and the **Quality Check Cleanup** tab (all-pairs check) explaining why
  overlap counts may differ between them.
* Added **Overview** tab with markdown methodology description for PET comparison.
* Added episode and person count denominators to the precision days export.
* Various UI improvements: formatted tables, interactive plotly charts, download
  buttons for plots and tables.

## Export changes

* `gestational_duration_counts.csv` now includes `episode_count` and
  `person_count` columns per outcome category.
* Added `precision_days_denominators.csv` to the export folder.

## Documentation

* Updated PET comparison vignette: removed time-overlap section, added sections
  for date differences, date differences by outcome, and alignment distributions.
* Added `extras/algorithm_improvements_v2_v3.md` summarising episode-count
  differences between v2 and v3.
* Added `extras/v2_vs_v3_episode_drop.md` with detailed analysis of why
  episode/person counts differ.


# PregnancyIdentifier 3.0.5

## Algorithm changes

* Default output now matches v2 episode counts: ESD filters for observation
  period, start >= end, and zero-length episodes are no longer applied by
  default. Set `conformToValidation = TRUE` to enable cleanup, or use
  `conformToValidation = "both"` to export both versions.
  _Expected to **increase** episode and person counts vs v3.0.2-v3.0.4.
  Observation period filter was the single largest source of episode removal;
  making it conditional restores these episodes._
* Restored PREG-only concepts in `initPregnancies()` (reverted v3.0.0 filter
  that dropped PREG-category concepts without gestational age values).
  _Expected to **increase** episode and person counts (low-medium magnitude).
  Persons whose only pregnancy evidence was PREG-only concepts are now included._
* `fixStartBeforeEnd()` only runs when `conformToValidation = TRUE`. By
  default, episodes with inverted start/end dates are left as-is and
  characterized via the `is_start_gte_end` quality flag.
  _Expected to **increase** episode counts vs v3.0.2-v3.0.4 (episodes that
  would have been dropped or had their start date reset are now retained)._

## Export changes

* Added per-record quality flags to `final_pregnancy_episodes.rds`:
  `is_start_outside_observation_period`, `is_end_outside_observation_period`,
  `is_start_gte_end`, `is_zero_length`, `is_too_long`, `is_overlapping`.
  Flags characterize episode quality without removing records.
  _No impact on counts (flags are informational only)._
* Enriched `quality_check_cleanup.csv` with flag-based counts (records and
  persons for each quality flag category).
* Extended `attrition_if_cleanup.csv` with observation period and zero-length
  steps when quality flags are present.
* Added person-level summary to PET comparison output.
* Added PET-only HIP/PPS record characterization to PET comparison.

# PregnancyIdentifier 3.0.4

* Shiny app updates and fixes.
  _No impact on episode or person counts._
* Added min cell count to PET comparison.
  _No impact on episode or person counts._

# PregnancyIdentifier 3.0.3

* Renamed `exportDir` parameter to `exportFolder` and refactored output paths.
  _No impact on episode or person counts._
* Shiny app updates.
  _No impact on episode or person counts._

# PregnancyIdentifier 3.0.2

## Algorithm changes

* Added observation period filters (ESD steps 2-3). Pregnancy start date and
  end date must both fall within an `observation_period` for the person.
  Episodes where either date falls outside any observation period are dropped.
  Episodes with NA start dates are also dropped. Applied unconditionally.
  _Expected to **decrease** episode and person counts (very high magnitude).
  This was the single largest source of episode removal in v3.0.2-v3.0.4._
* Added start >= end filter (ESD step 4). Episodes where
  `final_episode_start_date >= final_episode_end_date` are dropped. When
  `conformToValidation = TRUE`, `fixStartBeforeEnd()` runs first to rescue
  some episodes by resetting `start = end - max_term`. Applied unconditionally.
  _Expected to **decrease** episode counts (medium magnitude)._
* Added zero-length episode filter (ESD step 6). Episodes where gestational
  length in days equals exactly 0 are dropped. Applied unconditionally.
  _Expected to **decrease** episode counts (low-medium magnitude)._
* Expanded gestational age concept IDs in ESD timing search to include
  concepts from `gestational_age_concepts.csv`.
  _Expected to **increase** start date precision (low indirect effect on counts).
  Better start dates can rescue episodes from observation period and
  start >= end filters._
* Added explicit `as.Date()` coercion before date comparisons in the study
  period filter, fixing edge cases where POSIXct or character date columns
  could produce incorrect comparison results due to timezone issues.
  _Variable impact on counts depending on backend._
* Added `conformToValidation = "both"` mode. Runs ESD without conformance,
  exports non-conformed result, then applies cleanup in memory and exports
  conformed result. Allows direct comparison.
  _No impact on default counts._

## Other changes

* Shiny app updates.
* Improved attrition output.
* Added PET comparison vignette.

# PregnancyIdentifier 3.0.1

## Bug fixes

* Fixed gestation record selection: v3.0.0 used two separate sources
  `union_all`'d together, both using only `value_as_number`. Records with null
  `value_as_number` but non-null `gest_value` (from the concept sheet) were
  never picked up. v3.0.1 uses `coalesce(value_as_number, gest_value)` in a
  single unified pipeline, allowing concept-sheet gestational ages to contribute
  even without a measurement value. Also fixes a varchar-to-integer cast issue
  on some backends (Spark/Redshift).
  _Expected to **increase** episode counts (medium-high magnitude). More
  records can now form gestation episodes._
* Fixed person-scoped overlap removal. v3.0.0 collected overlapping PREG
  `gest_id` values globally and filtered using `%in%`. If a `gest_id` string
  overlapped for person A, it could be removed for ALL persons sharing that
  string. v3.0.1 scopes removal to `(person_id, gest_id)` pairs via
  `anti_join`.
  _Expected to **increase** episode counts (medium magnitude). Episodes that
  were incorrectly removed from unrelated persons are now retained._
* Fixed NA-safe `restFinal` filter. v3.0.0's filter
  `!(!is.na(max_gest_week) & is_over_min == 0)` evaluated to NA in SQL when
  `is_over_min` was NA (common for gestation-only episodes), causing the row to
  be dropped. v3.0.1 explicitly includes `is.na(is_over_min)` as a keep
  condition.
  _Expected to **increase** episode counts (medium magnitude). Gestation-only
  episodes that were silently dropped by NA evaluation are now retained._
* Added post-union deduplication on `(person_id, final_episode_id)`. v3.0.0
  could produce duplicate rows for the same logical episode due to column
  misalignment in `union_all`.
  _Expected to **decrease** episode counts (low magnitude). Removes exact
  duplicate rows only._
* Added PREG filter when gestation-only episodes exist: when
  `justGestation = TRUE`, drops PREG-category outcome+gestation episodes when
  the same person has a later gestation-only episode.
  _Expected to **decrease** episode counts (low magnitude). Prevents
  double-counting the same pregnancy._
* Fixed union type-casting for strict database backends (PostgreSQL, Spark).
  v3.0.0 used R-level NA values as placeholder columns before `union_all`,
  which could cause failures or silent row drops on strict backends.
  _Expected to **increase** episode counts on affected backends._
* Changed intermediate `compute()` calls from `temporary = TRUE` to
  `temporary = FALSE, overwrite = TRUE`. Some backends (Snowflake, Spark) have
  issues with temporary tables in complex queries.
  _Expected to **increase** episode counts on affected backends._

## Other changes

* Shiny app updates.

# PregnancyIdentifier 3.0.0

## Major refactoring

Complete rewrite of the pregnancy identification pipeline. The core algorithm
(HIP + PPS + Merge + ESD) is preserved, but every stage was refactored with
significant algorithmic improvements. See
`extras/algorithm_changes_by_version.md` for a comprehensive comparison.

**Empirical impact (IPCI):** v3.0.0 with default settings produced approximately
29% fewer episodes and 19% fewer persons than v2.0.1. The drops concentrate in
multi-episode outliers and persons with weak or spurious pregnancy evidence.

## Init stage (`initPregnancies.R`)

* Reviewed and consolidated concept files. v3 loads
  `HIP_concepts_reviewed17022026.xlsx` and `PPS_concepts_reviewed1702026.xlsx`
  instead of the originals. Clinically ambiguous concepts removed.
  _Expected to **decrease** episode and person counts (medium magnitude)._
* Added `gest_value` column from HIP concept spreadsheet through to
  `preg_hip_records`. Allows condition, procedure, and observation records
  (which lack `value_as_number`) to contribute gestational age information via
  the concept sheet. In v2 only measurement records with non-NA
  `value_as_number` could contribute gestational age.
  _Expected to **increase** episode counts (high magnitude). Many more records
  can now provide gestational timing._
* Fixed `condition_occurrence` date filter. v2 filtered on
  `condition_end_date <= endDate`, which excluded conditions whose end date
  extended past the study window (even when start date was in range) and
  excluded all conditions with NULL `condition_end_date` (common in OMOP).
  v3 filters only on `condition_start_date`, consistent with all other domains.
  _Expected to **increase** episode and person counts (low-medium magnitude)._
* PPS data extraction reduced from 5 to 3 source domains (dropped `observation`
  and `visit_occurrence`).
  _No expected impact on counts (neutral). All current PPS concepts resolve to
  Condition, Procedure, or Measurement domains. The inner join against the
  dropped domains returned zero rows in v2._
* Age and sex filtering now applied to PPS records at extraction time. v2 only
  applied female sex and age 15-55 filters to HIP records at extraction; PPS
  records were extracted without demographic filtering and only filtered during
  episode construction. v3 filters both HIP and PPS at extraction.
  _Expected to **decrease** person counts (high magnitude). Persons whose only
  pregnancy evidence was PPS records from an ineligible age or sex are removed
  earlier and more consistently._
* Age calculation changed from `datediff / 365` to `datediff / 365.25`. The
  `/365` method did not account for leap years, making the lower bound ~3-4
  days too generous (including people before their 15th birthday) and the upper
  bound ~14 days too strict (excluding people before their 56th birthday).
  The `/365.25` method is more accurate.
  _Expected to slightly **decrease** counts at the lower age boundary (very
  small magnitude) and slightly **increase** counts at the upper boundary._
* Upper age bound effectively changed from `age < 56` to `age < 57`. v3 uses
  `ageBounds = c(15L, 56L)` but computes `upperAge = ageBounds[2] + 1L = 57`,
  so the filter is `age < 57`. v2 hardcoded `age < 56`. This means all
  56-year-old women are now eligible.
  _Expected to **increase** person counts at the upper age boundary (low-medium
  magnitude). Note: the docstring incorrectly says "< 56"._
* PREG-only concepts (HIP concepts with category "PREG" but no `gest_value`)
  dropped from initial cohort.
  _Expected to **decrease** counts (negligible magnitude). Reverted in v3.0.5._

## HIP stage (`HIP.R`)

* Effective gestation via `coalesce(value_as_number, gest_value)`. Any record
  where the concept sheet provides a `gest_value` can form a gestation episode,
  even without a measurement value. v2 used only 3 hardcoded concept IDs from
  measurements.
  _Expected to **increase** episode counts (high magnitude)._
* Expanded gestational age concept IDs. v3 loads from
  `gestational_age_concepts.csv` instead of 3 hardcoded IDs in v2.
  _Expected to **increase** episode counts (medium magnitude)._
* Default Matcho term days for PREG episodes. v3 coalesces missing
  `min_term`/`max_term` to 140/301 days respectively. In v2, PREG episodes
  got NA for both values, preventing them from participating in the overlap join
  with gestation episodes.
  _Expected to **increase** episode counts (high magnitude)._
* Added `justGestation` filter for PREG episodes. When `justGestation = TRUE`,
  drops PREG-category outcome+gestation episodes if a later gestation-only
  episode exists for the same person. Prevents double-counting.
  _Expected to **decrease** episode counts (low magnitude)._
* Max gestation term changed from 301 to 308 days. Fewer episodes are
  reclassified from their outcome category to "PREG" due to exceeding
  `max_term`.
  _Expected to **increase** episode counts in non-PREG categories (low
  magnitude)._
* Fixed `removeOverlaps` bug with empty `gest_id_list`. v2 set an empty list
  to `""`, causing incorrect filtering. v3 uses a sentinel value.
  _Variable impact on counts (low magnitude)._

## PPS stage (`PPS.R`)

* PPS `min_date` imputation. v3 imputes missing PPS episode min dates as
  `max_date - 280` days. v2 left them as NA, causing the overlap join with HIP
  episodes to fail (because `pregnancy_end >= NA` evaluates to NULL in SQL).
  _Expected to **increase** episode counts (medium magnitude). PPS episodes
  that previously failed to join with HIP episodes now participate in merging._

## Merge stage (`mergeHIPPSEpisodes.R`)

* Fixed merged episode start date. v2 computed `merged_episode_start` using
  `pmin(first_gest_date, episode_min_date, pregnancy_end)`, where
  `pregnancy_end` (the outcome date) was used instead of `pregnancy_start`.
  v3 corrects this. The incorrect start dates in v2 were too late, making
  episodes appear shorter, reducing detected overlaps during dedup, and
  keeping more episodes.
  _Expected to **decrease** episode counts (medium magnitude). More overlaps
  are detected with correct start dates._
* Iterative deduplication rewrite. v2 used 5 hardcoded rounds; duplicates
  remaining after round 5 were silently kept. v3 uses up to 10 iterative
  rounds with multi-criteria tie-breaking and drops unresolved duplicates
  with a warning.
  _Expected to **decrease** episode counts (high magnitude). This is the
  primary driver of episode count reduction for multi-episode outliers._
* Fixed overlap join SQL generation. v3 uses explicit inequality filters
  instead of `join_by(overlaps(...))`, which had known SQL generation issues
  on PostgreSQL, Spark, and Snowflake.
  _Expected to **increase** episode counts on affected backends where rows
  were silently dropped._

## ESD stage (`ESD.R`)

* Added LMP and EDD date parsing. v3 parses Last Menstrual Period and
  Estimated Delivery Date concept date values to improve start date inference.
  v2 included these concepts but never parsed their date values.
  _Improves start date precision. No direct impact on counts._
* Added gestation-at-birth concept handling. v3 treats gestation-at-birth
  concepts (4260747, 46234792) as GW-type when they have valid values
  (1-44 weeks). v2 did not use them for GW classification.
  _Expected to **increase** episode counts (low magnitude)._
* Added distinct deduplication in ESD output. v3 applies
  `distinct(person_id, inferred_episode_end, final_outcome_category)` before
  final output. v2 could produce exact duplicate rows.
  _Expected to **decrease** episode counts (low-medium magnitude)._
* Added ESD overlap collapse. v3 merges overlapping [start, end] intervals
  within each person via `collapseOverlappingEpisodesWithinPerson()`. v2 had
  no overlap removal in ESD.
  _Expected to **decrease** episode counts (medium magnitude)._
* Added `conformToValidation` parameter (default `FALSE`). When enabled,
  applies a 7-step cohort attrition pipeline: observation period filters,
  start >= end filter with `fixStartBeforeEnd()` rescue, gestational length
  < 308 days, gestational length != 0, and overlap removal with priority
  hierarchy. Not applied by default.
  _Expected to **decrease** episode and person counts when enabled (very high
  magnitude). No impact with default `FALSE`._

## Other changes

* Refactored code: snake_case column names, modular file structure, improved
  logging.
* Additional testing with unit tests and database backend tests.
