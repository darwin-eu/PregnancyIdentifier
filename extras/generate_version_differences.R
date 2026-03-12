# generate_version_differences.R
# Run this script to (re)generate inst/shiny/data/version_differences.csv
# Uses readr::write_csv() to properly escape multi-line markdown in CSV fields.

library(tibble)
library(readr)

rows <- list()

# ──────────────────────────────────────────────────────────────────────────────
# v0.1.0 -> v0.1.1  (MS SQL fix)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "0.1.0",
  new_version = "0.1.1",
  difference_explanation = paste(sep = "\n",
"## v0.1.0 to v0.1.1 - MS SQL Server Compatibility",
"",
"**Net expected impact: NO CHANGE in episodes or persons (bug fix for MS SQL backends only)**",
"",
"- Fixed `date_of_birth` construction in `HIP.R` and `PPS.R`: changed `as.integer()` casts to `as.character()` for string concatenation. The `as.integer()` approach failed on MS SQL Server.",
"- Removed premature `runHipps` export from NAMESPACE (function was not yet ready).",
"",
"No algorithmic changes. Episode and person counts should be identical on backends that were already working."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# v0.1.1 -> v0.1.2  (MS SQL fix)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "0.1.1",
  new_version = "0.1.2",
  difference_explanation = paste(sep = "\n",
"## v0.1.1 to v0.1.2 - Further MS SQL Compatibility",
"",
"**Net expected impact: NO CHANGE in episodes or persons (bug fix for MS SQL and numeric-typed backends)**",
"",
"- Wrapped date components in `as.character(as.integer(...))` in `HIP.R` and `PPS.R` for `date_of_birth` construction. Handles backends where `year_of_birth` is stored as numeric/float.",
"- Replaced R-native date arithmetic in `calculate_start()` with `CDMConnector::dateadd()` for `min_start_date` and `max_start_date`. Required for MS SQL compatibility.",
"",
"No algorithmic changes."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# v0.1.2 -> v0.1.3  (MS SQL fix)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "0.1.2",
  new_version = "0.1.3",
  difference_explanation = paste(sep = "\n",
"## v0.1.2 to v0.1.3 - MS SQL Date Casting",
"",
"**Net expected impact: NO CHANGE in episodes or persons (bug fix for MS SQL backends only)**",
"",
"- Added `as.character()` casts for date-to-string conversion in `paste()` calls in `ESD.R` and `HIP.R`. Fixes implicit casting failures on MS SQL Server.",
"",
"No algorithmic changes."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# v0.1.3 -> v0.1.4  (Full pipeline re-enabled)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "0.1.3",
  new_version = "0.1.4",
  difference_explanation = paste(sep = "\n",
"## v0.1.3 to v0.1.4 - Full HIPPS Pipeline Re-enabled",
"",
"**Net expected impact: MAJOR FUNCTIONAL CHANGE**",
"",
"v0.1.4 is a milestone release that activates the complete pregnancy identification pipeline. Prior versions had the merge and ESD stages commented out.",
"",
"### Changes",
"",
"- **Full pipeline activation:** Uncommented and enabled the HIPPS merge pipeline (outcomes per episode, add outcomes, final merged episodes, deduplication, demographic details) and ESD pipeline (timing concepts, gestational timing info, metadata merge) in `runHIPPS.R`.",
"- Changed `runHip()` and `runPps()` to return CDM objects instead of separate result objects. Results are read from RDS files saved by each stage.",
"- Replaced R-native date arithmetic in `add_gestation()` with `CDMConnector::dateadd()` for `max_gest_start_date` and `min_gest_start_date`.",
"- Changed `runHip()` early return from `return(NULL)` to `return(cdm)` for pipeline flow.",
"",
"Before v0.1.4, only HIP and PPS stages ran independently. From v0.1.4 onward, the full algorithm (HIP + PPS + Merge + ESD) is operational."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# v0.1.4 -> v0.1.5  (SQL mutate split)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "0.1.4",
  new_version = "0.1.5",
  difference_explanation = paste(sep = "\n",
"## v0.1.4 to v0.1.5 - SQL Backend Compatibility",
"",
"**Net expected impact: NO CHANGE in episodes or persons (bug fix for strict SQL backends)**",
"",
"- Split a single large `dplyr::mutate()` call in `add_gestation()` into three separate `mutate() %>% compute()` steps. Some SQL backends cannot reference columns created in the same `mutate()` call; intermediate `compute()` materializes the results.",
"- Wrapped `CDMConnector::dateadd()` results in `as.Date()` in `calculate_start()`.",
"",
"No algorithmic changes. Fixes runtime errors on strict SQL backends."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# v0.1.5 -> v0.1.6  (SQL mutate split continued)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "0.1.5",
  new_version = "0.1.6",
  difference_explanation = paste(sep = "\n",
"## v0.1.5 to v0.1.6 - Additional SQL Backend Fixes",
"",
"**Net expected impact: NO CHANGE in episodes or persons (bug fix for strict SQL backends)**",
"",
"- Further split large mutate chains in `add_gestation()` (the `both_df` join section) and `remove_overlaps()` into separate `mutate()` calls for `gest_at_outcome`, `is_under_max`/`is_over_min`, and `days_diff`. Same pattern as v0.1.5.",
"",
"No algorithmic changes."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# v0.1.6 -> v0.1.7  (Robustness + logging)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "0.1.6",
  new_version = "0.1.7",
  difference_explanation = paste(sep = "\n",
"## v0.1.6 to v0.1.7 - Robustness and Logging Improvements",
"",
"**Net expected impact: VARIABLE (fixes edge cases that could crash or produce wrong results)**",
"",
"v0.1.7 is the largest pre-1.0 release, focused on robustness and logging.",
"",
"### Algorithm changes",
"",
"- **ESD empty data guard:** Wrapped gestational timing processing in a check for empty timing concepts. When no timing data exists, returns an empty tibble with the correct schema instead of crashing.",
"- **HIP `remove_overlaps()` empty list guard:** Added handling for empty `gest_id_list` to prevent errors when no overlapping episodes exist.",
"- **HIP `remove_overlaps()` date arithmetic:** Replaced R-native date arithmetic with `CDMConnector::dateadd()` for SQL compatibility.",
"- Added intermediate `dplyr::compute()` materializations with named tables throughout HIP stage.",
"",
"### Other changes",
"",
"- Replaced all `cat()` logging calls with `message(sprintf())` throughout all stages.",
"- Fixed bare column references to use `.data$` pronouns in `mergeHIPPSEpisodes.R`.",
"- Added guard for empty PPS episodes in merge stage.",
"- Added `^dev$` and `^extras$` to `.Rbuildignore`."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# v0.1.7 -> v1.0.0  (Export system + structured logging)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "0.1.7",
  new_version = "1.0.0",
  difference_explanation = paste(sep = "\n",
"## v0.1.7 to v1.0.0 - Export System and Structured Logging",
"",
"**Net expected impact: NO CHANGE in episodes or persons (new features, minor bug fixes)**",
"",
"v1.0.0 adds a complete export system and structured logging but makes no changes to the core pregnancy identification algorithm.",
"",
"### New features",
"",
"- **Export pipeline (`R/export.R`):** New `export()` function replaces the ad-hoc `extras/exportResults.R` script. Exports include: age summary, precision days density, episode frequency, pregnancy frequency per person, gestational age summary and counts, gestational duration by outcome, yearly/monthly time trends, observation period range, pregnancy overlap counts, date consistency checks, reversed dates counts, and outcome category counts for HIP, PPS, and final HIPPS. Auto-zips all CSVs.",
"- **Structured logging:** All algorithm functions now accept a `logger` parameter and use `log4r` for file and console logging, replacing `message()` calls.",
"- **`insertPregnancyEpisodesTable()`:** New utility to insert identified episodes back into the database.",
"- **`summariseColumn()`:** New utility computing mean, sd, median, Q1, Q3, min, max, and count.",
"",
"### Bug fixes",
"",
"- Fixed `dplyr::pull(n)` to `dplyr::pull(.data$n)` for strict tidy evaluation compliance.",
"- Fixed `clean_episodes()` log message missing the `%s` format specifier.",
"- Fixed ESD `inferred_episode_start` fallback: removed unnecessary `lubridate::days()` wrapping.",
"",
"### Dependencies added",
"",
"- `magrittr` (pipe re-export), `tidyr` (pivoting in time trends), `log4r` (structured logging)."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# v1.0.0 -> v2.0.0  (Critical datediff fix + study period + Shiny)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "1.0.0",
  new_version = "2.0.0",
  difference_explanation = paste(sep = "\n",
"## v1.0.0 to v2.0.0 - Critical Bug Fix + Study Period Filtering + Shiny App",
"",
"**Net expected impact: VARIABLE (critical bug fix changes date difference calculations throughout HIP)**",
"",
"v2.0.0 contains the most impactful single bug fix in the package history, plus new study period filtering and the first Shiny results viewer.",
"",
"### Algorithm changes",
"",
"- **CRITICAL: Fixed `datediff()` argument order throughout HIP stage - VERY HIGH magnitude:** All ~15 `CDMConnector::datediff()` calls in `HIP.R` had start and end date arguments reversed, producing wrong-sign date differences. This affected `final_visits()`, `add_stillbirth()`, `add_ectopic()`, `add_abortion()`, `add_delivery()`, `gestation_episodes()`, `add_gestation()`, `clean_episodes()`, `remove_overlaps()`, and `final_episodes_with_length()`. All date-difference-based logic (episode length, gap detection, gestation calculations) now produces correct values. The impact depends on how the downstream code handled negative values (some operations used absolute values, masking the bug; others did not).",
"- **Study period filtering:** New `startDate` and `endDate` parameters on `runHipps()`, `runHip()`, `runPps()`, and `runEsd()`. All CDM table queries filter records to the specified date range before processing. A final post-merge filter ensures output episodes fall within the study window.",
"- **`justGestation` parameter:** New parameter on `runHipps()` and `runHip()` (default `TRUE`). When `FALSE`, episodes with only gestational concepts and no outcome are excluded.",
"- **ESD `keep_value` range check tightened:** The gestational value validity check (`> 0` and `<= 44` weeks) now applies to ALL gestational timing concepts, not just three specific concept IDs. Upper bound changed from `< 44` to `<= 44`.",
"",
"### New features",
"",
"- **Shiny results viewer:** New `viewResults()` function launches an interactive dashboard for exploring exported results. Supports multi-database comparison. Includes demographics, incidence trends, gestational age distributions, and interactive tables.",
"- **Concept timing validation:** New `exportConceptTimingCheck()` validates 35 pregnancy-related concepts against expected gestational timing windows. New `inst/concepts/check_concepts.csv` defines the validation concepts.",
"",
"### Export changes",
"",
"- **`minCellCount` privacy parameter:** Added to `export()` (default: 5). Small counts suppressed to NA.",
"- **`pkg_version` tracking:** All exported CSVs now include a `pkg_version` column.",
"- **Age summary groups:** New `age_summary_groups.csv` with counts per rounded age year.",
"- Fixed `addAge()` to fall back to `year_of_birth` when `birth_datetime` is NULL.",
"- Fixed overlap count detection to exclude first episode per person.",
"",
"### Concept set changes",
"",
"- Updated HIP and PPS concept spreadsheets (previous versions saved as backups)."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# v2.0.0 -> v2.0.1  (Database compatibility patch)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "2.0.0",
  new_version = "2.0.1",
  difference_explanation = paste(sep = "\n",
"## v2.0.0 to v2.0.1 - Database Compatibility Patch",
"",
"**Net expected impact: NO CHANGE in episodes or persons (fixes for Spark and SQL Server backends)**",
"",
"v2.0.1 is a pure compatibility patch with no algorithmic changes.",
"",
"### Bug fixes",
"",
"- **Spark SQL:** Fixed `get_timing_concepts()` to explicitly upload `person_id_list` to the database via `CDMConnector::insertTable()` instead of `dplyr::inner_join(..., copy = TRUE)`, which failed on Spark.",
"- **SQL Server integer casting:** Added explicit `as.integer()` casts for `min_term`, `max_term`, `max_gest_day`, `min_gest_day`, and `prev_retry` to satisfy strict type conversion.",
"- **SQL Server named compute tables:** Changed anonymous `dplyr::compute()` calls in `clean_episodes()` to named non-temporary tables.",
"- **SQL Server `copy = TRUE` removal:** Rewrote `addAge()` and `exportConceptTimingCheck()` to upload local data frames before joining.",
"- Fixed missing `logger` argument in `log4r::warn()` call.",
"- Fixed `add_delivery()` to use pre-computed `prev_visit` column instead of re-calling `dplyr::lag()`.",
"",
"Counts should be identical between v2.0.0 and v2.0.1 on all backends."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# v2.0.1 -> v3.0.0  (Major rewrite, net DECREASE)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "2.0.1",
  new_version = "3.0.0",
  difference_explanation = paste(sep = "\n",
"## v2.0.1 to v3.0.0 - Major Algorithm Rewrite",
"",
"**Net expected impact: DECREASE in episodes and persons**",
"",
"v3.0.0 was a complete rewrite of the pregnancy identification pipeline. On IPCI the net effect was approximately **-29% episodes** and **-19% persons**, driven primarily by improved deduplication and stricter filtering. However several changes partially offset this by rescuing episodes that v2 missed.",
"",
"### Changes that DECREASE counts",
"",
"- **Iterative deduplication rewrite (Merge stage) - HIGH magnitude:** v2 used 5 hardcoded merge rounds; v3 uses up to 10 iterative rounds with improved tie-breaking (preferring later outcomes and longer gestation). This is the primary driver of the reduction in multi-episode outliers (max episodes per person dropped from 18 to 15 on IPCI).",
"- **Age/sex filtering now applied to PPS records (Init stage) - HIGH magnitude:** v2 only filtered HIP records by age (15-55) and sex (female); PPS records were extracted without any demographic filter. v3 filters both HIP and PPS at extraction, removing persons whose only pregnancy evidence came from ineligible PPS records.",
"- **Merged episode start date bug fix (Merge stage) - MEDIUM magnitude:** v2 incorrectly used the outcome date instead of the antenatal start date when merging overlapping episodes. Correcting this reveals additional overlaps between adjacent episodes, triggering more deduplication.",
"- **ESD overlap collapse (ESD stage) - MEDIUM magnitude:** v3 adds a new step that merges overlapping ESD intervals within each person before assigning final episode dates. This prevents a single person from generating duplicate episodes from overlapping timing evidence.",
"- **ESD distinct deduplication (ESD stage) - LOW-MEDIUM magnitude:** v3 removes exact duplicate rows in ESD intermediate tables, preventing inflation of episode counts from repeated records.",
"- **HIP concept reclassification (Init stage) - LOW magnitude:** 11 concepts were recategorized (9 DELIV to PREG, 1 DELIV to ECT, 1 DELIV to PREG). Moving presentation concepts (breech, face, brow, transverse) from DELIV to PREG means they no longer anchor delivery episodes on their own, reducing spurious DELIV outcomes.",
"",
"### Changes that INCREASE counts (partially offsetting the decrease)",
"",
"- **`gest_value` column integration (HIP stage) - HIGH magnitude:** v3 reads gestational age values from the concept sheet's `gest_value` column. Conditions, procedures, and observations now contribute gestational timing even without a `value_as_number` measurement. This allows more episodes to have valid gestation estimates and participate in overlap merging.",
"- **Default Matcho term days for PREG episodes (HIP stage) - HIGH magnitude:** v3 assigns default term durations of 140 (min) and 301 (max) days to PREG-category episodes instead of NA. This allows PREG episodes to participate in gestation-based overlap joins that previously failed.",
"- **PPS `min_date` imputation (PPS stage) - MEDIUM magnitude:** v3 imputes missing PPS minimum dates as `max_date - 280` days instead of leaving them as NA. This enables PPS-HIP overlap joins that silently failed in v2 due to NA comparisons.",
"- **Condition date filter bug fix (Init stage) - LOW-MEDIUM magnitude:** v2 excluded conditions whose `condition_end_date` fell outside the study window or was NULL. v3 filters only on `condition_start_date`, consistent with other domains, rescuing conditions that were incorrectly excluded.",
"- **Max gestation term increased 301 to 308 days (HIP stage) - LOW magnitude:** For DELIV, LB, and SB categories, the maximum allowed gestation increased from 301 to 308 days. Fewer episodes are reclassified to PREG for exceeding the term limit.",
"- **HIP concept set expansion (Init stage) - MEDIUM magnitude:** 553 new HIP concepts added (439 PREG, 93 DELIV, 10 LB, 4 ECT, 4 AB, 3 SA). No concepts removed. The broader concept set captures pregnancy evidence from more source records, increasing the pool of candidate episodes.",
"- **Expanded gestational age concepts - LOW magnitude:** Gestational age concept IDs moved from 3 hardcoded values to an external CSV with broader coverage, improving ESD start date estimation.",
"",
"### Concept set changes",
"",
"- **HIP concepts:** 941 to 1496 (+555 added, 0 removed, 11 recategorized). Additions: +439 PREG, +93 DELIV, +10 LB, +4 ECT, +4 AB, +3 SA.",
"- **PPS concepts:** 112 to 123 (+11 added, 0 removed). New ultrasound scan and premature birth concepts. Timing values unchanged for existing concepts.",
"- **New external files:** ESD_concepts.xlsx (31 concepts, previously hardcoded), gestational_age_concepts.csv (previously 3 hardcoded IDs), delivery_mode/ JSONs (46 cesarean, 173 vaginal concepts).",
"",
"### Other non-count changes",
"",
"- Age calculation changed from `/365` to `/365.25`, affecting boundary cases near age 15 and 56.",
"- Upper age bound changed from `< 56` to `< 57` (docstring says 15-56 but code computes `ageBounds[2] + 1`).",
"- PPS filtering moved from episode construction to extraction (same net effect, different code path)."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# 2. v3.0.0 -> v3.0.1  (Bug fixes, net INCREASE)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "3.0.0",
  new_version = "3.0.1",
  difference_explanation = paste(sep = "\n",
"## v3.0.0 to v3.0.1 - Bug Fixes",
"",
"**Net expected impact: INCREASE in episodes and persons**",
"",
"v3.0.1 fixed several bugs introduced in the v3.0.0 rewrite that caused episodes to be silently dropped or gestation values to be lost. All fixes increase or restore counts.",
"",
"### Changes that INCREASE counts",
"",
"- **Gestation record coalesce fix (HIP stage) - MEDIUM-HIGH magnitude:** v3.0.0 had two separate branches for gestation values, both only reading `value_as_number` and ignoring the concept sheet's `gest_value`. v3.0.1 unified to `coalesce(value_as_number, gest_value)`, restoring concept-sheet gestational ages. Also fixes varchar-to-integer cast failures on Spark/Redshift backends.",
"- **Person-scoped overlap removal (Merge stage) - MEDIUM magnitude:** v3.0.0 removed `gest_id` values globally during overlap resolution, potentially removing timing evidence from unrelated persons. v3.0.1 scopes removal to `(person_id, gest_id)` pairs.",
"- **NA-safe `restFinal` filter (Merge stage) - MEDIUM magnitude:** v3.0.0's filter on `is_over_min` evaluated to NA in SQL when the flag was NULL, causing gestation-only episodes to be silently dropped. v3.0.1 adds explicit NA handling.",
"- **Union type-casting fix (multiple stages) - VARIABLE magnitude (backend-dependent):** Fixes untyped NULL columns in `union_all()` operations that caused failures on PostgreSQL, Spark, and Snowflake. Backends that previously errored now produce results.",
"- **Permanent table materialization - VARIABLE magnitude (backend-dependent):** Fixes Snowflake and Spark issues where temporary tables were garbage-collected mid-pipeline.",
"",
"### Changes that DECREASE counts",
"",
"- **Post-union deduplication (Merge stage) - LOW magnitude:** Adds `dplyr::distinct()` after unioning logical episodes, removing exact duplicate rows. Minor reduction."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# 3. v3.0.1 -> v3.0.2  (New ESD filters, net DECREASE)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "3.0.1",
  new_version = "3.0.2",
  difference_explanation = paste(sep = "\n",
"## v3.0.1 to v3.0.2 - New ESD Validation Filters",
"",
"**Net expected impact: DECREASE in episodes and persons (very high magnitude)**",
"",
"v3.0.2 introduced three new validation filters in the Episode Start Date (ESD) stage. These filters are applied **unconditionally** in this version, causing a large drop in episode counts, particularly for databases with data quality issues (episodes outside observation periods, inverted dates, zero-length episodes).",
"",
"### Changes that DECREASE counts",
"",
"- **Observation period boundary filter (ESD steps 2-3) - VERY HIGH magnitude:** Both `final_episode_start_date` AND `final_episode_end_date` must fall within the person's `observation_period`. Episodes with dates outside the observation period are dropped entirely. This is the single largest source of episode removal in v3.0.2-v3.0.4.",
"- **Start >= end date filter (ESD step 4) - MEDIUM magnitude:** Episodes where `final_episode_start_date >= final_episode_end_date` (inverted or same-day) are dropped. The `fixStartBeforeEnd()` rescue function (which uses Matcho term durations to repair reversed dates) is only available when `conformToValidation = TRUE`.",
"- **Zero-length episode filter (ESD step 6) - LOW-MEDIUM magnitude:** Episodes with a calculated gestational length of 0 days are dropped.",
"",
"### Changes that INCREASE counts",
"",
"- **Expanded ESD gestational age concept search - LOW magnitude (indirect):** Broader concept coverage can improve start date inference, potentially rescuing some episodes from the date filters above. This is a minor offset."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# 4. v3.0.2 -> v3.0.3  (No algorithmic change)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "3.0.2",
  new_version = "3.0.3",
  difference_explanation = paste(sep = "\n",
"## v3.0.2 to v3.0.3 - Parameter Renaming",
"",
"**Net expected impact: NO CHANGE in episodes or persons**",
"",
"v3.0.3 contains only non-algorithmic changes:",
"",
"- `exportDir` parameter renamed to `exportFolder` for consistency.",
"- Output path refactoring for export file organization.",
"",
"No changes to the pregnancy identification algorithm. Episode and person counts should be identical between v3.0.2 and v3.0.3 on the same data."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# 5. v3.0.3 -> v3.0.4  (No algorithmic change)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "3.0.3",
  new_version = "3.0.4",
  difference_explanation = paste(sep = "\n",
"## v3.0.3 to v3.0.4 - Shiny App Updates",
"",
"**Net expected impact: NO CHANGE in episodes or persons**",
"",
"v3.0.4 contains only Shiny results app changes:",
"",
"- Shiny app UI improvements and new visualization tabs.",
"- Added `minCellCount` suppression to PET comparison exports.",
"",
"No changes to the pregnancy identification algorithm. Episode and person counts should be identical between v3.0.3 and v3.0.4 on the same data."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# 6. v3.0.4 -> v3.0.5  (ESD filters conditional, net INCREASE)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "3.0.4",
  new_version = "3.0.5",
  difference_explanation = paste(sep = "\n",
"## v3.0.4 to v3.0.5 - ESD Filters Made Conditional",
"",
"**Net expected impact: INCREASE in episodes and persons (very high magnitude)**",
"",
"v3.0.5 reversed the unconditional ESD validation filters introduced in v3.0.2 by making them conditional on `conformToValidation` (default `FALSE`). With the default setting, episodes are no longer removed by these filters but are instead flagged with per-record quality indicators.",
"",
"### Changes that INCREASE counts",
"",
"- **Observation period filters now conditional (default OFF) - VERY HIGH magnitude:** The filters requiring `final_episode_start_date` and `final_episode_end_date` to fall within the person's observation period are no longer applied by default. This was the single largest source of episode removal in v3.0.2-v3.0.4. Episodes with dates outside observation periods are now retained and flagged with `is_start_outside_obs` and `is_end_outside_obs` boolean columns.",
"- **Start >= end filter now conditional (default OFF) - MEDIUM magnitude:** Episodes with inverted dates (`start >= end`) are retained by default instead of being dropped. They are flagged with `is_start_gte_end`. The `fixStartBeforeEnd()` repair function is only invoked when `conformToValidation = TRUE`.",
"- **Zero-length episode filter now conditional (default OFF) - LOW-MEDIUM magnitude:** Episodes with gestational length of 0 days are retained and flagged with `is_zero_length` instead of being dropped.",
"- **PREG-only concepts restored (Init stage) - LOW-MEDIUM magnitude:** HIP concepts with category PREG but no `gest_value` were excluded in v3.0.0-v3.0.4. v3.0.5 restores them, adding pregnancy evidence for persons whose only records were PREG-category concepts without gestational timing.",
"",
"### Changes that DECREASE counts",
"",
"None.",
"",
"### New per-record quality flags (informational, no count impact)",
"",
"Six boolean columns are added to the final output characterizing each episode's data quality: `is_start_outside_obs`, `is_end_outside_obs`, `is_start_gte_end`, `is_zero_length`, `is_too_long` (> 308 days), `is_overlap` (overlaps another episode in the same person). These flags enable downstream analysis without removing records."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# 7. v3.0.5 -> v3.1.0  (PPS domains restored, neutral)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "3.0.5",
  new_version = "3.1.0",
  difference_explanation = paste(sep = "\n",
"## v3.0.5 to v3.1.0 - PPS Source Domains Restored",
"",
"**Net expected impact: NO CHANGE with current concept sets**",
"",
"v3.1.0 restored all 5 PPS source domains (`condition_occurrence`, `procedure_occurrence`, `observation`, `measurement`, `visit_occurrence`). v3.0.0 had dropped `observation` and `visit_occurrence` from PPS extraction.",
"",
"This change has **no impact on current counts** because all current PPS concepts resolve to the Condition, Procedure, or Measurement domains. The restoration is a precautionary measure: if future PPS concept sets include concepts mapped to Observation or Visit domains, they will now be captured.",
"",
"Episode and person counts should be identical between v3.0.5 and v3.1.0 on the same data with the current concept sets."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# 8. v3.1.0 -> v3.1.1  (Export changes only)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "3.1.0",
  new_version = "3.1.1",
  difference_explanation = paste(sep = "\n",
"## v3.1.0 to v3.1.1 - Export Enhancements",
"",
"**Net expected impact: NO CHANGE in episodes or persons**",
"",
"v3.1.1 only changes the export and Shiny visualization layer:",
"",
"- Age at first pregnancy export (`age_summary_first_pregnancy.csv`) now includes all episodes, not just live births. Output is stratified by `final_outcome_category` (overall, each individual outcome, and combined LB or PREG).",
"- Shiny app age tab updated with outcome picker and side-by-side boxplots.",
"",
"No changes to the pregnancy identification algorithm. Episode and person counts should be identical between v3.1.0 and v3.1.1 on the same data."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# 9. v3.1.1 -> v3.1.2  (addAge bug fix)
# ──────────────────────────────────────────────────────────────────────────────
rows[[length(rows) + 1]] <- list(
  old_version = "3.1.1",
  new_version = "3.1.2",
  difference_explanation = paste(sep = "\n",
"## v3.1.1 to v3.1.2 - Export Age Calculation Bug Fix",
"",
"**Net expected impact: NO CHANGE in episodes or persons**",
"",
"v3.1.2 fixes a bug in the `addAge()` export function that could produce implausible ages (e.g. 2776) in the age summary CSVs.",
"",
"### Root cause",
"",
"The init-stage age filter uses `addAgeSex()` which constructs date of birth from `year_of_birth`, `month_of_birth`, `day_of_birth`. But the export function `addAge()` used `birth_datetime` as the primary source, falling back to `year_of_birth` only when `birth_datetime` was NULL.",
"",
"If a person had a valid `year_of_birth` (e.g. 1990) but a corrupt `birth_datetime` (e.g. 0001-01-01 from an ETL default), they would pass the init filter (age ~30) but get a wild age (~2025) in the export.",
"",
"### Fix",
"",
"`addAge()` now uses the same `year_of_birth`/`month_of_birth`/`day_of_birth` construction as `addAgeSex()`, ensuring the export age is consistent with the pipeline age.",
"",
"**Impact on exports:** Max age values in `age_summary.csv` and `age_summary_groups.csv` will decrease for databases with corrupt `birth_datetime` values (e.g. IPCI had max age 2776, which will now show a realistic value). No impact on episode or person counts."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# NON-CONSECUTIVE JUMPS
# ──────────────────────────────────────────────────────────────────────────────

# 10. v2.0.1 -> v3.0.5 (common real-world comparison)
rows[[length(rows) + 1]] <- list(
  old_version = "2.0.1",
  new_version = "3.0.5",
  difference_explanation = paste(sep = "\n",
"## v2.0.1 to v3.0.5 - Full Upgrade Path (Baseline to Current Default)",
"",
"**Net expected impact: DECREASE in episodes and persons**",
"",
"This comparison spans the full v2-to-v3 upgrade. On IPCI the observed impact was approximately **-29% episodes** (431,375 to 306,269) and **-19% persons** (267,326 to 215,609). The decrease concentrates in multi-episode outliers and persons with weak pregnancy evidence.",
"",
"### Cumulative changes across v3.0.0 through v3.0.5",
"",
"The intermediate versions introduced and then reversed ESD validation filters:",
"- v3.0.0: Major rewrite with improved deduplication (decrease)",
"- v3.0.1: Bug fixes restoring lost episodes (increase)",
"- v3.0.2: Unconditional ESD filters added (large decrease)",
"- v3.0.3-v3.0.4: No algorithmic changes",
"- v3.0.5: ESD filters made conditional with default OFF (large increase, reverting v3.0.2)",
"",
"The net effect of v3.0.5 vs v2.0.1 is therefore dominated by the v3.0.0 rewrite changes, since the ESD filter addition (v3.0.2) and reversal (v3.0.5) largely cancel out.",
"",
"### Primary drivers of the DECREASE",
"",
"- **Iterative deduplication rewrite (Merge stage) - HIGH:** Up to 10 rounds with better tie-breaking vs v2's 5 hardcoded rounds. Primary driver of reduced multi-episode outliers.",
"- **Age/sex filtering on PPS records - HIGH:** v2 had no demographic filter on PPS extraction; v3 filters both HIP and PPS.",
"- **Merged episode start date bug fix - MEDIUM:** Correct start dates reveal more overlaps, triggering additional deduplication.",
"- **ESD overlap collapse and deduplication - MEDIUM:** Prevents duplicate episodes from overlapping timing evidence.",
"",
"### Primary drivers of the INCREASE (partially offsetting)",
"",
"- **`gest_value` integration and PREG defaults - HIGH:** More episodes have valid gestation, enabling overlap merging. PREG episodes get default 140/301 day terms.",
"- **PPS `min_date` imputation - MEDIUM:** Missing PPS min dates set to `max_date - 280` instead of NA, enabling joins.",
"- **Gestation coalesce bug fix (v3.0.1) - MEDIUM-HIGH:** Restored concept-sheet gestational ages lost in v3.0.0.",
"- **PREG-only concepts restored (v3.0.5) - LOW-MEDIUM:** PREG concepts without `gest_value` re-included.",
"",
"### Concept set changes",
"",
"- **HIP:** 941 to 1496 concepts (+555 added, 11 recategorized)",
"- **PPS:** 112 to 123 concepts (+11 added)",
"- **New files:** ESD_concepts.xlsx (31 concepts), gestational_age_concepts.csv, delivery_mode JSONs",
"",
"### Summary statistics (IPCI)",
"",
"| Metric | v2.0.1 | v3.0.5 | Change |",
"|--------|--------|--------|--------|",
"| Total episodes | 431,375 | 306,269 | -29.0% |",
"| Total persons | 267,326 | 215,609 | -19.3% |",
"| Mean episodes/person | 1.61 | 1.42 | -12% |",
"| Max episodes/person | 18 | 15 | -17% |"
  )
)

# 11. v2.0.1 -> v3.1.2 (full baseline to latest)
rows[[length(rows) + 1]] <- list(
  old_version = "2.0.1",
  new_version = "3.1.2",
  difference_explanation = paste(sep = "\n",
"## v2.0.1 to v3.1.2 - Full Upgrade (Baseline to Latest)",
"",
"**Net expected impact: DECREASE in episodes and persons**",
"",
"This spans the entire upgrade path from v2 to the latest release. The count impact is effectively identical to v2.0.1 to v3.0.5, since v3.1.0 through v3.1.2 contain no algorithmic changes that affect episode or person counts:",
"",
"- v3.1.0: PPS source domains restored (no impact with current concepts)",
"- v3.1.1: Export enhancements only (age at first pregnancy stratification)",
"- v3.1.2: Export age calculation bug fix (no count impact, fixes wild ages in CSVs)",
"",
"See the v2.0.1 to v3.0.5 comparison for the full breakdown of algorithmic changes and their impact on counts.",
"",
"### Additional changes in v3.1.0-v3.1.2 (no count impact)",
"",
"- **v3.1.0:** All 5 PPS source domains restored as a precaution (neutral with current concept sets).",
"- **v3.1.1:** Age at first pregnancy export now stratified by outcome category.",
"- **v3.1.2:** `addAge()` export function fixed to use `year_of_birth`/`month_of_birth`/`day_of_birth` instead of `birth_datetime`, eliminating implausible ages (e.g. 2776) in age summary CSVs."
  )
)

# 12. v3.0.1 -> v3.0.5 (pre/post ESD filter era)
rows[[length(rows) + 1]] <- list(
  old_version = "3.0.1",
  new_version = "3.0.5",
  difference_explanation = paste(sep = "\n",
"## v3.0.1 to v3.0.5 - ESD Filter Addition and Reversal",
"",
"**Net expected impact: SLIGHT INCREASE in episodes and persons**",
"",
"This comparison spans the ESD filter era: v3.0.2 added unconditional validation filters, then v3.0.5 made them conditional (default OFF). The filters largely cancel out, but v3.0.5 also restored PREG-only concepts, producing a net slight increase.",
"",
"### What happened between these versions",
"",
"- **v3.0.2:** Added unconditional observation period, start>=end, and zero-length filters (large decrease)",
"- **v3.0.3:** Parameter renaming only (no change)",
"- **v3.0.4:** Shiny app updates only (no change)",
"- **v3.0.5:** Made all ESD filters conditional with default OFF (large increase, largely reversing v3.0.2). Also restored PREG-only concepts.",
"",
"### Net changes from v3.0.1 to v3.0.5",
"",
"- **ESD filters:** Added in v3.0.2, disabled by default in v3.0.5 - net neutral (filters exist but are OFF by default).",
"- **PREG-only concepts restored - LOW-MEDIUM magnitude increase:** HIP concepts with PREG category but no `gest_value` were excluded since v3.0.0; v3.0.5 re-includes them.",
"- **Per-record quality flags added - no count impact:** Six boolean quality indicators added to the output.",
"",
"Episode and person counts with `conformToValidation = FALSE` (the default) should be very close to v3.0.1, with a small increase from restored PREG-only concepts."
  )
)

# 13. v3.0.2 -> v3.0.5 (ESD filter reversal)
rows[[length(rows) + 1]] <- list(
  old_version = "3.0.2",
  new_version = "3.0.5",
  difference_explanation = paste(sep = "\n",
"## v3.0.2 to v3.0.5 - ESD Filter Reversal",
"",
"**Net expected impact: INCREASE in episodes and persons (very high magnitude)**",
"",
"v3.0.2 introduced unconditional ESD validation filters that removed many episodes. v3.0.5 made these filters conditional (default OFF), restoring those episodes.",
"",
"### Changes that INCREASE counts",
"",
"- **Observation period filters disabled by default - VERY HIGH magnitude:** Episodes with start or end dates outside the observation period are no longer removed. This was the largest source of episode loss in v3.0.2-v3.0.4.",
"- **Start >= end filter disabled by default - MEDIUM magnitude:** Episodes with inverted dates are retained and flagged instead of dropped.",
"- **Zero-length filter disabled by default - LOW-MEDIUM magnitude:** Zero-length episodes retained and flagged.",
"- **PREG-only concepts restored - LOW-MEDIUM magnitude:** PREG concepts without `gest_value` re-included in extraction.",
"",
"### Changes that DECREASE counts",
"",
"None.",
"",
"If `conformToValidation = TRUE` is explicitly set, v3.0.5 applies the same filters as v3.0.2 (plus `fixStartBeforeEnd()` repair), so counts would be closer to v3.0.2 levels."
  )
)

# 14. v3.0.4 -> v3.1.1 (matches loaded SIDIAP data)
rows[[length(rows) + 1]] <- list(
  old_version = "3.0.4",
  new_version = "3.1.1",
  difference_explanation = paste(sep = "\n",
"## v3.0.4 to v3.1.1 - ESD Filters Reversed + Minor Updates",
"",
"**Net expected impact: INCREASE in episodes and persons (very high magnitude)**",
"",
"This comparison spans from v3.0.4 (unconditional ESD filters active) to v3.1.1 (filters conditional, default OFF). The dominant change is the ESD filter reversal in v3.0.5.",
"",
"### Changes that INCREASE counts",
"",
"- **Observation period filters disabled by default (v3.0.5) - VERY HIGH magnitude:** Episodes with dates outside observation periods are no longer removed. This was the largest filter in v3.0.2-v3.0.4.",
"- **Start >= end filter disabled by default (v3.0.5) - MEDIUM magnitude:** Inverted-date episodes retained and flagged.",
"- **Zero-length filter disabled by default (v3.0.5) - LOW-MEDIUM magnitude:** Zero-length episodes retained and flagged.",
"- **PREG-only concepts restored (v3.0.5) - LOW-MEDIUM magnitude:** PREG concepts without `gest_value` re-included.",
"",
"### Changes with NO count impact",
"",
"- **PPS domains restored (v3.1.0):** All 5 source domains enabled again. No impact with current concept sets.",
"- **Export enhancements (v3.1.1):** Age at first pregnancy export expanded to all outcome categories.",
"",
"### Changes that DECREASE counts",
"",
"None."
  )
)

# 15. v3.0.5 -> v3.1.2 (confirms count-neutral)
rows[[length(rows) + 1]] <- list(
  old_version = "3.0.5",
  new_version = "3.1.2",
  difference_explanation = paste(sep = "\n",
"## v3.0.5 to v3.1.2 - Count-Neutral Updates",
"",
"**Net expected impact: NO CHANGE in episodes or persons**",
"",
"All changes from v3.0.5 to v3.1.2 are non-algorithmic or have no impact on counts with current concept sets:",
"",
"- **v3.1.0 - PPS domains restored:** All 5 PPS source domains re-enabled as a precaution. No impact because current PPS concepts only map to Condition, Procedure, and Measurement domains.",
"- **v3.1.1 - Export enhancements:** Age at first pregnancy export stratified by outcome category. Shiny app updated with outcome-aware age visualization.",
"- **v3.1.2 - Export age fix:** `addAge()` now uses `year_of_birth`/`month_of_birth`/`day_of_birth` (matching `addAgeSex()`) instead of `birth_datetime`. Fixes implausible ages in export CSVs but does not affect episode identification.",
"",
"Episode and person counts should be identical between v3.0.5 and v3.1.2 on the same data."
  )
)

# 16. v0.1.0 -> v1.0.0 (entire 0.x series)
rows[[length(rows) + 1]] <- list(
  old_version = "0.1.0",
  new_version = "1.0.0",
  difference_explanation = paste(sep = "\n",
"## v0.1.0 to v1.0.0 - Entire 0.x Development Series",
"",
"**Net expected impact: VARIABLE (pipeline enablement + robustness, no algorithmic logic changes)**",
"",
"The v0.1.x series was focused on making the package work across database backends and enabling the full pipeline. v1.0.0 added the export system.",
"",
"### Cumulative changes",
"",
"- **v0.1.1-v0.1.3:** MS SQL Server compatibility fixes for date construction and arithmetic. Changed `as.integer()` casts to `as.character()`, adopted `CDMConnector::dateadd()` for database-agnostic date math.",
"- **v0.1.4 (major):** Activated the full HIPPS merge and ESD pipeline. Prior versions only ran HIP and PPS independently with merge/ESD commented out.",
"- **v0.1.5-v0.1.6:** Split large `dplyr::mutate()` chains into separate `mutate() %>% compute()` steps for SQL backends that cannot reference columns created in the same mutate.",
"- **v0.1.7:** Robustness improvements: empty data guards for ESD and PPS, empty overlap list handling in HIP, replaced `cat()` with `message(sprintf())`, adopted `.data$` pronouns.",
"- **v1.0.0:** Complete export system (`export()`), structured `log4r` logging, `insertPregnancyEpisodesTable()` utility, `summariseColumn()` utility, minor bug fixes.",
"",
"The core pregnancy identification algorithm logic is unchanged from v0.1.0 to v1.0.0. The changes are about making it run reliably across backends, enabling the full pipeline, and adding export infrastructure."
  )
)

# 17. v0.1.0 -> v2.0.1 (initial to pre-v3 baseline)
rows[[length(rows) + 1]] <- list(
  old_version = "0.1.0",
  new_version = "2.0.1",
  difference_explanation = paste(sep = "\n",
"## v0.1.0 to v2.0.1 - Initial Release to Pre-v3 Baseline",
"",
"**Net expected impact: VARIABLE (critical datediff fix + study period filtering + full pipeline enablement)**",
"",
"This spans the entire v0-v2 development history.",
"",
"### Key milestones",
"",
"- **v0.1.4:** Full HIPPS merge and ESD pipeline activated (was commented out in v0.1.0-v0.1.3).",
"- **v0.1.1-v0.1.7:** Progressive database compatibility (MS SQL, Spark, PostgreSQL) and robustness fixes.",
"- **v1.0.0:** Complete export system, structured logging, `insertPregnancyEpisodesTable()` utility.",
"- **v2.0.0:** Critical `datediff()` argument order fix (all ~15 calls had reversed start/end), study period filtering (`startDate`/`endDate`), `justGestation` parameter, Shiny results viewer, concept timing validation, `minCellCount` privacy, updated HIP/PPS concept sets.",
"- **v2.0.1:** Spark and SQL Server compatibility patch.",
"",
"### Most impactful change",
"",
"The `datediff()` argument order correction in v2.0.0 affects all date-difference calculations in the HIP stage. The impact depends on how downstream code handled the previously wrong-sign values."
  )
)

# 18. v1.0.0 -> v2.0.1 (v1 to v2 full upgrade)
rows[[length(rows) + 1]] <- list(
  old_version = "1.0.0",
  new_version = "2.0.1",
  difference_explanation = paste(sep = "\n",
"## v1.0.0 to v2.0.1 - Full v1 to v2 Upgrade",
"",
"**Net expected impact: VARIABLE (critical datediff fix dominates)**",
"",
"This spans both v2.0.0 and v2.0.1. The v2.0.1 patch adds database compatibility fixes only.",
"",
"### Algorithm changes (from v2.0.0)",
"",
"- **CRITICAL: Fixed `datediff()` argument order throughout HIP stage - VERY HIGH magnitude:** All ~15 `CDMConnector::datediff()` calls had reversed arguments. This is the most impactful change.",
"- **Study period filtering:** New `startDate`/`endDate` parameters filter all CDM queries and post-merge output to the study window.",
"- **`justGestation` parameter:** Controls whether gestation-only episodes (no outcome) are included.",
"- **ESD `keep_value` range check:** Validity check now applies to all gestational timing concepts.",
"",
"### New features (from v2.0.0)",
"",
"- **Shiny results viewer:** `viewResults()` launches an interactive dashboard supporting multi-database comparison.",
"- **Concept timing validation:** 35 check concepts validated against expected gestational timing windows.",
"- **`minCellCount` privacy:** Small counts suppressed in exports.",
"- **`pkg_version` tracking:** Version column added to all CSVs.",
"",
"### Database fixes (from v2.0.1)",
"",
"- Spark: explicit table upload for person_id_list in ESD.",
"- SQL Server: integer casts, named compute tables, removed `copy = TRUE` joins.",
"",
"### Concept set changes",
"",
"- Updated HIP and PPS concept spreadsheets."
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# Build tibble and write CSV
# ──────────────────────────────────────────────────────────────────────────────
df <- dplyr::bind_rows(rows)

outPath <- file.path(
  rprojroot::find_package_root_file(),
  "inst", "shiny", "data", "version_differences.csv"
)

readr::write_csv(df, outPath)
cat("Wrote", nrow(df), "rows to", outPath, "\n")
