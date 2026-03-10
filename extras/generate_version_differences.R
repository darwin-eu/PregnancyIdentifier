# generate_version_differences.R
# Run this script to (re)generate inst/shiny/data/version_differences.csv
# Uses readr::write_csv() to properly escape multi-line markdown in CSV fields.

library(tibble)
library(readr)

rows <- list()

# ──────────────────────────────────────────────────────────────────────────────
# 1. v2.0.1 -> v3.0.0  (Major rewrite, net DECREASE)
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
"- **Reviewed HIP concept set (Init stage) - MEDIUM magnitude:** Clinically ambiguous concepts were removed or recategorized. 11 concepts were recategorized (9 DELIV to PREG, 1 DELIV to ECT, 1 DELIV to PREG). While 553 concepts were added, the net effect of the review is a decrease because ambiguous concepts that previously generated false-positive episodes were removed or reclassified to lower-priority categories.",
"",
"### Changes that INCREASE counts (partially offsetting the decrease)",
"",
"- **`gest_value` column integration (HIP stage) - HIGH magnitude:** v3 reads gestational age values from the concept sheet's `gest_value` column. Conditions, procedures, and observations now contribute gestational timing even without a `value_as_number` measurement. This allows more episodes to have valid gestation estimates and participate in overlap merging.",
"- **Default Matcho term days for PREG episodes (HIP stage) - HIGH magnitude:** v3 assigns default term durations of 140 (min) and 301 (max) days to PREG-category episodes instead of NA. This allows PREG episodes to participate in gestation-based overlap joins that previously failed.",
"- **PPS `min_date` imputation (PPS stage) - MEDIUM magnitude:** v3 imputes missing PPS minimum dates as `max_date - 280` days instead of leaving them as NA. This enables PPS-HIP overlap joins that silently failed in v2 due to NA comparisons.",
"- **Condition date filter bug fix (Init stage) - LOW-MEDIUM magnitude:** v2 excluded conditions whose `condition_end_date` fell outside the study window or was NULL. v3 filters only on `condition_start_date`, consistent with other domains, rescuing conditions that were incorrectly excluded.",
"- **Max gestation term increased 301 to 308 days (HIP stage) - LOW magnitude:** For DELIV, LB, and SB categories, the maximum allowed gestation increased from 301 to 308 days. Fewer episodes are reclassified to PREG for exceeding the term limit.",
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
