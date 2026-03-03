# Algorithm Improvements: v2.0.1 to v3.0.5

This document catalogues every algorithm-level change between PregnancyIdentifier
v2.0.1 and v3.0.5, organized by pipeline stage. Cosmetic changes (renaming,
logging, code organization) are omitted unless they have a functional side-effect.

## Overview

| v2 file | v3 file | Notes |
|---|---|---|
| `R/runHIPPS.R` | `R/runPregnancyIdentifier.R` | Orchestrator rewrite |
| (inline in HIP.R + runHIPPS.R) | `R/initPregnancies.R` | Extracted to own file |
| `R/HIP.R` | `R/HIP.R` | Decomposed into 8 named functions |
| `R/PPS.R` | `R/PPS.R` | PPS outcome inference moved here |
| `R/mergeHIPPSEpisodes.R` | `R/mergeHIPPSEpisodes.R` | Iterative dedup rewrite |
| `R/ESD.R` | `R/ESD.R` | Major expansion: validation, delivery mode, quality flags |
| `R/utils.R` (5 lines) | `R/utils.R` (508 lines) | New shared utilities |

---

## 1. Init Stage (`initPregnancies.R`)

### 1.1 Reviewed concept files

v3 loads reviewed concept spreadsheets (`HIP_concepts_reviewed17022026.xlsx`,
`PPS_concepts_reviewed1702026.xlsx`) instead of the originals. Any concept added
or removed directly changes which OMOP records qualify as pregnancy-related.

**Impact:** Variable. Depends on differences between the reviewed and original files.

### 1.2 `gest_value` column from HIP concept sheet

v3 carries a `gest_value` column from the HIP concept spreadsheet through to
`preg_hip_records`. This allows condition, procedure, and observation records
(which lack `value_as_number`) to contribute gestational age information via the
concept sheet.

In v2 only measurement records with a non-NA `value_as_number` could contribute
gestational age. The v2 concept sheet did not include a `gest_value` column.

**Impact: Increase.** More records can form gestation episodes in HIP stage 2.

### 1.3 condition_occurrence date filter bug fix

v2 filtered condition records with `condition_end_date <= endDate`, which
excluded conditions whose end date extended past the study window even when the
start date was within range. v3 filters only on `condition_start_date`, which is
consistent with all other domains.

**Impact: Increase.** Some valid conditions that were excluded in v2 are now
included.

### 1.4 PPS data extraction reduced from 5 domains to 3

v2 pulled PPS records from condition_occurrence, procedure_occurrence,
observation, measurement, and visit_occurrence. v3 drops observation and
visit_occurrence.

**Impact: Decrease.** Any gestational timing evidence that existed only in
observation or visit_occurrence is lost.

### 1.5 Age/sex filtering applied to PPS records

v3 applies female sex and age-range filters to PPS records at extraction. v2
only applied these filters to HIP records.

**Impact: Decrease.** Removes PPS records from males and out-of-age-range persons
that v2 would have retained.

### 1.6 New concept count outputs

v3 writes `hip_concept_counts.csv` and `pps_concept_counts.csv` with per-concept
record and person counts. Diagnostic only, no algorithmic effect.

---

## 2. HIP Stage (`HIP.R`)

### 2.1 Effective gestation via coalesce

v3 computes `effective_gest = coalesce(value_as_number, gest_value)` when
building gestation episodes. This means any record where the concept sheet
provides a `gest_value` can form a gestation episode, even without a measurement
value.

v2 used only 3 hardcoded concept IDs (3002209, 3048230, 3012266) from
measurements for gestation episodes.

**Impact: Increase.** Significantly expands the pool of records that can form
gestation episodes.

### 2.2 Expanded gestational age concept IDs

v3 loads gestational age concept IDs from `gestational_age_concepts.csv` instead
of the 3 hardcoded IDs in v2. The external file may contain many more concepts.

**Impact: Increase.** More measurement records qualify as gestational age evidence.

### 2.3 Default Matcho term days for PREG episodes

v3 coalesces missing `min_term`/`max_term` to 140/301 days respectively. In v2,
PREG episodes (which are not in the Matcho term durations table) got NA for both
values, making `min_start_date` and `max_start_date` NA. This prevented PREG
episodes from participating in the overlap join with gestation episodes.

**Impact: Increase.** PREG episodes can now merge with gestation episodes, gaining
gestational evidence they could not have had in v2.

### 2.4 justGestation filter for PREG episodes

When `justGestation = TRUE`, v3 drops PREG outcome+gestation episodes if a later
gestation-only episode exists for the same person. This prevents double-counting
the same pregnancy. v2 had no such filter.

**Impact: Decrease.** Prevents duplicate pregnancy episodes.

### 2.5 Person-scoped overlap removal

v2's `remove_overlaps()` used an anti_join on just `gest_id` (a string that
embeds the person_id). v3 explicitly joins on both `person_id` and `gest_id`.
This is a correctness fix; the functional effect is equivalent in most datasets.

### 2.6 Empty cohort handling

v3 detects an empty initial cohort and returns an empty data frame with the
correct schema. v2 could fail with errors on empty cohorts.

---

## 3. PPS Stage (`PPS.R`)

### 3.1 PPS outcome inference moved into `runPps()`

In v2, PPS outcome inference (`outcomes_per_episode()` + `add_outcomes()`) was
called by the orchestrator between the PPS and merge stages. v3 does this inside
`runPps()`. The logic is functionally equivalent; only the call site changed.

### 3.2 Imputation of missing `pps_episode_min_date`

v3 imputes missing PPS episode min dates as `max_date - 280 days`. v2 left them
as NA, which caused the overlap join with HIP episodes to fail (because
`pregnancy_end >= NA` evaluates to `NULL`).

**Impact: Increase.** Single-concept PPS episodes can now successfully merge with
HIP episodes, producing more HIPPS episodes.

### 3.3 `%m+%` replaced with `lubridate::add_with_rollback`

Affects end-of-month edge cases (e.g., Jan 31 + 1 month). Minor impact on episode
boundaries in edge cases.

---

## 4. Merge Stage (`mergeHIPPSEpisodes.R`)

### 4.1 Merged episode start bug fix

v2 computed `merged_episode_start` using `pmin(first_gest_date, episode_min_date, pregnancy_end)`,
where `pregnancy_end` (the outcome date) was used instead of `pregnancy_start`.
v3 corrects this to `pmin(first_gest_date, pps_episode_min_date, pregnancy_start)`.

**Impact: Variable.** v2's merged episode start was incorrectly late (at least as
late as the outcome date). This affected episode length calculations, overlap
detection, and downstream deduplication.

### 4.2 Iterative deduplication rewrite

v2 used 5 hardcoded rounds (A through E), each copy-pasting the same dedup logic.
If duplicates remained after round E, they were silently included in the output.

v3 uses an initial pick followed by up to 10 iterative rounds with early exit. If
duplicates remain after the maximum rounds, they are dropped with a warning.

**Impact: Variable.** More rounds means more complex many-to-many overlaps get
resolved. Dropping unresolved duplicates (instead of silently including them)
reduces counts for datasets with pathological overlap patterns.

### 4.3 Overlap join uses explicit inequality instead of `overlaps()`

v3 uses `inner_join(...) %>% filter(max_start_date <= max_gest_date, ...)` instead
of `join_by(overlaps(...))`. Logically equivalent, but `overlaps()` had known SQL
generation issues on some database backends (PostgreSQL, Spark, Snowflake).

**Impact:** Correctness fix for multi-backend compatibility.

---

## 5. ESD Stage (`ESD.R`)

### 5.1 `conformToValidation` parameter (NEW)

v3 adds a `conformToValidation` parameter (default `FALSE`) with a 7-step cohort
attrition pipeline. When `TRUE`, it applies:

1. Initial qualifying events (baseline count)
2. In observation at pregnancy start date (joins `observation_period`)
3. In observation at pregnancy end date (joins `observation_period`)
4. Pregnancy end date > start date (`fixStartBeforeEnd()` correction + filter)
5. Gestational length < 308 days
6. Gestational length != 0 days
7. No overlapping pregnancy records

When `FALSE` (default), none of these filters are applied and episodes are
characterized via quality flags instead.

v2 had none of these steps. ESD was purely enrichment.

**Impact when enabled: Decrease (very high).** The observation period checks
(steps 2-3) are the largest reducers, followed by overlap removal and length
filters.

### 5.2 Per-record quality flags (NEW)

v3 adds six boolean flag columns to every output record regardless of
`conformToValidation`:

- `is_start_outside_observation_period`
- `is_end_outside_observation_period`
- `is_start_gte_end`
- `is_zero_length`
- `is_too_long` (>= 308 days)
- `is_overlapping`

These characterize episode quality without removing records.

### 5.3 `removeOverlaps()` with priority hierarchy (NEW)

When overlap removal is enabled, v3 uses an O(n log n) sweep-line algorithm to
find connected components of overlapping intervals, then selects the best episode
per group using multi-criteria priority:

1. HIP-identified preferred over PPS-only
2. Outcome hierarchy: LB > SB > DELIV > ECT > AB > SA > PREG
3. HIP outcome hierarchy
4. Concordant HIP/PPS outcome preferred
5. Lower `precision_days` preferred
6. Gestational week evidence preferred
7. Earliest end date
8. First row (tie-break)

v2 had no overlap removal in ESD.

### 5.4 `removeLongPregnancies()` (NEW)

Drops episodes longer than 308 days (~44 weeks) when cleanup is enabled. v2 had
no maximum pregnancy length filter.

### 5.5 `fixStartBeforeEnd()` (NEW)

Corrects inverted start/end dates by resetting start to `end - max_term` (using
outcome-specific Matcho term durations, defaulting to 301 days for PREG). Only
runs when `conformToValidation = TRUE`.

v2 had no equivalent correction.

### 5.6 LMP and EDD concept parsing (NEW)

v3 identifies LMP (Last Menstrual Period) and EDD (Estimated Date of Delivery)
concepts from `ESD_concepts.xlsx` and parses their date values. LMP provides a
direct pregnancy start date; EDD provides start as `EDD - 280 days`.

v2 included EDD/LMP concepts in its concept lists but never parsed their date
values.

**Impact:** Improves start date precision for episodes with LMP/EDD evidence.

### 5.7 Gestation-at-birth concepts

v3 treats gestation-at-birth concepts (4260747, 46234792) as GW-type when they
have valid values (1-44 weeks). v2 included these in its lists but did not use
them for GW classification.

**Impact: Increase.** More episodes get week-level precision for start date
inference.

### 5.8 ESD concept source

v3 loads ESD concepts from `ESD_concepts.xlsx` instead of hardcoded concept ID
lists. More maintainable and may contain different concept IDs.

### 5.9 Default Matcho term days for PREG in ESD

v3 coalesces missing `min_term`/`max_term` to 140/301 (matching the HIP stage
change). In v2, PREG episodes got NA for these values, making
`inferred_episode_start` NA and `precision_days` undefined.

**Impact:** PREG episodes get valid inferred start dates and precision scores.

### 5.10 Distinct deduplication

v3 applies `distinct(person_id, inferred_episode_end, final_outcome_category)`
before final output. v2 did not deduplicate and could produce duplicate rows from
complex merge patterns.

**Impact: Decrease.** Removes exact duplicate rows.

### 5.11 Delivery mode detection (NEW)

v3 detects cesarean and vaginal delivery using concept intersection within a
+/- 30 day window around the episode end date. Adds flag and count columns. v2
had no delivery mode detection. No impact on episode counts.

---

## 6. Orchestration (`runPregnancyIdentifier.R`)

### 6.1 `conformToValidation = "both"` dual export

When set to `"both"`, v3 runs ESD with `conformToValidation = FALSE`, exports the
non-conformed result, then applies cleanup in memory (fixStartBeforeEnd, filter
start < end, filter < 308 days, filter != 0, removeOverlaps) and exports the
conformed result. This allows direct comparison of both outputs.

### 6.2 Export pipeline (NEW)

v3 adds `exportPregnancies()` which writes shareable aggregated CSV files with
optional small-cell suppression. Includes `quality_check_cleanup.csv` with
per-flag counts and `attrition_if_cleanup.csv` showing what each cleanup step
would do.

---

## Summary: Impact on Episode Counts

Changes that **increase** episode counts relative to v2:

| Change | Stage | Magnitude |
|--------|-------|-----------|
| `gest_value` coalesce for non-measurement records | HIP | High |
| PREG default term days (140/301) | HIP + ESD | High |
| Expanded gestational age concept IDs | HIP | Medium |
| PPS `episode_min_date` imputation (NA to max - 280) | PPS | Medium |
| condition_occurrence date filter bug fix | Init | Low |
| Gestation-at-birth GW classification | ESD | Low |
| LMP/EDD date parsing | ESD | Low |

Changes that **decrease** episode counts relative to v2:

| Change | Stage | Magnitude |
|--------|-------|-----------|
| `conformToValidation` 7-step pipeline (when enabled) | ESD | Very High |
| `removeOverlaps()` with priority hierarchy (when enabled) | ESD | High |
| PPS domain reduction (5 to 3) | Init | Medium |
| `removeLongPregnancies()` > 308 days (when enabled) | ESD | Medium |
| PPS age/sex filtering | Init | Medium |
| Iterative dedup (10 rounds, drops unresolved) | Merge | Medium |
| `distinct()` on (person, end_date, outcome) | ESD | Low-Medium |
| justGestation PREG filter | HIP | Low |

Changes with **variable** impact:

| Change | Stage | Notes |
|--------|-------|-------|
| Reviewed concept files | Init | Depends on file differences |
| Merged episode start bug fix | Merge | Corrects episode boundaries |
| `overlaps()` to explicit inequality join | Merge | Backend-dependent SQL fix |

### Expected Behavior by Configuration

| Configuration | Expected counts vs v2 |
|---|---|
| `conformToValidation = FALSE` (default) | Close to v2. Increases from gest_value/PREG defaults/PPS imputation offset by decreases from PPS domain reduction/age filtering/dedup changes. |
| `conformToValidation = TRUE` | Lower than v2. All increases above plus all decreases from the 7-step cleanup pipeline. |
| `conformToValidation = "both"` | Both outputs available for comparison. |
