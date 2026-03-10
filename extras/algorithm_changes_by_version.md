# PregnancyIdentifier Algorithm Changes by Version: Impact on Person and Episode Counts

**Versions covered:** v2.0.1 through v3.0.5

---

## 1. Executive Summary

PregnancyIdentifier underwent a major refactoring from v2 to v3 with significant algorithmic changes that affect the number of pregnancy episodes and persons identified. This document catalogs every algorithm-level change between v2.0.1 and v3.0.5, organized by version transition, focusing exclusively on changes that affect person and/or episode counts in the output. Cosmetic changes (renaming, logging, code style, Shiny app UI) are omitted unless they have a functional side-effect.

**Key findings:**

- The **v2.0.1 to v3.0.0** transition was a complete rewrite with both count-increasing changes (condition date filter fix, `gest_value` coalesce, PREG default term days, PPS `min_date` imputation) and count-decreasing changes (PPS age/sex filtering, improved deduplication, merged episode start bug fix, reviewed concept files).
- **v3.0.1** introduced bug fixes that primarily increased counts (person-scoped overlap removal, NA-safe filtering, expanded gestation evidence via coalesce).
- **v3.0.2** introduced new unconditional ESD filters (observation period, start >= end, zero-length) that decreased counts.
- **v3.0.3** and **v3.0.4** contained no algorithmic changes affecting counts.
- **v3.0.5** reversed the unconditional ESD filters from v3.0.2, making them conditional on `conformToValidation` (default `FALSE`), and restored PREG-only concepts, returning counts closer to v2 levels.

Empirical validation on IPCI showed v3.0.5 with `conformToValidation = FALSE` produced 29% fewer episodes and 19% fewer persons than v2.0.1, primarily driven by improved deduplication and PPS age/sex filtering.

### Version Overview

| Version | Date | Net Impact on Counts | Primary Drivers |
|---------|------|---------------------|-----------------|
| v2.0.1 | 2026-02-04 | Baseline | -- |
| v3.0.0 | 2026-02-19 | **DECREASE** | Dedup rewrite, PPS age/sex filtering, concept review |
| v3.0.1 | 2026-02-20 | **INCREASE** vs v3.0.0 | Bug fixes: person-scoped overlap removal, NA-safe filter, coalesce gest_value |
| v3.0.2 | 2026-02-23 | **DECREASE** vs v3.0.1 | New unconditional ESD filters: obs period, start>=end, zero-length |
| v3.0.3 | 2026-02-24 | NO CHANGE | Parameter renaming only |
| v3.0.4 | 2026-02-25 | NO CHANGE | Shiny app updates only |
| v3.0.5 | 2026-03-03 | **INCREASE** vs v3.0.4 | ESD filters made conditional, PREG-only concepts restored |

---

## 2. Version-by-Version Changes

### 2.1 v2.0.1 to v3.0.0 (Major Rewrite)

#### 2.1.1 Init Stage (`initPregnancies.R`)

| Change | Description | Direction | Magnitude |
|--------|-------------|-----------|-----------|
| Reviewed concept files | v3 loads reviewed concept spreadsheets (`HIP_concepts_reviewed17022026.xlsx`, `PPS_concepts_reviewed1702026.xlsx`) instead of the originals. Concepts were consolidated and clinically ambiguous entries removed. | DECREASE | Medium |
| `gest_value` column from HIP concept sheet | v3 carries a `gest_value` column from the HIP concept spreadsheet through to `preg_hip_records`. This allows condition, procedure, and observation records (which lack `value_as_number`) to contribute gestational age information via the concept sheet. In v2 only measurement records with a non-NA `value_as_number` could contribute gestational age. | INCREASE | High |
| `condition_occurrence` date filter bug fix | v2 filtered condition records with `condition_end_date <= endDate`, which excluded conditions whose end date extended past the study window even when the start date was within range. Also excluded all conditions with NULL `condition_end_date` (common in OMOP). v3 filters only on `condition_start_date`, consistent with all other domains. | INCREASE | Low-Medium |
| PPS data extraction reduced from 5 to 3 domains | v2 pulled PPS records from `condition_occurrence`, `procedure_occurrence`, `observation`, `measurement`, and `visit_occurrence`. v3 drops `observation` and `visit_occurrence`. However, empirical testing showed all current PPS concepts resolve to Condition, Procedure, or Measurement domains, so the inner join against observation and visit_occurrence always returned zero rows in v2. This change has no practical effect on results with current concept sets. | Neutral | None |
| Age/sex filtering applied to PPS records | v3 applies female sex and age-range (15-56) filters to PPS records at extraction. v2 only applied these filters to HIP records. Persons whose only pregnancy evidence was PPS records from an ineligible age or sex are removed. | DECREASE | High |
| PREG-only concepts dropped from initial cohort | v3.0.0 filtered out HIP concepts that had category "PREG" but no `gest_value` and were not in `gestational_age_concepts.csv`. These concepts indicate general pregnancy status without specifying a specific outcome or gestational timing. Since PREG-only concepts were not used for episode definition in v2 either, this had minimal practical effect. | DECREASE | Negligible |

#### 2.1.2 HIP Stage (`HIP.R`)

| Change | Description | Direction | Magnitude |
|--------|-------------|-----------|-----------|
| Effective gestation via coalesce | v3 computes `effective_gest = coalesce(value_as_number, gest_value)` when building gestation episodes. Any record where the concept sheet provides a `gest_value` can form a gestation episode, even without a measurement value. v2 used only 3 hardcoded concept IDs from measurements. | INCREASE | High |
| Expanded gestational age concept IDs | v3 loads gestational age concept IDs from `gestational_age_concepts.csv` instead of the 3 hardcoded IDs in v2. The external file may contain additional concepts. | INCREASE | Medium |
| Default Matcho term days for PREG episodes | v3 coalesces missing `min_term`/`max_term` to 140/301 days respectively. In v2, PREG episodes (not in the Matcho term durations table) got NA for both values, making `min_start_date` and `max_start_date` NA. This prevented PREG episodes from participating in the overlap join with gestation episodes. | INCREASE | High |
| `justGestation` filter for PREG episodes | When `justGestation = TRUE`, v3 drops PREG outcome+gestation episodes if a later gestation-only episode exists for the same person. This prevents double-counting the same pregnancy. v2 had no such filter. | DECREASE | Low |
| Max gestation term changed from 301 to 308 days | The maximum term (`max_term`) for certain outcome categories was increased from 301 to 308 days. Fewer episodes are reclassified from their outcome category to "PREG" due to exceeding `max_term`. | INCREASE | Low |
| `removeOverlaps` bug fix (empty `gest_id_list`) | v2 had a bug where an empty `gest_id_list` was set to `""`, causing incorrect filtering behavior. v3 uses a sentinel value to prevent incorrect overlap removal. | Variable | Low |

#### 2.1.3 PPS Stage (`PPS.R`)

| Change | Description | Direction | Magnitude |
|--------|-------------|-----------|-----------|
| PPS `min_date` imputation | v3 imputes missing PPS episode min dates as `max_date - 280` days. v2 left them as NA, which caused the overlap join with HIP episodes to fail (because `pregnancy_end >= NA` evaluates to NULL in SQL). | INCREASE | Medium |

#### 2.1.4 Merge Stage (`mergeHIPPSEpisodes.R`)

| Change | Description | Direction | Magnitude |
|--------|-------------|-----------|-----------|
| Merged episode start date bug fix | v2 computed `merged_episode_start` using `pmin(first_gest_date, episode_min_date, pregnancy_end)`, where `pregnancy_end` (the outcome date) was used instead of `pregnancy_start`. v3 corrects this to use `pregnancy_start`. The incorrect start dates in v2 were too late, which made episodes appear shorter, reduced detected overlaps during dedup, and kept more episodes. | DECREASE | Medium |
| Iterative deduplication rewrite | v2 used 5 hardcoded rounds. If duplicates remained after round 5, they were silently kept. v3 uses up to 10 iterative rounds with multi-criteria tie-breaking and drops unresolved duplicates with a warning. This is the primary driver of episode count reduction for multi-episode outliers. | DECREASE | High |
| Overlap join backend fix | v3 uses explicit inequality filters instead of `join_by(overlaps(...))`, which had known SQL generation issues on PostgreSQL, Spark, and Snowflake. Logically equivalent but fixes backend-specific failures that could silently drop rows. | INCREASE on affected backends | Variable |

#### 2.1.5 ESD Stage (`ESD.R`)

| Change | Description | Direction | Magnitude |
|--------|-------------|-----------|-----------|
| LMP and EDD date parsing (NEW) | v3 parses Last Menstrual Period (LMP) and Estimated Delivery Date (EDD) concept date values. LMP provides a direct pregnancy start date; EDD provides start as `EDD - 280` days. v2 included these concepts but never parsed their date values. | Improves start date precision | Low |
| Gestation-at-birth concepts | v3 treats gestation-at-birth concepts (4260747, 46234792) as GW-type when they have valid values (1-44 weeks). v2 included these concepts but did not use them for GW classification. | INCREASE | Low |
| Distinct deduplication in ESD | v3 applies `distinct(person_id, inferred_episode_end, final_outcome_category)` before final output. v2 did not deduplicate and could produce exact duplicate rows. | DECREASE | Low-Medium |
| ESD overlap collapse (NEW) | v3 adds `collapseOverlappingEpisodesWithinPerson()` to merge overlapping [start, end] intervals within each person. v2 had no overlap removal in ESD. | DECREASE | Medium |
| `conformToValidation` option (NEW, default `FALSE`) | Adds a 7-step cohort attrition pipeline that, when enabled, applies: observation period filters, start >= end filter with `fixStartBeforeEnd()` rescue, gestational length < 308 days, gestational length != 0, and overlap removal with priority hierarchy. v2 had none of these steps. | DECREASE (when enabled) | Very High |

---

### 2.2 v3.0.0 to v3.0.1 (Bug Fixes)

| Change | Description | Direction | Magnitude |
|--------|-------------|-----------|-----------|
| Gestation record selection rewrite | v3.0.0 used two separate sources union_all'd together for gestation episodes, with both only using `value_as_number`. Records with null `value_as_number` but non-null `gest_value` (from the concept sheet) were never picked up. v3.0.1 uses `coalesce(value_as_number, gest_value)` in a single unified pipeline, allowing concept-sheet gestational ages to contribute even without a measurement value. Also fixes a varchar-to-integer cast issue on some backends (Spark/Redshift) where `value_as_number` stored as varchar would silently produce NULLs. | INCREASE | Medium-High |
| Post-union deduplication on `(person_id, final_episode_id)` | v3.0.0 could produce duplicate rows for the same logical episode due to column misalignment in `union_all` of the "both", "outcome-only", and "gestation-only" branches. v3.0.1 adds explicit deduplication ensuring exactly one row per `(person_id, final_episode_id)`. | DECREASE | Low |
| PREG filter when gestation-only episodes exist | When `justGestation = TRUE`, v3.0.1 drops PREG-category outcome+gestation episodes when the same person has a later gestation-only episode (superseded PREG episodes). v3.0.0 did not have this cross-branch filter. | DECREASE | Low |
| Person-scoped overlap removal (bug fix) | v3.0.0 collected overlapping PREG `gest_id` values globally and filtered them using `%in%`. If `gest_id` `"42_G_2020-01-15"` overlapped for person A, it could be removed for ALL persons who shared that `gest_id` string. v3.0.1 scopes removal to `(person_id, gest_id)` pairs using `anti_join`, fixing the cross-person removal bug. | INCREASE | Medium |
| NA-safe `restFinal` filter (bug fix) | v3.0.0's filter `!(!is.na(max_gest_week) & is_over_min == 0)` evaluated to NA in SQL when `max_gest_week` was not null but `is_over_min` was NA (common for gestation-only episodes). In SQL, NA in WHERE drops the row. v3.0.1 explicitly includes `is.na(is_over_min)` as a keep condition. | INCREASE | Medium |
| Union type-casting fix for strict database backends | v3.0.0 used R-level NA values (`NA_integer_`, `as.Date(NA)`) as placeholder columns in the outcome-only branch before `union_all`. On strict backends (PostgreSQL, Spark), untyped NULLs could cause the `UNION ALL` to fail or silently drop rows due to type mismatches. v3.0.1 uses `CAST(NULL AS INTEGER)` and `CAST(NULL AS DATE)`. | INCREASE on affected backends | Variable |
| Permanent table materialization | Changed `temporary = TRUE` to `temporary = FALSE, overwrite = TRUE` for intermediate `compute()` calls. Some database backends (Snowflake, Spark) have issues with temporary tables in complex queries. Using permanent tables ensures intermediate results are not lost. | INCREASE on affected backends | Variable |

---

### 2.3 v3.0.1 to v3.0.2 (New ESD Filters)

| Change | Description | Direction | Magnitude |
|--------|-------------|-----------|-----------|
| Observation period filters (steps 2-3) -- ALWAYS APPLIED | Two new mandatory filtering steps require that the pregnancy start date AND end date fall within an `observation_period` for the person. Episodes where either date falls outside any observation period are dropped via `inner_join`. Episodes with NA start dates are also dropped (they cannot satisfy the inner_join). v3.0.1 had no observation period checks. | DECREASE | Very High |
| End > Start filter (step 4) -- ALWAYS APPLIED | New unconditional filter drops episodes where `final_episode_start_date >= final_episode_end_date`. When `conformToValidation = TRUE`, `fixStartBeforeEnd()` runs first to rescue some episodes by resetting `start = end - max_term`. The rescue is only available with `conform = TRUE`; with the default `FALSE`, inverted-date episodes are simply dropped. | DECREASE | Medium |
| Zero-length episode filter (step 6) -- ALWAYS APPLIED | New unconditional filter drops episodes where gestational length in days equals exactly 0 (computed as `coalesce(esd_gestational_age_days_calculated, end - start)`). v3.0.1 only removed zero-length episodes when `conformToValidation = TRUE`. | DECREASE | Low-Medium |
| Expanded gestational age concept search in ESD timing | `gestational_age_concepts.csv` concept IDs are now included in the ESD timing concept search alongside PPS and ESD concepts. More timing evidence means better start date inference, which can rescue episodes from the observation period and start >= end filters. | INCREASE (indirect) | Low |
| Date coercion in study period filter | Explicit `as.Date()` coercion before date comparisons in the study period filter. Fixes edge cases where POSIXct or character date columns could produce incorrect comparison results due to timezone issues. | Variable | Low |
| `conformToValidation = "both"` mode (NEW) | New option runs ESD without conformance, exports the non-conformed result, then applies cleanup in memory and exports the conformed result. Allows direct comparison of both outputs. Does not change the default algorithm behavior. | NO CHANGE | None |

---

### 2.4 v3.0.2 to v3.0.3 (No Algorithmic Changes)

No algorithmic changes affecting person or episode counts. All changes were parameter renaming (`exportDir` to `exportFolder`) and output path refactoring.

---

### 2.5 v3.0.3 to v3.0.4 (No Algorithmic Changes)

No algorithmic changes affecting person or episode counts. Changes were limited to the Shiny app UI (max 308 days display) and adding `minCellCount` to PET comparison output.

---

### 2.6 v3.0.4 to v3.0.5 (ESD Filters Made Conditional)

| Change | Description | Direction | Magnitude |
|--------|-------------|-----------|-----------|
| PREG-only concepts restored in initial cohort | Reverts the v3.0.0 filter that dropped HIP concepts in the "PREG" category without `gest_value`. All HIP concepts (including PREG-only) are now used to build the initial cohort. This preserves v2 behavior where PREG-only concepts contribute to the initial pregnant cohort and may indirectly affect downstream episode construction. | INCREASE | Low-Medium |
| Observation period filters now conditional on `conformToValidation` | The observation period checks (steps 2-3 in ESD) that were unconditional in v3.0.2-v3.0.4 are now wrapped in `if (conformToValidation)`. Since the default is `FALSE`, episodes with start/end dates outside observation periods are no longer dropped by default. | INCREASE | Very High |
| Start >= end filter now conditional on `conformToValidation` | Both `fixStartBeforeEnd()` and the filter dropping episodes where `start >= end` are now wrapped in `if (conformToValidation)`. With the default `FALSE`, episodes with inverted dates are retained and characterized via the `is_start_gte_end` quality flag. | INCREASE | Medium |
| Zero-length episode filter now conditional on `conformToValidation` | The zero-length episode filter is now wrapped in `if (conformToValidation)`. With the default `FALSE`, zero-length episodes are retained and characterized via the `is_zero_length` quality flag. | INCREASE | Low-Medium |
| Per-record quality flags added (NEW) | Six boolean quality flag columns are added to every output record regardless of `conformToValidation`: `is_start_outside_observation_period`, `is_end_outside_observation_period`, `is_start_gte_end`, `is_zero_length`, `is_too_long` (>= 308 days), `is_overlapping`. These characterize episode quality without removing records. | NO CHANGE | None |

---

## 3. Cumulative Impact Summary

| Version | Change | Pipeline Stage | Direction | Magnitude |
|---------|--------|---------------|-----------|-----------|
| **v3.0.0** | Reviewed concept files | Init | DECREASE | Medium |
| v3.0.0 | `gest_value` column from HIP concept sheet | Init | INCREASE | High |
| v3.0.0 | `condition_occurrence` date filter bug fix | Init | INCREASE | Low-Medium |
| v3.0.0 | PPS domains reduced from 5 to 3 | Init | Neutral | None |
| v3.0.0 | Age/sex filtering applied to PPS records | Init | DECREASE | High |
| v3.0.0 | PREG-only concepts dropped | Init | DECREASE | Negligible |
| v3.0.0 | Effective gestation via coalesce | HIP | INCREASE | High |
| v3.0.0 | Expanded gestational age concept IDs | HIP | INCREASE | Medium |
| v3.0.0 | Default Matcho term days for PREG episodes | HIP | INCREASE | High |
| v3.0.0 | `justGestation` filter for PREG episodes | HIP | DECREASE | Low |
| v3.0.0 | Max gestation term 301 to 308 days | HIP | INCREASE | Low |
| v3.0.0 | `removeOverlaps` bug fix | HIP | Variable | Low |
| v3.0.0 | PPS `min_date` imputation | PPS | INCREASE | Medium |
| v3.0.0 | Merged episode start date bug fix | Merge | DECREASE | Medium |
| v3.0.0 | Iterative deduplication rewrite | Merge | DECREASE | High |
| v3.0.0 | Overlap join backend fix | Merge | INCREASE (backend-dependent) | Variable |
| v3.0.0 | LMP and EDD date parsing | ESD | Improves precision | Low |
| v3.0.0 | Gestation-at-birth concepts | ESD | INCREASE | Low |
| v3.0.0 | Distinct deduplication in ESD | ESD | DECREASE | Low-Medium |
| v3.0.0 | ESD overlap collapse | ESD | DECREASE | Medium |
| v3.0.0 | `conformToValidation` option (default FALSE) | ESD | DECREASE (when enabled) | Very High |
| **v3.0.1** | Gestation record coalesce rewrite | HIP | INCREASE | Medium-High |
| v3.0.1 | Post-union deduplication | HIP | DECREASE | Low |
| v3.0.1 | PREG filter for superseded episodes | HIP | DECREASE | Low |
| v3.0.1 | Person-scoped overlap removal (bug fix) | HIP | INCREASE | Medium |
| v3.0.1 | NA-safe `restFinal` filter (bug fix) | HIP | INCREASE | Medium |
| v3.0.1 | Union type-casting fix | HIP | INCREASE (backend-dependent) | Variable |
| v3.0.1 | Permanent table materialization | HIP | INCREASE (backend-dependent) | Variable |
| **v3.0.2** | Observation period filters (unconditional) | ESD | DECREASE | Very High |
| v3.0.2 | End > Start filter (unconditional) | ESD | DECREASE | Medium |
| v3.0.2 | Zero-length episode filter (unconditional) | ESD | DECREASE | Low-Medium |
| v3.0.2 | Expanded ESD gestational age concepts | ESD | INCREASE (indirect) | Low |
| v3.0.2 | Date coercion in study period filter | ESD | Variable | Low |
| **v3.0.3** | *(No algorithmic changes)* | -- | -- | -- |
| **v3.0.4** | *(No algorithmic changes)* | -- | -- | -- |
| **v3.0.5** | PREG-only concepts restored | Init | INCREASE | Low-Medium |
| v3.0.5 | Observation period filters now conditional | ESD | INCREASE | Very High |
| v3.0.5 | Start >= end filter now conditional | ESD | INCREASE | Medium |
| v3.0.5 | Zero-length episode filter now conditional | ESD | INCREASE | Low-Medium |

---

## 4. Configuration Guide

### 4.1 Expected Behavior by Configuration

| Configuration | Expected counts vs v2 |
|---|---|
| `conformToValidation = FALSE` (default) | Close to v2. Increases from `gest_value`/PREG defaults/PPS imputation partially offset by decreases from PPS age/sex filtering, concept review, and dedup rewrite. Net effect depends on dataset characteristics. |
| `conformToValidation = TRUE` | Lower than v2. All increases above plus all decreases from the 7-step ESD cleanup pipeline (observation period, start >= end, zero-length, > 308 days, overlaps). |
| `conformToValidation = "both"` | Both outputs available for comparison. Non-conformed output exported to `exportFolder/conform_false`, conformed output to `exportFolder/conform_true`. |

### 4.2 Empirical Results (IPCI)

| Metric | v2.0.1 | v3.0.5 (conform=FALSE) | Change |
|---|---|---|---|
| Total episodes | 431,375 | 306,269 | -29% |
| Total individuals | 267,326 | 215,609 | -19% |
| Mean episodes/person | 1.61 | 1.42 | -12% |
| Max episodes/person | 18 | 15 | -17% |
| SD episodes/person | 0.89 | 0.70 | -21% |
| Median episodes/person | 1 | 1 | unchanged |

The unchanged median and quartiles indicate the typical patient is unaffected. The drops concentrate in multi-episode outliers and in persons with weak or spurious pregnancy evidence. The primary drivers of the IPCI reduction were: (1) improved merge deduplication, (2) PPS age/sex filtering, and (3) reviewed concept files.
