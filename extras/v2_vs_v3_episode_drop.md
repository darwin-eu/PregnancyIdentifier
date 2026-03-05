# Why v3 Produces Fewer Episodes and Persons Than v2

Comparison of PregnancyIdentifier v2.0.1 to v3.0.5 on IPCI data shows a 29%
drop in episodes (431,375 → 306,269) and a 19% drop in persons (267,326 →
215,609). This document traces those drops to specific algorithm changes.

## Observed results (IPCI)

| Metric                   | v2        | v3        | Change  |
|--------------------------|-----------|-----------|---------|
| Total episodes           | 431,375   | 306,269   | −29%    |
| Total individuals        | 267,326   | 215,609   | −19%    |
| Mean episodes/person     | 1.61      | 1.42      | −12%    |
| Max episodes/person      | 18        | 15        | −17%    |
| SD episodes/person       | 0.89      | 0.70      | −21%    |
| Median episodes/person   | 1         | 1         | unchanged |
| 25th/75th percentile     | 1 / 2     | 1 / 2     | unchanged |

The unchanged median and quartiles indicate the typical patient is unaffected.
The drops concentrate in multi-episode outliers and in persons with weak or
spurious pregnancy evidence.

---

## Causes of the person drop (−19%)

### 1. Age and sex filtering applied to PPS records

v2 applied female sex and age-range (15–56) filters only to HIP records. PPS
records from males and out-of-range ages were retained and could form episodes.
v3 applies the same filters to PPS at extraction (`initPregnancies.R` lines
202–203). Persons whose only pregnancy evidence was PPS records from an
ineligible age or sex are removed entirely.

### 2. Reviewed concept files

v3 uses curated concept spreadsheets (`HIP_concepts_reviewed17022026.xlsx`,
`PPS_concepts_reviewed1702026.xlsx`) that consolidated mappings and removed
duplicate or clinically ambiguous concepts. Fewer matching concepts means fewer
initial records. Any person whose only qualifying records used a removed concept
is lost.

### Not a factor: PPS domain reduction

v3 extracts PPS records from 3 domains (condition, procedure, measurement)
instead of v2's 5 (which also included observation and visit_occurrence).
However, all 110 PPS concepts that resolve in the OMOP vocabulary belong to
Condition, Procedure, or Measurement domains. The inner join against observation
and visit_occurrence always returned zero rows in v2. This change has no effect
on results.

---

## Causes of the episode drop beyond the person drop (−29% vs −19%)

Episodes dropped faster than persons, meaning multi-episode persons lost
episodes. Three changes explain this.

### 3. Merge deduplication rewrite

This is the primary driver of the excess episode drop and of the max
episodes/person decrease (18 → 15) and SD decrease (0.89 → 0.70).

v2 used 5 hardcoded rounds of deduplication. If many-to-many HIP↔PPS overlaps
remained after round 5, they were silently kept in the output. v3 replaces this
with up to 10 iterative rounds using multi-criteria tie-breaking (end-date
proximity, outcome concordance, duration plausibility ≤ 310 days) and drops
unresolved duplicates with a warning.

Persons with complex overlap patterns — the high-episode outliers — are the
ones most affected. The unchanged median confirms this: persons with 1–2 clean
episodes are untouched.

### 4. Merged episode start date bug fix

v2 computed `merged_episode_start` as
`pmin(first_gest_date, episode_min_date, pregnancy_end)`, where
`pregnancy_end` (the outcome date) was used instead of `pregnancy_start`. v3
corrects this to `pmin(first_gest_date, pps_episode_min_date, pregnancy_start)`.

The incorrect start dates in v2 were too late (at least as late as the outcome),
which made episodes appear shorter and reduced the number of detected overlaps
during deduplication. With corrected (earlier) start dates, v3 detects more
overlaps between episodes within the same person, which the dedup logic then
resolves by keeping fewer episodes.

### 5. Distinct deduplication in ESD

v3 applies `distinct(person_id, inferred_episode_end, final_outcome_category)`
before final output. v2 did not deduplicate and could produce exact duplicate
rows from complex merge patterns. This removes a small number of spurious
duplicate episodes.

---

## Partially offsetting increases

Three v3 bug fixes recover episodes that v2 missed, partially offsetting the
drops above. Without these fixes the drop would be larger than 29%.

- **condition_end_date NULL filter fix.** v2 filtered condition records with
  `condition_end_date <= endDate`, which excluded all conditions with NULL end
  dates (common in OMOP). v3 filters only on `condition_start_date`. Verified
  in test comparison: person 12 was missed entirely by v2 and correctly
  identified by v3 as a live birth.

- **PPS min_date imputation.** v2 left single-concept PPS episode min dates as
  NA, which caused the overlap join with HIP episodes to fail
  (`pregnancy_end >= NA` evaluates to NULL). v3 imputes as `max_date − 280`,
  enabling these episodes to merge with HIP.

- **PREG default term days.** v2 returned NA for `min_term`/`max_term` on PREG
  episodes (not in the Matcho table), preventing them from participating in the
  HIP overlap join. v3 coalesces to 140/301 days.

---

## Summary

| Cause | Affects | Magnitude |
|---|---|---|
| PPS age/sex filtering | Persons and episodes | High |
| Reviewed concept files | Persons and episodes | Medium |
| Merge deduplication rewrite | Episodes (multi-episode outliers) | High |
| Merged episode start bug fix | Episodes (overlap detection) | Medium |
| Distinct dedup in ESD | Episodes (exact duplicates) | Low |
| condition_end_date fix (offset) | Persons and episodes | Low (adds back) |
| PPS min_date imputation (offset) | Episodes | Low (adds back) |
| PREG default terms (offset) | Episodes | Low (adds back) |
