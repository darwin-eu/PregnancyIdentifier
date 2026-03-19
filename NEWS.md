# PregnancyIdentifier 3.2.1

## New features

* Added `computeIncidencePrevalence()` pipeline step for incidence, prevalence, and cohort characteristics.
* Added `compareWithNationalStats()` with Shiny tab comparing results to European national statistics.
* Added age at first pregnancy by end date Shiny subtab.
* Expanded `compareWithPET()` with additional comparison metrics.

## Bug fixes

* Changed default max term from 301 to 308 days (44-week upper bound).
* Fixed `at_midpoint` calculation in `exportConceptTimingCheck()`.
* Fixed `buildGestationEpisodes()` grouping bug in `secondMin` (extra `gest_week` group).
* Fixed DuckDB type mismatch (`gest_value` INTEGER vs DOUBLE) in `buildGestationEpisodes()` and `attachGestationAndLength()`.

## Other changes

* Updated HIP concept set (`HIP_concepts_reviewed_12032026.xlsx`).
* Pipeline now cleans up intermediate database tables after completion.
* Age at first pregnancy export now includes both start and end date, overall and by year.

# PregnancyIdentifier 3.2.0

## Shiny app - New features

* **National statistics comparison tab:** New sidebar tab that compares
  PregnancyIdentifier database results with published national statistics
  across 12 European countries (Croatia, Denmark, Finland, France, Germany,
  Hungary, Netherlands, Norway, Portugal, Spain, Sweden, UK). The tab contains
  five subtabs:
  - **Overview:** gt summary table of all national indicators (birth rates,
    maternal age, live births, foetal mortality) pivoted by country, with
    country/year picker filters and Word (.docx) download.
  - **Gestational Duration:** Side-by-side comparison of gestational-age bin
    distributions (% of live births per bin) between database results and
    national statistics. Bins are harmonised across sources (<32, 32-36,
    37-41, >=42 weeks). Includes gt table and grouped bar chart
    (ggplot2/plotly) with PNG download.
  - **Delivery Mode:** Comparison of cesarean vs. vaginal delivery rates
    between database results and national statistics. Includes gt table and
    stacked bar chart with PNG download.
  - **Birth Rates & Live Births:** Three sub-subtabs showing crude birth rate,
    live births per year, and foetal mortality rate across countries and years
    (all with plotly rendering and PNG download).
  - **Raw Data:** Filterable DT table of all parsed national statistics with
    CSV download.

  All plots use ggplot2 rendered via `plotly::ggplotly()` with configurable
  height/width/dpi inputs for PNG download. All gt tables support Word (.docx)
  export. National statistics data sourced from
  `National_Statistics_Obj2_v2.csv` (bundled in `inst/shiny/`). European
  number formatting (dots as thousands separators) is handled automatically.

## Bug fix

* Fixed `addAge()` in export to use `year_of_birth`, `month_of_birth`, and
  `day_of_birth` (matching `addAgeSex()`) instead of `birth_datetime`. Previously
  persons with a valid `year_of_birth` but a corrupt or default `birth_datetime`
  (e.g. 0001-01-01) could pass the init-stage age filter yet produce implausible
  ages (e.g. 2776) in the exported age summary CSVs.
  _Expected to **decrease** the max age values in age_summary exports by removing
  implausible outliers. No impact on episode or person counts._

## Shiny app

* **PET comparison - Unmatched characterization tab:** New subtab in the PET
  comparison module that characterizes unmatched HIPPS-only and PET-only
  episodes. Shows gestational length summary (mean, median, IQR) by matching
  group (matched, HIPPS-only, PET-only), outcome distribution as a stacked
  bar chart and detail table, and HIP/PPS concept presence in unmatched PET
  episodes. When no HIP or PPS concepts are found in PET-only episodes, a
  clear explanatory message is displayed.

* **Explain Differences tab:** New sidebar section that compares
  PregnancyIdentifier package versions. Users select an old and new version to
  see computed episode/person count deltas (with N and %) alongside detailed
  markdown explanations of algorithmic changes driving those differences.
  Expanded to 31 pre-written version-pair explanations covering all
  consecutive versions from v0.1.0 through v3.2.0 plus key jump comparisons.
  Fixed version dropdown rendering issue where selectize.js failed to display
  all options due to a race condition with `updateSelectInput`.

* **PET comparison - Unmatched LSC tab:** New subtab showing large-scale
  characteristics (conditions, drugs, procedures, observations, measurements)
  of unmatched PET-only episodes in the 365 days prior to pregnancy end date.
  Filterable by database with downloadable CSV export.

* **PET comparison - Table of metrics fix:** Fixed `visOmopTable` error
  ("no applicable method for 'settings'") when the PET comparison summarised
  result loaded from CSV lost its `summarised_result` class. The table now
  reconstructs the class via `omopgenerics::newSummarisedResult()` before
  rendering.

* **Incidence & Prevalence picker fix:** Fixed empty picker inputs on the
  Incidence and Prevalence tabs. Choices were previously extracted at file
  source time (before data loading); moved extraction into helper functions
  called at UI build time when data is available.

* **Gestational age bins fix:** Corrected gestational age binning to use
  non-overlapping integer-week bins (e.g. "12-27", "28-31", "32-36", "37-41")
  instead of the previous overlapping ranges. Weeks are rounded to integers
  and counts aggregated before binning.

* **Data loading fixes:** Fixed `many-to-many` join warning in episode
  frequency summary, deduplicated delivery mode summary rows to avoid
  `many-to-many` pivot issues, and fixed `tidyr::pivot_longer` /
  `pivot_wider` calls to use tidy-select strings instead of `.data$` pronouns
  for compatibility.

* **Data loading:** `version_differences.csv` is now skipped by `loadFile()`
  to prevent it from being treated as a database result table.

* Minor UI polish: moved download buttons below data tables, narrowed filter
  column widths, added consistent horizontal padding to content area.

## Concept sets

* **Delivery mode - Cesarean:** Added 8 SNOMED concepts for cesarean delivery
  variants (elective, emergency, repeat, breech presentation, hysterectomy,
  and term cesarean sections).

* **Delivery mode - Vaginal:** Expanded with 30 concepts covering spontaneous
  vaginal delivery, forceps delivery, vacuum extraction, breech delivery,
  episiotomy, amniotomy, and ICD9Proc/ICD10PCS procedure codes. Corrected
  inclusion/exclusion flags: excluded broad "Delivery procedure" and
  "Cesarean section" parent concepts (previously included) to avoid capturing
  cesarean deliveries in the vaginal set; re-included ICD9Proc forceps/vacuum
  codes that were previously excluded.

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

## Concept set changes

All concept set files are unchanged across v3.0.0 through v3.0.5. The
concept set changes below apply to the entire v3 release series.

### HIP concepts (`HIP_concepts.xlsx` renamed to `HIP_concepts_reviewed17022026.xlsx`)

* **553 concepts added** (941 in v2 to 1496 in v3). No concepts were removed.
  The vast majority of additions (439) are in the PREG category, with smaller
  additions to DELIV (+93 formerly coded as DELIV presentation concepts now
  available as PREG), LB (+10), ECT (+4), AB (+4), and SA (+3). Most added
  PREG concepts are antenatal care procedures, pregnancy-specific findings,
  and prenatal screening concepts (e.g. amniocentesis, antenatal ultrasound
  scans, pregnancy-specific lab tests). All added PREG concepts have
  `gest_value = NA`.
  _Expected to **increase** episode and person counts (medium magnitude).
  More concepts can trigger inclusion in the initial pregnant cohort._
* **11 concepts recategorized.** 9 concepts moved from DELIV to PREG
  (presentation-related concepts such as breech, face, brow, and transverse
  presentations), 1 from DELIV to ECT, and 1 from DELIV to PREG. Reclassifying
  presentation concepts from DELIV to PREG means they no longer define a
  delivery outcome on their own, reducing the chance of spurious DELIV episodes
  from non-delivery encounters.
  _Expected to **decrease** DELIV episode counts and may **increase** PREG
  episode counts (low magnitude)._
* **`gest_value` column** was already present in v2 but only 61 concepts had
  non-NA values. This is unchanged in v3 (same 61 concepts, same values). The
  difference is that v3 now uses this column during HIP episode construction
  via `coalesce(value_as_number, gest_value)`.

### PPS concepts (`PPS_concepts.xlsx` renamed to `PPS_concepts_reviewed1702026.xlsx`)

* **11 concepts added** (112 in v2 to 123 in v3). No concepts were removed.
  Added concepts include antenatal ultrasound scans at specific gestational
  windows (4-8 weeks, 9-16 weeks, 17-22 weeks, 22-40 weeks), premature birth
  at specific gestational ages (24-26, 26-28, 28-32, 32-36, 36 weeks), the
  "Double test" prenatal screening concept, and "Cervical length scanning at 24
  weeks". Gestational timing values (min_month, max_month) for the 112 common
  concepts are unchanged.
  _Expected to **increase** episode counts (low magnitude). Additional PPS
  concepts provide more gestational timing evidence._
* **Columns renamed** with `pps_` prefix (e.g. `domain_concept_id` to
  `pps_concept_id`, `min_month` to `pps_min_month`).
  _No impact on counts (column rename only)._

### Matcho term durations (`Matcho_term_durations.xlsx`)

* **`max_term` increased from 301 to 308 days** for DELIV, LB, and SB
  categories. ECT (84), AB (168), and SA (139) are unchanged. The wider
  `max_term` means fewer episodes with gestation evidence exceeding the maximum
  are reclassified to PREG.
  _Expected to **increase** episode counts in DELIV, LB, and SB categories
  (low magnitude). Episodes with 302-308 day gestation that were previously
  reclassified as PREG now retain their original outcome category._

### Matcho outcome limits (`Matcho_outcome_limits.xlsx`)

* Unchanged between v2 and v3 (36 rows, identical content).
  _No impact on counts._

### New files in v3 (not present in v2)

* **`ESD_concepts.xlsx`** (NEW, 31 concepts). ESD timing concepts were
  previously hardcoded in `R/ESD.R` function `get_timing_concepts()` across
  three lists: `est_date_of_conception_concepts` (LMP dates, 6 concepts),
  `est_date_of_delivery_concepts` (EDD dates, 15 concepts), and
  `len_of_gestation_at_birth_concepts` (gestation-at-birth, 5 concepts). v3
  externalizes these to an Excel file with structured columns (`esd_concept_id`,
  `esd_concept_name`, `esd_domain_id`, `esd_category`, `is_gw_concept`),
  adding 5 additional concepts not in the v2 hardcoded lists. The v2 hardcoded
  lists totaled 26 unique concepts; v3 has 31.
  _Expected to **increase** start date precision (low indirect effect on
  counts). No direct impact on episode identification but better timing
  evidence can improve episode start dates._
* **`gestational_age_concepts.csv`** (NEW, 3 concept IDs: 3048230, 3002209,
  3012266). These gestational age measurement concept IDs were hardcoded in v2
  in `R/ESD.R` within `get_timing_concepts()` and `R/HIP.R`. v3 externalizes
  them to a CSV file and uses them in both HIP (for gestation episode
  construction) and ESD (for timing search). The concept IDs themselves are
  unchanged from v2.
  _No direct impact on counts from externalization. However, v3 now uses these
  concepts in additional pipeline stages (e.g. ESD timing search), which may
  slightly **increase** start date precision._
* **`delivery_mode/` concept sets** (NEW). Two ATLAS-format JSON concept set
  expression files for cesarean (46 concepts) and vaginal (173 concepts)
  delivery classification. v2 had no delivery mode classification at all. These
  are used in the ESD stage to classify the delivery mode of episodes.
  _No impact on episode or person counts. Delivery mode is an enrichment
  attribute, not a filter._

### check_concepts.csv

* 9 concepts removed and 5 added (34 in v2 to 30 in v3). Removed concepts
  include fetal non-stress tests, Doppler velocimetry, ultrasound procedures,
  and fetal biophysical profiles. Added concepts include glucose tolerance
  gestational panels, first/second trimester pregnancy concepts, and added
  `concept_id_standard` and `standard_concept` columns.
  _Expected to slightly **decrease** episode counts (low magnitude).
  `check_concepts.csv` is used for supplementary pregnancy evidence checks._

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

# PregnancyIdentifier 2.0.1

## Bug fixes

* **Spark SQL compatibility:** Fixed `get_timing_concepts()` to explicitly upload
  `person_id_list` to the database via `CDMConnector::insertTable()` instead of
  relying on `dplyr::inner_join(..., copy = TRUE)`, which failed on Spark.
* **SQL Server integer type casting:** Added explicit `as.integer()` casts for
  `min_term`, `max_term`, `max_gest_day`, `min_gest_day`, and `prev_retry` in
  HIP stage functions to satisfy SQL Server's strict type conversion rules.
* **SQL Server named compute tables:** Changed anonymous `dplyr::compute()` calls
  in `clean_episodes()` to named non-temporary tables (`final_df`, `neg_days_df`,
  `clean_episodes_df`) for SQL Server compatibility.
* **SQL Server `copy = TRUE` removal:** Rewrote `addAge()` and
  `exportConceptTimingCheck()` in `export.R` to upload local data frames to the
  database via `omopgenerics::insertTable()` before joining, replacing
  `copy = TRUE` joins that failed on SQL Server.
* Fixed missing `logger` argument in `log4r::warn()` call in `initial_pregnant_cohort()`.
* Fixed `add_delivery()` to use pre-computed `prev_visit` column instead of
  re-calling `dplyr::lag(visit_date)`, which could produce incorrect results
  due to ordering context differences.

# PregnancyIdentifier 2.0.0

## Algorithm changes

* **Critical: Fixed `datediff()` argument order throughout HIP stage.** All ~15
  `CDMConnector::datediff()` calls in `HIP.R` had start and end date arguments
  reversed, producing wrong-sign date differences. Corrected in `final_visits()`,
  `add_stillbirth()`, `add_ectopic()`, `add_abortion()`, `add_delivery()`,
  `gestation_episodes()`, `add_gestation()`, `clean_episodes()`,
  `remove_overlaps()`, and `final_episodes_with_length()`.
  _This is the most impactful change. All date-difference-based logic (episode
  length, gap detection, gestation calculations) now produces correct values._
* **Study period filtering:** Added `startDate` and `endDate` parameters to
  `runHipps()`, `runHip()`, `runPps()`, and `runEsd()`. All CDM table queries
  now filter records to the specified date range before processing. A final
  post-merge filter ensures output episodes fall within the study period.
* **`justGestation` parameter:** Added to `runHipps()` and `runHip()` (default
  `TRUE`). When `FALSE`, episodes with only gestational concepts and no outcome
  are excluded.
* **ESD `keep_value` range check:** The gestational value validity check
  (`> 0` and `<= 44` weeks) now applies to all gestational timing concepts,
  not just three specific concept IDs. Upper bound changed from `< 44` to
  `<= 44`.

## New features

* **Shiny results viewer:** New `viewResults()` function launches an interactive
  Shiny dashboard for exploring exported results. Supports multi-database
  comparison from multiple zip files. Includes tabs for demographics, incidence
  trends, gestational age distributions, and interactive data tables.
* **Concept timing validation:** New `exportConceptTimingCheck()` function
  validates 35 pregnancy-related concepts against expected gestational timing
  windows. Exports `concept_check.csv` with in-span and at-midpoint counts.
  New `inst/concepts/check_concepts.csv` file defines the validation concepts.

## Export changes

* **`minCellCount` privacy parameter:** Added to `export()` (default: 5).
  Counts between 0 (exclusive) and `minCellCount` are suppressed to NA in
  `exportAgeSummary()`, `exportEpisodeFrequency()`, and
  `exportPregnancyFrequency()`.
* **`pkg_version` tracking:** All exported CSVs now include a `pkg_version`
  column. Zip file name changed to `{date}-{version}-{cdm_name}-results.zip`.
* **Age summary groups:** `exportAgeSummary()` now also exports
  `age_summary_groups.csv` with counts per rounded age year and `<12` / `>55`
  groups.
* Fixed `addAge()` to fall back to `year_of_birth` when `birth_datetime` is NULL.
* Fixed `exportPregnancyOverlapCounts()` to exclude first episodes per person
  (which have no previous end date) from overlap detection.

## Concept set changes

* Updated HIP and PPS concept spreadsheets. Previous versions saved as
  `HIP_concepts-backup.xlsx` and `PPS_concepts-backup.xlsx`.

## Other changes

* Added `R/insertPregnancyEpisodesTable.R` for inserting identified episodes
  back into the database.
* Added explicit `format = "%Y-%m-%d"` to `as.Date()` calls in ESD to avoid
  locale-dependent date parsing.
* Added tests for episode identification (`test-episodes.R`).

# PregnancyIdentifier 1.0.0

## New features

* **Export system (`R/export.R`):** Complete export pipeline replacing the ad-hoc
  `extras/exportResults.R` script. New `export()` entry point runs all
  sub-exports and zips results. Exports include: age summary, precision days
  density, episode frequency, pregnancy frequency per person, gestational age
  summary and counts, gestational duration by outcome, yearly and monthly time
  trends, observation period range, pregnancy overlap counts, date consistency
  checks, reversed dates counts, and outcome category counts for HIP, PPS,
  and final HIPPS.
* **Structured logging:** All algorithm functions (`runHip`, `runPps`, `runEsd`,
  `mergeHipPps`) now accept a `logger` parameter and use `log4r` for structured
  file and console logging, replacing ad-hoc `message()` calls.
* **`insertPregnancyEpisodesTable()`:** New utility function to insert
  identified pregnancy episodes back into the database via
  `CDMConnector::insertTable()`.
* **`summariseColumn()`:** New utility computing mean, sd, median, Q1, Q3, min,
  max, and count for a specified column.

## Bug fixes

* Fixed `dplyr::pull(n)` to `dplyr::pull(.data$n)` in multiple HIP functions
  for strict tidy evaluation compliance.
* Fixed `clean_episodes()` log message that was missing the `%s` format
  specifier for episode count.
* Fixed ESD `inferred_episode_start` fallback calculation: removed unnecessary
  `lubridate::days()` wrapping around `max_term`.

## Other changes

* Added `magrittr`, `tidyr`, and `log4r` as package dependencies.
* Added proper package documentation in `R/PregnancyIdentifier-package.R` with
  `globalVariables()` declarations.
* Re-exported the magrittr pipe operator via `R/utils-pipe.R`.

# PregnancyIdentifier 0.1.7

## Algorithm changes

* **ESD robustness:** Wrapped gestational timing processing in a guard for empty
  timing concepts. When no timing data exists, returns an empty tibble with the
  correct schema instead of failing.
* **HIP `remove_overlaps()` fixes:** Added guard for empty `gest_id_list`
  (prevents errors when no overlapping episodes exist). Replaced R-native date
  arithmetic with `CDMConnector::dateadd()` for SQL compatibility. Added
  intermediate `dplyr::compute()` materializations with named tables.

## Other changes

* Replaced all `cat()` logging calls with `message(sprintf())` throughout HIP,
  PPS, ESD, and merge stages.
* Fixed bare column references to use `.data$` pronouns in
  `mergeHIPPSEpisodes.R`.
* Added guard for empty PPS episodes in merge stage: if zero rows, adds
  `person_episode_number` column with default value.
* Added `^dev$` and `^extras$` to `.Rbuildignore`.
* Re-exported `runHipps()` in NAMESPACE.

# PregnancyIdentifier 0.1.6

* **HIP `add_gestation()` SQL fix:** Split large mutate in the `both_df` join
  section into separate `mutate()` calls for `gest_at_outcome`, then
  `is_under_max`/`is_over_min`, then `days_diff`. Ensures SQL backends can
  reference columns created in prior steps.
* Same split applied to `remove_overlaps()`.

# PregnancyIdentifier 0.1.5

* **HIP `add_gestation()` SQL fix:** Split a single large `dplyr::mutate()`
  call into three separate `mutate() %>% compute()` steps (gest_id, then
  max/min_gest_day, then start dates and comparisons). Some SQL backends cannot
  reference columns created in the same `mutate()` call.
* Wrapped `CDMConnector::dateadd()` results in `as.Date()` in
  `calculate_start()`.

# PregnancyIdentifier 0.1.4

## Algorithm changes

* **Full pipeline re-enabled:** Uncommented and activated the complete
  HIPPS merge and ESD pipeline in `runHIPPS.R`. The merge pipeline (outcomes
  per episode, add outcomes, final merged episodes, deduplication, demographic
  details) and ESD pipeline (timing concepts, gestational timing info, metadata
  merge) are now fully operational.
* Changed `runHip()` and `runPps()` to return `cdm` objects instead of
  separate result objects. Results are read from RDS files saved by each stage.
* Replaced R-native date arithmetic in `add_gestation()` with
  `CDMConnector::dateadd()` for `max_gest_start_date` and
  `min_gest_start_date`.
* Changed `runHip()` early return from `return(NULL)` to `return(cdm)`.

# PregnancyIdentifier 0.1.3

* **MS SQL compatibility:** Added `as.character()` casts for date-to-string
  conversion in `paste()` calls in `ESD.R` (`episodes_with_gestational_timing_info`)
  and `HIP.R` (`add_gestation`). Fixes implicit casting failures on MS SQL Server.

# PregnancyIdentifier 0.1.2

* **MS SQL compatibility:** Wrapped date component columns in
  `as.character(as.integer(...))` in `HIP.R` and `PPS.R` for date_of_birth
  construction. Handles backends where `year_of_birth` is stored as
  numeric/float.
* Replaced R-native date arithmetic with `CDMConnector::dateadd()` in
  `calculate_start()` for `min_start_date` and `max_start_date`.

# PregnancyIdentifier 0.1.1

* **MS SQL compatibility:** Changed `as.integer()` casts to `as.character()` in
  `HIP.R` and `PPS.R` when building `date_of_birth` via string concatenation.
  The `as.integer()` approach failed on MS SQL Server.
* Removed premature `runHipps` export from NAMESPACE.

# PregnancyIdentifier 0.1.0

* Initial release of the PregnancyIdentifier package implementing the HIPPS
  algorithm (HIP + PPS + Merge + ESD) for identifying pregnancy episodes in
  OMOP CDM databases.
