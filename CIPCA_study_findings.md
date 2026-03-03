# CIPCA Study Findings & TODO List

## Study Context

- **Database:** CIPCA (City App database, Spain)
- **Package:** PregnancyIdentifier v3

---

## Key Findings

### 1. Long Gestational Ages (>308 days)
- ~10% of pregnancy episodes have gestational ages longer than 308 days.
- Cause is unknown.

#### Code Investigation Results

**Root cause identified: `conformToValidation = FALSE` by default.**

In `ESD.R`, the `runEsd()` function has parameter `conformToValidation = FALSE` (line 43). The 308-day filter that removes long episodes is **only applied when `conformToValidation = TRUE`**:

```r
# ESD.R lines 289-300
if (conformToValidation) {
  mergedDf <- mergedDf %>%
    dplyr::mutate(.gest_days = dplyr::coalesce(
      as.numeric(.data$esd_gestational_age_days_calculated),
      as.numeric(as.Date(.data$final_episode_end_date) -
                 as.Date(.data$final_episode_start_date))
    )) %>%
    dplyr::filter(is.na(.data$.gest_days) | .data$.gest_days < 308) %>%
    dplyr::select(-".gest_days")
}
```

Because the default is `FALSE`, episodes exceeding 308 days are **never filtered out** in the standard pipeline. A `removeLongPregnancies()` function exists (line 410) but is also never called in the main flow — it is only used in the optional `exportCleanupQualityCheck()` function which reports what *would* happen if cleanup were applied.

**How episodes exceed 308 days:**
- ESD infers `inferred_episode_start` from gestational timing concepts (GW = gestational week, GR3m = gestational range ≤3 months). If a timing concept provides a very early gestational week, the inferred start can be pushed far back.
- When HIP and PPS episodes are merged (`mergeHIPPSEpisodes.R`), the merged episode span can exceed 308 days if the constituent episodes are far apart temporally.
- The Matcho term durations define max_term for DELIV/LB/SB as 308 days, but this is only used as the *fallback* duration when no timing info is available — it doesn't cap episodes that have timing info placing them longer.

**Possible fix options:**
1. Set `conformToValidation = TRUE` when running the algorithm for CIPCA
2. Add a separate post-processing step to cap/flag episodes >308 days
3. Apply `removeLongPregnancies()` explicitly after running `runEsd()`

---

### 2. Abortion / Spontaneous Abortion: No Gestational Time Distribution
- AB and SA episodes all have the exact same gestational time (~168 days for AB).
- Appears to be back-calculated from a single date, producing a fixed duration rather than a distribution.
- 168 days seems too long for many abortions, which typically occur earlier in pregnancy.
- Possible cause: only one code per episode, so ESD always forces the start date to a fixed offset before the abortion code date, defaulting to the maximum possible length.

#### Code Investigation Results

**Root cause confirmed: fallback to `max_term` when no timing concepts found.**

In `ESD.R`, `mergedEpisodesWithMetadata()` (line 1204) computes `inferred_episode_start` from gestational timing concepts (GW/GR3m). When NO timing concepts are found for an episode, `inferred_episode_start` remains `NA`. The fallback logic at lines 1296-1307:

```r
# ESD.R lines 1296-1307
finalDf <- finalDf %>%
  dplyr::left_join(termMaxMin, by = c("final_outcome_category" = "category")) %>%
  dplyr::mutate(
    min_term = dplyr::coalesce(.data$min_term, 140),
    max_term = dplyr::coalesce(.data$max_term, 301),
    inferred_episode_start = dplyr::if_else(
      is.na(.data$inferred_episode_start),
      as.Date(as.Date(.data$inferred_episode_end) - as.numeric(.data$max_term)),
      as.Date(.data$inferred_episode_start)
    ),
    precision_days = dplyr::if_else(
      is.na(.data$precision_days),
      as.numeric(.data$max_term) - as.numeric(.data$min_term),
      .data$precision_days
    )
  )
```

This always sets `inferred_episode_start = inferred_episode_end - max_term` when no timing info exists.

**From Matcho_term_durations.xlsx:**

| Category | max_term | min_term | retry |
|----------|----------|----------|-------|
| DELIV    | 308      | 140      | 28    |
| ECT      | 84       | 42       | 14    |
| AB       | **168**  | 42       | 14    |
| LB       | 308      | 161      | 28    |
| SB       | 308      | 140      | 28    |
| SA       | **139**  | 28       | 14    |

So for AB episodes without timing concepts: gestational time = **exactly 168 days**, precision_days = 168 - 42 = **126 days**.
For SA episodes without timing concepts: gestational time = **exactly 139 days**, precision_days = 139 - 28 = **111 days**.

**Why this happens in CIPCA:**
AB/SA episodes likely have only a single diagnostic code (the abortion/miscarriage code itself) and no accompanying gestational timing concepts (e.g., gestational week records). Without timing information, the algorithm has no way to estimate how far along the pregnancy was, so it defaults to the maximum possible duration.

**The precision_days value is the key signal:** a precision of 126 days (for AB) means the algorithm is saying "the start date could be anywhere from 42 to 168 days before the end date." This is essentially uninformative.

**Possible improvements:**
1. Use a clinically reasonable midpoint or median instead of `max_term` as the fallback (e.g., ~84 days for AB, ~70 days for SA)
2. Flag episodes where the fallback was used (e.g., add a column `start_date_method = "fallback_max_term"`)
3. Use `precision_days` in downstream analyses to weight or filter episodes — episodes with high precision_days should be treated as uncertain

---

### 3. Mode of Delivery Not Detected
- Delivery mode data was not picked up in the Shiny app.
- Could be a Shiny app bug or a study code issue — needs investigation.

#### Code Investigation Results

**Root cause: Shiny app cdm_name mismatch (now fixed).**

The package-side logic for delivery mode detection is working correctly:

1. **`addDeliveryMode()`** (ESD.R lines 1420-1474) uses `PatientProfiles::addConceptIntersectFlag()` and `addConceptIntersectCount()` to detect cesarean and vaginal delivery concepts within a ±30 day window of the episode end date. It adds columns: `cesarean_m30_to_30`, `vaginal_m30_to_30`, and count variants.

2. **`exportDeliveryModeSummary()`** (exportPregnancies.R lines 858-881) aggregates these into `delivery_mode_summary.csv` with columns for outcome category, cesarean counts, vaginal counts, both, neither, and unknown. The CSV file exists and has data (DELIV, LB, AB, ECT outcome rows).

3. **The Shiny `DeliveryModeModule`** filters for `final_outcome_category %in% c("DELIV", "LB")` and pivots the data for display.

**The bug was in the Shiny app's data filtering.** The `cdm_source.csv` file has no `pkg_version` column, so `allDP` was set to `"ipci"`. But the delivery_mode_summary.csv (and all other data CSVs) have `pkg_version = 3.0.1`, which gets normalized to `cdm_name = "ipci_v3"` by `loadFile()`. The `DeliveryModeModule` filtered by `dp = allDP` (i.e., `"ipci"`), but the data had `cdm_name = "ipci_v3"` — so no rows matched and the table was empty.

**This has already been fixed** in the commit addressing the empty tables bug. The fix was applied to 5 modules (DeliveryModeModule, OutcomeCategoriesModule, EpisodeFrequencyModule, MissingDatesModule, and observationPeriodRange FilterTableModule) to use:
```r
dp = if (nrow(data) > 0 && "cdm_name" %in% colnames(data))
  unique(c(data$cdm_name, allDP))
else
  allDP
```

**Status: RESOLVED** — Delivery mode detection should now display correctly in the Shiny app.

---

### 4. PET Matching Rate is Low
- Only ~380 episodes matched, roughly 60% match rate between PET (Pregnancy Extension Table) and the algorithm output.
- High-risk pregnancies appeared more likely to be matched by the algorithm.
- The unmatched episodes need characterization.

---

## TODO List

### PregnancyIdentifier Package

- [x] **Investigate long episodes (>308 days):** Root cause is `conformToValidation = FALSE` by default — the 308-day filter is never applied. See Finding #1 above.
- [ ] **Characterize long episodes:** Analyze whether episodes >308 days cluster by time period, outcome category, number of codes, or other features.
- [ ] **Decide on fix for long episodes:** Choose between enabling `conformToValidation`, adding a separate post-processing cap, or applying `removeLongPregnancies()` explicitly.
- [x] **Investigate AB/SA fixed gestational time:** Confirmed — fallback to `max_term` when no timing concepts found. AB always gets 168 days, SA always gets 139 days. See Finding #2 above.
- [ ] **Summarize AB/SA codes:** Look at the concept codes driving AB/SA episodes — are they always a single code? Does the algorithm always default to max duration when back-calculating?
- [ ] **Decide on fix for AB/SA duration:** Consider using midpoint instead of max_term, or flagging fallback episodes with a `start_date_method` column.
- [x] **Investigate mode of delivery detection:** Was a Shiny app bug (cdm_name mismatch). Already fixed. See Finding #3 above.

### Study Package (Incidence/Prevalence)

- [ ] **Audit prevalence calculation:** Verify that episodes are only counted in the year of the pregnancy start date, not for the full duration. Ensure no double-counting across calendar years.
- [ ] **Check incidence calculation:** Review incidence calculation for similar issues.

### PET Comparison & Matching

- [ ] **Investigate unmatched PET episodes:** Characterize the ~40% of PET episodes not matched by the algorithm. Look at:
  - Number of records per episode
  - Number of distinct codes
  - Specific outcomes
  - Episode length
  - Any patterns distinguishing matched vs. unmatched episodes
- [ ] **Investigate unmatched algorithm episodes:** Similarly characterize algorithm episodes not matched to PET.
- [ ] **Explore more sophisticated matching:** Consider improving the matching algorithm between PET and algorithm output (e.g., overlapping time windows, flexible date matching).
- [ ] **Look for HIP/PPS concepts within PET episode windows:** After running PregnancyIdentifier, check how many HIP and PPS concepts fall within each PET episode's time window. Examine:
  - Which concepts appear and their timing within the PET episode
  - What outcomes are associated with those concepts
  - Whether concept coverage explains why some PET episodes are not picked up
