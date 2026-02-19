# Concept files used by PregnancyIdentifier

This folder contains Excel/CSV concept sets and configuration tables used by the HIP, PPS, and ESD algorithms. Below is how each file is used in the codebase.

---

## HIP (outcome-based pregnancy identification)

### `HIP_concepts_reviewed17022026.xlsx`

**Purpose:** Defines which OMOP concepts are treated as pregnancy-related for the HIP algorithm. Records with these concepts are pulled from **condition_occurrence**, **procedure_occurrence**, **observation**, and **measurement** into `preg_hip_records`.

**Expected columns:**

| Column       | Description |
|-------------|-------------|
| `concept_id` | OMOP concept ID (integer). |
| `concept_name` | Concept name (for reference; not used in logic). |
| `category` | One of: **LB** (live birth), **SB** (stillbirth), **DELIV** (unspecified delivery), **ECT** (ectopic), **AB** (abortion), **SA** (miscarriage), **PREG** (ongoing/unspecified). |
| `gest_value` | Optional. If present and non-NA, gestational weeks for this concept (e.g. for “Gestation period, 12 weeks”–style concepts). Used so condition/procedure/observation rows without `value_as_number` can still contribute to gestation episodes. |

**How it’s used:**

1. **Load and filter** (`initPregnancies`): Only concepts that are *used* for episode definition are kept. A concept is kept if **any** of:
   - `category` is in **LB, SB, DELIV, ECT, AB, SA** (outcome categories), or  
   - `gest_value` is non-NA, or  
   - `concept_id` is in **gestational_age_concepts.csv**.  
   Concepts that are PREG-only with no `gest_value` and not in the gestational-age list are dropped at load (and a log message reports how many were dropped).

2. **Outcome episodes:** Records whose `category` is in **AB, SA, DELIV, ECT, SB, LB** are used to build outcome visits and episodes. Spacing between outcome visits is controlled by **Matcho_outcome_limits.xlsx**.

3. **Gestation episodes:** Gestational-weeks evidence comes from:
   - Records whose `concept_id` is in **gestational_age_concepts.csv** and have `value_as_number` in 1–44 (weeks), or  
   - Records with non-NA **gest_value** from this file (e.g. “Gestation period, N weeks” concepts).  

So: **HIP_concepts_reviewed17022026** supplies both outcome labels (`category`) and, when `gest_value` is set, direct gestation weeks for building episodes. Records can come from any of the four domains above; the algorithm does not restrict by domain.

---

### `gestational_age_concepts.csv`

**Purpose:** Small list of concept IDs treated as **gestational age in weeks** (numeric value 1–44). Used by both HIP and ESD.

**Expected columns:**

| Column       | Description |
|-------------|-------------|
| `concept_id` | OMOP concept ID (integer). |

**How it’s used:**

- **HIP** (`buildGestationEpisodes`, `attachGestationAndLength`): Records in `preg_hip_records` with this `concept_id` and `value_as_number` in 1–44 are used to build gestation episodes and to attach gestation length to final episodes. Also used in **initPregnancies** to decide which HIP concepts to keep (see above).
- **ESD** (`getTimingConcepts`): For these concept IDs, if the parsed value is 1–44, the record gets `keep_value = 1` and `extrapolated_preg_start = record_date - (value * 7)` (gestation-week back-calculation). They are also eligible for `gt_type = "GW"` via **ESD_concepts2.xlsx**.

Typical contents: e.g. 3048230 (Gestational age in weeks), 3002209 (Gestational age Estimated), 3012266 (Gestational age).

---

### `Matcho_outcome_limits.xlsx`

**Purpose:** Defines minimum spacing (days) between outcome events of the same or different types for the HIP outcome logic (Matcho et al.–style rules).

**How it’s used:** **HIP** (`buildFinalOutcomeVisits`, `buildOutcomeEpisodes`): For each outcome category (LB, SB, ECT, AB/SA, DELIV), the algorithm uses these limits to decide whether two visits are the same episode or separate episodes (e.g. minimum days between two live births, or between a delivery and a stillbirth). The table is joined by `first_preg_category` and `outcome_preg_category` to get `min_days`.

---

### `Matcho_term_durations.xlsx`

**Purpose:** Gives minimum and maximum gestation length (term), in days, per outcome category. Used to bound the possible pregnancy start date given an outcome date.

**Expected columns (conceptually):** Include at least `category` (e.g. LB, SB, AB, …) and term lengths (e.g. `min_term`, `max_term` in days).

**How it’s used:** **HIP** (`estimateOutcomeStarts`): For each outcome episode, the algorithm looks up the category in this table and computes `min_start_date` and `max_start_date` as outcome_date minus the max/min term. **ESD** also uses the same table (via the merged CDM) for term-based checks and gestational-age-at-outcome calculations (e.g. term duration flags, preterm status). If a category is missing (e.g. PREG), the code uses default min/max term days so that start dates are still defined.

---

## PPS (Pregnancy Prevention Score / gestational timing)

### `PPS_concepts_reviewed1702026.xlsx`

**Purpose:** Concepts and their gestational-time windows (in months) used for PPS episode building and for ESD “GR3m” (gestational timing ≤ 3 months) evidence.

**Expected columns (after lowercasing):** Include at least `pps_concept_id`, `pps_min_month`, `pps_max_month` (and typically name and other PPS-specific columns).

**How it’s used:**

- **PPS algorithm:** Records with these concept IDs are pulled from condition, procedure, and measurement (no observation) and used to build PPS episodes and outcomes.
- **ESD** (`getTimingConcepts`): All PPS concept IDs are included in the set of concepts to pull for timing. After pulling, each record is left-joined to this table by `domain_concept_id = pps_concept_id`. If `min_month` (and `max_month`) are present, the record is assigned **gt_type = "GR3m"** and contributes a pregnancy-start *range*: `min_pregnancy_start = record_date - (min_month * 30.4)` and similarly for max. That range is used in ESD’s timing/intersection logic.

So: **PPS_concepts_reviewed1702026** drives both PPS episode definition and ESD’s GR3m timing evidence. Records can come from condition, procedure, or measurement.

---

## ESD (Estimated Start Date)

### `ESD_concepts.xlsx`

**Purpose:** Master list of concept IDs used for ESD timing. All of these IDs are pulled from condition, observation, measurement, and procedure; then **esd_category** determines how the value is interpreted (GW weeks, LMP date, EDD date, or gestation-at-birth weeks).

**Expected columns:**

| Column           | Description |
|------------------|-------------|
| `esd_concept_id` | OMOP concept ID (integer or string that coerces to integer). |
| `esd_concept_name` | Concept name (for reference / display). |
| `esd_domain_id`  | Domain label (e.g. Observation, Measurement); for reference only—the code does not filter by domain. |
| `esd_category`   | Drives how the record is used (see below). |

**esd_category values and usage:**

- **estConceptionConceptIds**  
  **LMP-type concepts.** The record’s value is parsed as a **date** (e.g. `YYYY-MM-DD`). If parsing succeeds, that date is used directly as **pregnancy start** (`extrapolated_preg_start`), and the record is tagged **date_type = "LMP"** and contributes to ESD. Records can come from observation or measurement; the code uses the same date parsing for both.

- **estDeliveryConceptIds**  
  **EDD-type concepts.** The record’s value is parsed as a **date**. If parsing succeeds, **pregnancy start = parsed_date − 280 days** (40 weeks before EDD), and the record is tagged **date_type = "EDD"**. Again, records can be in observation or measurement.

- **gestAtBirthConceptIds**  
  **Length of gestation at birth.** The record’s value is interpreted as **gestational weeks** (must be 1–44). If so, `extrapolated_preg_start = record_date - (value * 7)` and the concept is treated like a GW concept (eligible for **gt_type = "GW"**). Concept IDs 4260747 and 46234792 are handled explicitly in code; others in this category can be added here and in the GW/gest-at-birth logic if desired.

- **measurementConceptIds** / **observationConceptIds**  
  General measurement/observation concepts. They are **pulled** with the rest of ESD concepts. They only **contribute** to ESD if they:
  - Are also in **gestational_age_concepts.csv**, or have **is_gw_concept = TRUE** (see below), or  
  - Have a concept name containing **"gestation period"** or **"gestational age"** and a numeric value in 1–44 (GW), or  
  - Are in **gestAtBirthConceptIds** with value 1–44.  

  So only include concepts here that you intend to use for timing (e.g. gestational age, LMP, EDD, or gestation-at-birth). Non-timing concepts (e.g. “Number of fetuses”) will be pulled but never affect inferred start dates.

**Optional column: `is_gw_concept`**  
To use a **single ESD concept file** and avoid maintaining **ESD_concepts2.xlsx**, add a column **`is_gw_concept`** (logical or 1/0) to **ESD_concepts.xlsx**. Set it to **TRUE** (or 1) for every concept that should be assigned **gt_type = "GW"** when they have a valid gestation-week value (e.g. 3048230, 3002209, 3012266, 3050433). If this column exists and has at least one TRUE, the package uses it and does **not** load **ESD_concepts2.xlsx**. If the column is missing or has no TRUE values, the package falls back to **ESD_concepts2.xlsx** for the GW concept list (legacy behaviour).

**Summary:** **ESD_concepts.xlsx** defines *which* concepts are in scope for ESD; **esd_category** defines *how* each is used (LMP date, EDD date, GW weeks, or gestation-at-birth weeks). Records can be in measurement or observation (or condition/procedure) regardless of category.


## Export and reporting

### `check_concepts.csv`

**Purpose:** List of concept IDs used for a **concept-timing check** in export/reporting (e.g. which of these concepts occur within each episode and whether they fall within expected month windows).

**How it’s used:** **exportPregnancies** (`exportConceptTimingCheck`): Reads this file and uses `concept_id` to filter condition, procedure, and observation records that fall within each exported episode’s time window. The result is used for summaries (e.g. min/month/max_month, span, midpoint, whether the concept fell within the expected span). Columns typically include at least `concept_id` and may include concept_name, min_month, max_month, span, midpoint, table (domain).

---

### `delivery_mode/` (JSON concept sets)

**Purpose:** OMOP concept set definitions for **delivery mode** (e.g. vaginal, cesarean). Used to add delivery-mode flags/counts to the final episode table.

**How it’s used:** **ESD** (delivery-mode step): The code imports the concept set from `concepts/delivery_mode` and uses it with `PatientProfiles::addConceptIntersectFlag` and `addConceptIntersectCount` around the episode end date to flag or count delivery-mode concepts per episode. File names in this folder are typically like `3861-cesarean.json`, `3862-vaginal.json`; the numeric prefix is stripped for the column names (e.g. `cesarean`, `vaginal`).

---

## Quick reference

| File                         | Used by | Role |
|-----------------------------|--------|------|
| **HIP_concepts_reviewed17022026.xlsx** | HIP    | Pregnancy-related concepts and outcome category; optional gest_value for gestation weeks. Filtered at load to outcome + gest_value + gestational_age_concepts. |
| **gestational_age_concepts.csv** | HIP, ESD | Concept IDs treated as GA-in-weeks (value 1–44) for gestation episodes (HIP) and GW timing (ESD). |
| **Matcho_outcome_limits.xlsx**   | HIP    | Min days between outcome events for episode logic. |
| **Matcho_term_durations.xlsx**  | HIP, ESD | Min/max term (days) per outcome category for start-date bounds. |
| **PPS_concepts_reviewed1702026.xlsx** | PPS, ESD | PPS episode concepts and GR3m windows (min/max month). |
| **ESD_concepts.xlsx**       | ESD    | All ESD timing concept IDs; esd_category controls LMP/EDD/GW/gest-at-birth handling. |
| **check_concepts.csv**      | Export | Concept IDs for concept-timing check in export. |
| **delivery_mode/*.json**    | ESD    | Delivery-mode concept sets for episode-level flags/counts. |
