# HIP algorithm

## Overview

The **HIP** (outcome-anchored) algorithm identifies pregnancy episodes
by combining **high-specificity pregnancy outcomes** (e.g., live birth,
stillbirth, abortion) with **gestational-age evidence** (weeks of
gestation) when available. It produces one record per inferred
pregnancy, including an **episode end date** (`hip_pregnancy_end`), an
**estimated start date** (`hip_pregnancy_start`), and a **pregnancy
category**.

HIP is run via
[`runHip()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/runHip.md),
which assumes
[`initPregnancies()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/initPregnancies.md)
has already been run and that the CDM contains `preg_hip_records` and
`preg_hip_concepts`. HIP writes `hip_episodes.rds` to `outputDir` and
adds the internal table `cdm$preg_hip_episodes` during execution.

## Two-pass structure

HIP runs in two conceptual passes:

1.  **Outcome-first pass (high specificity):** Treat recorded pregnancy
    outcomes as the anchor (the pregnancy “end”), and infer plausible
    start dates using outcome-specific gestational duration ranges
    (Matcho term durations).
2.  **Gestation pass (higher sensitivity):** Group gestational-age
    records into pregnancy episodes for pregnancies that may not have a
    recorded outcome, then merge these episodes with outcome-based
    episodes and resolve conflicts.

The result is a single episode table that can include:

- Pregnancies with **outcome evidence** (outcome anchors the end date).
- Pregnancies with **gestation evidence only** (optionally included via
  `justGestation = TRUE`).
- Pregnancies where **both** sources reinforce one another.

## What HIP is trying to solve

Observational data commonly contains:

- Multiple outcome records for the *same* pregnancy (duplicate coding,
  follow-up visits).
- Outcome records close together that may represent the *same* episode
  or *distinct* pregnancies depending on spacing.
- Pregnancies with **no explicit outcome** but with gestational age
  documentation (ultrasound, prenatal visits).
- Conflicts between outcome timing and gestational timing (e.g.,
  gestational evidence implies an implausible duration).

HIP addresses this by:

- Enforcing **minimum spacing rules** between outcomes (Matcho outcome
  limits) so pregnancies are not over-counted.
- Inferring start dates using **outcome-specific term ranges** (min/max
  gestational days per category).
- Using **gestational-age trajectories** to detect pregnancies that
  outcomes miss and to attach gestation-only episodes when
  `justGestation` is TRUE.

## Main stages (internal logic)

1.  **Matcho outcome limits** — A table of minimum spacing (days)
    between consecutive outcome categories is inserted into the CDM and
    used to decide when two outcome visits belong to the same
    vs. different pregnancies.

2.  **Build final outcome visits** — For each outcome category group
    (AB/SA, DELIV, ECT, SB, LB), HIP builds candidate outcome visits
    from `preg_hip_records`, applies spacing rules, and produces a list
    of final visit tables (e.g. `finalLbVisits`, `finalSbVisits`).

3.  **Build outcome episodes** — These final visit tables are combined
    into a single outcome-episode table (`outcome_episodes_df`) with
    `person_id`, `outcome_category`, `outcome_date`, and `outcome_id`.
    Outcome categories are ordered (e.g. LB, SB, ECT, AB/SA, DELIV) and
    spacing between categories is enforced using Matcho limits.

4.  **Estimate outcome starts** — Each outcome episode is joined to
    Matcho term durations to get `min_term` and `max_term` (gestational
    days). From these, HIP derives `min_start_date` and `max_start_date`
    for the inferred pregnancy interval.

5.  **Build gestation episodes** — From `preg_hip_records`, HIP builds
    gestation-based episodes (grouped by person and visit/gestational
    evidence), computes max/min gest dates and gest weeks, and stores
    them in `gest_episodes_df`.

6.  **Merge outcome and gestation** — Outcome episodes (with start
    bounds) and gestation episodes are joined by person and overlapping
    intervals. For each person, episodes are classified as: (a) both
    outcome and gestation, (b) outcome only, or (c) gestation only (if
    `justGestation`). These are unioned into `merged_episodes_df`.

7.  **Clean and resolve overlaps** — Merged episodes are cleaned
    (e.g. reclassify implausible term durations to PREG, set
    removed_outcome), then overlapping PREG gestation episodes are
    resolved so each final episode has a single `final_start_date` and
    no duplicate overlap.

8.  **Attach gestation and length** — Final episodes are joined to
    gestation visit-level data to attach `gest_flag` and
    `episode_length`, producing `preg_hip_episodes`. This table is
    collected and saved as `hip_episodes.rds`.

## HIP outputs and categories

HIP episodes include:

- `person_id` — Unique person identifier.
- `hip_first_gest_date` — First gestation date in the episode (or NA if
  no gestational visit in the episode window).
- `hip_pregnancy_end` — Episode end date (outcome date or max gest
  date).
- `hip_pregnancy_start` — Inferred pregnancy start date.
- `hip_outcome_category` — One of: `LB`, `SB`, `AB`, `SA`, `DELIV`,
  `ECT`, `PREG`.
- `hip_gest_flag` — Whether gestational evidence supported the episode.
- `hip_episode_length` — Duration of the episode in days (from
  `hip_pregnancy_start` to `hip_pregnancy_end`).
- `hip_episode` — Within-person episode number.

Interpretation of categories:

- **LB** — Live birth outcome anchors the episode end.
- **SB** — Stillbirth.
- **AB** — Abortion.
- **SA** — Miscarriage (spontaneous abortion).
- **DELIV** — Delivery record present but not classified as LB/SB.
- **ECT** — Ectopic pregnancy.
- **PREG** — Pregnancy inferred from gestational evidence without a
  usable outcome anchor (or outcome evidence was deemed implausible).

## Running HIP alone

``` r
library(PregnancyIdentifier)
cdm <- mockPregnancyCdm()
logger <- makeLogger("hip_output")
cdm <- initPregnancies(cdm, startDate = as.Date("2000-01-01"), endDate = Sys.Date(), ageBounds = c(15L, 56L), logger = logger)

cdm <- runHip(
  cdm,
  outputDir  = "hip_output",
  startDate  = as.Date("2000-01-01"),
  endDate    = Sys.Date(),
  justGestation = TRUE,
  logger     = logger
)
# hip_episodes.rds is written to hip_output/
```

For the full pipeline (HIP → PPS → merge → ESD), use
[`runPregnancyIdentifier()`](https://darwin-eu-dev.github.io/PregnancyIdentifier/reference/runPregnancyIdentifier.md);
see the [pipeline
overview](https://darwin-eu-dev.github.io/PregnancyIdentifier/articles/algorithm.md).
