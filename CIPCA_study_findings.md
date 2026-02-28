# CIPCA Study Findings & TODO List

## Study Context

- **Database:** CIPCA (City App database, Spain)
- **Package:** PregnancyIdentifier v3

---

## Key Findings

### 1. Long Gestational Ages (>308 days)
- ~10% of pregnancy episodes have gestational ages longer than 308 days.
- Cause is unknown.

### 2. Abortion / Spontaneous Abortion: No Gestational Time Distribution
- AB and SA episodes all have the exact same gestational time (~168 days for AB).
- Appears to be back-calculated from a single date, producing a fixed duration rather than a distribution.
- 168 days seems too long for many abortions, which typically occur earlier in pregnancy.
- Possible cause: only one code per episode, so ESD always forces the start date to a fixed offset before the abortion code date, defaulting to the maximum possible length.

### 3. Mode of Delivery Not Detected
- Delivery mode data was not picked up in the Shiny app.
- Could be a Shiny app bug or a study code issue — needs investigation.

### 4. PET Matching Rate is Low
- Only ~380 episodes matched, roughly 60% match rate between PET (Pregnancy Extension Table) and the algorithm output.
- High-risk pregnancies appeared more likely to be matched by the algorithm.
- The unmatched episodes need characterization.

---

## TODO List

### PregnancyIdentifier Package

- [ ] **Investigate long episodes (>308 days):** Have Claude investigate what causes gestational times longer than 308 days. Check algorithm logic for edge cases that could produce implausibly long durations.
- [ ] **Characterize long episodes:** Analyze whether episodes >308 days cluster by time period, outcome category, number of codes, or other features.
- [ ] **Investigate AB/SA fixed gestational time:** Determine why abortion and spontaneous abortion episodes all have the same gestational duration (~168 days for AB). Check whether ESD always back-calculates start date using the maximum possible pregnancy length when only one code is present.
- [ ] **Summarize AB/SA codes:** Look at the concept codes driving AB/SA episodes — are they always a single code? Does the algorithm always default to max duration when back-calculating?
- [ ] **Investigate mode of delivery detection:** Determine why delivery mode is not being picked up. Check whether this is a Shiny app bug (data loading/display) or a study code issue (data not being exported).

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
