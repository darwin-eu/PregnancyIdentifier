# Merge HIP and PPS episodes into a unified HIPPS episode table

Reads previously-saved HIP and PPS outputs from \`outputDir\`, merges
HIP and PPS episodes based on temporal overlap, resolves many-to-many
overlaps by selecting the best-matching pairs, and returns a single
per-person episode table with standardized columns used downstream by
ESD and reporting.

## Usage

``` r
mergeHipps(outputDir, logger)
```

## Arguments

- outputDir:

  (\`character(1)\`) Directory containing intermediate RDS artifacts:
  \`pps_episodes.rds\`, \`hip_episodes.rds\`

- logger:

  (\`logger\`) log4r logger created with \`makeLogger()\`.

## Value

Invisibly returns \`NULL\` and writes \`hipps_episodes.rds\` to
\`outputDir\`.

## Details

Key steps:

- Load HIP episodes and PPS episodes from \`outputDir\`

- Merge HIP + PPS episodes by overlap; flag episodes involved in
  many-to-many overlaps

- Resolve many-to-many overlaps by selecting best-matching pairs (retain
  best matches by end-date proximity and episode plausibility)

- Add standardized columns and per-person episode ordering; write
  \`hipps_episodes.rds\`
