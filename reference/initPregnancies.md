# Create Initial Pregnancy Table

Get initial cohort based on HIP concepts for women who are within
specific age at the time of the pregnancy related record

## Usage

``` r
initPregnancies(
  cdm,
  startDate = as.Date("1900-01-01"),
  endDate = Sys.Date(),
  ageBounds = c(15L, 55L),
  logger,
  outputFolder = NULL
)
```

## Arguments

- cdm:

  A CDM reference

- startDate:

  Earliest date to look for pregnancies in the CDM

- endDate:

  Latest date to look for pregnancies in the CDM

- ageBounds:

  The upper and lower bounds for age at pregnancy end date represented
  using a length 2 integer vector. By default this will be c(15, 55) and
  will include anyone \>= 15 and \< 56.

- logger:

  A log4r logger object that can be created with \`makeLogger()\`

- outputFolder:

  Optional directory path. When provided, an `attrition.csv` file is
  created with initial record and person counts for `preg_hip_records`
  and `preg_pps_records`.

## Value

The input CDM with new tables added called preg_hip_records,
preg_pps_records, preg_hip_concepts, preg_pps_concepts.

## Examples

``` r
if (FALSE) { # \dontrun{
logger <- makeLogger(tempdir())
cdm <- mockPregnancyCdm()
cdm <- initPregnancies(cdm, logger = logger)
} # }
```
