# Create a mock pregnancy cdm for examples and testing

Create a mock pregnancy cdm for examples and testing

## Usage

``` r
mockPregnancyCdm(fullVocab = TRUE)
```

## Arguments

- fullVocab:

  If \`TRUE\` (default), the CDM includes the full vocabulary tables. If
  \`FALSE\`, concept, concept_relationship, concept_ancestor, and
  concept_synonym are subset to only concept IDs that appear in the data
  (from \[CDMConnector::cdmFlatten()\] `observation_concept_id`), for
  lighter testing on different database systems.

## Value

A cdm reference with some example pregnancy data in it

## Examples

``` r
if (FALSE) { # \dontrun{
cdm <- mockPregnancyCdm()
cdm_small_vocab <- mockPregnancyCdm(fullVocab = FALSE)
} # }
```
