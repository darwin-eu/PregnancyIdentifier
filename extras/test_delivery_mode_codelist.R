# Test that delivery_mode concept set expands to a codelist correctly via
# omopgenerics::validateConceptSetArgument (replacement for deprecated
# CodelistGenerator::asCodelist). Run from package root with:
#   Rscript extras/test_delivery_mode_codelist.R

library(PregnancyIdentifier)

# Mock CDM with vocabulary so concept set expression can be expanded
cdm <- mockPregnancyCdm(fullVocab = TRUE)

# Same logic as addDeliveryMode(): import concept set, expand to codelist with CDM
conceptSet <- omopgenerics::importConceptSetExpression(
  path = system.file(package = "PregnancyIdentifier", "concepts/delivery_mode")
)
conceptSet <- omopgenerics::validateConceptSetArgument(conceptSet, cdm = cdm)

# Expect two delivery-mode groups (cesarean and vaginal from inst/concepts/delivery_mode/)
stopifnot(
  "delivery_mode codelist should have 2 groups" = length(conceptSet) == 2L
)
stopifnot(
  "each group should have at least one concept ID" = all(lengths(conceptSet) >= 1L)
)
# Names are from JSON filenames (e.g. "3861-cesarean", "3862-vaginal")
nms <- names(conceptSet)
stopifnot(
  "codelist names should include cesarean and vaginal" =
    any(grepl("cesarean", nms, ignore.case = TRUE)) &&
    any(grepl("vaginal", nms, ignore.case = TRUE))
)

message("OK: delivery_mode codelist has 2 groups (cesarean, vaginal) with concept IDs.")

cleanupCdmDb(cdm)
