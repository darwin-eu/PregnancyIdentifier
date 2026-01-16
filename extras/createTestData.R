
# Test data from https://github.com/darwin-eu-dev/PregnancyIdentifier/issues/61#issuecomment-3675914672
# create the test json data
TestGenerator::readPatients.xl(
  here::here("extras/TestData_P4_C5_002.1.xlsx"),
  outputPath = here::here("inst/testCases"),
  testName = "TestData_P4_C5_002_1",
  extraTable = TRUE
)


cdm <- TestGenerator::patientsCDM(
  pathJson = system.file("testCases", package = "PregnancyIdentifier"),
  testName = 'TestData_P4_C5_002_1',
  cdmVersion = "5.3",
  cdmName = "TestData_P4_C5_002_1"
)

library(CDMConnector)
library(dplyr)
cdm <- cdmSubset(cdm, personId = 21L)

cdm$condition_occurrence |>
  select(1:4, condition_source_value)

outputFolder <- file.path(tempdir(), "testHipps")

debugonce(runHipps)
runHipps(cdm, outputFolder, continue = FALSE)
list.files(outputFolder)
df <- readRDS(file.path(outputFolder, "identified_pregancy_episodes.rds"))

printLong <- function(x) {
  x |>
    dplyr::collect() |>
    dplyr::mutate_all(as.character) |>
    tidyr::gather() |>
    print(n=1000)
}

library(dplyr)
df |>
  filter(person_id == 28) |>
  mutate_all(as.character) |>
  slice(2) |>
  tidyr::gather() |>
  print(n=100)





