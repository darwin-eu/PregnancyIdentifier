test_that("test gestational episodes", {
  testPath <- system.file("testCases", mustWork = TRUE, package = "PregnancyIdentifier")
  # Read patients from JSON
  cdm <- TestGenerator::patientsCDM(
    pathJson = testPath,
    testName = "testData"
  )

  # start run analysis
  outputDir <- file.path(tempdir(), "Results")
  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = T)
  }
  logger <- PregnancyIdentifier:::makeLogger(outputDir)

  cdm <- initPregnancies(cdm)

  # insert gestation_visits data
  gestation_visits <- utils::read.csv("gestation_visits.csv", sep = ";") %>%
    dplyr::mutate(visit_date = as.Date(visit_date),
                  date_of_birth = as.Date(date_of_birth))
  cdm <- omopgenerics::insertTable(cdm,
                                   name = "gestation_visits_df",
                                   table = gestation_visits,
                                   overwrite = TRUE,
                                   temporary = FALSE)

  cdm <- PregnancyIdentifier:::gestationEpisodes(cdm, minDays = 70, bufferDays = 28)
  episodes <- dplyr::collect(cdm$gestation_episodes_df)

  testthat::expect_equal(length(unique(episodes$episode)), 3)

  unlink(outputDir, recursive = TRUE)
})
