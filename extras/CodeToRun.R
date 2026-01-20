library(dplyr)
library(PregnancyIdentifier)

cdmConnection <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "...",
  host = "...",
  user = "...",
  password = "..."
)

cdm <- CDMConnector::cdmFromCon(
  con = cdmConnection,
  cdmSchema = "...",
  writeSchema = "..."
)

outputDir <- "./dev/output/"

runPregnancyIdentifier(cdm = cdm, outputDir = outputDir, minCellCount = 5L)

