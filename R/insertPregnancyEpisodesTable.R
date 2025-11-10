insertPregnancyEpisodesTable <- function(cdm, path, tableName, schema = NULL, overwrite = FALSE) {
  # Do checks on result

  pet <- readRDS(path)
  if (!is.null(schema)) {
    con <- attr(cdm, "dbcon")
    DBI::dbWriteTable(
      conn = con,
      name = sprintf("%s.%s", schema, tableName),
      value = pet,
      overwrite = overwrite
    )
  } else {
    CDMConnector::insertTable(
      cdm = cdm,
      name = tableName,
      table = pet,
      overwrite = overwrite
    )
  }
}

str(pet)

insertPregnancyEpisodesTable(cdm, "./dev/output/result.rds", tableName = "PET")
