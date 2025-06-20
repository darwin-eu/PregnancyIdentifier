runHipps <- function(cdm) {
  # Original code works with DBI in database. We have 2 options:
  #   1. Run everything in the database, using CDMConnector (this locks us in to using the CDMConnector paradigm)
  #   2. Select subjects of interrest filtering early on characteristics (From HIP.R):
  #      # 45878463: Female
  #      # 46273637: Intersex
  #      # 45880669: Male
  #      # 1177221: I prefer not to answer
  #      # 903096: Skip
  #      # 4124462: None
  #      sex_at_birth_concept_id != 45880669 (Male)
  #
  #      Pull filtered relevant tables and columns into Andromeda, relatively
  #      easy to do in SQL too, for DatabaseConnector support.
}
