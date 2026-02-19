# Database test setup: env, Eunomia data, and connection helpers.
# testthat loads helper-*.R automatically.

library(testthat)

# Read env var from ~/.Renviron if unset (e.g. R CMD check doesn't load .Renviron).
# Returns value or NULL if not found / invalid.
# read_renviron_var <- function(var_name, require_dir_exists = FALSE) {
#   if (nzchar(Sys.getenv(var_name))) return(NULL)
#   renv_path <- path.expand("~/.Renviron")
#   if (!file.exists(renv_path)) return(NULL)
#   pattern <- paste0("^", var_name, "\\s*=")
#   for (line in readLines(renv_path, warn = FALSE)) {
#     if (grepl(pattern, line)) {
#       value <- sub(pattern, "", line)
#       value <- trimws(value)
#       value <- gsub("^['\"]|['\"]$", "", value)
#       if (!nzchar(value)) return(NULL)
#       if (require_dir_exists && !dir.exists(value)) return(NULL)
#       return(value)
#     }
#   }
#   NULL
# }

prefix <- function() {
  paste0("temp_", gsub("-", "", uuid::UUIDgenerate()))
}

get_connection <- function(dbms) {

  if (dbms == "postgres") {
    dbname <- Sys.getenv("CDM5_POSTGRESQL_DBNAME")
    host <- Sys.getenv("CDM5_POSTGRESQL_HOST")
    user <- Sys.getenv("CDM5_POSTGRESQL_USER")
    password <- Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
    port <- as.integer(Sys.getenv("CDM5_POSTGRESQL_PORT", "5432"))
    return(DBI::dbConnect(RPostgres::Postgres(),
                          dbname = dbname,
                          host = host,
                          port = port,
                          user = user,
                          password = password))
  }

  if (dbms == "snowflake" && Sys.getenv("SNOWFLAKE_USER") != "") {

    return(DBI::dbConnect(odbc::odbc(),
                          SERVER = Sys.getenv("SNOWFLAKE_SERVER"),
                          UID = Sys.getenv("SNOWFLAKE_USER"),
                          PWD = Sys.getenv("SNOWFLAKE_PASSWORD"),
                          DATABASE = "SCRATCH",
                          WAREHOUSE = Sys.getenv("SNOWFLAKE_WAREHOUSE"),
                          DRIVER = Sys.getenv("SNOWFLAKE_DRIVER")))
  }

  if (dbms == "spark" && Sys.getenv("DATABRICKS_HTTPPATH") != "") {
    message("connecting to databricks")
    con <- DBI::dbConnect(
      odbc::databricks(),
      httpPath = Sys.getenv("DATABRICKS_HTTPPATH"),
      useNativeQuery = FALSE,
      bigint = "numeric"
    )
    return(con)
  }

  if (dbms == "sqlserver" && Sys.getenv("CDM5_SQL_SERVER_SERVER") != "") {
    message("connecting to sql server")

    con <- DBI::dbConnect(odbc::odbc(),
                          Driver   = Sys.getenv("SQL_SERVER_DRIVER"),
                          Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                          Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
                          UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
                          PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                          TrustServerCertificate="yes",
                          Port     = Sys.getenv("CDM5_SQL_SERVER_PORT"))

    return(con)
  }


  rlang::abort("Could not create connection. Are some environment variables missing?")
}


