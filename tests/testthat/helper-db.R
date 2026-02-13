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

  rlang::abort("Could not create connection. Are some environment variables missing?")
}

get_cdm_schema <- function(dbms) {
  switch(dbms,
         "postgres" = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
         "snowflake" = strsplit(Sys.getenv("SNOWFLAKE_CDM_SCHEMA"), "\\.")[[1]],
         "spark" = Sys.getenv("DATABRICKS_CDM_SCHEMA"),
         ""
  )
}

get_write_schema <- function(dbms, prefix = paste0("temp", (floor(as.numeric(Sys.time()) * 100) %% 100000L + Sys.getpid()) %% 100000L, "_")) {
  s <- switch(dbms,
              "postgres" = Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
              "snowflake" = strsplit(Sys.getenv("SNOWFLAKE_SCRATCH_SCHEMA"), "\\.")[[1]],
              "spark" = Sys.getenv("DATABRICKS_SCRATCH_SCHEMA"),
              ""
  )

  if (!is.null(prefix)) {
    if (length(s) == 1) {
      s <- c(schema = s[1], prefix = prefix)
    } else {
      s <- c(catalog = s[1], schema = s[2], prefix = prefix)
    }
  }

  return(s)
}

disconnect <- function(con) {
  if (is.null(con)) return(invisible(NULL))
  DBI::dbDisconnect(con)
}

