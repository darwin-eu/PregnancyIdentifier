# config.R
# Central configuration: env vars and defaults (hecate, llm, app).
# Do not source other app R files from here.

#' Application configuration
#'
#' Returns configuration read from environment variables (or defaults).
#' Use this instead of scattering \code{Sys.getenv()} across the codebase.
#'
#' @return List with elements \code{hecate} (base_url, timeout_ms, api_key),
#'   \code{llm} (system_prompt_paths), \code{app} (undo_stack_size, debug).
#' @export
app_config <- function() {
  list(
    hecate = list(
      base_url = Sys.getenv("HECATE_BASE_URL", "https://hecate.pantheon-hds.com/api"),
      timeout_ms = as.integer(Sys.getenv("HECATE_TIMEOUT_MS", "10000")),
      api_key = Sys.getenv("HECATE_API_KEY", "")
    ),
    llm = list(
      system_prompt_paths = c(
        "system_prompt.txt",
        file.path(getwd(), "system_prompt.txt"),
        file.path(dirname(getwd()), "system_prompt.txt")
      )
    ),
    app = list(
      undo_stack_size = 50L,
      debug = isTRUE(as.logical(Sys.getenv("APP_DEBUG", "FALSE")))
    )
  )
}
