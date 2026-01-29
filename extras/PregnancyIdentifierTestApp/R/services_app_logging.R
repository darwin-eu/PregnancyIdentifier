# services_app_logging.R
# App logging: console + optional file (outputDir/log.txt) + optional in-app buffer.

#' Log a message to console, optionally to outputDir/log.txt, and optionally to an in-app buffer.
#'
#' @param msg Character. Message to log.
#' @param outputDir Character or NULL. If non-null and log.txt exists or can be created, append with timestamp.
#' @param level One of "INFO", "WARN", "ERROR".
#' @param append_cb Optional function(line) called with one string (timestamp + level + msg) for UI buffer.
#' @noRd
log_app <- function(msg,
                    outputDir = NULL,
                    level = c("INFO", "WARN", "ERROR"),
                    append_cb = NULL) {
  level <- match.arg(level)
  line <- sprintf("[%s] %s", level, msg)
  message(line)

  if (length(outputDir) > 0 && !is.null(outputDir) && !is.na(outputDir) && outputDir != "") {
    log_path <- file.path(outputDir, "log.txt")
    tryCatch(
      {
        ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        cat(sprintf("%s %s\n", ts, line), file = log_path, append = TRUE)
      },
      error = function(e) NULL
    )
  }

  if (is.function(append_cb)) {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    full_line <- sprintf("%s %s", ts, line)
    tryCatch(append_cb(full_line), error = function(e) NULL)
  }
  invisible(NULL)
}
