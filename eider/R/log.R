#' Log a message showing the current execution context
#'
#' @param context A character vector
#' @param severity A string in c("fatal", "error", "warn", "info", "debug",
#' "trace")
#'
#' @returns NULL
#' @noRd
log_context <- function(context, severity, message) {
  logging_function <- switch(severity,
    "fatal" = log_fatal,
    "error" = log_error,
    "warn" = log_warn,
    "info" = log_info,
    "debug" = log_debug,
    "trace" = log_trace
  )
  logging_function(paste0("context: ", context_message(context, message)))
}

#' Concatenate context into string and append a message
#'
#' @param context A character vector
#' @param message A single string
#'
#' @returns result A single string
#' @noRd
context_message <- function(context, message) {
  context_string <- stringr::str_c(context, collapse = " > ")
  if (message == "") {
    context_string
  } else {
    stringr::str_c(context_string, ": ", message)
  }
}

trace_context <- function(context, message = "") {
  log_context(
    context = context,
    severity = "trace",
    message = message
  )
}

debug_context <- function(context, message = "") {
  log_context(
    context = context,
    severity = "debug",
    message = message
  )
}

info_context <- function(context, message = "") {
  log_context(
    context = context,
    severity = "info",
    message = message
  )
}

warn_context <- function(context, message = "") {
  log_context(
    context = context,
    severity = "warn",
    message = message
  )
}

error_context <- function(context, message = "") {
  log_context(
    context = context,
    severity = "error",
    message = message
  )
}

fatal_context <- function(context, message = "") {
  log_context(
    context = context,
    severity = "fatal",
    message = message
  )
}

#' On top of logging a FATAL message, stop execution entirely, and print a
#' message showing the current execution context
#' @noRd
stop_context <- function(context, message = "") {
  ctx_str <- context %>%
    lapply(function(x) paste0(" > ", x)) %>%
    stringr::str_c(collapse = "\n")

  stop(message, "\nContext:\n", ctx_str, call. = FALSE)
}
