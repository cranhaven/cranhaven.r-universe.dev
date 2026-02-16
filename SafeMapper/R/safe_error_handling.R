# =============================================================================
# Safe Error Handling Functions - Wrappers for purrr error handling
# =============================================================================

#' Safe Safely - Wrap Function to Capture Errors
#'
#' Drop-in replacement for purrr::safely that captures errors and returns
#' them in a structured format.
#'
#' @param .f A function to wrap for safe execution.
#' @param otherwise Default return value when an error occurs.
#' @param quiet Logical. Hide errors from console if TRUE.
#'
#' @return A function that returns a list with 'result' and 'error' components.
#'
#' @examples
#' safe_log <- s_safely(log)
#' safe_log(10) # Returns list(result = 2.30, error = NULL)
#' safe_log("a") # Returns list(result = NULL, error = <error>)
#'
#' @export
s_safely <- function(.f, otherwise = NULL, quiet = TRUE) {
  purrr::safely(.f, otherwise = otherwise, quiet = quiet)
}

#' Safe Possibly - Wrap Function to Return Default on Error
#'
#' Drop-in replacement for purrr::possibly that returns a default value
#' when an error occurs instead of throwing the error.
#'
#' @param .f A function to wrap for safe execution.
#' @param otherwise Default return value when an error occurs.
#' @param quiet Logical. Hide errors from console if TRUE.
#'
#' @return A function that returns the result or the default value.
#'
#' @examples
#' safe_log <- s_possibly(log, otherwise = NA)
#' safe_log(10) # Returns 2.30
#' safe_log("a") # Returns NA
#'
#' @export
s_possibly <- function(.f, otherwise, quiet = TRUE) {
  purrr::possibly(.f, otherwise = otherwise, quiet = quiet)
}

#' Safe Quietly - Wrap Function to Capture Side Effects
#'
#' Drop-in replacement for purrr::quietly that captures all side effects
#' (output, messages, warnings) along with the result.
#'
#' @param .f A function to wrap for quiet execution.
#'
#' @return A function that returns a list with 'result', 'output', 'warnings', and 'messages'.
#'
#' @examples
#' quiet_summary <- s_quietly(summary)
#' result <- quiet_summary(cars)
#' # result$result contains the summary
#' # result$output contains any printed output
#'
#' @export
s_quietly <- function(.f) {
  purrr::quietly(.f)
}
