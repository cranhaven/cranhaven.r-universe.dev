# CLASSED CONDITIONS AND STRUCTURED ERROR HANDLERS
# Defines error conditions (simtab_error_*) and informative diagnostic abort helpers.

#' SimtablR condition classes
#'
#' SimtablR errors inherit from `simtab_error`. More specific classes identify
#' the failing surface so callers can assert on behavior without matching prose:
#' `simtab_error_input` for invalid user-supplied arguments at a public entry
#' point, `simtab_error_binding` for variable binding and role-selection
#' failures, `simtab_error_spec` for invalid SimtablR specifications,
#' `simtab_error_engine` for engine validation or compute failures,
#' `simtab_error_flag` for invalid table flags, `simtab_error_render` for
#' missing renderer methods, and `simtab_error_dependency` for a suggested
#' package that is needed but not installed. `simtab_error_export` identifies
#' unsafe paths, destination collisions, and failed file writes.
#' `simtab_error_model` identifies unsupported model access and invalid outcome
#' selectors on retained model evidence.
#'
#' Messages follow one house form: an `x` line stating the problem, an `i` line
#' giving the context that produced it, and a `v` line naming the next action.
#'
#' `{}` expressions in the message are evaluated in the calling function's
#' environment, so a caller can interpolate its own locals directly rather than
#' pre-building the string with `sprintf()`.
#'
#' @family errors
#' @return This documentation-only topic evaluates to `NULL`; condition helpers
#'   signal classed errors rather than returning values.
#' @examples
#' tryCatch(
#'   tb(epitabl, non_existent_var, diabetes),
#'   simtab_error = function(e) message("Caught simtab error")
#' )
#' @name simtab_errors
NULL

#########
# ABORT HELPERS BY FAILING SURFACE
# Classed wrappers around cli::cli_abort preserving error classification.

#' Base classed error abort helper
#' @keywords internal
#' @noRd
simtab_abort <- function(
  message,
  class,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  cli::cli_abort(
    message,
    class = unique(c(class, "simtab_error")),
    ...,
    call = call,
    .envir = .envir
  )
}

#' Signals renderer dispatch or formatting failure
#' @keywords internal
#' @noRd
simtab_abort_render <- function(
  message,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  simtab_abort(
    message,
    class = "simtab_error_render",
    ...,
    call = call,
    .envir = .envir
  )
}

#' Signals variable binding or role selection failure
#' @keywords internal
#' @noRd
simtab_abort_binding <- function(
  message,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  simtab_abort(
    message,
    class = "simtab_error_binding",
    ...,
    call = call,
    .envir = .envir
  )
}

#' Signals invalid specification structure or missing required slots
#' @keywords internal
#' @noRd
simtab_abort_spec <- function(
  message,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  simtab_abort(
    message,
    class = "simtab_error_spec",
    ...,
    call = call,
    .envir = .envir
  )
}

#' Signals computation engine resolution or execution failure
#' @keywords internal
#' @noRd
simtab_abort_engine <- function(
  message,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  simtab_abort(
    message,
    class = "simtab_error_engine",
    ...,
    call = call,
    .envir = .envir
  )
}

#' Signals unrecognised or misapplied formatting flag
#' @keywords internal
#' @noRd
simtab_abort_flag <- function(
  message,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  simtab_abort(
    message,
    class = "simtab_error_flag",
    ...,
    call = call,
    .envir = .envir
  )
}

#' Signals invalid user argument at a public entry point
#' @keywords internal
#' @noRd
simtab_abort_input <- function(
  message,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  simtab_abort(
    message,
    class = "simtab_error_input",
    ...,
    call = call,
    .envir = .envir
  )
}

#' Signals missing optional or suggested dependency
#' @keywords internal
#' @noRd
simtab_abort_dependency <- function(
  message,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  simtab_abort(
    message,
    class = "simtab_error_dependency",
    ...,
    call = call,
    .envir = .envir
  )
}

#' Signals unsafe file path, collision, or export write failure
#' @keywords internal
#' @noRd
simtab_abort_export <- function(
  message,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  simtab_abort(
    message,
    class = "simtab_error_export",
    ...,
    call = call,
    .envir = .envir
  )
}

#' Signals unsupported model accessor or missing model evidence
#' @keywords internal
#' @noRd
simtab_abort_model <- function(
  message,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  simtab_abort(
    message,
    class = "simtab_error_model",
    ...,
    call = call,
    .envir = .envir
  )
}
