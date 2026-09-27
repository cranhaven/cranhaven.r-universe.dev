#' Collect Packages From an Expression
#'
#' @description
#' Walk an expression and collect the packages of every call found within it,
#' without reproducing any of the code. This is a deliberately cheap alternative
#' to [repro_chunk()] for the cases that only need the package list, such as
#' [reprex_packages()] and [reprex_lockfile()].
#'
#' ## Why Not Reproduce the Code
#' Reproducing a chunk substitutes reactive and input values into the
#' expression, which means constructing every module variable as code via
#' `constructive::construct`. For a module holding a data frame that dominates
#' the run time, and all of it is discarded when only the packages are wanted.
#'
#' ## Branches Are Not Evaluated
#' Every branch of an `if` or `switch` is walked, rather than evaluating the
#' condition and following only the branch that would be taken. A lockfile is
#' safer for containing a package that turns out not to be needed than for
#' missing one, and not evaluating keeps the walk both faster and independent of
#' the current input values.
#'
#' @param expr An expression to walk
#' @param env The environment the expression belongs to, used to identify calls
#' to other reactives
#' @param seen Names of reactives already walked, preventing a reactive that is
#' referenced more than once from being walked repeatedly
#' @param packages Packages collected so far
#'
#' @returns
#' A character vector of unique package names, excluding base packages.
#'
#' @keywords internal
walk_packages <- function(expr, env, seen = character(), packages = character()) {
  if (!rlang::is_call(expr)) {
    return(packages)
  }

  # A namespaced function passed as a value, such as `do.call(purrr::map, ...)`,
  # rather than called directly.
  if (rlang::is_call(expr, "::")) {
    return(union(packages, as.character(expr[[2L]])))
  }

  call_name <- rlang::call_name(expr)

  # Shiny calls that are stripped when reproducing code contribute nothing
  if (!is.null(call_name) && call_name %in% IGNORED_SHINY_CALLS) {
    return(packages)
  }

  reactive_env <- call_reactive_env(expr, env)

  if (!is.null(reactive_env)) {
    reactive <- reactive_env[[call_name]]

    if (inherits(reactive, "reactiveExpr") && !call_name %in% seen) {
      inner <- reactive_expression(reactive)
      packages <- walk_packages(inner$body, inner$env, c(seen, call_name), packages)
    }

    return(packages)
  }

  packages <- union(packages, get_pkg_name(expr))

  # Omitted arguments, such as the empty index in `iris[cond, ]` cannot be walked
  arguments <- purrr::discard(as.list(expr)[-1L], rlang::is_missing)

  for (arg in arguments) {
    packages <- walk_packages(arg, env, seen, packages)
  }

  packages
}

#' Environment of a Called Reactive
#'
#' @description
#' Find the environment holding the reactive a call refers to, checking the
#' expression's own environment before the environment it was passed in from.
#'
#' @param expr An expression to check
#' @param env The environment the expression belongs to
#'
#' @returns
#' The environment holding the reactive, or `NULL` if the call is not to a
#' reactive.
#'
#' @keywords internal
call_reactive_env <- function(expr, env) {
  if (is_reactive_call(expr, env)) {
    env
  } else if (is_reactive_call(expr, parent.env(env))) {
    parent.env(env)
  }
}
