#' Reactive Expression and Environment
#'
#' @description
#' Extract the unevaluated body of a reactive, along with the environment it was
#' assigned in.
#'
#' ## Environments
#' The `Observable` object attached to the given reactive is extracted. Within the
#' `Observable`, the `.origFunc` contains the environment that the reactive expression
#' was created - the parent environment being the module that the reactive is assigned
#' in. This allows the variables in the module to be found and set as pre-requisites
#' for the given reactive.
#'
#' If `bindCache` or `bindEvent` are used, then the environment found is the call within
#' the relevant function. To get to the module environment, we find that the `reactive`
#' is assigned as "`wrappedFunc`", so that is used to find the module environment.
#'
#' @param x A `shiny::reactive` object
#'
#' @returns
#' A list with two elements: `body`, the unevaluated body of the reactive, and
#' `env`, the module environment the reactive was assigned in.
#'
#' @keywords internal
reactive_expression <- function(x) {
  observer <- attr(x, "observable", exact = TRUE)
  module_env <- rlang::env_parent(env = environment(observer$.origFunc))
  inner_reactive <- observer$.origFunc

  # Accounts for bindEvent and bindCache
  while ("wrappedFunc" %in% names(attributes(module_env$valueFunc))) {
    inner_reactive <- attr(module_env$valueFunc, "wrappedFunc", exact = TRUE)
    module_env <- rlang::env_parent(env = environment(inner_reactive))
  }

  list(
    body = rlang::fn_body(inner_reactive),
    env = module_env
  )
}
