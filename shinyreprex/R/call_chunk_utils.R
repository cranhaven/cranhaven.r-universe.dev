#' Call Checks
#'
#' @description
#' A set of helper functions that determine what type of call is being made within
#' an expression.
#'
#' `is_reactive_call` checks whether or not the call is evaluating a
#'  `shiny::reactive` variable.
#'
#' @param x An R call object
#' @param env The environment the call is being made, by default it is the environment
#' calling the check, but is likely the environment the call is being made i.e. the
#' reactive expression.
#'
#' @returns
#' A boolean value determining whether or not the call check has passed.
#'
#' @keywords internal
#' @rdname call_chunk_checks
is_reactive_call <- function(x, env = rlang::caller_env()) {
  call_name <- if (rlang::is_call(x)) rlang::call_name(x)

  !is.null(call_name) &&
    length(rlang::call_args(x)) == 0 &&
    call_name %in% names(env) &&
    inherits(env[[call_name]], "reactive")
}

#' @description
#' `is_reactive_val_call` checks whether or not the call is evaluating a
#' `shiny::reactiveVal` variable.
#'
#' @rdname call_chunk_checks
is_reactive_val_call <- function(x, env = rlang::caller_env()) {
  is_reactive_call(x = x, env = env) &&
    inherits(env[[rlang::call_name(x)]], "reactiveVal")
}

#' @description
#' `is_reactive_val_setter_call` checks whether or not the call is setting the value
#' of a `shiny::reactiveVal` variable.
#'
#' @rdname call_chunk_checks
is_reactive_val_setter_call <- function(x, env = rlang::caller_env()) {
  call_name <- if (rlang::is_call(x)) rlang::call_name(x)

  !is.null(call_name) &&
    length(rlang::call_args(x)) == 1L &&
    call_name %in% names(env) &&
    inherits(env[[call_name]], "reactiveVal")
}

#' @description
#' `is_reactive_values_call` checks whether or not the call is evaluating an item
#' within a `shiny::reactiveValues` variable.
#'
#' @rdname call_chunk_checks
is_reactive_values_call <- function(x, env = rlang::caller_env()) {
  (rlang::is_call(x, "$") || rlang::is_call(x, "[[")) &&
    tryCatch(inherits(get(rlang::call_args(x)[[1]], envir = env), "reactivevalues"), error = \(e) FALSE) &&
    as.character(rlang::call_args(x)[[1]]) != "input"
}

#' @description
#' `is_any_reactive_call` checks whether or not the call points to evaluating a
#' `reactive`, `reactiveVal` or `reactiveValues`.
#'
#' @rdname call_chunk_checks
is_any_reactive_call <- function(x, env = rlang::caller_env()) {
  is_reactive_call(x, env) || is_reactive_call(x, parent.env(env)) || is_reactive_values_call(x, env)
}

#' @description
#' `is_variable_call` checks whether or not the call point to a variable that is defined
#' within the given module.
#'
#' @param existing_vars A character vector of variable definitions that exist in the `Repro` object
#'
#' @rdname call_chunk_checks
is_variable_call <- function(x, existing_vars = NULL, env = rlang::caller_env()) {
  is.name(x) &&
    (as.character(x) %in% names(env) || as.character(x) %in% names(parent.env(env))) &&
    !as.character(x) %in% existing_vars
}

#' @description
#' `is_input_call` checks whether or not the call points to evaluate an input value.
#'
#' @rdname call_chunk_checks
is_input_call <- function(x) {
  (rlang::is_call(x, "$") || rlang::is_call(x, "[[")) &&
    startsWith(as.character(x)[[2]], "input")
}

#' @description
#' `is_session_user_data` checks whether or not the call points to evaluate an object
#' within `session$userData`
#'
#' @rdname call_chunk_checks
is_session_user_data <- function(x) {
  (rlang::is_call(x, "$") || rlang::is_call(x, "[[")) &&
    startsWith(as.character(x)[[2]], "session$userData")
}

#' Assign Reactive Call
#'
#' @noRd
assign_reactive_call <- function(x, repro_call) {
  if (length(repro_call@code) == 1) {
    rlang::call2("<-", as.symbol(rlang::call_name(x)), !!!repro_call@code)
  } else {
    rlang::call2(
      "<-",
      as.symbol(rlang::call_name(x)),
      rlang::call2("local", rlang::call2("{", !!!repro_call@code))
    )
  }
}

#' Get Call Package Name
#'
#' @noRd
get_pkg_name <- function(x, base_pkgs = BASE_PKGS) {
  if (rlang::is_call(x[[1]], "::")) return(as.character(x[[1]][[2]]))

  pkg_name <- tryCatch(
    x |> rlang::call_name() |> get() |> environment() |> getNamespaceName() |> unname(),
    error = \(e) NULL
  )

  if (is.null(pkg_name) || pkg_name %in% base_pkgs) {
    NULL
  } else {
    pkg_name
  }
}

BASE_PKGS <- c("base", "stats", "graphics", "utils", "grDevices", "datasets", "methods")

#' Construct Reactive Value
#'
#' @noRd
construct_reactive <- function(x, env = rlang::caller_env()) {
  x |>
    rlang::eval_bare(env = env) |>
    constructive::construct(one_liner = TRUE) |>
    _[["code"]]
}
