#' Generic Method for Reproducing Code
#'
#' @description
#' Standard response is to return the called object
#'
#' Only the branch matching the current value of the condition is included in the
#' output. A braced branch is unwrapped into its component expressions, whereas an
#' unbraced branch is walked as a single expression.
#'
#' An `if` without an `else` has nothing to reproduce when its condition is not met,
#' in which case an empty `Repro` object is returned.
#'
#' @noRd
S7::method(repro_call_chunk, class_call_if) <- function(x, repro_code = Repro(), env = rlang::caller_env()) {
  if_args <- rlang::call_args(x)
  check <- eval(if_args[[1]], envir = env)
  has_else <- length(if_args) > 2L

  # No branch is taken by an `if` without an `else` when the condition is not met
  if (!check && !has_else) return(repro_code)

  # Adapts for if ... else if ... else
  if (!check && rlang::is_call(if_args[[3]], "if")) {
    return(repro_chunk(if_args[[3]], env = env))
  }

  branch <- if (check) if_args[[2]] else if_args[[3]]
  branch_exprs <- if (rlang::is_call(branch, "{")) as.list(branch)[-1L] else list(branch)

  check_calls <- purrr::map(branch_exprs, repro_chunk, env = env)
  repro_code@packages <- purrr::map(check_calls, "packages") |> unlist()
  repro_code@prerequisites <- purrr::map(check_calls, "prerequisites") |>
    purrr::discard(identical, list()) |>
    unlist(recursive = FALSE)
  eval_call <- purrr::map(check_calls, "code") |> unlist(recursive = FALSE)

  repro_code@packages <- get_pkg_name(x)
  repro_code@code <- eval_call
  repro_code
}
