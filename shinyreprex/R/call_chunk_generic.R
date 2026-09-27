#' Generic Method for Reproducing Code
#'
#' @description
#' Standard response is to return the called object
#'
#' @noRd
S7::method(repro_call_chunk, S7::class_any) <- function(x, repro_code = Repro(), env = rlang::caller_env()) {
  stopifnot("Object passed to repro_call_chunk must be a call" = is.call(x))

  x_args <- x |> unclass() |> rlang::call_args()

  repro_args <- lapply(x_args, \(y) repro_chunk(y, env = env))
  eval_args <- purrr::map(repro_args, "code") |> unlist(recursive = FALSE)

  existing_vars <- names(repro_code@code)
  reactive_calls <- vapply(x_args, is_any_reactive_call, env = env, logical(1L))
  variable_calls <- vapply(x_args, is_variable_call, env = env, existing_vars = existing_vars, logical(1L))
  if (inherits(x, "<-")) variable_calls[1] <- FALSE

  if (any(variable_calls)) {
    pre_variable_calls <- unname(repro_args[variable_calls])
    pre_variable_args <- purrr::map(pre_variable_calls, \(x) x@code[[1L]])

    pre_variable_assignments <- purrr::map(
      pre_variable_args,
      env = env,
      \(x, env) str2lang(paste(x, "<-", construct_reactive(x, env)))
    )
    repro_code@prerequisites <- purrr::set_names(pre_variable_assignments, pre_variable_args)

    eval_args[variable_calls] <- pre_variable_args
  }

  if (any(reactive_calls)) {
    pre_reactive_calls <- unname(repro_args[reactive_calls])

    pre_req_calls <- purrr::map(pre_reactive_calls, "prerequisites") |>
      purrr::discard(identical, list()) |>
      unlist(recursive = FALSE)
    repro_code@prerequisites <- pre_req_calls

    pre_req_args <- purrr::map(pre_reactive_calls, \(y) rlang::call_args(y@code[[1]])[[1]])
    repro_code@prerequisites <- purrr::set_names(
      purrr::map(pre_reactive_calls, "code"),
      pre_req_args
    )

    eval_args[reactive_calls] <- pre_req_args
  }

  repro_code@prerequisites <- repro_args[!(reactive_calls | variable_calls)] |>
    purrr::map("prerequisites") |>
    purrr::discard(identical, list()) |>
    unname() |>
    unlist(recursive = FALSE)

  if (rlang::is_call(x[[1]], "::")) pkg <- as.character(x[[1]][[2]]) else pkg <- NULL
  eval_call <- rlang::call2(rlang::call_name(x), !!!eval_args, .ns = pkg)
  if (inherits(x, "<-")) eval_call <- stats::setNames(list(eval_call), deparse(eval_args[[1L]]))
  repro_code@code <- eval_call

  repro_code@packages <- c(unlist(purrr::map(repro_args, "packages")), get_pkg_name(x))
  repro_code
}
