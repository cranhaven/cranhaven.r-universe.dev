#' Function Method for Reproducing Code Call
#'
#' @include repro_call_chunk.R
#' @noRd
S7::method(repro_call_chunk, class_call_function) <- function(x, repro_code = Repro(), env = rlang::caller_env()) {
  x_args <- rlang::call_args(x)

  fn_args <- purrr::map_if(
    x_args[[1L]],
    rlang::is_missing,
    \(x) Repro(),
    .else = \(x) repro_chunk(x, repro_code = repro_code, env = env)
  )

  x_args[[1L]] <- purrr::map(fn_args, "code") |>
    purrr::imap(\(x, y) if (length(x)) x[[1L]] else x_args[[1L]][[y]]) |>
    as.pairlist()

  fn_body <- as.list(x_args[[2L]])[-1L]
  fn_body <- purrr::map(fn_body, repro_chunk, repro_code = repro_code, env = env)
  fn_body_eval <- purrr::map(fn_body, "code") |> unlist(recursive = FALSE)

  x_args[[2L]] <- as.call(c(x_args[[2L]][[1L]], fn_body_eval))
  repro_code@packages <- purrr::map(fn_body, "packages") |> unlist()
  repro_code@prerequisites <- purrr::map(fn_body, "prerequisites") |>
    purrr::discard(identical, list()) |>
    unlist(recursive = FALSE)

  repro_code@code <- rlang::call2("function", !!!x_args)
  repro_code
}
