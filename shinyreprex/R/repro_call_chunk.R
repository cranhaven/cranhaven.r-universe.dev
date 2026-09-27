#' Reproduce Call Chunk
#'
#' @description
#' A short description...
#'
#' @param x `call` object to make reproducible, with prefixed class of the call name
#' @param env The environment `x` is defined in. By default it is the environment of where
#' `reprex_reactive` is called
#' @param ... Additional arguments to pass to other methods
#'
#' @details
#' Whilst a default is provided to `env`, it is unlikely that this is the same environment
#' `x` is defined in. This is more of a placeholder for sending the correct environment to
#' evaluate any reactive mentioned in the call.
#'
#' @returns
#' A `Repro` object containing all the necessary code and packages to recreate
#' the provided expression when evaluated.
#'
#' @noRd
repro_call_chunk <- S7::new_generic(
  name = "repro_call_chunk",
  dispatch_args = "x",
  fun = function(x, ..., env = rlang::caller_env()) S7::S7_dispatch()
)
