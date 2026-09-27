#' Reproduce Code Chunk
#'
#' @description
#' Evaluate a chunk of code to extract Shiny inputs and reactives, replacing the inputs with
#' the values selected by the user, and the reactives with the code bodies used to generate
#' them.
#'
#' @param x [shiny::reactive()] object to make reproducible
#' @param repro_code A `Repro` object to store calls found in `x`. By default it is
#' empty, but if `x` is not the first call within an expression, this will have
#' prior calls and pre-requisites that might be used in `x`.
#' @param env The environment `x` is defined in. By default it is the environment of where
#' `reprex_reactive` is called
#'
#' @details
#' Whilst a default is provided to `env`, it is unlikely that this is the same environment
#' `x` is defined in. This allows the top-level `reprex_reactive` call to pass through
#' environments found for calls to other reactives in the chunk.
#'
#' @returns
#' A `Repro` object containing all the necessary code and packages to recreate
#' the provided expression when evaluated.
#'
#' @keywords internal
repro_chunk <- S7::new_generic(
  name = "repro_chunk",
  dispatch_args = "x",
  fun = function(x, repro_code = Repro(), env = rlang::caller_env()) S7::S7_dispatch()
)
