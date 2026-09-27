#' @description
#' Extracting the contents out of a reactive object and creating a set of calls that can evaluate the call
#' in a local environment when run outside of Shiny, preventing unnecessary variables
#'
#' @noRd
S7::method(repro_call_chunk, class_call_reactive) <- function(x, repro_code = Repro(), env = rlang::caller_env()) {
  repro_call <- repro_chunk(env[[rlang::call_name(x)]])
  repro_code@prerequisites <- repro_call@prerequisites
  repro_code@packages <- repro_call@packages

  eval_call <- assign_reactive_call(x, repro_call)
  repro_code@code <- eval_call

  repro_code
}
