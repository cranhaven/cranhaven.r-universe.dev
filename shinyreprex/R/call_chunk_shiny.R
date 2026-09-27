#' Generic Method for Reproducing Code
#'
#' @description
#' Standard response is to return the called object
#'
#' @noRd
S7::method(repro_call_chunk, class_call_shiny) <- function(x, repro_code = Repro(), env = rlang::caller_env()) {
  repro_code
}
