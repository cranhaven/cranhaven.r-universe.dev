#' @description
#' When reproducing a reactive object, a step is required to get the environment that
#' the reactive was assigned in, rather than the environment that is calling `shiny_reprex`.
#' That dive into the internals of the observable object is handled by
#' [reactive_expression()], which returns both the expression and the module
#' environment needed to generate the reproducible code.
#'
#' @include repro_chunk.R
#' @noRd
S7::method(repro_chunk, class_reactive) <- function(x, repro_code = Repro(), env = rlang::caller_env()) {
  reactive <- reactive_expression(x)
  reactive_exprs <- as.list(reactive$body)[-1]

  for (reactive_expr in reactive_exprs) {
    repro_code <- repro_chunk(reactive_expr, repro_code = repro_code, env = reactive$env)
  }

  repro_code
}
