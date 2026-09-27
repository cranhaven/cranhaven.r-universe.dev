#' @description
#' When checking a code chunk of a subset, it can be one of threr things:
#'
#' - An input value i.e. `input$value`
#' - A `reactiveValues` object
#' - A natural subset of an R object
#'
#' The first two scenarios require their own evaluation to make sure we get the stored values.
#'
#' @include repro_call_chunk.R
#' @noRd
S7::method(repro_call_chunk, class_call_subset) <- function(x, repro_code = Repro(), env = rlang::caller_env()) {
  if (is_input_call(x) || is_session_user_data(x)) {
    eval_call <- str2lang(construct_reactive(x, env = env))
  } else if (is_reactive_values_call(x, env)) {
    reactive_val <- construct_reactive(x, env = env)
    eval_call <- str2lang(paste(rlang::call_args(x)[[2]], "<-", reactive_val))
  } else {
    class(x) <- c(".__generic", class(unclass(x)))
    return(
      repro_call_chunk(
        x = x,
        repro_code = repro_code,
        env = env
      )
    )
  }

  repro_code@packages <- get_pkg_name(x)
  repro_code@code <- eval_call
  repro_code
}
