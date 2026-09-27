#' @description
#' When reproducing an R call, a check is required to see if it is a standard R expression,
#' or if it a call to a reactive object. This is to either extract the stored `reactiveVal`,
#' or to get the expression used within the given `reactive`.
#'
#' @include repro_chunk.R
#' @noRd
S7::method(repro_chunk, S7::class_call) <- function(x, repro_code = Repro(), env = rlang::caller_env()) {
  call_env <- env
  call_name <- rlang::call_name(x) %||% "NULL"


  if (is_reactive_val_setter_call(x, env)) {
    # reactiveVal setter: warn and skip
    call_name <- ".__reactval_setter"
  } else if (is_reactive_val_call(x, env)) {
    # reactiveVal getter: evaluate and include
    call_name <- ".__reactval"
  } else if (is_reactive_call(x, env)) {
    # Reactive object created within the module
    call_name <- ".__reactive"
  } else if (is_reactive_call(x, parent.env(env))) {
    # Reactive object sent to the module
    call_name <- ".__reactive"
    call_env <- parent.env(env)
  }

  class(x) <- c(call_name, unclass(class(x)))

  repro_call_chunk(
    x = x,
    env = call_env,
    repro_code = repro_code
  )
}
