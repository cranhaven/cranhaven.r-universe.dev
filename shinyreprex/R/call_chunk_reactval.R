#' @description
#' Extracting the contents of the `reactiveVal` in a human-readable way
#'
#' @noRd
S7::method(repro_call_chunk, class_call_reactval_setter) <- function(x,
                                                                     repro_code = Repro(),
                                                                     env = rlang::caller_env()) {
  warning(
    "`", rlang::call_name(x), "()` is a reactiveVal setter and has been omitted from the ",
    "reproduced script - setting reactive values cannot be reproduced outside of Shiny",
    call. = FALSE
  )
  repro_code
}

S7::method(repro_call_chunk, class_call_reactval) <- function(x, repro_code = Repro(), env = rlang::caller_env()) {
  reactive_val <- construct_reactive(x, env = env)
  eval_call <- str2lang(paste(rlang::call_name(x), "<-", reactive_val))

  repro_code@packages <- get_pkg_name(x)
  repro_code@code <- eval_call
  repro_code
}
