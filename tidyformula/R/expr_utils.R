#' Utility functions for traversing ASTs
#'
#' Taken verbatim from Section 18.5 of Wickham, Hadley (2019) <doi:10.1201/9781351201315>
#' URL: https://adv-r.hadley.nz/expressions.html#ast-funs
#'
#' @noRd
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

switch_expr <- function(x, ...) {
  switch(expr_type(x),
    ...,
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}
