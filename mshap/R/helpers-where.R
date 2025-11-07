#' Select variables with a function
#'
#' This [selection helper][language] selects the variables for which a
#' function returns `TRUE`.
#'
#' @param fn A function that returns `TRUE` or `FALSE` (technically, a
#'   _predicate_ function). Can also be a purrr-like formula.
#'   
#' @return A selection of columns
#'
#' @name where
where <- function(fn) {
  predicate <- rlang::as_function(fn)
  
  function(x, ...) {
    out <- predicate(x, ...)
    
    if (!rlang::is_bool(out)) {
      rlang::abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }
    
    out
  }
}