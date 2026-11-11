#' @title Print methods
#' @param x [`ppred_model`]: An object of class `ppred_model`.
#' @param ... Additional Arguments.
#' @return prints a Transformer model from a list returned by `create_model()`.
#'
#' @export
print.ppred_model <- function(x, ...) {
  print(x$model)
}



