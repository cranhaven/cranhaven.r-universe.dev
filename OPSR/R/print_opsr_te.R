#' Print Method for OPSR ATE Objects
#'
#' @param x an object of class `"opsr.te"`.
#' @param ... further arguments passed to [`summary.opsr.te`].
#'
#' @return Returns `x` invisibly.
#'
#' @details
#' This is just a wrapper around [`summary.opsr.te`] and a subsequent call to
#' [`print.summary.opsr.te`].
#'
#' @seealso [`print.summary.opsr.te`]
#'
#' @method print opsr.te
#' @export
print.opsr.te <- function(x, ...) {
  sx <- summary(x, ...)
  sx$call <- match.call()
  print(sx)
  invisible(x)
}
