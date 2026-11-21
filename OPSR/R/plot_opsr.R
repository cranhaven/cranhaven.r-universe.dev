#' Plot Method for OPSR Model Fits
#'
#' Wrapper around [`pairs.opsr.te`].
#'
#' @param x an object of class `"opsr"`.
#' @inheritParams opsr_te
#' @param ... further arguments passed to [`pairs.opsr.te`].
#'
#' @returns Returns `x` invisibly.
#' @seealso [`opsr_te`], [`pairs.opsr.te`]
#'
#' @method plot opsr
#' @export
plot.opsr <- function(x, type = c("response", "unlog-response", "prob", "mills", "correction", "Xb"),
                      weights = NULL, ...) {
  type <- match.arg(type)
  te <- opsr_te(x, type = type, weights = weights)
  graphics::pairs(te, ...)
  invisible(x)
}
