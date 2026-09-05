## Snake-case wrappers for the exported S3 methods.
##
## The S3 methods themselves must keep names such as predict.imr() and
## summary.imr() so that R can dispatch predict(fit), summary(fit), and friends.
## These wrappers give users a package-style snake_case API without bypassing
## that dispatch mechanism.

#' @rdname predict.imr
#' @export
predict_imr <- function(object, newdata, ...) {
  stats::predict(object, newdata = newdata, ...)
}


#' @rdname summary.imr
#' @export
summary_imr <- function(object, ...) {
  base::summary(object, ...)
}


#' @rdname coef.imr
#' @export
coef_imr <- function(object, ...) {
  stats::coef(object, ...)
}


#' @rdname plot.imr
#' @export
plot_imr <- function(x, ...) {
  graphics::plot(x, ...)
}
