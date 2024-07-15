#' \code{summary} method for \code{optrefit} objects.
#'
#' @name summary.optrefit
#' @param object An \code{optrefit} object, i.e., output from the function \code{\link{optim_refit}}.
#' @param xnames Optional vector of names for the elements of the potential solution.
#' @param ... Further arguments to be passed to or from other methods.
#' @return An object of class \code{summary.optrefit} inheriting from \code{summary.optcheck}, with elements:
#' \describe{
#'   \item{\code{xsol}}{The potential solution vector.}
#'   \item{\code{ysol}}{The value of the objective function at \code{xsol}.}
#'   \item{\code{maximize}}{Logical indicating whether the potential solution should maximize or minimize the objective function.}
#'   \item{\code{xopt}}{A vector containing the argmax/argmin in each projection plot.}
#'   \item{\code{yopt}}{The scalar value of the max/min found by \code{optim_refit}.}
#'   \item{\code{xdiff}}{A two-column matrix containing the differences between \code{xsol} and \code{xopt}.  The first column is the absolute difference \code{D = xopt - xsol}, the second is the relative difference \code{R = D/|xsol|}.}
#'   \item{\code{ydiff}}{A length-two vector containing the absolute and relative difference between \code{ysol} and \code{yopt}.}
#' }
#' @seealso \code{\link{print.summary.optcheck}} for \code{print} method.
#' @export
summary.optrefit <- function(object, xnames, ...) {
  xsol <- object$xsol
  ysol <- object$ysol
  nx <- length(xsol)
  if(missing(xnames)) {
    xnames <- names(xsol)
    if(is.null(xnames)) xnames <- paste0("x",1:nx)
  }
  # optimal value
  xopt <- object$xopt
  yopt <- object$yopt
  # differences in solution
  xdiff <- xopt - xsol
  xdiff <- cbind(abs = xdiff, rel = xdiff/abs(xsol))
  # differences in solution value
  ydiff <- yopt - ysol
  ydiff <- c(abs = ydiff, rel = ydiff/abs(ysol))
  ans <- list(xsol = xsol, ysol = ysol,
              maximize = object$maximize,
              xopt = xopt, yopt = yopt,
              xdiff = xdiff, ydiff = ydiff)
  class(ans) <- c("summary.optrefit", "summary.optcheck")
  ans
}

#--- print methods -------------------------------------------------------------

## #' @rdname summary.optrefit
## #' @export
## print.summary.optrefit <- function(x,
##                                    digits = max(3L, getOption("digits")-3L)) {
##   res <- cbind(x$xsol, x$xdiff[,"abs"], x$xdiff[,"rel"])
##   colnames(res) <- c("xsol", "D=xopt-xsol", "R=D/|xsol|")
##   print(signif(res, digits = digits))
## }

## #' @rdname summary.optrefit
## #' @export
## print.optrefit <- function(x,
##                             digits = max(3L, getOption("digits")-3L)) {
##   osum <- summary(x)
##   print(osum, digits = digits)
## }

