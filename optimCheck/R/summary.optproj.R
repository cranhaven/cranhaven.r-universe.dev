#' \code{summary} method for projection plots.
#'
#' @name summary.optproj
#' @param object An \code{optproj} object, i.e., output from the function \code{\link{optim_proj}}.
#' @param xnames Optional vector of names for the elements of the potential solution.
#' @param ... Further arguments to be passed to or from other methods.
#' @return An object of class \code{summary.optproj} inheriting from \code{summary.optcheck}, with elements:
#' \describe{
#'   \item{\code{xsol}}{The potential solution vector.}
#'   \item{\code{ysol}}{The value of the objective function at \code{xsol}.}
#'   \item{\code{maximize}}{Logical indicating whether the potential solution should maximize or minimize the objective function.}
#'   \item{\code{xopt}}{A vector containing the argmax/argmin in each projection plot.}
#'   \item{\code{yopt}}{A vector containing the max/min in each projection plot.}
#'   \item{\code{xdiff}}{A two-column matrix containing the differences between \code{xsol} and \code{xopt}.  The first column is the absolute difference \code{D = xopt - xsol}, the second is the relative difference \code{R = D/|xsol|}.}
#'   \item{\code{ydiff}}{Same thing, but between \code{ysol} and \code{yopt}.}
#' }
#' @details The \code{print} methods for \code{summary.optproj} and \code{optproj} objects themselves both return a three-column matrix, consisting of the potential solution (\code{xsol}), the optimal solution in each projection plot (\code{xopt}), and the relative difference between the two (\code{R = (xopt - xsol)/|xsol|}).
#' @seealso \code{\link{print.summary.optproj}} for \code{print} method.
#' @export
summary.optproj <- function(object, xnames, ...) {
  xsol <- object$xsol
  ysol <- object$ysol
  nx <- length(xsol)
  maximum <- object$maximize
  opt.fun <- if(maximum) max else min
  which.opt <- if(maximum) which.max else which.min
  if(missing(xnames)) {
    xnames <- names(xsol)
    if(is.null(xnames)) xnames <- paste0("x",1:nx)
  }
  # store argmax/argmin and max/min in each projection plot
  opt.res <- matrix(NA, nx, 2)
  for(ii in 1:nx) {
    iopt <- which.opt(object$yproj[,ii])
    opt.res[ii,] <- c(object$xproj[iopt,ii], object$yproj[iopt,ii])
  }
  # differences in solution
  xdiff <- opt.res[,1] - xsol
  xdiff <- cbind(abs = xdiff, rel = xdiff/abs(xsol))
  # differences in solution value
  ydiff <- opt.res[,2] - ysol
  ydiff <- cbind(abs = ydiff, rel = ydiff/abs(ysol))
  # add names
  names(xsol) <- xnames
  rownames(opt.res) <- xnames
  rownames(xdiff) <- xnames
  rownames(ydiff) <- xnames
  ans <- list(xsol = xsol, ysol = ysol,
              maximize = maximum,
              xopt = opt.res[,1], yopt = opt.res[,2],
              xdiff = xdiff, ydiff = ydiff)
  class(ans) <- c("summary.optproj", "summary.optcheck")
  ans
}

#--- print methods -------------------------------------------------------------

## #' @rdname summary.optproj
## #' @export
## print.summary.optproj <- function(x,
##                                    digits = max(3L, getOption("digits")-3L)) {
##   res <- cbind(x$xsol, x$xdiff[,"abs"], x$xdiff[,"rel"])
##   colnames(res) <- c("xsol", "D=xopt-xsol", "R=D/|xsol|")
##   print(signif(res, digits = digits))
## }

## #' @rdname summary.optproj
## #' @export
## print.optproj <- function(x,
##                            digits = max(3L, getOption("digits")-3L)) {
##   osum <- summary(x)
##   print(osum, digits = digits)
## }

