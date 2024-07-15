#' Projection plot test.
#'
#' Given the objective function of an optimization problem and a potential solution, calculates "projection plots" along each coordinate of the solution vector, with all other coordinates being fixed at the input values.
#'
#' @param xsol Potential solution vector of length \code{nx}.
#' @param fun Objective function to be maximized (or minimized), with first argument the length-\code{nx} parameter vector over which optimization is to take place.  Should return a scalar result.
#' @param maximize Logical, whether a maximum or a minimum of the objective function is sought.
#' @param xrng Optional specification of the range of each projection plot.  Can be: (i) a \code{2 x nx} matrix giving the endpoints of the range, (ii) a scalar or vector of length \code{nx}, such that the range in each plot is \code{theta +/- xrange * abs(theta)}.
#' @param npts Number of points in each projection plot.
#' @param plot Logical, whether or not to display the projection plots or just return their contents.
#' @param ... Further arguments to pass to the \code{plot} method (see \code{\link{plot.optproj}}).
#' @return An object of class \code{optproj} inheriting from \code{optcheck} (returned invisibly if \code{plot = TRUE}, with elements:
#' \describe{
#'   \item{\code{xsol}}{The potential solution.}
#'   \item{\code{ysol}}{The value of \code{fun(xsol)}.}
#'   \item{\code{maximize}}{Logical indicating whether the potential solution should maximize or minimize the objective function.}
#'   \item{\code{xproj}}{An \code{npts x nx} matrix where each column is the \code{x}-axis of the projection plot along the given component of \code{theta}.}
#'   \item{\code{yproj}}{An \code{npts x nx} matrix where each column is the \code{y}-axis of the corresponding projection plot.}
#' }
#' @seealso \code{plot}, \code{summary}, \code{print}, and \code{diff} methods for projection plots are available; see \code{plot.optproj}, \code{\link{summary.optproj}}, \code{\link{print.optproj}}, and \code{\link{diff.optproj}}.
#' @export
optim_proj <- function(xsol, fun, maximize = TRUE, xrng = .1,
                       npts = 100, plot = TRUE, ...) {
  nx <- length(xsol) # number of parameters
  xproj <- matrix(NA, npts, nx) # x-axis of plots
  yproj <- matrix(NA, npts, nx) # y-axis of plots
  ## equalize <- FALSE # disabled
  if(!is.matrix(xrng)) {
    # default range is +/- .1 * max(abs(xsol))
    xrng <- xrng * abs(xsol)
    xrng <- rbind(xsol - xrng, xsol + xrng)
  } else {
    if(!all(dim(xrng) == c(2,nx))) {
      stop("xrng must be a scalar, vector of length(xsol), or a 2 x length(xsol) matrix.")
    }
  }
  for(ii in 1:nx) {
    xseq <- seq(from = xrng[1,ii],
                to = xrng[2,ii], len = npts)
    ## for(jj in 1:2) {
    # evaluate likelihood fixing all components except one
    yval <- sapply(xseq, function(xi) {
      x <- xsol
      x[ii] <- xi
      fun(x)
    })
    ## if(!maximize) yval <- -yval
    ##   if(jj == 1 && equalize) {
    ##     xseq <- .equalize_xlim(xseq, yval, xsol[ii])
    ##     xseq <- seq(xseq[1], xseq[2], len = npts)
    ##   } else break
    ## }
    ## if(!maximize) yval <- -yval
    # store calcs
    xproj[,ii] <- xseq
    yproj[,ii] <- yval
  }
  ans <- list(xsol = xsol, ysol = fun(xsol), maximize = maximize,
              xproj = xproj, yproj = yproj)
  class(ans) <- c("optproj", "optcheck")
  if(plot) {
    plot.optproj(ans, ...)
    return(invisible(ans))
  } else return(ans)
}
