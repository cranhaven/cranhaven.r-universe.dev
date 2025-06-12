#' @name plot.OEFPIL
#' @title Plot the estimate from an OEPFIL object
#' @description Plot of the iterated linearization estimate of a function from an \code{"OEFPIL"} object with pointwise confidence and prediction bands.
#'
#' @param x an object of class \code{"OEFPIL"} (a result of a call to \code{\link{OEFPIL}}).
#' @param xx  a sequence of x-coordinates of points for computing and plotting confidence bands. If missing, the default sequence \code{seq(from = min(x), to = max(x), length.out = 301)} is used.
#' @param signif.level a numerical value or a vector of significance levels for confidence bands.
#' @param interval a character vector. It states type of an interval to draw. Following values are possible: \code{"conf"} for confidence interval, \code{"pred"} for prediction interval or \code{c("conf", "pred")} for both. If missing, no intervals are plotted.
#' @param new.obs.variance the variance of a new observation for prediction interval computing (see \code{\link{confBands.OEFPIL}}).
#' @param ... additional arguments (same as in \link{plot} function) affecting the plot.
#' @details If the \code{signif.level} argument is missing, even though an \code{interval} argument is set to \code{"conf"}, the default value 0.05 is used.
#'          The line type is set to \code{'dashed'} for confidence bands and \code{'dotted'} for prediction bands.
#'          The confidence and prediction bands are computed under normality assumption. If the vector \code{signif.level} length is greater than 1,
#'          then multiple bands are plotted. The widest band has colour no. 2. The second widest band has colour no. 3 etc.
#'
#' @return Returns an object of type list containing at least the following components
#'
#' \item{xx}{a numerical vector of points where bands are calculated.}
#' \item{yy}{a numerical vector with values of estimated function in \code{xx}.}
#' \item{PointwiseCB}{a matrix of confidence intervals at points \code{xx}.}
#' \item{PredictCB}{a matrix of prediction intervals at points \code{xx}.}
#'
#'
#' @seealso \code{\link{OEFPIL}}
#'
#' @examples
#' library(MASS)
#'
#' ##Use of plot function with default parameters
#' ##(only estimation of the function is plotted, without confidence or prediction bands)
#' steamdata <- steam
#' colnames(steamdata) <- c("x","y")
#' n <- nrow(steamdata)
#' CM1 <- diag(rep(10,2*n))
#' st1 <- OEFPIL(steamdata, y ~ b1 * 10^(b2 * x/ (b3 + x)), list(b1 = 5, b2 = 8, b3 = 200),
#'              CM1, useNLS = FALSE)
#' plot(st1)
#'
#' ##Use of plot function for plotting confidence bands
#' plot(st1, seq(0,113,0.1), signif.level = c(0.01,0.05), interval = "conf",
#'  main = "Graph of estimated function")
#'
#'##Use of plot function for plotting prediction bands
#' plot(st1, seq(0,113,0.1), interval = "pred", new.obs.variance = 15)
#'
#' ##Return values of plot function
#' (a <- plot(st1, signif.level = 0.05, interval = "conf"))
#'
#' @import graphics
#'
#' @method plot OEFPIL
#' @export
plot.OEFPIL <- function(x, xx, signif.level, interval, new.obs.variance, ...) {

  ## Function plots confidence bands of list from OEFPIL() function.
  ## x           . . . output from OEFPIL()
  ## xx          . . . in these points we calculate and plot CI (confidence intervals) or
  ##                   CB (conf. bands)
  ## interval    . . . character vector; It states type of interval to draw. It can take values:
  ##                   "conf" (confidence int.) or "pred" (prediction int.) or c("conf", "pred") (both).

  if (missing(signif.level)) {
    signif.level <- 0.05

  }

  d <- length(signif.level)

  x_1 <- x$contents[[3]] ## x_1 data
  y <- x$contents[[4]] ## y data

  dep.var.name <- x$contents$dep.var.name ## name of dependant variabe
  idp.var.name <- x$contents$idp.var.name ## name of independant variable

  if (missing(xx)) {
    xx <- seq(from = min(x_1), to = max(x_1), length.out = 301)
  }

  if (missing(new.obs.variance)) {
    CM <- x$contents$CM
    n <- length(diag(CM)) / 2
    new.obs.variance <- mean(diag(CM)[(n+1):(2*n)])
    ## "estimation" of variance of the new observation (needed for prediction interval)
  }

  CB <- confBands(x, xx = xx, signif.level = signif.level, new.obs.variance = new.obs.variance)

  plot(x_1, y, xlab = idp.var.name, ylab = dep.var.name, ... = ...)

  lines(CB$xx, CB$yy, lwd = 2, col = "black")

  if(missing(interval)){
    interval <- "none"
  }


  if (any(interval == "conf")) {
    for (i in 0:(d-1)) {
      lines(CB$xx, CB$PointwiseCB[, i+1], lwd = 2, lty = 2, col = i + 2)
      lines(CB$xx, CB$PointwiseCB[, 2*d - i], lwd = 2, lty = 2, col = i + 2)
    }
  }

  if (any(interval == "pred")) {
    for (i in 0:(d-1)) {
      lines(CB$xx, CB$PredictCB[, i+1], lwd = 2, lty = 3, col = i + 2)
        lines(CB$xx, CB$PredictCB[, 2*d - i], lwd = 2, lty = 3, col = i + 2)
    }
  }

  return(invisible(list(xx = xx, yy = CB$yy, PointwiseCB = CB$PointwiseCB,
                        PredictCB = CB$PredictCB)))
}
