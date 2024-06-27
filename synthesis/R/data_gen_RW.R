#' Generate Random walk time series.
#'
#' @param nobs    the data length to be generated
#' @param drift   drift
#' @param sd      the white noise in the data
#'
#' @return A list of 2 elements: random walk and random walk with drift
#' @export
#'
#' @references Shumway, R. H. and D. S. Stoffer (2011). Time series regression and exploratory data analysis. Time series analysis and its applications, Springer: 47-82.
#'
#' @examples
#' set.seed(154)
#' data.rw <- data.gen.rw(200)
#' plot.ts(data.rw$xd, ylim=c(-5,55), main='random walk', ylab='')
#' lines(data.rw$x, col=4); abline(h=0, col=4, lty=2); abline(a=0, b=.2, lty=2)

data.gen.rw <- function(nobs, drift = 0.2, sd = 1) {
    w <- rnorm(nobs, sd = sd)
    x <- cumsum(w)

    wd <- w + drift
    xd <- cumsum(wd)

    return(list(x = x, xd = xd))

}


