#' Find Local Minima in a vector
#'
#' Given a density object, find the position of local minima (inflection points)
#'
#' @param x a vector of density values, as generated through a call to \code{\link{density}}
#' @return a vector of index corresponding to local minima
#' @examples
#' x <- c(rnorm(100),rnorm(100,3))
#' dx <- density(x)
#' plot(dx)
#' abline(v=dx$x[localMinima(dx$y)],col=2,lty=2)
#' @author Tommy \url{http://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima}
#' @export
localMinima <- function(x) {
  y <- diff(c(Inf, x)) > 0L
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}
