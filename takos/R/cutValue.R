#
#' Title cutValue
#'
#' @param x  to be cut
#' @param i.start index value of the starting point for the cut to be performed
#' @param i.end   index value of the ending point for the cut to be performed
#' @param value.start desired value at point i.start
#' @param value.end  desired value at point i.end
#'
#' @return x after cut
#' @export
#'
#' @description cut a region of a spectra and substitutes it with a sequence with initial value i.start and end valye i.end
#'
#'@examples npoints=1000
#'x=seq(1,npoints)
#'y=(dnorm(x, mean=npoints/2, sd=npoints/10))
#'ycut=cutSelect(y,10,40)
#'plot(y)
#'lines(ycut,col="red")
#'

cutValue <- function(x, i.start, i.end, value.start, value.end) {
  x.start <- value.start
  x.end <- value.end
  x.length <- length(x)
  xl.start <- rep(x.start, i.start - 1)
  xl.end <- rep(x.end, (length(x) - i.end))
  x.new <- c(xl.start, x[i.start:i.end], xl.end)
  return(x.new)
}
