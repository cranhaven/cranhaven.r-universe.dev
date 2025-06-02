
#' Title cutSelect
#'
#' @param x  x to be cut
#' @param i.start index value of the starting point for the cut to be performed
#' @param i.end  index value of the ending point for the cut to be performed
#'
#' @description cut a region of a spectra and substitutes it with a sequence with initial value i.start and end valye i.end
#'
#' @export
#' @examples npoints=1000
#'x=seq(1,npoints)
#'y=(dnorm(x, mean=npoints/2, sd=npoints/10))
#'ycut=cutValue(y,10,40,0.003,0.001)
#'plot(y)
#'lines(ycut,col="red")
#'

cutSelect <- function(x, i.start, i.end) {
  x.start <- x[i.start]
  x.end <- x[i.end]
  x.length <- length(x)
  xl.start <- rep(x.start, i.start - 1)
  xl.end <- rep(x.end, (length(x) - i.end))
  x.new <- c(xl.start, x[i.start:i.end], xl.end)
  return(x.new)
}
