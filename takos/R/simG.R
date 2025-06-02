#'  Title simG
#' @description create a simulated spectra with gaussian shape
#' @param vlen desired length of the spectra
#' @param i.start starting value for the peak
#' @param gheight height value
#' @param shift shift from 0
#' @param wd width of the gaussian curve
#' @export
#'
#' @examples y=(simG(500,35,1,0,w=20))
#' plot(y)
#'
simG <- function(vlen, i.start, gheight,shift=0, wd=30) {
  gzero <- floor(vlen / 2)
  set.seed(3333)
  gseq <- seq(-gzero,gzero-1)
  if ((vlen%%2==!0)== TRUE) {
    gseq <- seq(-gzero,gzero)
  }
  gx <- (dnorm(gseq, i.start - gzero, wd)) * (gheight / max(dnorm(gseq, i.start - gzero, wd)))
  gx <- gx-shift
  return(gx)
}
