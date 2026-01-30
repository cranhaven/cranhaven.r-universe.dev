#########1#########2#########3#########4#########5#########6#########7#########8
#' Round to the Nearest Number
#'
#' Round to the nearest number with the number of digits as indicated. NOTE:
#' Unlike the base \code{round} function it rounds a 5 to the higher number,
#' rather than the nearest even number.
#'
#' @param x number to be rounded
#' @param digits number of digits to round to
#' @return Number rounded to the number of \code{digits} indicated
#' @examples
#' round2(2.5)
#' @export
################################################################################
round2<-function(x,digits=0) {
  if (!isInt(digits)) stop("digits must be an integer")
  x=x*10^digits
  floor(x+.5)/10^digits
}
