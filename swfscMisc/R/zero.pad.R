#' @title Zero Pad Integers
#' @description Return character representation of integers that are zero-padded 
#'   to the left so all are the same length.
#' 
#' @param x a vector of integers.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' x <- c(0, 1, 3, 4, 10) 
#' zero.pad(x)
#' x <- c(x, 11, 12, 100, 1000)
#' zero.pad(x)
#' 
#' @export
#' 
zero.pad <- function(x) {
  if(!is.numeric(x)) stop("'x' must be a numeric vector")
  is.whole <- abs(x - round(x)) < .Machine$double.eps ^ 0.5
  if(!all(is.whole, na.rm = TRUE)) stop("'x' must be a vector of integers")
  digits <- floor(log10(max(x, na.rm = TRUE)))
  sapply(x, function(xi) {
    if(is.na(xi)) xi else {
      formatC(xi, digits = digits, flag = "0", mode = "integer")
    }
  })
}