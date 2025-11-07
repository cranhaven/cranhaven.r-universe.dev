#' Skewness
#'
#' Calculate the skewness of a numeric variable
#'
#' @param x numeric vector.
#' @param na.rm if \code{TRUE}, delete missing values.
#' @export
#' @return a number
#' @examples
#' skewness(mtcars$mpg)
skewness <- function(x, na.rm=TRUE){
  if(na.rm){
    v <- stats::na.omit(x)
  }
  n <- length(v)
  v <- v - mean(v)
  v <- sqrt(n) * sum(v^3)/(sum(v^2)^(3/2))
  skewness <- v * ((1 - 1/n))^(3/2)
  return(skewness)
}
