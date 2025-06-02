#' @title Calculates the log-likelihood of a grid type copula
#' @description Returns the log-likelihood of a grid type copula.
#' @param x a vector with the density values of a grid type copula.
#' @param y a vector with the counting values in the grid.
#' @examples


ll.grid <- function(x, y) {
  if( (0 < sum(x<0)) | (0<sum((x==0) & (0<y))) ) {
    value <- -1*sum((x-1)^2) #-1e100
  } else {
    position <- (0<x) & (0<y)
    value <- sum(y[position] * log(x[position]))
  }
  return(value)
}
