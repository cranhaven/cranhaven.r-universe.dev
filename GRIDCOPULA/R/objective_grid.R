#' @title Evaluates the negative log-likelihood function associated to a grid type copula
#' @description Returns the corresponding value of the least squares function.
#' @param x a vector with the density values of a grid type copula.
#' @param A a matrix with the counting values in the grid.
#' @examples



objective.grid <- function(x, A) {
  value <- -1*ll.grid(x=x, y=as.vector(A))
  return(value)
}