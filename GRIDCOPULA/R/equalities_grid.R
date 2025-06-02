#' @title Evaluates the equalities for the parameters of a grid type copula
#' @description Returns the corresponding value of the equalities.
#' @param x a vector with the density values of a grid type copula.
#' @param A a matrix considered as a reference with the size of the grid.
#' @examples





equalities.grid <- function(x, A) {
  m <- ncol(A)
  k <- nrow(A)
  value <- rep(NA, m+k)
  for(i in 1:m) {
    value[i] <- sum(x[((i-1)*k+1):(i*k)])
  }
  for(i in 1:k) {
    value[(m+i)] <- sum(x[seq(i, m*k, by=k)])
  }
  return(value[-(m+k)])
}
