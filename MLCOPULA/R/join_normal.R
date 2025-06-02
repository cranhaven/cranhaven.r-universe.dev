#' @title Normal Joint Density
#' @description Returns a list with the product of the normal
#'  densities and input data transformed by its distribution
#'  function.
#' @param X A data frame with the predictor variables.
#' @param mu A vector with the estimates for \eqn{\mu}.
#' @param sd A vector with the estimates for \eqn{\sigma}.


join.normal <- function(X,mu,sd){
  res <- t(apply(X, 1, function(x) dnorm(x,mu,sd,log = FALSE)))
  U <- t(apply(X, 1, function(x) pnorm(x,mu,sd)))

  res[res == 0] <- 1e-200
  res <- apply(res, 1, function(x)  sum(log(x)))
  return(list(den = res, U = U))
}
