#' @title Estimation of the parameters of a normal
#' @description Returns a list with the parameters and
#'  input data transformed by its distribution function.
#' @param X A data frame with the predictor variables.

normal.estimation <- function(X){
  mu <- apply(X, 2, mean)
  sd <- apply(X, 2, sd)
  U <- t(apply(X, 1, function(x) pnorm(x,mu,sd)))
  value <- list(mu = mu,sd = sd,U = U)
  return(value)
}

