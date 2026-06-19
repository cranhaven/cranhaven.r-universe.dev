#' @title Estimation of the parameters of a normal
#' @description Returns a list with the parameters and
#'  input data transformed by its distribution function.
#' @param X A data frame with the predictor variables.

normal.estimation <- function(X){
  mu <- mean(X)
  sd <- sd(X)
  value <- list(mu = mu,sd = sd, den = "normal")
  return(value)
}

