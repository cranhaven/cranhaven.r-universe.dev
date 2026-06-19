#' @title Density estimation using Gaussian kernels
#' @description Returns a list with the parameters and
#'  input data transformed by its distribution function.
#' @param X A data frame with the predictor variables.

kernel.estimation <- function(X){
  kernel <- kde1d(X)
  value <- list(kernel = kernel, den = "kernel")
  return(value)
}



