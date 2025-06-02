#' @title Density estimation using Gaussian kernels
#' @description Returns a list with the parameters and
#'  input data transformed by its distribution function.
#' @param X A data frame with the predictor variables.

kernel.estimation <- function(X){
  f <- function(x){
    den <- kde1d(x)
    U <- pkde1d(q = x, obj = den)
    value <- list(den = den, U = U)
    return(value)
  }
  
  den <- apply(X, 2, f)
  kernel <- lapply(den, function(x) x$den)
  U <- lapply(den, function(x) x$U)
  U <- as.matrix(as.data.frame(U))

  value <- list(kernel = kernel, U = U)
  return(value)
}
