#' @title Joint density using Gaussian kernels
#' @description Returns a list with the product of the normal
#'  densities and input data transformed by its distribution
#'  function.
#' @param X A data frame with the predictor variables.
#' @param den A list of estimated kernels.

join.kernel <- function(X,den){
  res <- matrix(ncol = dim(X)[2], nrow = dim(X)[1])
  U <- matrix(ncol = dim(X)[2], nrow = dim(X)[1])
  
  colnames(res) <- colnames(X)
  colnames(U) <- colnames(X)
  
  for(i in 1:dim(X)[2]){
    res[,i] <- dkde1d(x = X[,i], den[[i]])
    U[,i] <- pkde1d(q = X[,i], den[[i]])
  }

  res[res == 0] <- 1e-200
  res <- apply(res, 1, function(x)  sum(log(x)))
  return(list(den = res, U = U))
}
