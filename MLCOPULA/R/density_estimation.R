#' @title Estimation of the density of some distribution
#' @param X Matrix with predictor variables.
#' @param distribution Distribution to be used: normal or kernels, 
#' by default normal.  


density.estimation <- function(X, distribution = NULL){
  U <- pobs(X)
  if(is.null(distribution)){
    method <- apply(X,2,norm.test)
  } else{
    method <- rep(distribution,ncol(X))
  }
  X_cols <- split(as.matrix(X), col(X))
  res <- mapply(function(x, method) {
      if(method == "kernel"){
        return(kernel.estimation(x))
      }else{
        return(normal.estimation(x))
      }
    },X_cols,method, SIMPLIFY = FALSE)
  return(list(den = res, U = U, distribution =  method))
}

