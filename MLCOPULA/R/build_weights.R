#' @title Build the weight matrix
#' @description Returns a list with the estimated parameters
#'  of the copula, the log likelihood, the mutual information
#'   (optional) and the weight matrix: log likelihood or mutual
#'    information.
#' @param U Values of u and v.
#' @param cop.est Function to estimate the 
#' parameters of copula lagoon.
#' @param weights Character with the weight construction method:
#'  "likelihood" or "mutual_information", by default likelihood.
#' @param cop Name of the copula to be used.


build.weights <- function(U, cop.est,weights,cop){
  n_col <- dim(U)[2]
  ll <- matrix(nrow = n_col,ncol = n_col)
  colnames(ll) <- colnames(U)
  rownames(ll) <- colnames(U)
  
  mi <- matrix(nrow = n_col,ncol = n_col)
  colnames(mi) <- colnames(U)
  rownames(mi) <- colnames(U)
  
  theta <- matrix(nrow = n_col,ncol = n_col)
  colnames(theta) <- colnames(U)
  rownames(theta) <- colnames(U)
  
  
  for (i in 1:(n_col - 1)) {
    for (j in (i + 1):n_col) {
      values <- cop.est(U[,c(i,j)])
      ll[i,j] <- values$logv
      theta[i,j] <- values$theta
      if(weights == "mutual_information"){
        mi[i,j] <- mi.cop(theta = theta[i,j], cop = cop)
      }
    }
  }
  
  if(weights == "mutual_information"){
    w <- mi
  }else{
    w <- ll
  }
  
  lower_triangle <- lower.tri(w)
  w[lower_triangle] <- t(w)[lower_triangle]
  theta[lower_triangle] <- t(theta)[lower_triangle]
  diag(w) <- 0
  diag(theta) <- 0
  
  res <- list(w = w, theta = theta, ll = ll, mi = mi,
              cop = cop)
  return(res)
}
