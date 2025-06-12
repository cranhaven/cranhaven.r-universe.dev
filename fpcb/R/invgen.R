#' Generalized inverse
#' 
#' Computes a regularized inverse of a square matrix using eigen decomposition
#' 
#' 
#' @param A A square m x m matrix
#' @param tol A tolerance to keep the first d eigenvalues of A. Default =
#' 1e-05.
#' @return The generalized inverse of A.
#' @author J. Cugliari
#' @noRd
#' @examples
#' 
#' A<-matrix(rnorm(100),nrow=10)
#' invgen(A)
#' 
invgen <-
function(A, tol=1e-05){
  spa <- eigen(A, symmetric = T)
  p <- ncol(A)
  ind <- (1:p)[spa$values > tol] # eigen values that are not zero
  #  print(paste("[@invgen]:", length(ind), "out of", p, ">0"))
  U <- spa$vectors
  U <- U[, ind]
  if(length(ind) > 1) {
    B <- U %*% diag(1/spa$values[ind]) %*% t(U)
  } else {
    B <- 1 / spa$values[ind] * as.matrix(U) %*%
      t(as.matrix(U))
  }
  B
}
