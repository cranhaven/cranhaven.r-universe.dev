
#' Anti transpose (internal)
#' 
#' @param m square matrix
#' @return the anti transpose of \code{m}
#' @keywords internal
anti_t <- function (m){
  p <- nrow(m)
  j <- matrix(ncol = p, nrow = p, data = 0)
  for (i in 1:p) {
    j[i, p - i + 1] <- 1
  }
  return(j %*% t(m) %*% j)
}


#' List all the inverse matrices of the leading sub-matrix of P=Sigma^{-1}
#' (INTERNAL)
#'
#' @param Sigma an invertible  matrix
#'
#' @return a list with the inverse of the leading sub-matrices of Sigma^(-1)
#' @keywords internal
listInverseBlocks <- function(Sigma){
   l <- list()
   l[[1]] <- Sigma
   for (i in 2:(nrow(Sigma) )){
     l[[i]] <-l[[i - 1]][ - 1, - 1] - (l[[i - 1]][ - 1, 1] %*%
                                              t(l[[i - 1]][1, - 1])) /
       l[[i - 1]][1, 1]
   }
   return(l[-1])
}


#' Generate a naive stable matrix 
#' 
#' @param p dimension of the matrix
#' @return a stable matrix with off-diagonal entries equal to 1 and 
#' diagonal entries equal to \code{-p}
#' @export
B0 <- function(p){
  M <- matrix(nrow = p, ncol = p, 1)
  diag(M) <- -p
  return(M)
}
