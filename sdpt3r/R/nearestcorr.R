#' Nearest Correlation Matrix Problem
#'
#'\code{nearcorr} creates input for sqlp to solve the nearest correlation matrix problem -
#'given a approximate correlation matrix H, find the nearest correlation matrix X.
#'
#'@details
#' For a given approximate correlation matrix H, determines the nearest correlation matrix X. 
#' Mathematical and implementation details can be found in the vignette
#' 
#' @param H A symmetric matrix
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' data(Hnearcorr)
#'
#' out <- nearcorr(Hnearcorr)
#' 
#' @export
nearcorr <- function(H){
  
  #Error Checking
  stopifnot(is.matrix(H), is.numeric(H), nrow(H) == ncol(H), isSymmetric(H,check.attributes = FALSE), all(diag(H) == 1))
  
  #Define Variables
  blk <- matrix(list(),2,2)
  At <- matrix(list(),2,1)
  C <- matrix(list(),2,1)
  
  n <- max(dim(H))
  n2 <- n*(n+1)/2
  
  AA <- matrix(list(),1,n)
  
  blk[[1,1]] <- "s"
  blk[[1,2]] <- n
  
  for(k in 1:n){
    AA[[1,k]] <- Matrix(0,n,n)
    AA[[1,k]][k,k] <- 1
  }
  matrepdiag <- svec(blk[1,,drop=FALSE],AA)
  At[[1,1]] <- cbind(matrepdiag[[1]],Diagonal(n2))
  
  blk[[2,1]] <- "q"
  blk[[2,2]] <- n2+1
  At[[2,1]] <- rbind(Matrix(0,n,n2+1,sparse=TRUE),cbind(Matrix(0,n2,1,sparse=TRUE),Diagonal(n2)))
  
  Htmp <- H
  H <- matrix(list(),1,1)
  H[[1]] <- Htmp
  b <- rbind(matrix(1,n,1),svec(blk[1,,drop=FALSE],H)[[1]])
  
  C[[1,1]] <- Matrix(0,n,n,sparse=TRUE)
  C[[2,1]] <- rbind(1,matrix(0,n2,1))
  
  out <- sqlp_base(blk=blk, At=At, b=b, C=C, OPTIONS = list())
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
}