#' Educational Testing Problem
#'
#'\code{etp} creates input for sqlp to solve the Educational Testing Problem -
#'given a symmetric positive definite matrix S, how much can be subtracted from the diagonal
#'elements of S such that the resulting matrix is positive semidefinite definite.
#'
#'@details
#' Solves the education testing problem. Mathematical and implementation
#' details can be found in the vignette
#' 
#' @param B A symmetric positive definite matrix
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' data(Betp)
#' 
#' out <- etp(Betp)
#' 
#' @export
etp <- function(B){
  
  #Error Checking
  stopifnot(is.matrix(B), is.numeric(B), isSymmetric(B,check.attributes = FALSE))
  
  #Define Variables
  n <- max(dim(B))
  
  blk <- matrix(list(),2,2)
  C <- matrix(list(),2,1)
  At <- matrix(list(),2,1)
  A <- matrix(list(),2,n)
  
  blk[[1,1]] <- "s"
  blk[[1,2]] <- n
  blk[[2,1]] <- "l"
  blk[[2,2]] <- n
  
  b <- matrix(1,n,1)
  C[[1,1]] <- B
  C[[2,1]] <- matrix(0,n,1)
  
  for(k in 1:n){
    A[[1,k]] <- Matrix(0,n,n)
    A[[1,k]][k,k] <- 1
    A[[2,k]] <- rbind(matrix(0,k-1,1),-1,matrix(0,n-k,1))
  }
  
  At <- svec(blk,A,matrix(1,nrow(blk),1))
  
  out <- sqlp_base(blk=blk, At=At, b=b, C=C, OPTIONS = list())
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
  
}