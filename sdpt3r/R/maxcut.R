#' Max-Cut Problem
#'
#'\code{maxcut} creates input for sqlp to solve the Max-Cut problem -
#'given a graph B, find the maximum cut of the graph
#'
#'@details
#' Determines the maximum cut for a graph B. Mathematical and implementation
#' details can be found in the vignette
#' 
#' @param B A (weighted) adjacency matrix corresponding to a graph
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' data(Bmaxcut)
#'
#' out <- maxcut(Bmaxcut)
#' 
#' @export
maxcut <- function(B){
  
  #Error Checking
  stopifnot(is.matrix(B), is.numeric(B), isSymmetric(B,check.attributes = FALSE), nrow(B) == ncol(B), !all(B == 0))
  
  #Define Variables
  n <- max(dim(B))
  e <- matrix(1,n,1)
  
  C <- matrix(list(), 1, 1)
  blk <- matrix(list(),1,2)
  A <- matrix(list(), 1,n)
  
  C[[1]] <- matrix(0,n,n)
  diag(C[[1]]) <- B %*% e
  C[[1]] <- -(C[[1]] - B)/4
  b <- e
  
  blk[[1,1]] <- 's'
  blk[[1,2]] <- n
  
  for(k in 1:n){
    A[[k]] <- Matrix(0,n,n)
    A[[k]][k,k] <- 1
  }
  
  Avec <- svec(blk,M=A,isspx=matrix(0,nrow(blk),1))
  
  out <- sqlp_base(blk, Avec, C, b)
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
  
}