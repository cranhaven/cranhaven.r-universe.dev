#' Graph Partitioning Problem
#' 
#'\code{gpp} creates input for sqlp to solve the graph partitioning problem.
#'
#'@details
#'
#' Solves the graph partitioning problem. Mathematical and implementation
#' details can be found in the vignette
#' 
#' @param B A weighted adjacency matrix
#' @param alpha Any real value in (0,n^2)
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' data(Bgpp)
#' alpha <- nrow(Bgpp)
#'
#' out <- gpp(Bgpp, alpha)
#'
#' @export
gpp <- function(B, alpha){
  
  #Error Checking
  stopifnot(is.matrix(B), is.numeric(B), isSymmetric(B,check.attributes = FALSE), ncol(B)==nrow(B), !all(B == 0), is.numeric(alpha), alpha > 0, alpha < nrow(B)^2)
  
  #Define Variables
  blk <- matrix(list(),1,2)
  C <- matrix(list(),1,1)
  At <- matrix(list(),1,1)
  
  n <- max(dim(B))
  e <- matrix(1,n,1)
  C[[1]] <- -(Diagonal(n,B%*%e) - B)
  b <- rbind(alpha,e)
  blk[[1,1]] <- "s"
  blk[[1,2]] <- n
  
  A <- matrix(list(),1,n+1)
  A[[1]] <- e %*% t(e)
  for(k in 1:n){
    A[[k+1]] <- Matrix(0,n,n)
    A[[k+1]][k,k] <- 1
  }
  
  At <- svec(blk,A,1)
  
  out <- sqlp_base(blk=blk, At=At, b=b, C=C, OPTIONS = list())
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
}