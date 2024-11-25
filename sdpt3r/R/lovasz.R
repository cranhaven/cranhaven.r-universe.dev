#' Lovasz Number of a Graph
#'
#'\code{lovasz} creates input for sqlp to find the Lovasz Number of a graph
#'
#'@details
#' Finds the maximum Shannon entropy of a graph, more commonly known as the Lovasz number. 
#' Mathematical and implementation details can be found in the vignette
#' 
#' @param G An adjacency matrix corresponding to a graph
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' data(Glovasz)
#'
#' out <- lovasz(Glovasz)
#' 
#' @export
lovasz <- function(G){
  
  #Error Checking
  stopifnot(is.matrix(G), is.numeric(G),isSymmetric(G,check.attributes = FALSE), nrow(G) == ncol(G), !all(G == 0))
  
  #Define Variables
  blk <- matrix(list(),1,2)
  C <- matrix(list(),1,1)
  At <- matrix(list(),1,1)
  
  n <- max(dim(G))
  m <- sum(G[upper.tri(G)]) + 1
  e1 <- matrix(c(1,rep(0,m-1)),m,1)
  
  C[[1]] <- matrix(-1,n,n)
  b <- e1
  blk[[1,1]] <- "s"
  blk[[1,2]] <- n
  
  A <- matrix(list(),1,m)
  A[[1]] <- Diagonal(n,1)
  cnt <- 2
  for(i in 1:(n-1)){
    idx <- which(G[i,(i+1):n] != 0)
    idx <- idx + i
    if(length(idx) > 0){
      for(j in 1:length(idx)){
        A[[cnt]] <- Matrix(0,n,n)
        A[[cnt]][i,idx[j]] <- 1
        A[[cnt]][idx[j],i] <- 1
        cnt <- cnt + 1
      }
    }
  }
  
  At <- svec(blk,A,1)
  
  out <- sqlp_base(blk=blk, At=At, b=b, C=C, OPTIONS = list())
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
  
}