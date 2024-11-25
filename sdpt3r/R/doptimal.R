#'D-Optimal Experimental Design
#'
#'\code{doptimal} creates input for sqlp to solve the D-Optimal Experimental Design problem -
#'given an nxp matrix with p <= n, find the portion of points that maximizes det(A'A)
#'
#'@details
#' Solves the D-optimal experimental design problem. Mathematical and implementation
#' details can be found in the vignette
#' 
#' @param V a pxn matrix containing a set of n test vectors in dimension p (with p <= n) 
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' data(DoptDesign)
#' 
#' out <- doptimal(DoptDesign)
#'
#' @export
doptimal <- function(V){
  
  #Error Checking
  stopifnot(is.matrix(V), nrow(V) <= ncol(V), is.numeric(V))
  
  #Define Variables
  blk <- matrix(list(), 3, 2)
  C <- matrix(list(), 3, 1)
  At <- matrix(list(), 3, 1)
  OPTIONS <- list(parbarrier = matrix(list(),3,1))
  
  n <- nrow(V)
  p <- ncol(V)
  
  b <- matrix(0, p, 1)
  
  blk[[1,1]] <- "s"
  blk[[1,2]] <- n
  
  Ftmp <- matrix(list(),1,p)
  
  for(k in 1:p){
    Ftmp[[1,k]] <- -V[,k] %*% t(V[,k])
  }
  
  At[1] <- svec(blk[1,,drop=FALSE],Ftmp,1)
  C[[1,1]] <- Matrix(0,n,n,sparse=TRUE)
  
  blk[[2,1]] <- "l"
  blk[[2,2]] <- p
  
  At[[2,1]] <- -diag(1,p,p)
  C[[2,1]] <- matrix(0,p,1)
  
  blk[[3,1]] <- "u"
  blk[[3,2]] <- 1
  At[[3,1]] <- matrix(1, 1, p)
  C[[3,1]] <- 1
  
  OPTIONS$parbarrier[[1,1]] <- 1
  OPTIONS$parbarrier[[2,1]] <- 0
  OPTIONS$parbarrier[[3,1]] <- 0
  
  out <- sqlp_base(blk=blk, At=At, b=b, C=C, OPTIONS = OPTIONS)
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
}