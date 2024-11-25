#' Toeplitz Approximation Problem
#'
#'\code{toep} creates input for sqlp to solve the Toeplitz approximation problem -
#'given a symmetric matrix F, find the nearest symmetric positive definite Toeplitz matrix.
#'
#'@details
#' For a symmetric matrix A, determines the closest Toeplitz matrix. Mathematical and implementation
#' details can be found in the vignette
#' 
#' @param A A symmetric matrix
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' data(Ftoep)
#' 
#' #Not Run
#' #out <- toep(Ftoep)
#' 
#' @export
toep <- function(A){
  
  #Error Checking
  stopifnot(is.matrix(A), is.numeric(A), nrow(A) == ncol(A), isSymmetric(A,check.attributes = FALSE))
  
  #Define Variables
  
  n <- max(dim(A))
  gam <- sqrt(c(n,2*seq(n-1,1,-1)))
  q <- matrix(0,n,1)
  q[1] <- -sum(diag(A))
  for(k in 1:(n-1)){
    tmp <- c()
    #Get kth diagonal
    for(i in 1:(nrow(A)- k)){
      tmp <- c(tmp,A[i,i+k])
    }
    q[k+1] <- -2*sum(tmp)
  }
  beta <- norm(A,type="F")^2
  
  blk <- matrix(list(),2,2)
  C <- matrix(list(),2,1)
  At <- matrix(list(),2,1)
  
  blk[[1,1]] <- "s"
  blk[[1,2]] <- n+1
  blk[[2,1]] <- "s"
  blk[[2,2]] <- n+1
  
  b <- matrix(c(rep(0,n),-1),ncol=1)
  
  C[[1,1]] <- Matrix(0,n+1,n+1,sparse=TRUE)
  C[[2,1]] <- Diagonal(n+1,c(rep(1,n),-beta))
  
  Acell <- matrix(list(),1,n+1)
  Acell[[1]] <- -Diagonal(n+1,c(rep(1,n),0))
  tmpvec <- c(rep(-1,n),0)
  for(k in 1:(n-1)){
    tmp <- Matrix(0, n+1,n+1,sparse=TRUE)
    for(j in 1:(nrow(tmp)-k)){
      tmp[j,j+k] <- tmpvec[j+k]
    }
    Acell[[k+1]] <- tmp + t(tmp)
  }
  Acell[[n+1]] <- Matrix(0,n+1,n+1,sparse=TRUE)
  Acell[[n+1]][n+1,n+1] <- -1
  At[[1,1]] <- svec(blk[1,,drop=FALSE],Acell,1)[[1]]
  
  for(k in 1:n){
    Acell[[k]] <- Matrix(0,n+1,n+1)
    Acell[[k]][k,n+1] <- -gam[k]
    Acell[[k]][n+1,k] <- -gam[k]
    Acell[[k]][n+1,n+1] <- 2*q[k]
  }
  At[[2,1]] <- svec(blk[2,,drop=FALSE],Acell,1)[[1]]

  out <- sqlp_base(blk=blk, At=At, b=b, C=C)
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
  
}