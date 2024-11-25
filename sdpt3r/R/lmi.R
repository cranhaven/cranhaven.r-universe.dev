#'Linear Matrix Inequality 1
#'
#'\code{lmi1} creates input for sqlp to solve a linear matrix inequality problem
#'
#'@details
#' Solves the type-1 linear matrix inequality problem. Mathematical and implementation
#' details can be found in the vignette
#' 
#' @param B An mxn real valued matrix
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' B <- matrix(c(-1,5,1,0,-2,1,0,0,-1), nrow=3)
#' 
#' #Not Run
#' #out <- lmi1(B)
#'
#' @export
lmi1 <- function(B){
  
  #Error Checking
  stopifnot(is.matrix(B), is.numeric(B))
  
  #Variables
  blk <- matrix(list(),4,2)
  At <- matrix(list(),4,1)
  C <- matrix(list(),4,1)
  
  n <- max(dim(B))
  n2 <- n*(n+1)/2
  I <- diag(n)
  z0 <- Matrix(0, n2,1, sparse=TRUE)
  
  blktmp <- matrix(list(),1,2)
  blktmp[[1,1]] <- "s"
  blktmp[[1,2]] <- n
  
  blk[[1,1]] <- "s"
  blk[[1,2]] <- n
  blk[[2,1]] <- "s"
  blk[[2,2]] <- n
  blk[[3,1]] <- "s"
  blk[[3,2]] <- n
  blk[[4,1]] <- "u"
  blk[[4,2]] <- 1
  
  At[[1,1]] <- cbind(lmifun(B,I), z0)
  At[[2,1]] <- cbind(lmifun(-I/2,I), z0)
  Itmp <- matrix(list(),1,1)
  Itmp[[1]] <- -I
  At[[3,1]] <- cbind(lmifun(I/2,I), svec(blktmp,M=Itmp,isspx=1)[[1]]) 
  At[[4,1]] <- Matrix(c(1, rep(0,n2)),nrow=1) 
  
  C[[1,1]] <- Matrix(0,n,n,sparse=TRUE) 
  C[[2,1]] <- -Diagonal(n) 
  C[[3,1]] <- Matrix(0,n,n,sparse=TRUE) 
  C[[4,1]] <- 1
  
  b <- matrix(c(rep(0,n2),-1),ncol=1) 
  
  out <- sqlp_base(blk=blk, At=At, b=b, C=C, OPTIONS = list())
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
}

#'Linear Matrix Inequality 2
#'
#'\code{lmi2} creates input for sqlp to solve a linear matrix inequality problem
#'
#'@details
#' Solves the type-2 linear matrix inequality problem. Mathematical and implementation
#' details can be found in the vignette
#' 
#' @param A1 An nxm real valued matrix
#' @param A2 An nxm real valued matrix
#' @param B An nxp real valued matrix
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' A1 <- matrix(c(-1,0,1,0,-2,1,0,0,-1),3,3)
#' A2 <- A1 + 0.1*t(A1)
#' B  <- matrix(c(1,3,5,2,4,6),3,2)
#' 
#' out <- lmi2(A1,A2,B)
#'
#' @export
lmi2 <- function(A1,A2,B){
  
  #Error Checking
  stopifnot(is.matrix(A1),
            is.matrix(A2),
            is.matrix(B),
            is.numeric(A1),
            is.numeric(A2),
            is.numeric(B),
            nrow(A1) == nrow(A2),
            nrow(A2) == nrow(B),
            ncol(A1) == ncol(A2))
  
  #Define Variables
  blk <- matrix(list(),4,2)
  At <- matrix(list(),4,1)
  C <- matrix(list(),4,1)
  
  n1 <- ncol(A1)
  n2 <- ncol(A2)
  
  n <- n1
  I <- Diagonal(n)
  blk[[1,1]] <- "s"
  blk[[1,2]] <- n
  At[[1,1]] <- lmifun(A1,I,B)
  C[[1,1]] <- Matrix(0,n,n)
  
  blk[[2,1]] <- "s"
  blk[[2,2]] <- n
  At[[2,1]] <- lmifun(A2,I,B)
  C[[2,1]] <- Matrix(0,n,n)
  
  N <- n*(n+1)/2
  dlen <- ncol(B) 
  blk[[3,1]]  <- "l"
  blk[[3,2]] <- dlen
  At[[3,1]] <- cbind(Matrix(0,dlen,N), -Diagonal(dlen)) 
  C[[3,1]] <- matrix(0,dlen,1)
  
  blk[[4,1]]  <- "u"
  blk[[4,2]] <- 1
  At[[4,1]] <- Matrix(c(rep(0,N), rep(1,dlen)), nrow=1) 
  C[[4,1]] <- 1
  
  blktmp <- matrix(list(),1,2)
  blktmp[[1,1]] <- "s"
  blktmp[[1,2]] <- n   
  b <- rbind(-svec(blktmp,M=as.matrix(I)), matrix(0,dlen,1))
  
  out <- sqlp_base(blk=blk, At=At, b=b, C=C, OPTIONS = list())
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
  
}


#'Linear Matrix Inequality 3
#'
#'\code{lmi3} creates input for sqlp to solve a linear matrix inequality problem
#'
#'@details
#' Solves the type-3 linear matrix inequality problem. Mathematical and implementation
#' details can be found in the vignette
#' 
#' @param A An nxn real valued matrix
#' @param B An mxn real valued matrix
#' @param G An nxn real valued matrix
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' A <- matrix(c(-1,0,1,0,-2,1,0,0,-1),3,3)
#' B <- matrix(c(1,2,3,4,5,6), 2, 3)
#' G <- matrix(1,3,3)
#'
#' out <- lmi3(A,B,G)
#'
#' @export
lmi3 <- function(A,B,G){
  
  #Error Checking
  stopifnot(is.matrix(A),
            is.matrix(B),
            is.matrix(G),
            is.numeric(A),
            is.numeric(B),
            is.numeric(G),
            nrow(A) == ncol(B),
            ncol(B) == nrow(G),
            ncol(A) == ncol(G))
  
  #Define Variables
  blk <- matrix(list(),1,2)
  Avec <- matrix(list(),1,1)
  C <- matrix(list(),1,1)
  
  m <- nrow(A)
  n <- ncol(A)
  m2 <- nrow(B)
  n2 <- ncol(B)

  blk[[1,1]] <- "s"
  blk[[1,2]] <- m + m2
  
  I <- Diagonal(n)  
  Atmp <- lmifun2(A,I,I,B)  
  tmp <-  rbind(Matrix(0,m,n+m2, sparse=TRUE),  cbind(Matrix(0,m2,n,sparse=TRUE), Diagonal(m2)))
  Avec[[1]] <- cbind(Atmp[[1]], svec(blk,M=as.matrix(tmp),isspx=0))
  C[[1]] <- rbind(cbind(-G,  Matrix(0,m,m2,sparse=TRUE)), Matrix(0,m2,n+m2,sparse=TRUE))
  N <- n*(n+1)/2 
  b <- rbind(matrix(0,N,1),1)
  
  out <- sqlp_base(blk=blk, At=Avec, b=b, C=C, OPTIONS = list())
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
}

# #' @export
lmifun <- function(A,B,H=NULL){
  n <- ncol(A)
  n2 <- n*(n+1)/2
  r2 <- sqrt(2)
  if(is.null(H)){
  Acell <- matrix(list(),1,n2)
  }else{
    Acell <- matrix(list(),1,n2+ncol(H))
  }
  cnt <- 1
  
  for(j in 1:n){
    Bj <- B[,j]
    Aj <- A[,j]
    for(i in 1:j){
      Ai <- A[,i]
      Bi <- B[,i]
      if(i < j){
        tmp <- Ai %*% t(Bj) + Aj %*% t(Bi)
        Acell[[cnt]] <- Matrix((tmp + t(tmp))/r2, sparse=TRUE)
      }else{
        tmp <- Ai %*% t(Bi)
        Acell[[cnt]] <- Matrix(tmp + t(tmp), sparse=TRUE)
      }
      cnt <- cnt + 1
    }
  }
  
  if(!is.null(H)){
    for(k in 1:ncol(H)){
      Hk <- H[,k,drop=FALSE]
      Acell[[cnt]] <- Matrix(Hk %*% t(Hk), sparse=TRUE)
      cnt <- cnt + 1
    }
  }
  
  blktmp <- matrix(list(),1,2)
  blktmp[[1,1]] <- "s"
  blktmp[[1,2]] <- nrow(A)
  Atmp <- svec(blktmp, M=Acell, isspx=1)
  A <- Atmp[[1]]
  
  return(A)
}

# #' @export
lmifun2 <- function(A,B,C,D){
  n <- ncol(A)
  n2 <- n*(n+1)/2
  Acell <- matrix(list(),1,n2)
  m <- nrow(D)
  ir2 <- 1/sqrt(2)
  cnt <- 1
  
  
  for(j in 1:n){
    Bj <- B[,j,drop=FALSE]
    Aj <- A[,j,drop=FALSE]
    Cj <- C[,j,drop=FALSE]
    Dj <- D[,j,drop=FALSE]
    for(i in 1:j){
      Ai <- A[,i,drop=FALSE]
      Bi <- B[,i,drop=FALSE]
      Ci <- C[,i,drop=FALSE]
      Di <- D[,i,drop=FALSE]
      if(i < j){
        tmp <- Ai %*% t(Bj) + Aj %*% t(Bi)
        tmp <- tmp + t(tmp)
        tmp2 <- Ci %*% t(Dj) + Cj %*% t(Di)
        Acell[[cnt]] <- ir2*Matrix(rbind(cbind(tmp,tmp2),cbind(t(tmp2),Matrix(0,m,m))), sparse=TRUE)
      }else{
        tmp <- Ai %*% t(Bi)
        tmp <- tmp + t(tmp)
        tmp2 <- Ci %*% t(Di)
        Acell[[cnt]] <- Matrix(rbind(cbind(tmp,tmp2),cbind(t(tmp2),Matrix(0,m,m))), sparse=TRUE)
      }
      cnt <- cnt + 1
    }
  }
  
  blktmp <- matrix(list(),1,2)
  blktmp[[1,1]] <- "s"
  blktmp[[1,2]] <- nrow(A) + nrow(D)
  A <- svec(blktmp, M=Acell, isspx=0)
  
  return(A)
}