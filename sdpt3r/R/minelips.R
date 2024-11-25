#' The Minimum Ellipsoid Problem
#'
#'\code{minelips} creates input for sqlp to solve the minimum ellipsoid problem -
#'given a set of n points, find the minimum volume ellipsoid that contains all the points
#'
#'@details
#' for a set of points (x1,...,xn) determines the ellipse of minimum volume that contains all points.
#' Mathematical and implementation details can be found in the vignette
#' 
#' @param V An nxp matrix consisting of the points to be contained in the ellipsoid
#' 
#' @return 
#' \item{X}{A list containing the solution matrix to the primal problem}
#' \item{y}{A list containing the  solution vector to the dual problem}
#' \item{Z}{A list containing the  solution matrix to the dual problem}
#' \item{pobj}{The achieved value of the primary objective function}
#' \item{dobj}{The achieved value of the dual objective function}
#' 
#' @examples 
#' data(Vminelips)
#' 
#' #Not Run
#' #out <- minelips(Vminelips)
#' 
#' @export
minelips <- function(V){
  
  #Error Checking
  stopifnot(is.matrix(V), is.numeric(V))
  
  if(ncol(V) > nrow(V)){
    warning("Point Configuration has higher dimension than number of points.")
  }
  
  #Define Variables
  V <- t(V)
  
  p <- nrow(V)
  m <- ncol(V)
  N <- (p+1)*m
  
  blk <- matrix(list(),2,2)
  At <- matrix(list(),2,1)
  C <- matrix(list(),2,1)
  parbarrier <- matrix(list(),2,1)
  OPTIONS <- list()
  
  blk[[1,1]] <- "s"
  blk[[1,2]] <- (p+1)*matrix(1,1,m)
  blk[[2,1]] <- "s"
  blk[[2,2]] <- p
  
  count <- 0
  Ftmp <- matrix(list(),2,2*p+p*(p-1)/2)
  
  for(j in 1:p){
    s1 <- V[j,]
    i1 <- seq(j, (p+1)*(m-1)+j, p+1)
    j1 <- seq(p+1,(p+1)*m,p+1)
    tmp <- Matrix(0, N, N,sparse=TRUE)
    for(i in 1:length(i1)){
      tmp[i1[i],j1[i]] <- s1[i]
    }
    tmp <- tmp + t(tmp)
    count <- count + 1
    Ftmp[[1,count]] <- -tmp
    Ftmp[[2,count]] <- Matrix(0,p,p,sparse=TRUE)
    Ftmp[[2,count]][j,j] <- -1
  }
  
  for(j in 2:p){
    for(k in 1:(j-1)){
      s1 <- V[k,]
      i1 <- seq(j,(p+1)*(m-1)+j,p+1)
      j1 <- seq(p+1,(p+1)*m,p+1)
      
      s2 <- V[j,]
      i2 <- seq(k,(p+1)*(m-1)+k,p+1)
      j2 <- seq(p+1,(p+1)*m,p+1)
      
      tmp1 <- Matrix(0,N,N,sparse=TRUE)
      tmp2 <- Matrix(0,N,N,sparse=TRUE)
      for(i in 1:length(i1)){
        tmp1[i1[i],j1[i]] <- s1[i]
      }
      for(i in 1:length(i2)){
        tmp2[i2[i],j2[i]] <- s2[i]
      }
      tmp <- tmp1 + tmp2
      tmp <- tmp + t(tmp)
      
      count <- count + 1
      Ftmp[[1,count]] <- -tmp
      Ftmp[[2,count]] <- Matrix(0,p,p,sparse=TRUE)
      Ftmp[[2,count]][j,k] <- -1
      Ftmp[[2,count]][k,j] <- -1
    }
  }
  
  for(j in 1:p){
    s1 <- rep(1,m)
    i1 <- seq(j,(p+1)*(m-1)+j,p+1)
    j1 <- seq(p+1,(p+1)*m,p+1)
    tmp <- Matrix(0,N,N,sparse=TRUE)
    for(i in 1:length(i1)){
      tmp[i1[i],j1[i]] <- s1[i]
    }
    tmp <- tmp + t(tmp)
    count <- count + 1
    Ftmp[[1,count]] <- -tmp
    Ftmp[[2,count]] <- Matrix(0,p,p,sparse=TRUE)
  }
  
  At <- svec(blk,Ftmp,rep(1,2))
  C[[1,1]] <- Diagonal(N,1)
  C[[2,1]] <- matrix(0,p,p)
  b <- matrix(0,p*(p+1)/2+p,1)
  
  parbarrier[[1,1]] <- 0
  parbarrier[[2,1]] <- 1
  OPTIONS$parbarrier <- parbarrier
  
  out <- sqlp_base(blk=blk, At=At, b=b, C=C, OPTIONS = OPTIONS)
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
  
}