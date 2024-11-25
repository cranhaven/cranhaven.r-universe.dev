# #' @export
distanceGeometry <- function(D){ #D is a distance matrix
  
  blk <- matrix(list(),2,2)
  At <- matrix(list(),2,1)
  C <- matrix(list(),2,1)
  
  m <- length(which(D != 0))/2
  n <- nrow(D)
  
  blk[[1,1]] <- "s"
  blk[[1,2]] <- n
  blk[[2,1]] <- "l"
  blk[[2,2]] <- 2*m
  
  C[[1]] <- Matrix(0,n,n)
  C[[2]] <- matrix(1,2*m,1)
  
  b <- matrix(0,m,1)
  cnt <- 0
  AA <- matrix(list(),1,m)
  
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      if(D[i,j] != 0){
        cnt <- cnt + 1
        AA[[cnt]] <- Matrix(0,n,n)
        AA[[cnt]][i,i] <- 1
        AA[[cnt]][i,j] <- -1
        AA[[cnt]][j,i] <- -1
        AA[[cnt]][j,j] <- 1
        b[cnt] <- D[i,j]^2
      }
    }
  }
  
  At[[1]] <- svec(blk[1,,drop=FALSE],AA)[[1]]
  At[[2]] <- cbind(-diag(m),diag(m))
  
  out <- sqlp_base(blk=blk, At=At, b=b, C=C, OPTIONS = list())
  dim(out$X) <- NULL
  dim(out$Z) <- NULL
  
  return(out)
  
}