#' Upper Triangular Vectorization
#'
#'\code{svec} takes the upper triangular matrix (including the diagonal) and vectorizes
#'it column-wise.
#'
#'@param blk 1x2 matrix detailing the type of matrix ("s", "q", "l", "u"), and the size of the matrix
#'@param M matrix which is to be vectorized
#'@param isspx if M is sparse, isspx = 1, 0 otherwise. Default is to assume M is dense.
#'
#'@return
#'\item{x}{vector of upper triangular components of x}
#'
#'@examples
#'
#'data(Hnearcorr)
#'blk <- matrix(list(),1,2)
#'blk[[1]] <- "s"
#'blk[[2]] <- nrow(Hnearcorr)
#'
#'svec(blk,Hnearcorr)
#'
#' @export
svec <- function(blk,M,isspx=NULL){
  
  if(is.vector(blk)){
    blk = cbind(list(names(blk)), list(blk))
  }
  
  if(is.list(M)){
    M = matrix(M, nrow=nrow(blk))
  }
  
  if(is.list(M)){
    if(is.null(isspx)){
      isspx <- matrix(1,nrow=nrow(blk), ncol=1)
    }else{
      if(length(isspx) < nrow(blk)){
        isspx <- matrix(1,nrow=nrow(blk), ncol=1)
      }
    }
    
    x <- matrix(list(),nrow=nrow(blk), ncol=1)
    for(p in 1:nrow(blk)){
      n <- sum(blk[[p,2]])
      m <- ncol(M)
      if(blk[[p,1]] == "s"){
        n2 <- sum(blk[[p,2]] * (blk[[p,2]] + 1))/2
        if(isspx[p]){
          x[[p,1]] <- Matrix(0,n2,m)
        }else{
          x[[p,1]] <- matrix(0,n2,m)
        }
        numblk <- length(blk[[p,2]])
        if(all(blk[[p,2]] > 0)){
          for(k in 1:m){
            if(numblk > 1 & !is(M[[p,k]],"sparseMatrix")){
              x[[p,1]][,k] <- mexsvec(blk[p,,drop=FALSE],as.matrix(M[[p,k]]),isspx[p])
            }else{
              x[[p,1]][,k] <- mexsvec(blk[p,,drop=FALSE],as.matrix(M[[p,k]]),isspx[p])
            }
          }
        }
      }else{
        x[[p,1]] <- matrix(0,n,m)
        for(k in 1:m){
          x[[p,1]][,k] <- M[[p,k]]
        }
      }
    }
  }else{
    if(blk[[1]] == "s"){
      numblk <- length(blk[[2]])
      if(numblk > 1 & !is(M, "sparseMatrix")){
        x <- mexsvec(blk,as.matrix(M),1)
      }else{
        x <- mexsvec(blk,as.matrix(M))
      }
    }else{
      x = M
    }
  }
  
  return(x)
  
}