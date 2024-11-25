AXfun <- function(blk, At, permA=NULL, X){
  
  if(is.null(permA)){
    ismtpermA <- 1
  }else{
    ismtpermA <- 0
  }
  
  for(p in 1:nrow(blk)){
    if(blk[[p,1]] == "s"){
      m1 <- ncol(At[[p,1]])
      if(is.null(m1)){
        m1 <- 0
      }
      if(p == 1){
        if(ncol(blk) > 2){
          m2 <- max(dim(blk[[p,3]]))
        }else{
          m2 <- 0
        }
        m <- m1 + m2
        AX <- matrix(0,m,1)
        tmp <- matrix(0,m,1)
      }
      if(length(At[[p,1]]) > 0){
        if(ismtpermA){
          tmp <- t(t(svec(blk[p,,drop=FALSE],X[[p,1]])) %*% At[[p,1]])
        }else{
          tmp[permA[p,1:m1],1] <- as.numeric(t(t(svec(blk[p,,drop=FALSE],X[[p,1]])) %*% At[[p,1]]))
        }
      }
      if(ncol(blk) > 2){
        m2 <- max(dim(blk[[p,3]]))
        dd <- At[[p,3]]
        len <- sum(blk[[p,3]])
        DD <- Matrix(0,len,len, sparse=TRUE)
        for(i in 1:length(dd[,3])){
          DD[dd[i,2],dd[i,3]] <- dd[i,4]
        }
        XVD <- X[[p,1]] %*% At[[p,2]] %*% DD
        if(max(dim(X[[p,1]])) > 1){
          tmp2 <- as.matrix(colSums(At[[p,2]] * XVD))
        }else{
          tmp2 <- t(At[[p,2]] * XVD)
        }
        tmp[m1+(1:m2)] <- mexqops(blk[[p,3]],tmp2,matrix(1,length(tmp2),1),1)
      }
      AX <- AX + tmp
    }else{
      if(p == 1){
        m <- ncol(At[[p,1]])
        AX <- matrix(0,m,1)
        tmp <- matrix(0,m,1)
      }
      AX <- AX + t(t(X[[p,1]]) %*% At[[p,1]])
    }
  }
  return(AX)
}