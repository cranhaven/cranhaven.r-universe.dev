Atyfun <- function(blk, At, permA=NULL, isspAy=NULL,y){
  
  if(is.null(permA)){
    ismtpermA <- 1
  }else{
    ismtpermA <- 0
  }
  
  Q <- matrix(list(), nrow(blk),1)
  
  if(is.null(isspAy)){
    isspAy <- matrix(1,nrow(blk),1)
  }
  
  for(p in 1:nrow(blk)){
    if(blk[[p,1]] == "s"){
      n <- sum(blk[[p,2]])
      m1 <- ncol(At[[p,1]])
      if(is.null(m1)){
        m1 <- 0
      }
      if(length(At[[p,1]]) > 0){
        if(ismtpermA){
          tmp <- At[[p,1]] %*% y[1:m1]
        }else{
          tmp <- At[[p,1]] %*% y[permA[p,c(1:m1)]]
        }
        #Q[[p,1]] <- smat(blk[p,,drop=FALSE],p,tmp,isspAy[p])
        Q[[p,1]] <- smat(blk[p,,drop=FALSE],p,tmp,0)
      }else{
        Q[[p,1]] <- matrix(0,n,n)
      }
      if(ncol(blk) > 2){
        len <- sum(blk[[p,3]])
        m2 <- max(dim(blk[[p,3]]))
        y2 <- y[m1+c(1:m2)]
        dd <- At[[p,3]]
        idxD <- c(0,which(diff(dd[,1]) != 0), nrow(dd))
        yy2 <- mexexpand(as.matrix(diff(idxD)),y2)
        
        tmp <- dd[,4] * yy2
        DD <- Matrix(0,len,len, sparse=TRUE)
        for(i in 1:length(dd[,2])){
          DD[dd[i,2],dd[i,3]] <- tmp[i]
        }
        Q[[p,1]] <- Q[[p,1]] + At[[p,2]] %*% DD %*% t(At[[p,2]])
      }
    }else{
      Q[[p,1]] <- At[[p,1]] %*% y
    }
  }
  return(Q)
}
