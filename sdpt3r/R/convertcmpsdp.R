convertcmpsdp <- function(blk,A,C,b){
  
  m <- length(b)
  pp <- nrow(A)
  mm <- ncol(A)
  
  if(pp != nrow(blk)){
    stop("blk and A not compatible")
  }
  
  numblk <- nrow(blk)
  iscmp <- matrix(0, numblk, m+1)
  
  for(p in 1:nrow(blk)){
    len <- ncol(A)
    if(len > 0){
    for(k in 1:len){
      if(length(A[[p,k]]) > 0){
        iscmp[p,k] <- 1 - !is.complex(A[[p,k]])
      }
    }
    }
    iscmp[p, m+1] <- 1 - !is.complex(C[[p]])
  }
  
  iscmp <- norm(iscmp, type="F")
  ##
  if(iscmp == 0){
    return(list(bblk = blk, AAt = A, CC=C, bb=b, iscmp=iscmp))
  }
  ##
  bb <- Re(b)
  bblk <- matrix(list(), nrow(blk), 2)
  for(p in 1:nrow(blk)){
    if(nrow(as.matrix(blk[[p,2]])) > ncol(as.matrix(blk[[p,2]]))){
      blk[[p,2]] <- t(blk[[p,2]])
    }
    
    if(blk[[p,1]] == "s"){
      ss <- c(0, cumsum(blk[[p,2]]))
      ss2 <- c(0, cumsum(2*blk[[p,2]]))
      n <- sum(blk[[p,2]])
      n2 <- sum(blk[[p,2]] * blk[[p,2]] + 1)/2
      AR <- matrix(list(),1,m)
      Ctmp <- Matrix(0,2*n,2*n,sparse=TRUE)
      if(nrow(as.matrix(A[[p]])) == n2 & ncol(as.matrix(A[[p]])) == m){
        Atype <- 1
      }else if(nrow(as.matrix(A[[p]])) == 1 & ncol(as.matrix(A[[p]])) == 1){
        Atype <- 2
      }else{
        stop("convertcmp: At is not properly coded")
      }
      
      for(k in 0:m){
        if(k == 0){
          Ak <- C[[p]]
        }else{
          if(Atype == 1){
            Ak <- smat(blk,p,A[[p]][,k],1)
          }else if(Atype == 2){
            Ak <- A[[p,k]]
          }
        }
        Atmp <- Matrix(0, 2*n, 2*n, sparse=TRUE)
        if(length(blk[[p,2]]) == 1){
          tmp <- rbind(cbind(Re(Ak), -Im(Ak)), cbind(Im(Ak), Re(Ak)))
          if(k == 0){
            Ctmp <- tmp
          }else{
            Atmp <- tmp
          }
        }else{
          for(j in 1:max(dim(as.matrix(blk[[p,2]])))){
            idx <- c((ss[j]+1):ss[j+1])
            Akj <- Ak[idx,idx]
            tmp <- tmp <- rbind(cbind(Re(Akj), -Im(Akj)), cbind(Im(Akj), Re(Akj)))
            idx2 <- c((ss2[j]+1):ss2[j+1])
            if(k == 0){
              Ctmp[idx2,idx2] <- tmp
            }else{
              Atmp[idx2,idx2] <- tmp
            }
          }
        }
        if(k == 0){
          CC[[p,1]] <- Ctmp
        }else{
          AR[[k]] <- Atmp
        }
      }
      bblk[[p,1]] <- "s"
      bblk[[p,2]] <- 2*blk[[p,2]]
      AAt[p,1] <- svec(bblk, p, AR)
    }else if(blk[[p,1]] == "q"){
      stop("SOCP block with complex data not allowed")
    }else if(blk[[p,1]] == "l"){
      if(!is.complex(A[[p]]) & !is.complex(C[[p]])){
        bblk[p,] <- blk[p,]
        AAt[[p,1]] <- A[[p]]
        CC[[p,1]] <- C[[p]]
      }else{
        stop("data for linear block must be real")
      }
    }else if(blk[[p,1]] == "u"){
      if(!is.complex(A[[p]]) & !is.complex(C[[p]])){
        bblk[p,] <- blk[p,]
        AAt[[p,1]] <- A[[p]]
        CC[[p,1]] <- C[[p]]
      }else{
        stop("data for linear block must be real")
      }
    }
  }
  return(list(bblk = blk, AAt = A, CC=C, bb=b, iscmp=iscmp))
}

