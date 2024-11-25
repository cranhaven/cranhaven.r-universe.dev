detect_lblk <- function(blk,At,C,b,parbarrier,X,Z){
  
  blkold <- blk
  m <- max(dim(b))
  ee <- matrix(1,nrow=m,ncol=1)
  numdiagelt <- 0
  numblknew <- 0
  blockchange <- matrix(0, nrow(blk), 1)
  Acum <- matrix(list(), nrow(blk), 1)
  diagblkinfo <- matrix(list(), nrow(blk), 3)
  
  for(p in 1:nrow(blk)){
    n <- sum(blk[[p,2]])
    if(blk[[p,1]] == "s" & length(blk[[p,2]]) == 1 & ncol(At[[p,1]]) == m){
      Acumtmp <- smat(blk[p,,drop=FALSE],p, abs(At[[p,1]]) %*% ee, 0) + abs(C[[p,1]])
      Acum[[p,1]] <- Acumtmp - diag(diag(Acumtmp),n,n)
      rownorm <- t(sqrt(colSums(Acum[[p,1]] * Acum[[p,1]])))
      idxdiag <- which(rownorm < 1e-15)
      if(length(idxdiag != 0)){
        blockchange[p] <- 1
        numdiagelt <- numdiagelt + length(idxdiag)
        idxnondiag <- setdiff(c(1:n), idxdiag)
        diagblkinfo[[p,2]] <- idxdiag
        diagblkinfo[[p,3]] <- idxnondiag
        
        if(length(idxnondiag) != 0){
          numblknew <- numblknew + 1
          diagblkinfo[[p,1]] <- numblknew
        }
      }else{
        numblknew <- numblknew + 1
        diagblkinfo[[p,1]] <- numblknew
      }
    }else{
      numblknew <- numblknew + 1
      diagblkinfo[[p,1]] <- numblknew
    }
  }
  
  if(any(blockchange == 1)){
    numblk <- nrow(blkold)
    idx_keepblk <- c()
    Atmp <- matrix(list(),1,m)
    Adiag <- matrix(list(),1,m)
    C <- rbind(C,matrix(list(),1,1))
    Cdiag <- c()
    Xdiag <- c()
    Zdiag <- c()
    parbarrierdiag <- c()
    
    for(p in 1:numblk){
      n <- sum(blkold[[p,2]])
      if(blockchange[p] == 1){
        idxdiag <- diagblkinfo[[p,2]]
        idxnondiag <- diagblkinfo[[p,3]]
        if(length(idxdiag) != 0){
          blk[[p,2]] <- length(idxnondiag)
          len <- length(idxdiag)
          for(k in 1:m){
            Ak <- mexsmat(blkold, At, 0, p, k)
            tmp <- as.matrix(diag(Ak))
            Atmp[[1,k]] <- Ak[idxnondiag, idxnondiag]
            Adiag[[1,k]] <- rbind(Adiag[[1,k]], tmp[idxdiag,,drop=FALSE])
          }
          tmp <- as.matrix(diag(C[[p,1]]))
          Cdiag <- rbind(Cdiag, tmp[idxdiag,,drop=FALSE])
          C[[p,1]] <- C[[p,1]][idxnondiag, idxnondiag]
          At[p] <- svec(blk[p,,drop=FALSE],Atmp,1)
          
          if(!is.null(X) & !is.null(Z)){
            parbarrierdiag <- c(parbarrierdiag, parbarrier[[p,1]] %*% matrix(1,1,len))
            tmp <- as.matrix(diag(X[[p,1]]))
            Xdiag <- rbind(Xdiag, tmp[idxdiag,,drop=FALSE])
            tmp <- as.matrix(diag(Z[[p,1]]))
            Zdiag <- rbind(Zdiag, tmp[idxdiag,,drop=FALSE])
            X[[p,1]] <- X[[p,1]][idxnondiag, idxnondiag]
            Z[[p,1]] <- Z[[p,1]][idxnondiag, idxnondiag]
          }
        }
        if(length(idxnondiag) != 0){
          idx_keepblk = c(idx_keepblk, p)
        }
      }else{
        idx_keepblk = c(idx_keepblk, p)
      }
    }

    blk <- rbind(blk,matrix(list(),1,ncol(blk)))
    At <- rbind(At,matrix(list(),1,1))
    X <- rbind(X,matrix(list(),1,1))
    Z <- rbind(Z,matrix(list(),1,1))
    parbarrier <- rbind(parbarrier,matrix(list(),1,1))
    
    blk[[numblk+1,1]] <- "l"
    blk[[numblk+1,2]] <- numdiagelt
    if(!is.null(Cdiag)){
      C[[numblk+1,1]] <- as.matrix(Cdiag)
    }
    if(!any(sapply(Adiag,is.null))){
      At[[numblk+1,1]] <- svec(blk[numblk+1,,drop=FALSE], Adiag, 1)[[1]]
    }
    idx_keepblk <- c(idx_keepblk, numblk+1)
    blk <- blk[idx_keepblk,]
    C <- C[idx_keepblk,]
    At <- At[idx_keepblk,]
    
    if(!is.null(X) & !is.null(Z)){
      parbarrier[[numblk+1,1]] <- as.matrix(parbarrierdiag)
      X[[numblk+1,1]] <- as.matrix(Xdiag)
      Z[[numblk+1, 1]] <- as.matrix(Zdiag)
      parbarrier <- parbarrier[idx_keepblk]
      X <- X[idx_keepblk]
      Z <- Z[idx_keepblk]
    }
  }
  
  return(list(blk=blk, At=At, C=C, diagblkinfo = diagblkinfo, blockchange=blockchange, parbarrier=parbarrier, X=X, Z=Z))
  
}