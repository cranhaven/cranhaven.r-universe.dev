sortA <- function(blk, At, C, b, X0, Z0, spdensity=NULL, smallblkdim=NULL){
  
  if(length(get("spdensity", pos=sys.frame(which = -2))) > 0){
    spdensity <- get("spdensity", pos=sys.frame(which = -2))
  }else{
    assign("spdensity",0.4, pos=sys.frame(which = -2))
  }
  
  if(length(get("smallblkdim", pos=sys.frame(which = -2))) > 0){
    smallblkdim <- get("smallblkdim", pos=sys.frame(which = -2))
  }else{
    assign("smallblkdim",50, pos=sys.frame(which = -2))
  }
  
  numblk <- nrow(blk)
  m <- length(b)
  nnzA <- matrix(0,numblk,m)
  permA <- kronecker(matrix(1,numblk,1),matrix(c(1:m), nrow=1))
  permZ <- matrix(list(),nrow=nrow(blk), ncol=1)
  
  for(p in 1:nrow(blk)){
    n <- sum(blk[[p,2]])
    numblk <- length(blk[[p,2]])
    if(blk[[p,1]] == "s" & max(blk[[p,2]]) > smallblkdim){
      n2 <- sum(blk[[p,2]]*blk[[p,2]])
      n22 <- sum(blk[[p,2]]*(blk[[p,2]] + 1))/2
      m1 <- ncol(At[[p,1]])
      if(is.null(m1)){
        m1 <- 0
      }
      
      if(length(blk[[p,2]]) == 1){
        tmp <- abs(C[[p,1]]) + abs(Z0[[p,1]])
        if(length(At[[p,1]]) > 0){
          tmp <- tmp + smat(blk[p,,drop=FALSE],p,abs(At[[p,1]]) %*% matrix(1,m1,1),0)
        }
        if(length(which(tmp != 0)) < spdensity*n22){
          per <- symamdR(tmp)
          invper <- rep(0,n)
          invper[per] <- 1:n
          permZ[[p,1]] <- invper
          if(length(At[[p,1]]) > 0){
            isspAt <- is(At[[p,1]], 'sparseMatrix')
            for(k in 1:m1){
              Ak <- smat(blk[p,,drop=FALSE],p,At[[p,1]][,k],0)
              At[[p,1]][,k] <- svec(blk[p,,drop=FALSE],Ak[per,per],isspAt)
            }
          }
          C[[p,1]] <- C[[p,1]][per,per]
          Z0[[p,1]] <- Z0[[p,1]][per,per]
          X0[[p,1]] <- X0[[p,1]][per,per]
        }else{
          per <- c()
        }
        if(ncol(blk) > 2 & length(per) > 0){
          m2 <- length(blk[[p,3]])
          P <- matrix(0,n,max(per))
          for(i in 1:n){
            P[i,per[i]] <- 1
          }
          At[[p,2]] <- P %*% At[[p,2]]
        }
      }
      
      if(length(At[[p,1]]) > 0 & mexnnz(At[[p,1]]) < m*n22/2){
        for(k in 1:m1){
          Ak <- At[[p,1]][,k]
          nnzA[p,k] <- length(which(abs(Ak) > 2.2204e-16))
        }
        permAp <- order(nnzA[p,1:m1])
        At[[p,1]] <- At[[p,1]][,permAp]
        permA[p,1:m1] <- permAp
      }
    }else if(blk[[p,1]] == "q" | blk[[p,1]] == "l" | blk[[p,1]] == "u"){
      if(!is(At[[p,1]], 'sparseMatrix')){
        At[[p,1]] <- Matrix(At[[p,1]], sparse=TRUE)
      }
    }
  }
  
  return(list(At=At, C=C, X=X0, Z=Z0, permA = permA, permZ=permZ))
  
}