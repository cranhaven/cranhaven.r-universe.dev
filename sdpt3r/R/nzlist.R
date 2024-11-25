nzlist <- function(blk,At,par){
  
  spdensity <- par$spdensity
  smallblkdim <- par$smallblkdim
  m <- par$numcolAt
  ##
  numblk <- nrow(as.matrix(blk))
  isspA <- matrix(0, numblk,m)
  nzlistA <- matrix(list(),numblk,2)
  nzlistAsum <- matrix(list(),numblk,2)
  isspAy <- rep(0,numblk)
  nzlistAy <- matrix(list(),numblk,1)
  ##
  for(p in 1:nrow(blk)){
    if(blk[[p,1]] == "s" && (max(blk[[p,2]]) > smallblkdim || max(dim(as.matrix(blk[[p,2]]))) <= 10)){
      numblk <- max(dim(as.matrix(blk[[p,2]])))
      n <- sum(blk[[p,2]])
      n2 <- sum(blk[[p,2]] * blk[[p,2]])
      if(numblk == 1){
        nztol <- spdensity*n
        nztol2 <- spdensity*n2/2
      }else{
        nztol <- spdensity*n/2
        nztol2 <- spdensity*n2/4
      }
      
      nzlist1 <- matrix(0,1,m+1)
      nzlist2 <- c()
      nzlist3 <- matrix(0,1,m+1)
      nzlist4 <- c()
      breakyes <- rep(FALSE,2)
      Asum <- Matrix(0,n,n, sparse=TRUE)
      ##
      m1 <- ncol(At[[p,1]])
      if(is.null(m1)){
        m1 <- 0
      }
      if(m1 > 0){
        for(k in 1:m1){
          #Ak <- smat(blk,p,At[[p]][,k],0) #changed from mexsmat
          Ak <- mexsmat(blk,At,0,p,k)
          nnzAk <- length(which(Ak != 0))
          isspA[p,k] <- (nnzAk < spdensity*n2 | numblk > 1)
          if(!all(breakyes)){
            out <- which(abs(Ak) > 0, arr.ind = TRUE)
            I <- out[,1]
            J <- out[,2]
            #
            # Nonzero elemnts of Ak
            #
            if(breakyes[1] == 0){
              if(nnzAk <= nztol){
                idx <- which(I <= J)
                nzlist1[k+1] <- nzlist1[k] + length(idx)
                nzlist2 <- rbind(nzlist2,cbind(I[idx], J[idx]))
              }else{
                nzlist1[(k+1):(m+1)] <- rep(Inf,m-k+1)
                breakyes[1] <- TRUE
              }
            }
            #
            #nonzero elements of A1 + ... + Ak
            #
            if(breakyes[2] == 0){
              nztmp <- rep(0,length(I))
              if(length(I) > 0){
                for(t in 1:length(I)){
                  i <- I[t]
                  j <- J[t]
                  nztmp[t] <- Asum[i,j]
                }
              }
              #Find new nonzero positions when Ak is added to Asum
              idx <- which(nztmp == 0)
              nzlist3[k+1] <- nzlist3[k] + length(idx)
              if(nzlist3[k+1] < nztol2){
                nzlist4 <- rbind(nzlist4,cbind(I[idx], J[idx]))
              }else{
                nzlist3[(k+1):(m+1)] <- rep(Inf,m-k+1)
                breakyes[2] <- TRUE
              }
              Asum <- Asum + abs(Ak)
            }
          }
        }
      }
      if(numblk == 1){
        isspAy[p] <- (is.finite(nzlist1[m+1]) | is.finite(nzlist3[m+1]))
      }else{
        isspAy[p] <- 1
      }
      if(!is.null(nzlist1)) nzlistA[[p,1]] <- nzlist1
      if(!is.null(nzlist2)) nzlistA[[p,2]] <- nzlist2
      if(!is.null(nzlist3)) nzlistAsum[[p,1]] <- nzlist3
      if(!is.null(nzlist4)) nzlistAsum[[p,2]] <- nzlist4
      #
      # nonzero elements of (A1*y1 ... Am*ym)
      #
      if(is.finite(nzlist3[m+1])){
        if(ncol(blk) > 2){
          m2 <- length(blk[[p,3]])
          len <- sum(blk[[p,3]])
          DD <- matrix(0,len,len)
          for(t in 1:nrow(At[[p,3]])){
            DD[At[[p,3]][t,2], At[[p,3]][t,3]] <- At[[p,3]][t,4]
          }
          Asum <- Asum + abs(At[[p,2]] %*% DD %*% t(At[[p,2]]))
        }
        out <- which(Asum > 0, arr.ind = TRUE)
        I <- out[,1]
        J <- out[,2]
        if(length(I) < nztol2){
          nzlistAy[[p,1]] <- cbind(I,J)
        }else{
          nzlistAy[[p,1]] <- Inf
        }
      }else{
        nzlistAy[[p,1]] <- Inf
      }
    }
  }
  
  return(list(isspA = isspA, nzlistA = nzlistA, nzlistAsum=nzlistAsum, isspAy=isspAy, nzlistAy=nzlistAy))
  
}