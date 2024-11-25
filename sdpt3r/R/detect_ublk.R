detect_ublk <- function(blk, At, C, parbarrier, X, Z){
  blk2 <- blk
  At2 <- At
  C2 <- C
  parbarrier2 <- parbarrier
  X2 <- X
  Z2 <- Z
  
  numblk <- nrow(blk)
  ublkinfo <- matrix(list(), nrow=nrow(blk), ncol=3)
  tol <- 1e-14
  
  numblknew <- numblk
  
  for(p in 1:numblk){
    
    m <- ncol(At[[p,1]])
    
    if(blk[[p,1]] == "l"){
      r <- randmat(1,m,0,"n")
      Ap <- t(At[[p,1]])
      Cp <- C[[p,1]]
      ApTr <- t(r %*% Ap)
      
      dummy <- intersect(ApTr, -ApTr)
      if(length(dummy) > 0){
        II <- which(as.numeric(ApTr) %in% as.numeric(dummy))
      }else{
        II <- c()
      }
      
      if(length(II) != 0){
        tmp <- sort(abs(ApTr[II]))
        perm <- order(abs(ApTr[II]))
        idx0 <- which(diff(tmp) < tol)
        i1 <- II[perm[idx0]]
        i2 <- II[perm[idx0+1]]
        n <- blk[[p,2]]
        Api1 <- Ap[,i1]
        Api2 <- Ap[,i2]
        Cpi1 <- t(Cp[i1])
        Cpi2 <- t(Cp[i2])
        if(length(i1) != 0){
          idxzr <- which((abs(Cpi1 + Cpi2) < tol) & (colSums(abs(Api1 + Api2)) < tol))
        }else{
          idxzr <- c()
        }
        if(length(idxzr) != 0){
          i1 <- i1[idxzr]
          i2 <- i2[idxzr]
          blk2[[p,1]] <- "u"
          blk2[[p,2]] <- length(i1)
          At2[[p,1]] <- t(Ap[,i1])
          C2[[p,1]] <- Cp[i1]
          
          parbarrier2[[p,1]] <- parbarrier[[p,1]][i1]
          X2[[p,1]] <- X[[p,1]][i1] - X[[p,1]][i2]
          Z2[[p,1]] <- matrix(0,nrow=length(i1),1)
          
          i3 <- setdiff(c(1:n), union(i1,i2))
          if(length(i3) > 0){
            numblknew <- numblknew + 1
            blk2[[numblknew,1]] <- "l"
            blk2[[numblknew,2]] <- length(i3)
            At2[[numblknew,1]] <- t(Ap[,i3])
            C2[[numblknew,1]] <- Cp[i3]
            
            parbarrier2[[numblknew,1]] <- parbarrier[[p,1]][i3]
            X2[[numblknew,1]] <- X[[p,1]][i3]
            Z2[[numblknew,1]] <- Z[[p,1]][i3]
          }
          ublkinfo[[p,1]] <- i1
          ublkinfo[[p,2]] <- i2
          ublkinfo[[p,3]] <- i3
        }
      }
    }
  }
  return(list(blk2=blk2, At2=At2, C2=C2, ublkinfo=ublkinfo, parbarrier2=parbarrier2, X2=X2, Z2=Z2))
}