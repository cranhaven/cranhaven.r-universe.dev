sqlpu2lblk <- function(blk,At,C,X,Z,par,convertlen){
  
  ublk2lblk <- matrix(0,nrow=nrow(blk), ncol=1) 
  ublkidx <- matrix(list(),nrow=nrow(blk), ncol=2)
  
  for(p in 1:nrow(blk)){
    n0 <- sum(blk[[p,2]])
    if(blk[[p,1]] == "u" & all(blk[[p,2]] > 0)){
      ublk2lblk[p] <- 1
      if(blk[[p,2]] > convertlen){
        return(list(blk=blk,At=At,C=C,X=X,Z=Z,u2lblk=ublk2lblk,ublkidx=ublkidx))
      }
      
      AAt <- At[[p]] %*% t(At[[p]])
      AAt <- mexschurfun(AAt, 1e-15*pmax(1,diag(AAt)))
      indef <- 0
      
      L <- list(R=c(),
                perm = c(),
                d = c())
      
      indef <- !is.positive.definite(.5*(AAt+t(AAt)))
      L$R <- chol(AAt, pivot=TRUE)
      L$perm <- attr(L$R, "pivot")
      L$d <- diag(L$R)^2
      
      if(!indef & max(L$d)/min(L$d) < 1e6){
        ublk2lblk <- 0
      }else{
        dd <- matrix(0, length(L$perm),1)
        dd[L$perm,1] <- abs(L$d)
        idxN <- which(dd < 1e-11 * mean(L$d))
        idxB <- setdiff(c(1:n0), idxN)
        ddB <- dd[idxB]
        ddN <- dd[idxN]
        
        if(length(ddN) > 0 & length(ddB) > 0 & min(ddB)/max(ddN) < 10){
          idxN <- c()
          idxB <- 1:n0
        }
        
        ublkdix[[p,1]] <- n0
        ublkidx[[p,2]] <- idxN
        if(length(idxN) > 0){
          restol <- 1e-8
          out <- findcoeff(t(At[[p]]), idxB, idxN)
          W <- out$W
          resnorm <- out$resnorm
          
          resnorm <- c(resnorm, base::norm(C[[p]][idxN] - t(W) %*% C[[p]][idxB], type="2"))
          
          if(max(resnorm) < restol){
            feasible <- 1
            blk[[p,2]] <- length(idxB)
            Atmp <- t(At[[p]])
            At[[p]] <- t(Atmp[,idxB])
            C[[p]] <- C[[p]][idxB]
            X[[p]] <- X[[p]][idxB]
            Z[[p]] <- Z[[p]][idxB]
          }
        }
      }
    }
  }
  return(list(blk=blk,At=At,C=C,X=X,Z=Z,u2lblk=ublk2lblk,ublkidx=ublkidx))
} 