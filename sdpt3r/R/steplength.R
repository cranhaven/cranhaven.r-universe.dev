steplength <- function(blk,X,dX,Xchol,invXchol){
  
  xstep <- rep(0, nrow(blk))
  
  for(p in 1:nrow(blk)){
    numblk <- max(dim(as.matrix(blk[[p,2]])))
    pblksize <- sum(blk[[p,2]])
    if(any(is.nan(as.matrix(dX[[p]]))) | any(is.infinite(dX[[p]]))){
      xstep <- 0
      break
    }
    if(blk[[p,1]] == "s"){
      if(max(blk[[p,2]]) > 200){
        use_lanczos <- 1
      }else{
        use_lanczos <- 0
      }
      if(use_lanczos){
        tol <- 1e-3
        maxit <- max(min(pblksize,30), round(sqrt(pblksize)))
        out <- lanczosfun(Xchol[[p]], -dX[[p]], maxit, tol)
        lam <- out$lam
        delta <- out$delta
        res <- out$res
        d <- lam+delta
      }else{
        if(length(invXchol[[p]]) == 0){
          invXchol[[p]] <- solve(Xchol[[p]])
        }
        tmp <- Prod2(blk,p,dX[[p]], invXchol[[p]],0)
        M <- Prod2(blk,p,t(invXchol[[p]]),tmp,1)
        d <- blkeig(blk,p,-M)[[1]]
      }
      tmp <- max(d) + 1e-15*max(abs(d))
      if(tmp > 0){
        xstep[p] <- 1/max(tmp)
      }else{
        xstep[p] <- 1e12
      }
    }else if(blk[[p,1]] == "q"){
      aa <- qops(blk,p,as.matrix(dX[[p]]),as.matrix(dX[[p]]),2)
      bb <- qops(blk,p,as.matrix(dX[[p]]),as.matrix(X[[p]]),2)
      cc <- qops(blk,p,as.matrix(X[[p]]),as.matrix(X[[p]]),2)
      dd <- bb^2 - aa*cc
      tmp <- pmin(aa,bb)
      idx <- which(dd > 0 & tmp < 0)
      steptmp <- 1e12*matrix(1,numblk,1)
      if(length(idx) > 0){
        steptmp[idx] <- -(bb[idx] + sqrt(dd[idx]))/aa[idx]
      }
      idx <- which(abs(aa) < .Machine$double.eps & bb < 0)
      if(length(idx) > 0){
        steptmp[idx] <- -cc[idx]/(2*bb[idx])
      }
      ##
      ##
      ##
      ss <- 1 + c(0, cumsum(blk[[p,2]]))
      ss <- ss[1:length(blk[[p,2]])]
      dX0 <- dX[[p]][ss]
      X0 <- X[[p]][ss]
      idx <- which(dX0 < 0 & X0 > 0)
      if(length(idx) > 0){
        steptmp[idx] <- pmin(steptmp[idx], -X0[idx]/dX0[idx])
      }
      xstep[p] <- min(steptmp)
    }else if(blk[[p,1]] == "l"){
      idx <- which(dX[[p]] < 0)
      if(length(idx) > 0){
        xstep[p] <- min(-X[[p]][idx]/dX[[p]][idx])
      }else{
        xstep[p] <- 1e12
      }
    }else if(blk[[p,1]] == "u"){
      xstep[p] <- 1e12
    }
  }
  xstep <- min(xstep)
  return(list(xstep=xstep, invXchol=invXchol))
}
