blkbarrier <- function(blk, X, Z, Xchol, Zchol, v){
  objadd <- rep(0,2)
  tmp <- rep(0,2)
  
  for(p in 1:nrow(blk)){
    vp <- v[[p,1]]
    idx <- which(vp > 0)
    if(length(idx) > 0){
      vpsub <- vp[idx]
      if(blk[[p,1]] == "s"){
        ss <- c(0,cumsum(blk[[p,2]]))
        logdetX <- 2*log(diag(Xchol[[p,1]]))
        logdetZ <- 2*log(diag(Zchol[[p,1]]))
        logdetXsub <- matrix(0, length(idx), 1)
        logdetZsub <- matrix(0, length(idx), 1)
        for(k in 1:length(idx)){
          idxtmp <- c((ss[idx[k]]+1):ss[idx[k]+1])
          logdetXsub <- sum(logdetX[idxtmp])
          logdetZsub <- sum(logdetZ[idxtmp])
        }
        tmp[1] <- -sum(vpsub * logdetXsub)
        tmp[2] <- sum(vpsub*logdetZsub + blk[[p,2]][idx] * vpsub * (1-log(vpsub)))
      }else if(blk[[p,1]] == "q"){
        gamX <- sqrt(qops(blk,p,X[[p]],X[[p]],2))
        gamZ <- sqrt(qops(blk,p,Z[[p]],Z[[p]],2))
        tmp[1] <- -sum(vpsub * log(gamX[idx]))
        tmp[2] <- sum(vpsub*log(gamZ[idx]) + vpsub) 
      }else if(blk[[p,1]] == "l"){
        logX <- log(X[[p,1]])
        logZ <- log(Z[[p,1]])
        tmp[1] <- -sum(vpsub * logX[idx])
        tmp[2] <- sum(vpsub * logZ(idx) + vpsub *(1-log(vpsub)))
      }
      objadd <- objadd + tmp
    }
  }
  return(objadd)
}