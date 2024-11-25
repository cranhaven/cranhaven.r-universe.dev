NTscaling <- function(blk, X, Z, Zchol, invZchol){
  
  numblk <- nrow(blk)
  W <- matrix(list(), numblk,1)
  G <- matrix(list(), numblk,1)
  sv <- matrix(list(), numblk,1)
  gamx <- matrix(list(), numblk,1)
  gamz <- matrix(list(), numblk,1)
  dd <- matrix(list(), numblk,1)
  ee <- matrix(list(), numblk,1)
  ff <- matrix(list(), numblk,1)
  
  for(p in 1:nrow(blk)){
    numblk <- max(dim(as.matrix(blk[[p,2]])))
    n <- sum(blk[[p,2]])
    if(blk[[p,1]] == "l"){
      dd[[p,1]] <- X[[p,1]]/Z[[p,1]]
    }else if(blk[[p,1]] == "q"){
      gamx[[p]] <- sqrt(qops(blk,p,as.matrix(X[[p]]),as.matrix(X[[p]]),2))
      gamz[[p]] <- sqrt(qops(blk,p,as.matrix(Z[[p]]),as.matrix(Z[[p]]),2))
      w2 <- gamz[[p]]/gamx[[p]]
      w <- sqrt(w2)
      dd[[p]] <- qops(blk,p,1/w2,matrix(1,n,1),4)
      tt <- qops(blk,p,1/w,as.matrix(Z[[p]]),3) - qops(blk,p,w,as.matrix(X[[p]]),4)
      gamtt <- sqrt(qops(blk,p,tt,tt,2))
      ff[[p]] <- qops(blk,p,1/gamtt,tt,3)
      ee[[p]] <- qops(blk,p,sqrt(2)/w,ff[[p]],4)
    }else if(blk[[p,1]] == "s"){
      tmp <- Prod2(blk,p,Zchol[[p,1]], X[[p,1]], 0)
      tmp <- Prod2(blk,p,tmp, t(Zchol[[p,1]]), 1)
      out <- blkeig(blk,p,tmp)
      sv2 <- as.matrix(out$d)
      V <- out$V
      sv2 <- pmax(1e-20, sv2)
      sv[[p,1]] <- sqrt(sv2)
      tmp <- Prod2(blk,p,invZchol[[p,1]],V)
      G[[p,1]] <- Prod2(blk,p,diag(sqrt(c(sv[[p,1]])),n,n),t(tmp))
      W[[p,1]] <- Prod2(blk,p,t(G[[p,1]]),G[[p,1]],1)
    }
  }
  return(list(W=W,G=G,sv=sv,gamx=gamx,gamz=gamz,dd=dd,ee=ee,ff=ff))
}