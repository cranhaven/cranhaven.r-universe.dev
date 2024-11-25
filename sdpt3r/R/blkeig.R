blkeig <- function(blk,p,X){
  
  spdensity <- 0.5
  
  if(!is.list(X)){
    if(blk[[p,1]] =="s"){
      blktmp <- blk[[p,2]]
      if(length(blktmp) == 1){
        out <- eigen(X)
        d <- out$values
        V <- out$vectors
      }else{
        d <- matrix(0,sum(blktmp),1)
        V <- matrix(0,max(dim(X)),max(dim(X)))
        xx <- mexsvec(blk[p,,drop=FALSE],X,0)
        blktmp2 <- blktmp *(blktmp+1)/2
        s2 <- c(0, cumsum(blktmp2))
        blksub <- matrix(list(),1,2)
        blksub[[1,1]] <- "s"
        blksub[[1,2]] <- 0
        s <- c(0, cumsum(blktmp))
        for(i in 1:length(blktmp)){
          pos <- c((s[i] +1):s[i+1])
          blksub[[2]] <- blktmp[i]
          Xsub <- mexsmat(blksub,xx[(s2[i]+1):s2[i+1]],0)
          out <- eigen(Xsub)
          lam <- out$values
          d[pos,1] <- lam
          V[pos,pos] <- out$vectors
        }
      }
    }else if(blk[[p,1]] == "l"){
      d = X
      V <- matrix(1, nrow(X), ncol(X))
    }
  }else{
    d <- matrix(list(),nrow(X), ncol(X))
    V <- matrix(list(),nrow(X), ncol(X))
    for(p in 1:nrow(blk)){
      out<- blkeig(blk,p,X[[p]])
      d[[p,1]] <- out$d
      V[[p,1]] <- out$V
    }
  }
  return(list(d=d, V=V))
}