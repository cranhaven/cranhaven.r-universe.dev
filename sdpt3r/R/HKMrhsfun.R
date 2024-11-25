HKMrhsfun <- function(blk,At,par,X,Z,rp,Rd,sigmu,hRd=NULL,dX=NULL,dZ=NULL){
  
  ff <- matrix(list(),nrow(blk),1)
  
  m <- length(rp)
  if(!is.null(hRd)){
    corrector <- 1
  }else{
    corrector <- 0
    hRd <- matrix(0,m,1)
  }
  hEinvRc <- matrix(0,m,1)
  EinvRc <- matrix(list(), nrow(blk),1)
  rhsfree <- c()
  ##
  for(p in 1:nrow(blk)){
    n <- sum(blk[[p,2]])
    if(blk[[p,1]] == "l"){
      if(is.list(sigmu)){
        EinvRc[[p,1]] <- sigmu[[p]]/Z[[p]] - X[[p]]
      }else{
        EinvRc[[p,1]] <- sigmu/Z[[p,1]] - X[[p,1]]
      }
      Rq <- Matrix(0,n,1, sparse=TRUE)
      if(corrector & base::norm(par$parbarrier[[p,1]], type="2") == 0){
        Rq <- dX[[p,1]] * dZ[[p,1]]/Z[[p,1]]
      }else{
        tmp <- par$dd[[p,1]] * Rd[[p,1]]
        tmp2 <- mexMatvec(At[[p]],tmp,1)
        hRd <- hRd + tmp2
      }
      EinvRc[[p,1]] <- EinvRc[[p,1]] - Rq
      tmp2 <- mexMatvec(At[[p,1]], EinvRc[[p]],1)
      hEinvRc <- hEinvRc + tmp2
    }else if(blk[[p,1]] == "q"){
      if(is.list(sigmu)){
        EinvRc[[p]] <- qops(blk,p,sigmu[[p]], par$Zinv[[p]],3) - X[[p]]
      }else{
        EinvRc[[p]] <- sigmu * par$Zinv[[p]] - X[[p]]
      }
      
      Rq <- Matrix(0,n,1,sparse=TRUE)
      if(corrector & base::norm(par$parbarrier[[p]], type="2") == 0){
        ff[[p]] <- qops(blk,p,1/par$gamz[[p]], as.matrix(Z[[p]]),3)
        hdx <- qops(blk,p,par$gamz[[p]], as.matrix(ff[[p]]),5,as.matrix(dX[[p]]))
        hdz <- qops(blk,p,par$gamz[[p]],as.matrix(ff[[p]]),6,as.matrix(dZ[[p]]))
        hdxdz <- Arrow(blk,p,hdx,hdz)
        Rq <- qops(blk,p,par$gamz[[p]],as.matrix(ff[[p]]),6,hdxdz)
      }else{
        tmp <- par$dd[[p]] * Rd[[p]] + 
          qops(blk,p,qops(blk,p, as.matrix(Rd[[p]]), par$Zinv[[p]],1),as.matrix(X[[p]]),3) +
          qops(blk,p,qops(blk,p, as.matrix(Rd[[p]]), as.matrix(X[[p]]),1),as.matrix(par$Zinv[[p]]),3)
        tmp2 <- mexMatvec(At[[p,1]], tmp,1)
        hRd <- hRd + tmp2
      }
      
      EinvRc[[p]] <- EinvRc[[p]] - Rq
      tmp2 <- mexMatvec(At[[p,1]], EinvRc[[p]],1)
      hEinvRc <- hEinvRc + tmp2
    }else if(blk[[p,1]] == "s"){
      if(is.list(sigmu)){
        sigmuvec <- mexexpand(as.matrix(blk[[p,2]]),sigmu[[p,1]])
        EinvRc[[p]] <- par$Zinv[[p,1]] %*% diag(sigmuvec,n,n) - X[[p,1]]
      }else{
        EinvRc[[p]] <- sigmu*par$Zinv[[p]] - X[[p]]
      }
      Rq <- Matrix(0,n,n, sparse=TRUE)
      if(corrector & base::norm(par$parbarrier[[p]], type="2") == 0){
        Rq <- Prod3(blk,p,dX[[p]], dZ[[p]], par$Zinv[[p]],0)
        Rq <- 0.5*(Rq + t(Rq))
      }else{
        tmp <- Prod3(blk,p,X[[p]], Rd[[p]], par$Zinv[[p]],0,par$nzlistAy[[p]])
        tmp <- 0.5*(tmp + t(tmp))
        tmp2 <- AXfun(matrix(blk[p,],nrow=1),matrix(At[p,],nrow=1), par$permA[p,,drop=FALSE],matrix(list(tmp), nrow=1))
        hRd <- hRd + tmp2
      }
      EinvRc[[p]] <- EinvRc[[p]] - Rq
      EinvRctmp <- EinvRc
      EinvRctmp[[p]] <- as.matrix(EinvRctmp[[p]])
      tmp2 <- AXfun(matrix(blk[p,],nrow=1),matrix(At[p,],nrow=1),par$permA[p,,drop=FALSE],matrix(EinvRctmp[p], nrow=1))
      hEinvRc <- hEinvRc + tmp2
    }else if(blk[[p,1]] == "u"){
      rhsfree <- rbind(rhsfree, Rd[[p]])
    }
  }
  
  rhs <- rp + hRd - hEinvRc
  rhs <- rbind(rhs,rhsfree)
  
  return(list(rhs=rhs, EinvRc = EinvRc, hRd = hRd))
  
}