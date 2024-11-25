NTrhsfun <- function(blk,At,par,X,Z,rp,Rd,sigmu,hRd=NULL,dX=NULL,dZ=NULL){
  
  spdensity <- par$spdensity
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
    numblk <- max(dim(as.matrix(blk[[p,2]])))
    if(blk[[p,1]] == "l"){
      if(is.list(sigmu)){
        EinvRc[[p]] <- sigmu[[p]]/Z[[p]] - X[[p]]
      }else{
        EinvRc[[p]] <- sigmu/Z[[p]] - X[[p]]
      }
      Rq <- matrix(0,n,1)
      if(corrector & base::norm(par$parbarrier[[p]], type="2") == 0){
        Rq <- dX[[p]] * dZ[[p]]/Z[[p]]
      }else{
        tmp <- par$dd[[p]] * Rd[[p]]
        tmp2 <- mexMatvec(At[[p]],tmp,1)
        hRd <- hRd + tmp2
      }
      EinvRc[[p]] <- EinvRc[[p]] - Rq
      tmp2 <- mexMatvec(At[[p,1]], EinvRc[[p]],1)
      hEinvRc <- hEinvRc + tmp2
    }else if(blk[[p,1]] == "q"){
      if(is.list(sigmu)){
        EinvRc[[p]] <- qops(blk,p,-sigmu[[p]]/(par$gamz[[p]]^2), as.matrix(Z[[p]]),4) - X[[p]]
      }else{
        EinvRc[[p]] <- qops(blk,p,-sigmu/(par$gamz[[p]]^2), as.matrix(Z[[p]]),4) - X[[p]]
      }
      
      Rq <- Matrix(0,n,1,sparse=TRUE)
      if(corrector & base::norm(par$parbarrier[[p]], type="2") == 0){
        w <- sqrt(par$gamz[[p]]/par$gamx[[p]])
        hdx <- qops(blk,p,w, par$ff[[p]],5,as.matrix(dX[[p]]))
        hdz <- qops(blk,p,w,par$ff[[p]],6,as.matrix(dZ[[p]]))
        hdxdz <- Arrow(blk,p,hdx,hdz)
        vv <- qops(blk,p,w,par$ff[[p]],5,as.matrix(X[[p]]))
        Vihdxdz <- Arrow(blk,p, vv, hdxdz, 1)
        Rq <- qops(blk,p,w,par$ff[[p]],6,Vihdxdz)
      }else{
        tmp <- par$dd[[p]] * Rd[[p]] + 
          qops(blk,p,qops(blk,p, as.matrix(Rd[[p]]), par$ee[[p]],1),par$ee[[p]],3)
        tmp2 <- mexMatvec(At[[p,1]], tmp,1)
        hRd <- hRd + tmp2
      }
      
      EinvRc[[p]] <- EinvRc[[p]] - Rq
      tmp2 <- mexMatvec(At[[p,1]], EinvRc[[p]],1)
      hEinvRc <- hEinvRc + tmp2
    }else if(blk[[p,1]] == "s"){
      n2 <- blk[[p,2]] * (blk[[p,2]]+1)/2
      if(is.list(sigmu)){
        sigmuvec <- mexexpand(as.matrix(blk[[p,2]]),sigmu[[p]])
        tmp <- diag(sigmuvec/par$sv[[p]] - par$sv[[p]],n,n)
      }else{
        tmp <- diag(sigmu/par$sv[[p]] - par$sv[[p]],n,n)
      }
      EinvRc[[p]] <- Prod3(blk,p,t(par$G[[p]]),tmp,par$G[[p]],1)
      Rq <- matrix(0,n,n)
      if(corrector & base::norm(par$parbarrier[[p]], type="2") == 0){
        hdZ <- Prod3(blk,p,par$G[[p]], dZ[[p]], t(par$G[[p]]),1)
        hdX <- diag(qops(blk,p,t(par$parbarrier[[p]]),1/par$sv[[p]],3)-par$sv[[p,1]],n,n) - hdZ
        tmp <- Prod2(blk,p,hdX,hdZ,0)
        tmp <- 0.5*(tmp + t(tmp))
        if(numblk == 1){
          d <- par$sv[[p]]
          e <- matrix(1,blk[[p,2]],1)
          Rq <- 2*tmp/(d %*% t(e) + e %*% t(d))
        }else{
          Rq <- matrix(0,n,n)
          ss <- c(0, cumsum(blk[[p,2]]))
          for(i in 1:numblk){
            pos <- c((ss[i]+1):ss[i+1])
            d <- par$sv[[p]][pos]
            e <- matrix(1,length(pos),1)
            Rq[pos,pos] <- 2*tmp[pos,pos]/(d %*% t(e) + e %*% t(d))
          }
        }
        Rq <- Prod3(blk,p,t(par$G[[p]]), Rq, par$G[[p]],1)
      }else{
        tmp <- Prod3(blk,p,par$W[[p]], Rd[[p]], par$W[[p]],1,par$nzlistAy[[p]])
        tmp2 <- AXfun(matrix(blk[p,], nrow=1),matrix(At[p,],nrow=1), par$permA[p,],matrix(list(tmp), nrow=1))
        hRd <- hRd + tmp2
      }
      EinvRc[[p]] <- EinvRc[[p]] - Rq
      tmp2 <- AXfun(matrix(blk[p,], nrow=1),matrix(At[p,],nrow=1),par$permA[p,],matrix(EinvRc[p], nrow=1))
      hEinvRc <- hEinvRc + tmp2
    }else if(blk[[p,1]] == "u"){
      rhsfree <- rbind(rhsfree, Rd[[p]])
    }
  }
  
  rhs <- rp + hRd - hEinvRc
  rhs <- rbind(rhs,rhsfree)
  
  return(list(rhs=rhs, EinvRc = EinvRc, hRd = hRd))
  
}
