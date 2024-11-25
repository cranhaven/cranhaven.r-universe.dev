HKMpred <- function(blk,At,par,rp,Rd,sigmu,X,Z,invZchol){
  
  Zinv <- matrix(list(),nrow=nrow(blk),1)
  dd <- matrix(list(),nrow=nrow(blk),1)
  gamx <- matrix(list(),nrow=nrow(blk),1)
  gamz <- matrix(list(),nrow=nrow(blk),1)
  
  for(p in 1:nrow(blk)){
    n <- sum(blk[[p,2]])
    numblk <- max(dim(as.matrix(blk[[p,2]])))
    if(blk[[p,1]] == "l"){
      Zinv[[p,1]] <- 1/Z[[p,1]]
      dd[[p,1]] <- X[[p,1]]/Z[[p,1]]
    }else if(blk[[p,1]] == "q"){
      gaptmp <- qops(blk,p,as.matrix(X[[p]]),as.matrix(Z[[p]]),1)
      gamz2 <- qops(blk,p,as.matrix(Z[[p]]), as.matrix(Z[[p]]),2)
      gamz[[p]] <- sqrt(gamz2)
      Zinv[[p]] <- qops(blk,p,-1/gamz2,as.matrix(Z[[p]]),4)
      dd[[p]] <- qops(blk,p,gaptmp/gamz2, matrix(1,n,1),4)
    }else if(blk[[p,1]] == "s"){
      if(numblk == 1){
        Zinv[[p,1]] <- Prod2(blk,p,invZchol[[p,1]],t(invZchol[[p,1]]),1)
        if(par$iter == 2 | par$iter==3 & !is(Zinv[[p]], "sparseMatrix")){
          Zinv[[p]] <- Zinv[[p]] + 1e-16
        }
      }else{
        Zinv[[p,1]] <- Prod2(blk,p,invZchol[[p,1]],t(invZchol[[p,1]]),1)
      }
    }
  }
  par$Zinv <- Zinv
  par$gamx <- gamx
  par$gamz <- gamz
  par$dd <- dd
  ##
  ## compute schur matrix
  ##
  m <- length(rp)
  schur <- matrix(0,m,m)
  UU <- c()
  EE <- c()
  Afree <- c()
  dX <- matrix(list(),nrow(blk),1)
  dy <- c()
  dZ <- matrix(list(),nrow(blk),1)
  ##
  for(p in 1:nrow(blk)){
    if(blk[[p,1]] == "l"){
      out <- schurmat_lblk(blk,At,par,schur,UU,EE,p,par$dd)
      schur <- out$schur
      UU <- out$UU
      EE <- out$EE
    }else if(blk[[p,1]] == "q"){
      out <- schurmat_qblk(blk,At,par,schur,UU,EE,p,par$dd, par$Zinv,X)
      schur <- out$schur
      UU <- out$UU
      EE <- out$EE
    }else if(blk[[p,1]] == "s"){
      if(length(get("schurfun", pos=sys.frame(which = -2))[[p]]) == 0){
        schur <- schurmat_sblk(blk,At,par,schur,p,X,par$Zinv)
      }else if(is.character(get("schurfun", pos=sys.frame(which = -2))[[p]])){
        schurtmp <- Matrix(0,m,m,sparse=TRUE)
        if(length(par$permZ[[p]]) > 0){
          Zpinv <- Zinv[[p]][par$permZ[[p]], par$permZ[[p]]]
          Xp <- X[[p]][par$permZ[[p]], par$permZ[[p]]]
        }else{
          Xp <- X[[p]]
          Zpinv <- Zinv[[p]]
        }
        schurfun_input <- get("schurfun", pos=sys.frame(which = -2))
        schurfun_tmp <- match.fun(schurfun_input[[p]])
        schurtmp <- schurfun_tmp(Xp,Zpinv,get("schurfun_par", pos=sys.frame(which = -2))[p,])
        schur <- schur + schurtmp
      }
    }else if(blk[[p,1]] == "u"){
      Afree <- cbind(Afree,t(At[[p]]))
    }
  }
  ##
  ## compute rhs
  ##
  out <- HKMrhsfun(blk,At,par,X,Z,rp,Rd,sigmu)
  rhs <- out$rhs
  EinvRc <- out$EinvRc
  hRd <- out$hRd
  ##
  ## solve linear system
  ##
  out <- linsysolve(par, schur, UU, Afree, EE, rhs)
  xx <- as.matrix(out$xx)
  coeff <- out$coeff
  L <- out$L
  flag <- out$flag
  
  if(flag == 1){
    return(list(par=par,dX=c(),dy=c(),dZ=c(),coeff=c(),L=c(),hRd=c()))
  }
  
  ##
  ## compute dX & dZ
  ##
  out <- HKMdirfun(blk,At,par,Rd,EinvRc,X,xx,m)
  dX <- out$dX
  dy <- out$dy
  dZ <- out$dZ
  
  return(list(par=par,dX=dX,dy=dy,dZ=dZ,coeff=coeff,L=L,hRd=hRd))
}