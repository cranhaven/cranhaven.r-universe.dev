schurmat_qblk <- function(blk,At,par,schur,UU,EE,p,dd,ee,xx=NULL){
  
  assign("idxdenAq",matrix(list(),1,nrow(blk)), pos=sys.frame(which = -3))
  assign("nnzschur_qblk",matrix(list(),1,nrow(blk)), pos=sys.frame(which = -3))
  
  iter <- par$iter
  
  if(!is.null(xx)){
    options <- 0
  }else{
    options <- 1
  }
  
  if(length(EE) == 0){
    count <- 0
  }else{
    count <- max(max(EE[,2]), max(EE[,1]))
  }
  
  n <- sum(blk[[p,2]])
  numblk <- max(dim(as.matrix(blk[[p,2]])))
  
  Ae <- qprod(blk,p,t(At[[p]]),ee[[p]])
  if(options == 0){
    Ax <- qprod(blk,p,t(At[[p]]), xx[[p]])
  }
  idxden <- checkdense(Ae)
  ddsch <- dd[[p]]
  if(length(idxden) > 0){
    spcolidx <- setdiff(c(1:numblk), idxden)
    s <- 1 + c(0, cumsum(blk[[p,2]]))
    idx <- s[idxden]
    tmp <- matrix(0, n, 1)
    tmp[idx] <- sqrt(2*abs(ddsch[idx]))
    Ad <- qprod(blk,p,t(At[[p]]), tmp)
    ddsch[idx] <- abs(ddsch[idx])
    if(options == 0){
      len <- length(idxden)
      gamzsub <- par$gamz[[p]][idxden]
      lam <- gamzsub * gamzsub
      if(is.null(UU)){
        UU <- cbind(Ax[,idxden], Ae[,idxden] %*% Diagonal(len,lam), Ad[,idxden])
      }else{
        UU <- cbind(UU, cbind(Ax[,idxden], Ae[,idxden] %*% Diagonal(len,lam), Ad[,idxden]))
      }
      tmp <- count + c(1:len)
      if(is.null(EE)){
        EE <- rbind(c(tmp, len+tmp, -lam), c(len+tmp,tmp,-lam), c(2*len+tmp,2*len+tmp,rep(1,len)))
      }else{
        EE <- rbind(EE, rbind(c(tmp, len+tmp, -lam), c(len+tmp,tmp,-lam), c(2*len+tmp,2*len+tmp,rep(1,len))))
      }
      count <- count + 3*len
      Ax <- Ax[,spcolidx]
      Ae <- Ae[,spcolidx]
      tmp2 <- Ax %*% t(Ae)
      schur <- schur + (tmp2 + t(tmp2))
    }else{
      len <- length(idxden)
      w2 <- par$gamz[[p]]/par$gamx[[p]]
      lam <- w2[idxden]
      UU <- cbind(UU, Ae[,idxden] %*% Diagonal(len, sqrt(lam)), Ad[,idxden])
      tmp <- count + c(1:len)
      EE <- rbind(EE, cbind(c(tmp, tmp, -lam), c(len+tmp,len+tmp, rep(1,len))))
      count <- count + 2*len
      Ae <- Ae[,spcolidx]
      schur <- schur + Ae %*% t(Ae)
    }
  }else{
    if(length(which(Ae != 0)) > 0.2*length(Ae)){
      Ae <- as.matrix(Ae)
    }
    if(options == 0){
      if(length(which(Ax != 0)) > 0.2*length(Ax)){
        Ax <- as.matrix(Ax)
      }
      tmp <- Ax %*% t(Ae)
      schur <- schur + (tmp + t(tmp))
    }else{
      tmp <- Ae %*% t(Ae)
      schur <- schur + tmp
    }
  }
  if(iter == 1){
    tmp <- get("idxdenAq", pos=sys.frame(which = -3))
    if(!is.null(checkdense(t(At[[p]])))){
      tmp[[1,p]] <- checkdense(t(At[[p]]))
    }
    assign("idxdenAq",tmp, pos=sys.frame(which = -3))
  }
  if(length(get("idxdenAq", pos=sys.frame(which = -3))[[p]]) > 0){
    idxden <- get("idxdenAq", pos=sys.frame(which = -3))[[p]]
    len <- length(idxden)
    Ad <- t(At[[p]][idxden,]) %*% Diagonal(len, sqrt(abs(ddsch[idxden])))
    UU <- cbind(UU, Ad)
    tmp <- count + c(1:len)
    EE <- rbind(EE, cbind(tmp,tmp, -sign(ddsch[idxden])))
    count <- count + len
    ddsch[idxden] <- rep(0, len)
  }else if(length(which(At[[p]] != 0)) > 0.2*length(At[[p]])){
    At[[p]] <- as.matrix(At[[p]])
  }
  
  schurtmp <- t(At[[p]]) %*% Diagonal(n,ddsch) %*% At[[p]]
  schur <- Matrix(schur + schurtmp, sparse=TRUE)
  
  return(list(schur=schur, UU=UU, EE=EE))
  
}