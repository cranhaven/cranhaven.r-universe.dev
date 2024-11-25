schurmat_lblk <- function(blk,At,par,schur,UU,EE,p,dd){
  
  if(length(get("idxdenAl",pos=sys.frame(which = -3))) > 0){
    idxdenAl <- get("idxdenAl", pos=sys.frame(which = -3))
  }else{
    idxdenAl <- matrix(list(), nrow=nrow(blk), ncol=1)
    assign("idxdenAl",idxdenAl, pos=sys.frame(which = -3))
  }
  
  iter <- par$iter
  n <- sum(blk[[p,2]])
  
  if(iter == 1){
    tmp <- checkdense(t(At[[p]]))
    if(!is.null(tmp)){
      idxdenAl[[p,1]] <- tmp
    }
  }
  ddsch <- dd[[p,1]]
  if(length(idxdenAl[[p,1]]) > 0){
    idxden <- idxdenAl[[p,1]]
    len <- length(idxden)
    Ad <- t(At[[p,1]][idxden,]) %*% diag(sqrt(ddsch[idxden]),len,len)
    UU <- cbind(UU,Ad)
    if(length(EE) > 0){
      count <- 0
    }else{
      count <- max(max(EE[,1]),max(EE[,2]))
    }
    tmp <- count + c(1:len)
    EE <- rbind(EE,cbind(tmp,tmp,-rep(1,len)))
    ddsch[idxden] <- 0
  }
  schurtmp <- t(At[[p,1]]) %*% diag(as.numeric(ddsch),n,n) %*% At[[p,1]]
  schur <- schur + schurtmp
  return(list(schur=schur, UU=UU, EE=EE))
}
