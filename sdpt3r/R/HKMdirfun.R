HKMdirfun <- function(blk, At, par, Rd, EinvRc, X, xx, m){
  
  dX <- matrix(list(), nrow(blk),1)
  dZ <- matrix(list(), nrow(blk),1)
  dy <- c()
  
  if(any(is.nan(xx)) | any(is.infinite(xx))){
    assign("solve_ok",0, pos=sys.frame(which = -3))
    stop("solution contains NaN or inf")
  }
  ##
  dy <- xx[1:m]
  count <- m
  ##
  for(p in 1:nrow(blk)){
    if(blk[[p,1]] == "l"){
      dZ[[p,1]] <- ops(matrix(list(Rd[[p,1]]),1,1), "-", Atyfun(matrix(blk[p,], nrow=1),matrix(At[p,], nrow=1),c(),c(),dy))[[1]]
      dX[[p,1]] <- EinvRc[[p,1]] - par$dd[[p,1]] * dZ[[p,1]]
    }else if(blk[[p,1]] == "q"){
      dZ[[p]] <- ops(matrix(list(Rd[[p,1]]),1,1),"-",Atyfun(blk[p,,drop=FALSE],At[p,,drop=FALSE],par$permA[p,,drop=FALSE],par$isspAy[p],dy))[[1]]
      tmp <- par$dd[[p]] * dZ[[p]] +
        qops(blk,p,qops(blk,p,as.matrix(dZ[[p]]),as.matrix(par$Zinv[[p]]),1),as.matrix(X[[p]]),3) +
        qops(blk,p,qops(blk,p,as.matrix(dZ[[p]]),as.matrix(X[[p]]),1),par$Zinv[[p]],3)
      dX[[p]] <- EinvRc[[p]] - tmp
    }else if(blk[[p,1]] == "s"){
      dZ[[p]] <- ops(matrix(list(Rd[[p,1]]),1,1), "-", Atyfun(blk[p,,drop=FALSE],At[p,,drop=FALSE],par$permA[p,,drop=FALSE],0,dy))[[1]]
      tmp <- Prod3(blk,p,X[[p]],dZ[[p]], par$Zinv[[p]],0)
      tmp <- 0.5*(tmp + t(tmp))
      dX[[p]] <- EinvRc[[p]] - tmp
    }else if(blk[[p,1]] == "u"){
      n <- sum(blk[[p,2]])
      dZ[[p]] <- matrix(0,n,1)
      dX[[p]] <- xx[count + c(1:n)]
      count <- count + n
    }
  }
  
  return(list(dX=dX, dy=dy, dZ=dZ))
}