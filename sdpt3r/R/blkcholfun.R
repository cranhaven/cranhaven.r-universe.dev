blkcholfun <- function(blk,p=1,X,permX = NULL){
  
  if(is.null(permX)){
    permX <- c()
  }
  
  
  if(!is.list(X)){
    indef <- 0
    n <- max(dim(as.matrix(X)))
    if(blk[[p,1]] == "s"){
      
      if(length(permX) == 0){
        indef <- !(min(eigen(X)$values) > 0)
        Xchol <- base::chol(X)
      }else{
        indef <- !(min(eigen(X[permX,permX])$values) > 0)
        tmp <- base::chol(X[permX,permX])
        Xchol <- matrix(0,nrow(tmp),ncol(tmp))
        Xchol[,permX] <- tmp
      }
    }else if(blk[[p,1]] == "q"){
      gamx <- qops(blk,p,X,X,2)
      if(any(gamx <= 0)){
        indef <- 1
      }
      Xchol <- list()
    }else if(blk[[p,1]] == "l"){
      if(any(X <= 0)){
        indef <- 1
      }
      Xchol <- list()
    }else if(blk[[p,1]] == "u"){
      Xchol <- list()
    }
  }else{
    Xchol <- matrix(list(),nrow(X),1)
    indef <- rep(0,nrow(X))
    for(p in 1:nrow(blk)){
      out <- blkcholfun(blk,p,X[[p,1]])
      Xchol[[p,1]] <- as.matrix(out$Xchol)
      indef[p] <- out$indef
    }
  }
  return(list(Xchol=Xchol, indef=max(indef)))
  
}