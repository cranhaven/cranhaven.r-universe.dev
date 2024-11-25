blktrace <- function(blk,X,Z,parbarrier=NULL){
  
  if(is.null(parbarrier)){
    trXZ <- 0
    for(p in 1:nrow(blk)){
      if(blk[[p,1]] == "s"){
        if(length(blk[[p,2]]) == 1){
          trXZ <- trXZ + sum(sum(X[[p,1]] * Z[[p,1]]))
        }else{
          xx <- mexsvec(blk[p,,drop=FALSE],as.matrix(X[[p,1]]),0)
          zz <- mexsvec(blk[p,,drop=FALSE],as.matrix(Z[[p,1]]))
          trXZ <- trXZ + sum(xx * zz)
        }
      }else{
        trXZ <- trXZ + sum(X[[p,1]]*Z[[p,1]])
      }
    }
  }else{
    trXZ <- 0
    for(p in 1:nrow(blk)){
      if(base::norm(parbarrier[[p,1]], type="2") == 0){
        if(blk[[p,1]] == "s"){
          if(length(blk[[p,2]]) == 1){
            trXZ <- trXZ + sum(sum(X[[p,1]] * Z[[p,1]]))
          }else{
            xx <- mexsvec(blk[p,,drop=FALSE],as.matrix(X[[p,1]]),0)
            zz <- mexsvec(blk[p,,drop=FALSE],as.matrix(Z[[p,1]]))
            trXZ <- trXZ + sum(xx * zz)
          }
        }else{
          trXZ <- trXZ + sum(X[[p,1]]*Z[[p,1]])
        }
      }else{
        idx <- which(parbarrier[[p,1]] == 0)
        if(length(idx) > 0){
          if(blk[[p,1]] == "s"){
            sumXZ <- colSums(X[[p,1]] * Z[[p,1]])
            ss <- c(0,cumsum(blk[[p,2]]))
            for(k in 1:length(idx)){
              idxtmp <- c((ss[idx[k]]+1):ss[idx[k]+1])
              trXZ <- trXZ + sum(sumXZ[idxtmp])
            }
          }else if(blk[[p,1]] == "q"){
            tmp <- qops(blk,p,X[[p]],Z[[p]],1)
            trXZ <- trXZ + sum(tmp[idx])
          }else if(blk[[p,1]] == "l"){
            trXZ <- trXZ + sum(X[[p,1]][idx] * Z[[p,1]][idx])
          }
        }
      }
    }
  }
  
  return(trXZ)
  
}