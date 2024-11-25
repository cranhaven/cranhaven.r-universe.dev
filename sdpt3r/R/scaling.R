scaling <- function(blk, At, C, b, X0=NULL, y0=NULL, Z0=NULL){
  
  m <- length(b)
  numblk <- nrow(blk)
  
  normA <- matrix(list(), numblk,1)
  
  for(p in 1:numblk){
    if(blk[[p,1]] == "s"){
      m1 <- ncol(At[[p,1]])
      if(is.null(m1)){
        m1 <- 0
        normAp2 <- 0
      }else{
        normAp2 <- sum(At[[p,1]] * At[[p,1]])
      }
      
      if(max(dim(blk[p,])) > 2){
        dd <- At[[p,3]]
        m2 <- m-m1
        ss <- c(0, cumsum(blk[[p,3]]))
        for(k in 1:m2){
          idx <- c((ss[k]+1):ss[k+1])
          V <- At[[p,2]][,idx]
          ii <- dd[idx,1] - ss[k]
          jj <- dd[idx,2] - ss[k]
          len <- blk[[p,3]][k]
          D <- matrix(0, len, len)
          for(i in 1:length(ii)){
            DD[ii[i], jj[i]] <- dd[idx,3][i]
          }
          tmp <- t(V) %*% V %*% D
          normAp2 <- normAp2 + sum(tmp * t(tmp))
        }
      }
      
    }else{
      normAP2 <- sum(At[[p,1]] * At[[p,1]])
    }
    normAp <- sqrt(normAp2)
    normA[[p]] <- max(1, sqrt(normAp))
  }
##  
  normb <- max(1, norm(b, type="2"))
  normC <- 0
  for(p in 1:numblk){
    normc <- max(normC, norm(C[[p]], type="F"))
  }
  normC <- max(1, normC)
##
  for(i in 1:numblk){
    if(blk[[p,1]] == "s"){
      m1 <- ncol(At[[p,1]])
      m2 <- m - m1
      At[[p,1]] <- At[[p,1]]/normA[[p]]
      if(m2 > 0){
        At[[p,3]][,3] <- At[[p,3]][,3]/normA[[p]]
      }
    }else{
      At[[p,1]] <- At[[p,1]]/normA[[p]]
    }
    C[[p]] <- C[[p]]/(normC * normA[[p]])
    if(!is.null(X0) & !is.null(y0) & !is.null(Z0)){
      X0[[p]] <- X0[[p]]/normA[[p]]
      Z0[[p]] <- Z0[[p]]/(normC * normA[[p]])
    }
  }
  b <- b/normb
  
  return(list(At=At, C=C, b=b, normA=normA, normC=normC, normb=normb, X0=X0, y0=y0, Z0=Z0))
  
}