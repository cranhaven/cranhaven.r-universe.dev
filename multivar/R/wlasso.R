#' @export
wlasso <- function(B, Z, Y, W, k, d, lambda1,eps,intercept=FALSE){
  
  if(!"matrix"%in%class(Y)){
    Y <- matrix(Y,nrow=1)
  }

  if(intercept){
    
    YMean <- c(apply(Y, 1, mean))
    ZMean <- c(apply(Z, 1, mean))
    Y <- Y - YMean %*% t(c(rep(1, ncol(Y))))
    Z <- Z - ZMean %*% t(c(rep(1, ncol(Z))))
    
  }else{
    YMean <- rep(0,nrow(Y))
    ZMean <- rep(0,nrow(Z))
  }
  
  Y <- t(Y)

  tk <- 1/max(Mod(eigen(Z%*%t(Z),only.values=TRUE)$values))

  nc <- apply(B,3,ncol)[1]

  BFOO1 <- B[,2:ncol(B[,,1]),1] 
  BFOO  <- B[,2:nc,,drop=F]     
  
  beta <- lamloopFISTA(
    BFOO, 
    Y, 
    Z, 
    W,
    as.matrix(lambda1),
    eps,
    as.matrix(YMean), 
    as.matrix(ZMean), 
    BFOO1,
    tk)

  return(beta)
}








