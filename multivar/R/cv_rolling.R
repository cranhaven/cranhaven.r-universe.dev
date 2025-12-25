#' @export
cv_rolling <- function(B, Z, Y, W, Ak, k, d, lambda1, ratios, t1, t2, eps,intercept=FALSE, cv, nfolds){
  
  t1  <- t1[1]
  t2  <- t2[1]
  t3k <- cumsum(unlist(lapply(Ak,function(x){nrow(x)})))
  t2k <- c(t2, c(t3k+t2)[-k])
  t1k <- c(t1, c(t3k+t1)[-k])
  t0k <- c(1, c(t3k+1)[-k])
  
  rw_n <- length(c(1:(t2-t1)))
  MSFE <- matrix(NA, nrow = rw_n, ncol = nrow(lambda1)*length(ratios))
  pb   <- txtProgressBar(1, rw_n, style=3)
  
  for(rw_idx in 1:rw_n){
    
    setTxtProgressBar(pb, rw_idx)
    
    train_idx <- unlist(lapply(1:k,function(a){c(t0k[a]:(t1k[a]+(rw_idx-1)))}))
    test_idx  <- unlist(lapply(1:k,function(a){c(t1k[a]+rw_idx)}))
    
    #cat("index: ", rw_idx, "\n")
    #cat("test : ", train_idx, "\n")
    #cat("train: ", test_idx, "\n\n")
  
    beta <- wlasso(B, Z[,train_idx], Y[,train_idx], W, k, d, lambda1,eps,intercept)

    # Calculate h-step MSFE for each penalty parameter
    for (ii in 1:dim(beta)[3]) {
      MSFE[rw_idx,ii] <- norm2(Y[,test_idx,drop=F]- beta[,-1,ii] %*% Z[,test_idx,drop=F] )^2
    }
  }
  
  beta <- wlasso(B, Z, Y, W, k, d, lambda1,eps,intercept)
  
  return(list(beta,MSFE))
  
}