join.marginal <- function(X,den_est){
  X_cols <- split(as.matrix(X), col(X))
  
  res <- mapply(function(x, den) {
    if(den$den == "normal"){
      return(dnorm(x,den$mu,den$sd))
    }else{
      return(dkde1d(x,den$kernel))
    }
  },x = X_cols,den = den_est$den, SIMPLIFY = FALSE)
  
  res <- as.data.frame(do.call(cbind, res))
  
  U <- mapply(function(x, den) {
    if(den$den == "normal"){
      return(pnorm(x,den$mu,den$sd))
    }else{
      return(pkde1d(x,den$kernel))
    }
  },x = X_cols,den = den_est$den, SIMPLIFY = FALSE)
  
  U <- do.call(cbind, U)
  colnames(U) <- colnames(X)
  res[res == 0] <- 1e-200
  res <- apply(res, 1, function(x)  sum(log(x)))
  return(list(den = res, U = U))
}


