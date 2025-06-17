fa_exp=function(data, group, rept = 10000){

  n       <- tapply(data, group, length)
  k       <- length(tapply(data, group, length))
  gmin    <- tapply(data, group, min)
  sj      <- tapply(data, group, function(x) sum(x-min(x)))/(n-1)
  q0      <- gmin+(((n-1)/n)*sj)
  t0      <- sum((n * q0 ^ 2) / (sj ^ 2)) - ((sum(n * q0 / (sj ^ 2))) ^ 2 / sum(n / sj ^ 2))

  tf      <- numeric(rept)
  for(i in 1:rept){
    v     <- rchisq(k, 2)
    u     <- rchisq(k, 2 * n - 2)
    fi    <- ((n-1)*u)+((n-1)*(v-2*n))
    tf[i] <- sum(fi^2/(n*u^2))-(((sum(fi/(sj*u)))^2)/sum(n/sj^2))
  }

  pvalue <- mean(tf >= t0)
  result <- matrix(c(round(pvalue,digits=4)))
  rownames(result) <- c("p-value")
  colnames(result) <- c("Fiducial Approach test")
  return(t(result))
}
