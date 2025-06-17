gpv_exp=function(data, group, rept = 10000){

  n         <- tapply(data, group, length)
  k         <- length(tapply(data, group, length))
  gmin      <- tapply(data, group, min)
  sj        <- tapply(data, group, function(x) sum(x-min(x)))/(n-1)

  tr <- numeric(rept)
  t0 <- numeric(rept)
  for(j in 1 : rept){
    v       <- rchisq(k, 2)
    u       <- rchisq(k, 2 * n - 2)
    rl      <- gmin + ((n-1)*sj*(2*n-v)/(n*u))
    mr0     <- mean(rl)
    sr0     <- var(rl)
    mr      <- gmin + (((n-1)^2)*sj/((n^2)-(2*n)))
    sr      <- ((n-1)*(sj^2)/((n^2)*(n-2)))*(((n^2-2*n+3)/(n-3))-(((n-1)^2)/(n-2)))
    tr[j]   <- (sum(((rl-mr)^2)/sr))-(sum((rl-mr)/sr)^2)/sum(1/sr)
    t0[j]   <- sum((mr^2)/sr)-(sum(mr/sr)^2)/(sum(1/sr))
  }

  pvalue <- mean(tr >= t0)
  result <- matrix(c(round(pvalue,digits=4)))
  rownames(result) <- c("p-value")
  colnames(result) <- c("Generalized p-value test")
  return(t(result))
}
