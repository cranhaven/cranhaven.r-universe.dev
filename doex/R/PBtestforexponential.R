pb_exp=function(data, group, rept = 10000){

  n       <- tapply(data, group, length)
  k       <- length(tapply(data, group, length))
  gmin    <- tapply(data, group, min)
  sj      <- tapply(data, group, function(x) sum(x-min(x)))/(n-1)
  q0      <- gmin+(((n-1)/n)*sj)
  t0      <- sum((n * q0 ^ 2) / (sj ^ 2)) - ((sum(n * q0 / (sj ^ 2))) ^ 2 / sum(n / sj ^ 2))

  tb      <- numeric(rept)
  for(i in 1:rept){
    v     <- rchisq(k, 2)
    u     <- rchisq(k, 2 * n - 2)
    qb    <- (sj / (2 * n)) * (v + u) - sj
    sb    <- (sj * u) / (2 * n - 2)
    tb[i] <- sum((n * qb ^ 2) / (sb ^ 2)) - ((sum(n * qb / (sb ^ 2))) ^ 2 / sum(n / sb ^ 2))
  }

  pvalue <- mean(tb >= t0)
  result <- matrix(c(round(pvalue,digits=4)))
  rownames(result) <- c("p-value")
  colnames(result) <- c("Parametric Bootstrap test")
  return(t(result))
}

