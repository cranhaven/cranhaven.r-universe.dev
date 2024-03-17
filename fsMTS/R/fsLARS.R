fsLARS <- function(mts, max.lag, show.progress = TRUE) {
  k<-ncol(mts)
  res<-matrix(0, k*max.lag, k)
  res<-fsNames(res, mts, max.lag)
  for (i in 1:k){
    dat <- composeYX(mts, i, max.lag)
    dat <- stats::na.omit(dat)
    reg <- lars::lars(dat[,-1],dat[,1],type="lar")
    coef1 <- reg$beta
    s1 <- apply(abs(coef1), 1, sum)
    s1 <- s1/max(s1)
    s1<-s1[-1]
    s2 <- apply(abs(coef1), 2, function(c)sum(c!=0))
    sn<-names(sort(s2,  decreasing = T))
    res[sn,i] <- 1-as.vector(s1)
    if (show.progress) svMisc::progress(100*i/k)
  }
  return (res)
}
