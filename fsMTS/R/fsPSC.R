getMaxPSC<-function(mts){
  k<-ncol(mts)
  sden<-freqdom::spectral.density(mts)
  maxPSC <- matrix(0,ncol=k, nrow=k)
  res <- apply(sden$operators[,,], 3, function(x){
    S<-solve(x)
    PSC <- S/(sqrt(diag(S)*diag(S)))
    v<-(abs(PSC)^2)
    maxPSC<<-pmax(maxPSC, v)
    return(list(v))
  })
  return(maxPSC)
}
fsPSC <- function(mts, max.lag,  show.progress = TRUE) {
  k<-ncol(mts)
  #res <- simplify2array(res)
  # if (localized){
  #   res<-matrix(0, k*max.lag, k)
  #   res <- fsNames(res, mts, max.lag)
  #   for (i in 1:k){
  #     dat <- composeYX(mts, i, max.lag)
  #     mPSC<-getMaxPSC(dat)
  #     links<-mPSC[1,-1]
  #     res[,i] <- links
  #     if (show.progress) svMisc::progress(100*i/k)
  #   }
  # }else{
  #   dat<-mts
  #   for (l in 1:max.lag){
  #     dat<-cbind(dat[-nrow(dat),], mts[-c(1:l),])
  #   }
  #   res<-getMaxPSC(dat)[-c(1:k),1:k]
  #   res <- fsNames(res, mts, max.lag)
  # }
  mts<-stats::na.omit(mts)
  m<-getMaxPSC(mts)
  res<-matrix(0, k*max.lag, k)
  res <- fsNames(res, mts, max.lag)
  for (i in 1:max.lag){
    res[((i-1)*k+1):((i-1)*k+k),1:k]<-m
  }

  return (res)
}
