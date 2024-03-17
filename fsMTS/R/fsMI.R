fsMI <- function(mts, max.lag, show.progress = TRUE, localized = FALSE) {
  k<-ncol(mts)
  if (localized){
    res<-matrix(0, k*max.lag, k)
    res <- fsNames(res, mts, max.lag)
    for (i in 1:k){
      dat <- composeYX(mts, i, max.lag)
      mi <- mpmi::cminjk(dat,na.rm = T)[1,-1]
      res[,i] <- mi
      if (show.progress) svMisc::progress(100*i/k)
    }
  }else{
    dat<-mts
    for (l in 1:max.lag){
      dat<-cbind(dat[-nrow(dat),], mts[-c(1:l),])
    }
    dr <- mpmi::cminjk(dat,na.rm = T)
    res<-dr[-c(1:k),1:k]
    res <- fsNames(res, mts, max.lag)
  }
  return (res)
}
