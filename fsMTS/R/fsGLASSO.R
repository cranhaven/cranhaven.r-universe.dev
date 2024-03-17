fsGLASSO <- function(mts, max.lag, rho, absolute = TRUE, show.progress = TRUE, localized = FALSE) {
  k<-ncol(mts)
  if (localized){
    res<-matrix(0, k*max.lag, k)
    for (i in 1:k){
      dat <- composeYX(mts, i, max.lag)
      dat.cov<-stats::cor(dat, use="pairwise.complete.obs")
      gl<-glasso::glasso(dat.cov, rho=rho, penalize.diagonal=FALSE)
      links<-gl$wi[1,-1]
      res[,i] <- links
      if (show.progress) svMisc::progress(100*i/k)
    }
    res <- fsNames(res, mts, max.lag)
  }else{
    dat<-mts
    for (l in 1:max.lag){
      dat<-cbind(dat[-nrow(dat),], mts[-c(1:l),])
    }
    dat.cov<-stats::cor(dat, use="pairwise.complete.obs")
    gl<-glasso::glasso(dat.cov, rho=rho, penalize.diagonal=FALSE)
    res<-gl$wi[-c(1:k),1:k]
    res <- fsNames(res, mts, max.lag)
  }
  if (absolute) res <- abs(res)

  return (res)
}
