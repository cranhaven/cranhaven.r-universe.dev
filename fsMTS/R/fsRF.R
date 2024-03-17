fsRF <- function(mts, max.lag, show.progress = TRUE, localized = FALSE) {
  k<-ncol(mts)

  res<-matrix(0, k*max.lag, k)
  res <- fsNames(res, mts, max.lag)
  if (localized){
    for (i in 1:k){
      dat <- composeYX(mts, i, max.lag)
      dat <- stats::na.omit(dat)
      # rf <- randomForest::randomForest(dat[,-1],dat[,1],importance = T)
      # df <- as.data.frame(randomForest::importance(rf))
      # df<-df[df$`%IncMSE`>0,]
      # df$rn<-rownames(df)
      # df$s <- i
      # res[df$rn, i]<-df$`%IncMSE`

      sname <- colnames(mts)[i]
      f<-paste0(sname," ~.")
      obj <- randomForestSRC::rfsrc(stats::as.formula(f), data = as.data.frame(dat), importance = TRUE)
      res[,sname]<-obj$importance
      if (show.progress) svMisc::progress(100*i/k)
    }
  }else{
    dat<-mts
    for (l in 1:max.lag){
      ld<-mts[-c(1:l),]
      colnames(ld)<-paste0(colnames(mts),".l",l)
      dat<-cbind(dat[-nrow(dat),], ld)
    }
    dat <- stats::na.omit(dat)
    f<-paste0("Multivar(",paste0(colnames(mts),collapse = ","),") ~.")
    mv.obj <- randomForestSRC::rfsrc(stats::as.formula(f), data = as.data.frame(dat), importance = TRUE)

    for (v in colnames(mts)){
      res[,v]<-mv.obj$regrOutput[[v]]$importance
    }
  }
  res[res<0]<-0
  return (res)
}
