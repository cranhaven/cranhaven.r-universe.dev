fsDistance <- function(mts, max.lag, shortest, step=1) {
  n <- ncol(mts)
  lagMatrix <- round(shortest / step)
  lagMatrix[is.infinite(lagMatrix)]<-0
  res<-data.frame()
  for (i in 1:max.lag){
    res<-rbind(res,ifelse(lagMatrix==i,1,0))
  }
  res <- fsNames(as.matrix(res), mts, max.lag)
  return (res)
}
