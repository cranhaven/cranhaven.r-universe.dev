fsIndependent <- function(mts, max.lag) {
  k<-ncol(mts)
  res<-data.frame()
  for (l in 1:max.lag){
    res<-rbind(res,diag(k))
  }
  r<-as.matrix(res)
  r <- fsNames(r, mts, max.lag)
  return (r)
}
