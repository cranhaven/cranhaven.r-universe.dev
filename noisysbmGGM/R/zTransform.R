library(ppcor)

aux_ztransfor<-function(r){
  return(log((1+r)/(1-r))/2)}


zTransform<-function(X){
  n=dim(X)[1]
  p=dim(X)[2]
  samplepartialcorr=ppcor::pcor(X)$estimate
  studentStat =ppcor::pcor(X)$statistic
  gaussianStat = aux_ztransfor(samplepartialcorr)*sqrt(n-p-1)
  diag(gaussianStat)=0
  return(gaussianStat)
}
