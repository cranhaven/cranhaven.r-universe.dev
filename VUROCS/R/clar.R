#' @title Cumulative LGD Accuracy Ratio
#' @description Calculates for a vector of realized categories \code{y} and a vector of predicted categories \code{hx} the cumulative LGD accuarcy ratio (CLAR) according to Ozdemir and Miu 2009.
#' @param hx a vector of  predicted values.
#' @param y a vector of  realized values.
#' @return The function returns the CLAR for a vector of realized categories \code{y} and a vector of predicted categories \code{hx}.
#' @examples clar(rep(1:5,each=3),c(3,3,3,rep(2:5,each=3)))
#' @references Ozdemir, B., Miu, P., 2009. Basel II Implementation. A Guide to Developing and Validating a Compliant Internal Risk Rating System. McGraw-Hill, USA.

clar<-function(y,hx){

  if (any(is.na(hx)) | any(is.na(y))) {
    stop("\n both 'hx' and 'y' must not contain NA values")
  }
  
  if (length(hx)!=length(y)) {
    stop("\n both 'hx' and 'y' must be of the same length")
  }
  
  nx=length(hx)
  classes<-sort(union(unique(hx),unique(y)),decreasing=TRUE)
  num<-rep(NA,length(classes))
  for(i in 1:length(classes)){
    num[i]<-sum(hx==classes[i])
  }
  cnum=cumsum(num)
  index=order(hx,decreasing=TRUE)
  hx=hx[index]
  y=y[index]
  corr<-rep(0,length(classes))
  for(i in 1:(length(classes)-1)){
    if(cnum[i]>0) corr[i]=sum(y[1:cnum[i]]>=classes[i])
  }
  corr=corr/nx
  corr[length(classes)]=1
  obs=cnum/nx
  
  res=obs[1]*corr[1]/2
  for(i in 2:length(corr)){
    res=res+(corr[i]-corr[i-1])*(obs[i]-obs[i-1])/2+(obs[i]-obs[i-1])*corr[i-1]
  }
  res*2
}
