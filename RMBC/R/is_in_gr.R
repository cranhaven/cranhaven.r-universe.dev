#' is_in_gr
#' 
#' Given Y  data  and a set of mixture parameters, this function
#' returns a  boolean vector B whose length is equal than Y length.
#' A[i] is TRUE if only if Y[i] does not belong to the  union of
#' confidence ellipsoids of level given by the cutoff parameter. 

#' @param Y A matrix of size n x p.
#' @param theta.mu The estimated centers: A list with K elements, each of them 
#' is an array of length p.
#' @param theta.sigma The estimated scatter matrices: A list with K matrices, each of them 
#' has dimension p x p 
#' @param cutoff   quantiles of chi-square to be used as a threshold for outliers 
#' detection, defaults to 0.999
#' @return  A boolean vector of length n, true at j-th location indicates that the 
#' j-th element of Y is a regular observation (that is, it is not considered as an outlier) 
#'
#' @importFrom  stats mahalanobis
#' @importFrom  stats qchisq
#'  
#' @export

is_in_gr=function(Y,cutoff=0.999,theta.mu,theta.sigma){
  Y=as.matrix(Y)
  actualk=length(theta.mu)
  n=dim(Y)[1]; p=dim(Y)[2]
  value=qchisq(cutoff,df=p)
  d=matrix(0,nrow=n,ncol=actualk)
  is_in_gr_out=rep(F,n)
  for (j in 1:length(theta.mu)){
    d[,j]=mahalanobis(x=Y ,center= theta.mu[[j]],cov=MASS::ginv(theta.sigma[[j]]),inverted = TRUE)
  }
  for (i in 1:n){
    is_in_gr_out[i]=any(d[i,]<value)
  }
  A=is_in_gr_out
  A 
}