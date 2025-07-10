
#########################################################################
## Simulate Bivariate fractional white noise

#library(fracdiff)
#library(mvtnorm)

#' @title Simulate multivariate fractional white noise.
#' @description \code{FI.sim} Simulates a 
#' @param T positive integer determining the length of the simulated series.
#' @param q positive integer determining the dimension of the simulated series.
#' @param rho real value between 0 and 1 that determines correlation between the innovations.
#' @param d vector of memory parameters with length q.
#' @param B qxq matrix specifying cointegrating relations. By default \code{diag(q)}.
#' @param var positive real value that determines the variance of the innovations.
#' Default value is \code{var=1}.
#' @param burnin positive integer determining the length of the burnin period. 
#' Default is \code{burnin=250}. 
#' @import fracdiff
#' @import mvtnorm
#' @examples
#' T=1000
#' series<-FI.sim(T=T,q=2,rho=0.7,d=c(0.4,0.4))
#' ts.plot(series, col=1:2)
#' cor(series)
#' 
#' series<-FI.sim(T=T,q=2,rho=0,d=c(0.1,0.4), B=rbind(c(1,-1),c(0,1)))
#' ts.plot(series, col=1:2)
#' @export
FI.sim<-function(T,q,rho,d,B=diag(q),var=1,burnin=250){
  
  #corr.mat<-matrix(rho,q,q)
  #diag(corr.mat)<-1
  #sigma<-var*corr.mat
  #noise<-rmvnorm((T+burnin),mean=rep(0,q),sigma=sigma)
  #series<-matrix(NA,(T+burnin),q)
  #for(a in 1:q){
  #  series[,a]<-cumsum(diffseries(noise[,a],(1-d[a])))
  #}
  #series<-series[(burnin+1):(burnin+T),]
  #if(sum(B==diag(q))!=length(as.vector(B))){
  #  invB<-solve(B)
  #  series<-t(apply(t(series),2,function(x,invB){invB%*%x}, invB=invB))
  #}
  #return(series)
  
  dim_series<-q
  if(length(d)!=dim_series)stop("d has to be a vector of length q.")
  corr.mat<-matrix(rho,dim_series,dim_series)
  diag(corr.mat)<-1
  sigma<-var*corr.mat
  noise<-rmvnorm((T+burnin),mean=rep(0,dim_series),sigma=sigma)
  series<-matrix(NA,(T+burnin),dim_series)
  for(a in 1:dim_series){
    series[,a]<-fdiff(noise[,a],-d[a])
  }
  series<-series[(burnin+1):(burnin+T),]
  if(sum(B==diag(dim_series))!=length(as.vector(B))){
    invB<-solve(B)
    series<-t(apply(t(series),2,function(x,invB){invB%*%x}, invB=invB))
  }
  return(series)
  
}



#series<-FI.sim(T=1000,q=2,rho=0.7,d=c(0.4,0.4))
#ts.plot(series, col=1:2)
#cor(series)



