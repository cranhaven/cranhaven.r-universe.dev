
############################################################################################
#   Simulate VARFIMA(1,d,1) in final equations form
#########################################################

#' @title Simulation of a VARFIMA(1,1) in final equations form.
#' @description \code{VARFIMA.sim} returns a sample from a VARFIMA(1,1)-process.
#' @details add details here.
#' @param phi AR(1)-parameter.
#' @param THETA MA(1)-matrix.
#' @param d.vec vector of memory parameters.
#' @param T desired sample size.
#' @param Sigma Variance-Covariance-Matrix of the innovations.
#' @param approx order of the AR-approximation that is supposed to be used. Default is \code{approx=100}.
#' @param burnin length of the burnin period that is discarded. Default is \code{burnin=100}.
#' @references Lutkepohl, H. (2007): New introduction to multiple time series analysis. Springer.
#' @examples
#' series<-VARFIMA.sim(phi=0.4, THETA=matrix(c(0,0,0,0),2,2), 
#' d.vec=c(0.4,0.3), T=1000, Sigma=matrix(c(1,0.4,0.4,1),2,2))
#' ts.plot(series, col=1:2)
#' acf(series, lag=100)
#' @export

VARFIMA.sim<-function(phi, THETA, d.vec, T, Sigma, approx=100, burnin=100){
  
q<-length(d.vec)
D<-array(0, dim=c(q,q,approx))
for(a in 1:q){D[a,a,]<-ddiffw((approx-1),d.vec[a])}
D<--D
D[,,1]<-diag(q)
PHI<-diag(q)
diag(PHI)<-phi
PHID.a<-D
PHID.b<-array(NA, dim=c(q,q,approx))
PHID.b[,,1]<--PHI
for(i in 2:approx){PHID.b[,,i]<-PHI%*%D[,,i]}
PHID<-array(NA, dim=c(q,q,approx))
PHID[,,1]<-diag(q)
for(i in 2:(approx)){PHID[,,i]<-PHID.a[,,(i)]-PHID.b[,,(i-1)]}
  
  
A<-array(NA, dim=c(q,q,approx))
A[,,1]<-diag(q)
for(i in 2:approx){A[,,i]<--THETA^(i-1)}

PHID[,,1]<--PHID[,,1]
A[,,1]<--A[,,1]

PI<-array(NA, dim=c(q,q,approx))
for(i in 1:approx){
coef<-matrix(0,q,q)
for(j in 1:(i)){
coef<-coef+A[,,j]%*%PHID[,,((i+1)-j)]
}
PI[,,i]<--coef
}

PI[,,1]<-diag(q)

rmvnorm(1,c(0,0),diag(2))

noise<-rmvnorm(n=(T+burnin),mean=rep(0,q),sigma=Sigma)
series<-matrix(0,(T+burnin+approx),q)
series[(approx+1):(T+burnin+approx),]<-noise

for(t in 1:(T+burnin)){
  aux<-0
  for(t2 in 1:(approx)){aux<-aux+(PI[,,(t2)]%*%series[(approx+1+t-t2),])}
  series[(approx+t),]<-aux
}

series<-series[-(1:(approx+burnin)),]
series
}

