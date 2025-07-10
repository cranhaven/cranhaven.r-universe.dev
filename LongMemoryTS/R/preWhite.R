

#rm(list=ls())

##########################################################################
### Estimate VARFIMA in Final Equations Form

#library(mvtnorm)
#library(fracdiff)

#source("VARFIMA_sim.R")
#approx<-10
#q<-2
#d.vec<-c(0.4,0.2)
#phi<-0.4
#m11<-0.5
#m12<-0
#m21<-0
#m22<-0.3

#sig11<-1
#sig12<-0.9
#sig22<-1
#
#theta<-c(d.vec,phi,m11,m12,m21,m22,sig11,sig22,sig12)

#' @title Pre-whitening for application of semiparametric long memory estimator.
#' @description Given a parameter vector \code{theta} obtained using \code{VARFIMA_est}, 
#' \code{pre.White} returns the pre-whitened sample.
#' @details add details here.
#' @param theta estimated parameter vector.
#' @param data data matrix with T observations of q-dimensional process.
#' @param q dimension of the process.
#' @param approx order of the AR-approximation that is supposed to be used. Default is \code{approx=100}.
#' @references Sibbertsen, P., Leschinski, C. H., Holzhausen, M., (2015): A Multivariate Test 
#'              Against Spurious Long Memory. Hannover Economic Paper.
#' @export


pre.White<-function(theta,data,q,approx=100){

# recover parameter values from theta

T<-max(dim(data))
  
d.vec<-theta[1:q]
phi<-theta[q+1]
THETA<-matrix(theta[(q+2):(q+1+q^2)],q,q)

sigs<-theta[(q+2+q^2):(q+2*q^2)]

Sigma<-diag(q)

###################################################################################

PHID<-array(0, dim=c(q,q,approx))
PHID[,,1]<-diag(q)
diag(PHID[,,2])<-phi

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

# filter series to obtain residuals

series<-matrix(0,(T+approx),q)
series[1:approx,]<-0
series[(approx+1):(T+approx),]<-data

residuals.pre<-series

for(t in 1:(T)){
  aux<-PI[,,1]%*%series[(approx+t),]
  for(t2 in 2:(approx)){aux<-aux-(PI[,,(t2)]%*%series[(approx+1+t-t2),])}
  residuals.pre[(approx+t),]<-aux
}

residuals<-residuals.pre[-(1:(approx+1)),]

residuals
}

###########################################################################
#################################################################################

#d.vec<-c(0.4,0.4)
#phi<-0.8
#m11<-0
#m12<-0
#m21<-0
#m22<-0

#sig11<-1
#sig12<-sig21<-0.9
#sig22<-1

#theta<-c(d.vec,phi,m11,m12,m21,m22,sig11,sig22,sig12)
#M1<-matrix(theta[(q+2):(q+1+q^2)],q,q)
#sigs<-theta[(q+2+q^2):(q+2*q^2)]
#Sigma<-diag(q)
#diag(Sigma)<-sigs[1:q]
#for(a in 2:q){
#for(b in 1:(a-1)){
#Sigma[b,a]<-Sigma[a,b]<-sigs[(q+b)]
#}}

#varfima.series<-VARFIMA.sim(phi=phi,THETA=M1,d.vec=d.vec,T=500,Sigma=Sigma)
#ts.plot(varfima.series, col=1:2)
#acf(varfima.series)
#filtered<-pre.White(data=varfima.series, theta=theta, q=2)
#ts.plot(filtered, col=1:2)
#acf(filtered)

