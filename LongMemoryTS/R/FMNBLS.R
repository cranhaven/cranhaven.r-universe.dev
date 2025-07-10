

##############################################################################
#                                                                            #
#              FMNBLS Estimator of Nielsen and Frederiksen (2011, EJ)        #
#                                                                            #
##############################################################################



#library(longmemo)
#library(QZ)

# set directory
#setwd("D:\\Dropbox\\Multivariate Breaks\\R codes")   
#setwd("C:\\Users\\leschinski\\Dropbox\\Multivariate Breaks\\R codes")
#setwd("C:\\Users\\Christian\\Dropbox\\Multivariate Breaks\\R codes")


#source("localW.R") 
#source("Hou_Perron.R")
#source("FDLS.R")

#' helper function
#'@keywords internal
LAMBDA_m<-function(d.vec, m, T){
  lambda_m<-2*pi*m/T
  if(length(d.vec)>1){diag(lambda_m^d.vec)}else{lambda_m^d.vec}
}

#' helper function
#'@keywords internal
F.tilde<-function(a,b,da,db,k,l){
  a<-as.matrix(a)
  b<-as.matrix(b)
  T<-nrow(a)
  lambdaj<-2*pi*(1:floor(T/2))/T 
  periodogram<-cross.Peri(a,b)
  out<-weights<-matrix(0,nrow(periodogram),ncol(periodogram))
  for(i in 1:ncol(a)){for(j in 1:ncol(b)){weights[i,j]<-(da[i]-db[j])/2}}
  for(i in k:l){out<-out+Re(exp(1i*lambdaj[i]*weights)*periodogram[,,i])}
  scaled<-2*pi/T*out
  return(scaled)
}



#' @title Fully Modified Narrow Band Least Squares (FMNBLS) estimation of the cointegrating
#'        vector.
#' @description Semiparametric estimator for the cointegrating vector as suggested 
#'              by Nielsen and Frederiksen (2011). Refines the \code{FDLS}
#'              estimator by allowing for long run coherence between the regressors 
#'              and the errors.
#' @details add details here. Especially on the selection of 
#'          all these bandwidth parameters. carefull: it is not clear, whether HP an be used here.
#' @param X data matrix.
#' @param Y data matrix.
#' @param m0 bandwidth parameter.
#' @param m1 bandwidth parameter.
#' @param m2 bandwidth parameter.
#' @param m3 bandwidth parameter.
#' @param method one from \code{method=c("local.W","Hou.Perron","ELW")}, to determine which
#'        semiparametric long memory estimator is to be used.
#' @references Nielsen and Frederiksen (2011): Fully modified narrow-band least
#'             squares estimation of weak fractional cointegration. The Econometrics Journal,
#'             14, pp. 77-120.
#' @examples
#' T<-500
#' d<-0.4
#' beta<-1
#' 
#' m0<-m3<-floor(T^0.4)                        
#' m1<-floor(T^0.6)                       
#' m2<-floor(T^0.8)
#' 
#' data<-FI.sim(T, q=2, rho=0.8, d=c(d,0))
#' xt<-data[,1]
#' et<-data[,2]
#' yt<-beta*xt+et
#' FDLS(xt,yt,m=m0)
#' FMNBLS(xt,yt,m0=m0, m1=m1, m2=m2, m3=m3)
#' @seealso \code{\link{FDLS}}, \code{\link{local.W}}, \code{\link{Hou.Perron}}, \code{\link{ELW}}
#' @export
#' 
FMNBLS<-function(X, Y, m0, m1, m2, m3, method=c("local.W","Hou.Perron","ELW")){
X<-as.matrix(X)
Y<-as.matrix(Y)
beta_hat<-FDLS(X=X, Y=Y, m=m0)
T<-max(dim(X))
u_hat<-0
for(tt in 1:T){u_hat[tt]<-Y[tt]-t(beta_hat)%*%X[tt,]}

qx<-min(dim(X))
dx_hat<-numeric(qx)

if(method[1]=="local.W"){
du_hat<-local.W(u_hat, m=m1)$d
for(a in 1:qx){dx_hat[a]<-local.W(X[,a], m=m1)$d}
}
if(method[1]=="Hou.Perron"){
du_hat<-Hou.Perron(u_hat, m=m1)$d
for(a in 1:qx){dx_hat[a]<-Hou.Perron(X[,a], m=m1)$d}
}
if(method[1]=="ELW"){
  du_hat<-ELW(u_hat, m=m1)$d
  for(a in 1:qx){dx_hat[a]<-ELW(X[,a], m=m1)$d}
}

lambda_m2<-2*pi*m2/T
lambda_m3<-2*pi*m3/T
LAMBDA_m2_hat<-LAMBDA_m(d.vec=dx_hat, m=m2, T=T)
LAMBDA_m3_hat<-LAMBDA_m(d.vec=dx_hat, m=m3, T=T)
GAMMA_tilde<-solve(F.tilde(a=X,b=X,da=dx_hat,db=dx_hat, k=(m0+1), l=m2))%*%F.tilde(a=X,b=u_hat,da=dx_hat,db=du_hat, k=(m0+1), l=m2)
beta_tilde<-beta_hat-(lambda_m3^(-du_hat))*(lambda_m2^du_hat)*LAMBDA_m3_hat%*%solve(LAMBDA_m2_hat)%*%GAMMA_tilde
beta_tilde
}


#library(fracdiff)
#T<-500
#beta<-as.matrix(c(1,2,3))
#m0<-m3<-floor(T^0.4)                         # according to simulation study of Nielsen and Frederiksen p.94 
#m1<-floor(T^0.6)                             # according to simulation study of Nielsen and Frederiksen p.94
#m2<-floor(T^0.8)                             # according to simulation study of Nielsen and Frederiksen p.94
#X<-cbind(fracdiff.sim(d=0.4, n=T)$series,fracdiff.sim(d=0.4, n=T)$series,fracdiff.sim(d=0.4, n=T)$series)
#u<-fracdiff.sim(d=0.1, n=T)$series
#Y<-0
#for(tt in 1:T){Y[tt]<-sum(beta*X[tt,])+u[tt]}

#ts.plot(Y)
#for(i in 1:ncol(X)){lines(X[,i], col=i)}

#b.hat<-FMNBLS(X, Y, m0, m1, m2, m3)
#b.hat

#res<-0
#for(tt in 1:T){
#res[tt]<-Y[tt]-t(beta)%*%X[tt,]
#}

#local.W(Y, m=m1)
#apply(X,2,local.W, m=m1)
#local.W(res, m=m1)
#