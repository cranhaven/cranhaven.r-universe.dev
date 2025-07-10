

################################################################
#                                                              #
#         Frequency Domain Least Squares Estimation            #
#                                                              #
################################################################

#library(longmemo)
#library(QZ)

## equation numbers refer to Christensen and Nielsen (2006, JoE)

#' @title Cross periodogram of vector valued time series X and Y
#' @description Calculates the cross periodogram of the vector valued time series X and Y.
# #' @details add details here. 
#' @param X data matrix.
#' @param Y data matrix.
#' @examples
#' T<-500
#' d<-c(0.4, 0.2, 0.3)
#' data<-FI.sim(T, q=3, rho=0, d=d)
#' X<-data[,1:2]
#' Y<-data[,3]
#' cper<-cross.Peri(X, Y)
#' pmax<-max(Re(cper),Im(cper))
#' pmin<-min(Re(cper),Im(cper))
#' plot(Re(cper[1,,]), type="h", ylim=c(pmin,pmax))
#' lines(Im(cper[1,,]), col=2)
#' plot(Re(cper[2,,]), type="h", ylim=c(pmin,pmax))
#' lines(Im(cper[2,,]), col=2)
#' @export
#' 
cross.Peri<-function(X,Y){
  X<-as.matrix(X)
  Y<-as.matrix(Y)
  if(which.max(dim(X))==1){X<-t(X)}         # convert matrix in q x n - dimensional matrix if it is n x q
  if(which.max(dim(Y))==1){Y<-t(Y)}         # convert matrix in q x n - dimensional matrix if it is n x q
  Tx<-ncol(X)                                # number of observations
  qx<-nrow(X)                                # number of dimensions
  Ty<-ncol(Y)
  qy<-nrow(Y)
  if(Tx!=Ty)stop("X and Y must have same length.")
  n<-Ty
  lambdaj<-2*pi*(1:floor(n/2))/n            # vector of fourier frequencies
  weight.mat<-matrix(NA,n,floor(n/2))       # create weight matrix
  for(j in 1:floor(n/2)){weight.mat[,j]<-exp(1i*(1:n)*lambdaj[j])}    # fill weight matrix
  wx<-1/sqrt(2*pi*n)*X%*%weight.mat                                  # matrix w1 contains discrete fourier transform
  wy<-1/sqrt(2*pi*n)*Y%*%weight.mat 
  I.lambda<-array(NA,dim=c(qx,qy,floor(n/2)))                          # create periodogram array
  for(j in 1:floor(n/2)){I.lambda[,,j]<-wx[,j]%*%t(Conj(wy[,j]))}          # fill periodogram array with periodogram matrices w1%*%t(w2)
  I.lambda                                                     # return periodogram
}



#' @title Empirical cummulative spectral distribution function
#' @description Calculates the empirical cummulative spectral distribution function from the 
#' cross periodogram of the vector valued time series X and Y.
# #' @details add details here. 
#' @param X data matrix.
#' @param Y data matrix.
#' @param k integer that determines the order number of the first Fourier frequency used.
#' @param l integer that determines the order number of the last Fourier frequency used.
#' @examples
#' T<-500
#' d<-c(0.4, 0.2, 0.3)
#' 
#' data<-FI.sim(T, q=3, rho=0, d=d)
#' X<-data[,1:2]
#' Y<-data[,3]
#' F.hat(X, Y, 1, floor(T/2))
#' @export
F.hat<-function(X,Y,k,l){
  # (6)
X<-as.matrix(X)
Y<-as.matrix(Y)
T<-nrow(X)
periodogram<-cross.Peri(X,Y)
out<-matrix(0,nrow(periodogram),ncol(periodogram))
for(i in k:l){out<-out+Re(periodogram[,,i])}
scaled<-2*pi/T*out
return(scaled)
}


#' @title Narrow band estimation of the cointegrating vector.
#' @description Semiparametric estimator for the cointegrating vector as suggested 
#'              by Robinson (1994) and discussed by Robinson and Marinucci (2003) and
#'              Christensen and Nielsen (2006), among others.
#' @details add details here. Assumes that there is no long-run coherence between the errors
#'          and the regressors. Consistency and Normality, Stationarity, assumptions,...
#' @param X data matrix.
#' @param Y data matrix.
#' @param m bandwith parameter specifying the number of Fourier frequencies.
#' used for the estimation of d, usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @references Christensen, B. J. and Nielsen, M. O. (2006): Asymptotic normality
#'             of narrow-band least squares in the stationary fractional 
#'             cointegration model and volatility forecasting. Journal of Econometrics,
#'             133, pp. 343-371.
#' 
#'             Robinson, P. M., (1994): Semiparametric analysis of long-memory 
#'             time series. Annals of Statistics, 22, pp. 515-539.
#'              
#'             Robinson, P. M. and Marinucci, D. (2003): Semiparametric frequency
#'             domain analysis of fractional cointegration. In: Robinson, P. M. (Ed.),
#'             Time Series with Long Memory, Oxford University Press, Oxford, pp. 334-373.
#' @examples
#' T<-500
#' d<-0.4
#' beta<-1
#' 
#' data<-FI.sim(T, q=2, rho=0, d=c(d,0))
#' xt<-data[,1]
#' et<-data[,2]
#' yt<-beta*xt+et
#' FDLS(xt,yt,m=floor(1+T^0.4))
#' 
#' data<-FI.sim(T, q=2, rho=0.8, d=c(d,0))
#' xt<-data[,1]
#' et<-data[,2]
#' yt<-beta*xt+et
#' FDLS(xt,yt,m=floor(1+T^0.4))
#' @export

FDLS<-function(X,Y,m){
# (7)
X<-as.matrix(X)
Y<-as.matrix(Y)
beta.hat.m<-solve(F.hat(X,X,1,m))%*%F.hat(X,Y,1,m)
return(beta.hat.m)
}


#library(fracdiff)
#T<-500
#X<-fracdiff.sim(d=0.4, n=T)$series
#Y<-X+rnorm(T)
#ts.plot(cbind(X,Y), col=1:2)
#
#cross.Peri(X,Y)
#F.hat(X,Y,1,20)
#FDLS(X,Y,20)
