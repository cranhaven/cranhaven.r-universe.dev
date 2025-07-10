


######################################################
## GPH estimation


#library(fracdiff)
#library(longmemo)

#' @title GPH estimator of fractional difference parameter d.
#' @description \code{gph} log-periodogram estimator of Geweke and Porter-Hudak (1983) (GPH) and Robinson (1995a) for memory parameter d.
#' @details add details here.
#' @param X vector of length T.
#' @param m bandwith parameter specifying the number of Fourier frequencies.
#' used for the estimation usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param l trimming parameter that determines with which Fourier frequency to start.
#'        Default value is \code{l=1}.
#' @import longmemo
#' @references Robinson, P. M. (1995): Log-periodogram regression of time series with 
#' long range dependence. The Annals of Statistics, Vol. 23, No. 5, pp. 1048 - 1072.
#' 
#' Geweke, J. and Porter-Hudak, S. (1983): The estimation and application 
#' of long memory time series models. Journal of Time Series Analysis, 4, 221-238.
#' 
#' @examples
#' library(fracdiff)
#' T<-500
#' m<-floor(1+T^0.8)
#' d=0.4
#' series<-fracdiff.sim(n=T, d=d)$series
#' gph(X=series,m=m)
#' @export

gph<-function(X,m,l=1){
  T<-length(X)
  n<-floor(T/2)
  lambdaj<-2*pi/T*1:n
  logIj<-log(per(X)[-1])
  Yj<-log(abs(1-exp(1i*lambdaj)))
  Ybar<-1/(m-l+1)*sum(Yj[l:m])
  d.hat<--0.5*sum((Yj[l:m]-Ybar)*logIj[l:m])/sum((Yj[l:m]-Ybar)^2)
  d.hat
}

#T<-500
#m<-floor(1+T^0.8)
#d=0.4
#series<-fracdiff.sim(n=T, d=d)$series
#gph(X=series,m=m)


######################     Correction factor for small sample standard errors     ########################################

#' Correction factor for small sample standard errors
#' @keywords internal
Slm<-function(m,l){
  logj<-log(1:m)
  logj.bar<-1/(m-l+1)*sum(logj[l:m])
  nuj<-logj-logj.bar
  sum(nuj[l:m]^2)
}

#' @title GPH estimation of long memory parameter robust to low frequency contaminations.
#' @description \code{McC.Perron} trimmed and adaptive log-periodogram estimators of 
#'              McCloskey and Perron (2013, ET) for robust estimation of the memory parameter d.
#' @details add details here. Recommendation of McCloskey, A. and Perron, P. (2013): Use trimmed 
#' version of estimator if there is reason to assume that shifts are present and use adaptive with 
#' \code{epsilon=0.05} and \code{m=T^0.8} if you are agnostic about the presence of shifts.
#' @param X vector of length T.
#' @param m bandwith parameter specifying the number of Fourier frequencies.
#' used for the estimation usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param epsilon small constant that determines the choice of the trimming parameter l
#'        used by the \code{gph} estimator. Default is \code{epsilon=0.05}.
#' @param method either "adaptive" or "trimmed" for the corresponding estimator.
#'        Confer McCloskey and Perron (2013, ET) for details. Default is \code{method="adaptive"}.
#' @param Kl proportionality factor for bandwidth selection. Default is \code{Kl=1}.
#' @import longmemo
#' @references Robinson, P. M. (1995): Log-periodogram regression of time series with long range dependence. 
#' The Annals of Statistics, Vol. 23, No. 5, pp. 1048 - 1072.
#' 
#' McCloskey, A. and Perron, P. (2013): Memory parameter estimation in the presence of 
#' level shifts and deterministic trends. Econometric Theory, 29, pp. 1196-1237.
#' @examples
#' library(fracdiff)
#' T<-1000
#' m<-floor(1+T^0.8)
#' d=0.4
#' series<-fracdiff.sim(n=T, d=d)$series
#' McC.Perron(series,m)
#' @export

McC.Perron<-function(X,m,epsilon=0.05, method=c("adaptive","trimmed"), Kl=1){
if(method[1]%in%c("adaptive","trimmed")==FALSE)stop('method must be either "adaptive" or "trimmed".')
T<-length(X)
dhat<-numeric(10)
lvec<-numeric(10)
lvec[1]<-floor(1+Kl*T^(0.5+epsilon))
dhat[1]<-gph(X=X,m=m,l=lvec[1])
count<-1
if(method[1]=="adaptive"){
# iterate until convergence or 10 times
while(count<10){
    lvec[(count+1)]<-floor(1+Kl*T^((1-2*dhat[count])/(2-2*dhat[count])+epsilon))
    
    # break if trimming is larger than bandwidth
        if(lvec[(count+1)]>m){
                  count<-1
                  break}
    dhat[(count+1)]<-gph(X=X,m=m,l=lvec[(count+1)])
    count<-count+1
    # break in case of convergence
        if(abs(dhat[count]-dhat[(count-1)])<0.01){
                  cat("convergence","\n")
                  break}
}}
list("d"=dhat[count], "approx.s.e."=sqrt(pi^2/(24*Slm(m=m,l=lvec[count]))), "asympt.s.e"=sqrt(pi^2/(24*m)))
}

#T<-1000
#m<-floor(1+T^0.8)
#d=0.4

#series<-fracdiff.sim(n=T, d=d)$series
#McC.Perron(series,m)






