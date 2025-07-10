

##########################################################################################
####            Local Whittle / Gaussian Semiparametric Estimator                  #######
##########################################################################################


#library(longmemo)

#---------------------------       profiled/ concentrated likelihood function       ------------------------------------#

#'Concentrated local Whittle likelihood. Only for internal use. cf. Robinson (1995), p. 1633.
#'@keywords internal
R.lw<-function(d,peri,m,T,l=1){
  lambda<-2*pi/T
  K<-log(1/(m-l-1)*(sum(peri[l:m]*(lambda*(l:m))^(2*d))))-2*d/(m-l-1)*sum(log(lambda*(l:m)))
  K
}

#'Concentrated local Whittle likelihood of differenced and tapered estimator of 
#'Hurvich and Chen (2000). Only for internal use. 
#'@keywords internal
R.lw.hc<-function (d, peri, m, T) 
{
  j <- (1:m) + 1/2
  lambdaj <- 2 * pi *j /T
  K <- log(mean(peri[1:m] * (lambdaj )^(2 * d))) -  2 * d*mean(log(lambdaj ))
  K
}


#'Cosine Bell Taper
#'@keywords internal
cos_bell<-function(u){1/2*(1-cos(2*pi*u))}

#'Complex Cosine Bell Taper
#'@keywords internal
cos_bell_cmplx<-function(u){1/2*(1-cos(1i*2*pi*u))}

#'Concentrated local Whittle likelihood for tapered estimate. Only for internal use. Cf. Velasco (1999).
#'@keywords internal
R.lw.tapered<-function(d,peri,m,p,T){
  lambda<-2*pi/T
  j<-seq(p,m,p)
  K<-log(p/m*(sum(peri[j]*(lambda*(j))^(2*d))))-2*d*p/m*sum(log(lambda*(j)))
  K
}

#'Scaling factor in the asymptotic variance. Only for internal use. Cf. Velasco (1999).
#'@keywords internal
PHI_lw<-function(ht,T,p){
  k<-seq(0,T-p,p)
  lambdak<-2*pi/T*k
  enumerator<-0
  for(i in 1:length(k)){
    enumerator<-enumerator+sum(ht^2*cos((1:T)*lambdak[i]))^2
  }
  denominator<-sum(ht^2)^2
  enumerator/denominator
}

#---------------------------       local whittle estimation       ------------------------------------#

#' @title Local Whittle estimator of fractional difference parameter d.
#' @description \code{local.W} Semiparametric local Whittle estimator for memory parameter d following Robinson (1995).
#'  Returns estimate and asymptotic standard error.
# #' @details add details here.
#' @param data vector of length T.
#' @param m bandwith parameter specifying the number of Fourier frequencies.
#' used for the estimation usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param int admissible range for d. Restricts the interval 
#'        of the numerical optimization.
#' @param taper string that is either \code{"none"}, \code{"Velasco"}, or \code{"HC"} and determines whether the standard local Whittle estimator of Robinson (1995), 
#' the tapered version of Velasco (1999), or the differenced and tapered estimator of Hurvich and Chen (2000) is used.
#' @param l integer that determines how many frequencies (l-1) are trimmed out if taper="none" is selected. Default is l=1.
#' @param diff_param integer specifying the order of differentiation for the estimator of Hurvich and Chen (2000). Default is \code{diff_param=1}.
#' @import longmemo
#' @references Robinson, P. M. (1995): Gaussian Semiparametric Estimation of Long Range Dependence. 
#' The Annals of Statistics, Vol. 23, No. 5, pp. 1630 - 1661.
#' 
#' Velasco, C. (1999): Gaussian Semiparametric Estimation for Non-Stationary Time Series.
#' Journal of Time Series Analysis, Vol. 20, No. 1, pp. 87-126. 
#' 
#' Hurvich, C. M., and Chen, W. W. (2000): An Efficient Taper for Potentially 
#' Overdifferenced Long-Memory Time Series. Journal of Time Series Analysis, Vol. 21, No. 2, pp. 155-180.
#' @examples
#' library(fracdiff)
#' T<-1000
#' d<-0.4
#' series<-fracdiff.sim(n=T, d=d)$series
#' local.W(series,m=floor(1+T^0.65))
#' @export

local.W<-function(data,m,int=c(-0.5,2.5), taper=c("none","Velasco","HC"), diff_param=1, l=1){
  taper<-taper[1]
  if((taper%in%c("none","Velasco","HC"))==FALSE)stop('taper must be either "none", "Velasco", or "HC".')
  T<-length(data)
  if(taper=="Velasco"){
    p<-3
    ht<-cos_bell((1:T)/T)
    data<-ht*data
    peri<-per(data)[-1]
    d.hat<-optimize(f=R.lw.tapered, interval=int, peri=peri,  m=m, T=T, p=p)$minimum
    se<-sqrt(p*PHI_lw(ht=ht,T=T,p=p)/(4*m))
  }
  if(taper=="HC"){
    data<-diff(data,differences=diff_param)
    T<-length(data)
    ht<-cos_bell_cmplx((1:T)/T)
    data<-ht*data
    peri<-per(data)[-1]
    d.hat<-optimize(f=R.lw.hc, interval=int, peri=peri,  m=m, T=T)$minimum+1
    se<-sqrt(1.5/(4*m))
  }
  if(taper=="none"){
    peri<-per(data)[-1]
    d.hat<-optimize(f=R.lw, interval=int, peri=peri,  m=m, T=T, l=l)$minimum
    se<-1/(2*sqrt(m))
  }
  return(list("d"=d.hat, "s.e."=se))
}


# library(fracdiff)
# T<-1000
# d<-0.4
# series<-fracdiff.sim(n=T, d=d)$series
# local.W(series,m=floor(1+T^0.65))
