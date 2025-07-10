
###########################################################################################
#                 Modified local Whittle Estimator of Hou and Perron (2014)              ##
###########################################################################################

#library(longmemo)


#---------------------------       profiled/ concentrated likelihood function       ------------------------------------#


#' Modified concentrated local Whittle likelihood. Only for internal use. cf. Hou and Perron (2014), p. 312.
#'@keywords internal


J.M<-function(vec,peri,m,T){
  d<-vec[1]
  theta<-vec[2]
  lambda<-2*pi/T*(1:m)
  g.k<-lambda^(-2*d)+(theta*lambda^(-2)/T)
  K<-log(sum(peri/g.k)/m)+sum(log(g.k))/m
  K
}

#---------------------------       modified local whittle estimation       ------------------------------------#

#' @title Modified local Whittle estimator of fractional difference parameter d.
#' @description \code{Hou.Perron} Modified semiparametric local Whittle estimator of Hou and Perron (2014).
#' Estimates memory parameter robust to low frequency contaminations.
#' @author Christian Hendrik Leschinski
#' @details
#' add details here
#' @param data data vector of length T.
#' @param m bandwith parameter specifying the number of Fourier frequencies
#' used for the estimation usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @references Hou, J., Perron, P. (2014): Modified local Whittle estimator for long memory processes
#' in the presence of low frequency (and other) contaminations. Journal of Econometrics,  Vol. 182,
#' No. 2, pp. 309 - 328.
#' @examples
#' library(fracdiff)
#' T<-1000
#' d<-0
#' mean<-c(rep(0,T/2),rep(2,T/2))
#' FI<-fracdiff.sim(n=T, d=d)$series
#' series<-mean+FI
#' ts.plot(series)
#' lines(mean, col=2)
#' local.W(series, m=floor(1+T^0.65))
#' Hou.Perron(series, m=floor(1+T^0.65))
#' @export
Hou.Perron<-function(data,m){
lower<-c(-0.4999,0)
T<-length(data)
peri<-per(data)[2:(m+1)]
out<-optim(par=c(0,0), fn=J.M, peri=peri,  m=m, T=T, method="L-BFGS-B", lower=lower, upper=c(0.99,10000))
d<-out$par[1]
se<-1/(2*sqrt(m))
return(list("d"=d, "s.e."=se))
}



#library(fracdiff)
#T<-1000
#d<-0
#mean<-c(rep(0,T/2),rep(2,T/2))
#FI<-fracdiff.sim(n=T, d=d)$series
#series<-mean+FI
#ts.plot(series)
#lines(mean, col=2)
#Hou.Perron(series, m=floor(1+T^0.65))
#local.W(series, m=floor(1+T^0.65))



#library(fracdiff)
#series<-fracdiff.sim(n=1000, d=0.3)$series+c(rep(0,500),rep(2,500))
#Hou.Perron(series,m=floor(1000^0.65))

#M<-250
#d<-0.25
#T<-500
#m.grid<-seq(0.6,0.8,0.05)
#stats<-control<-matrix(NA,M,length(m.grid))
#for(i in 1:M){
#  series<-fracdiff.sim(n=T, d=d)$series#+c(rep(0,250),rep(1,250))
#  for(mm in 1:length(m.grid)){
#  stats[i,mm]<-Hou.Perron(series,m=floor(T^m.grid[mm]))$d
#  #control[i,mm]<-local.W(series,m=floor(T^m.grid[mm]))$d
#  }
#print(i)
#}


