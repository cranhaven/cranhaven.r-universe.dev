

##########################################################################################
####                               Qu-test                                         #######
##########################################################################################


#rm(list=ls())
#library(longmemo)


##-------------------             Test Statistic                   --------------------##  

#' @title Qu test for true long memory against spurious long memory.
#' @description
#' \code{Qu.test} Test statistic of Qu (2011) for the null hypotesis of true long memory 
#' against the alternative of spurious long memory.
# #' @details add details here
#' @param data data vector of length T.
#' @param m bandwith parameter specifying the number of Fourier frequencies used 
#' for the estimation usually \code{floor(1+T^delta)}, where 0.5<delta<0.8 for consistency.
#' @param epsilon trimming parameter \code{epsilon=0.05} by default. Determines
#' minimum number of Fourier frequencies used in test statistic. For T>500 it is recommended 
#' to use \code{epsilon=0.02}.Confer Qu (2011) for further details.
#' @examples
#' library(fracdiff)
#' T<-500
#' m<-floor(1+T^0.75)
#' series<-fracdiff.sim(n=T,d=0.4)$series
#' shift.series<-ARRLS.sim(T=500,phi=0.5, sig.shift=0.75, prob=5/T, sig.noise=1)
#' ts.plot(series, ylim=c(min(min(series),min(shift.series)),max(max(series),max(shift.series))))
#' lines(shift.series, col=2)
#' Qu.test(series,m=m, epsilon=0.05)
#' Qu.test(shift.series,m=m, epsilon=0.05)
#' @references Qu, Z. (2011): A Test Against Spurious Long Memory. Journal of Business and 
#' Economic Statistics, Vol. 29, No. 3, pp. 423 - 438. 
#' @export
Qu.test<-function(data,m,epsilon=0.05){
T<-length(data)
m<-min(m,floor(T/2))
r.grid<-1/m                                          
r<-seq(epsilon,1,r.grid)                # create grid to find supremum
peri<-per(data)[-1]                     # calculate periodogram
lambdaj<-2*pi*1:floor(T/2)/T
nuj<-log(lambdaj[1:m])-mean(log(lambdaj[1:m]))
d.hat<-local.W(data, m=m)$d
G.hat<-mean(lambdaj[1:m]^(2*d.hat)*peri[1:m])
mr<-floor(m*r)
if(mr[1]<1){mr<-mr[-1]}
W.aux<-0
for(i in mr){W.aux[i]<-1/sqrt(sum(nuj[1:m]^2))*abs(sum(nuj[1:i]*(peri[1:i]/(G.hat*lambdaj[1:i]^(-2*d.hat))-1)))}
W.aux<-na.omit(W.aux)
crit<-cbind(c(1.118,1.252,1.374,1.517),c(1.022,1.155,1.277,1.426))
colnames(crit)<-c("eps=.02","eps=.05")
rownames(crit)<-c("alpha=.1","alpha=.05","alpha=.025","alpha=.01")
list("W.stat"=max(W.aux), "CriticalValues"=crit)
}



# library(fracdiff)
# T<-500
# d<-0.4
# M<-250
# test<-numeric(M)
# for(i in 1:M){test[i]<-Qu.test(fracdiff.sim(n=T,d=0.4)$series,m=T^0.7)}
# plot(density(test2))
# mean(test2>1.1552)


