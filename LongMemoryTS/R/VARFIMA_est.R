
##################################################################
##         Estimate VARFIMA
####################################################


####
#' @title Maximum likelihood estimation of a VARFIMA(1,1) in final equations form.
#' @description \code{VARFIMA.est} returns the maximum likelihood estimate of the parameter vector of a VARFIMA(1,1) in final equations form.
#' @details add details here.
#' @param data data matrix with T observations of q-dimensional process.
#' @param approx order of the AR-approximation that is supposed to be used. Default is \code{approx=100}.
#' @param split to increase the speed the sample can be divided in \code{split} parts. 
#' Parmeter estimation is then carried out seperately for each subsample and results are averaged across the subsamples.
#' @param rep is passed to \code{ll_VARFIMA} and determines whether the current parameter vector is printed to the user in
#' every iteration of the numerical maximization procedure. 
#' @references Lutkepohl, H. (2007): New introduction to multiple time series analysis. Springer.
#' @examples
#' series<-VARFIMA.sim(phi=0.4, THETA=matrix(c(0,0,0,0),2,2), 
#' d.vec=c(0.4,0.3), T=1000, Sigma=matrix(c(1,0.4,0.4,1),2,2))
#' ts.plot(series, col=1:2)
#' acf(series, lag=100)
#' VARFIMA.est(series, approx=100, rep=FALSE)
#' @export

VARFIMA.est<-function(data,approx=100,split=1, rep=FALSE){
  
# define parameters
  
T<-nrow(data)
q<-ncol(data)
N<-T/split
from<-(0:(split-1))*N+1
to<-(1:split)*N

# determine starting values

d.start<-unname(apply(data,2,function(data){local.W(data, m=floor(1+length(data)^0.6),int=c(0,0.4999))$d}))
arma.start<-rep(0.1,(1+q^2))


aux.series<-matrix(NA,T,q)
for(a in 1:q){aux.series[,a]<-diffseries(data[,a], d.start[a])}
sigs<-apply(aux.series,2,sd)
ars<-apply(aux.series,2,arima, order=c(1,0,1))
cor.mat<-cor(data)

ar1s<-0
for(a in 1:q){ar1s[a]<-ars[[a]]$coef[1]}
arma.start[1]<-mean(ar1s)

cor.vec<-0
count<-0
for(b in 1:(q-1)){
for(a in (b+1):q){
count<-count+1
cor.vec[count]<-cor.mat[a,b]
}
}

start<-c(d.start,arma.start,sigs,cor.vec)
lower<-c(rep(0,q),0,rep(-0.99,q^2),rep(0.1,q),rep(-0.99,count))
upper<-c(rep(0.49999,q),0.99999,rep(0.99,q^2),rep(Inf,q),rep(0.95,count))

#### optimization

est<-matrix(NA,split,length(start))
for(aa in 1:split){
pre.sample<-if(from[aa]>approx){pre.sample=data[(from[aa]-approx):(from[aa]-1),]}else{pre.sample=matrix(0,approx,q)}
if(aa>1){start2<-est[(aa-1),]}else{start2<-start}
out<-optim(par=start2, fn=ll.VARFIMA, data=data[from[aa]:to[aa],], approx=approx, pre.sample=pre.sample, method="L-BFGS-B", lower=lower, upper=upper, rep=rep, q=q, control=list(fnscale=-1,trace=0))
est[aa,]<-out$par
#print(out$par)
}
apply(est,2,mean)
}

#ll.VARFIMA(theta=start2, data=data[from[aa]:to[aa],], q=q, pre.sample=pre.sample)

#VARFIMA.est(start=theta, data=data, approx=20, split=2)
