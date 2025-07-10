
#' @title MLWS test for multivariate spurious long memory.
#' @description Multivariate local Whittle Score type test for the null hypothesis of true long
#'              memory against the alternative of spurious long memory suggested by Sibbertsen, 
#'              Leschinski and Holzhausen (2018).
# #' @details add details here
#' @param X data matrix
#' @param m bandwith parameter specifying the number of Fourier frequencies used 
#' for the estimation usually \code{floor(1+T^delta)}, where 0.5<delta<0.8 for consistency.
#' @param epsilon trimming parameter \code{epsilon=0.05} by default. Determines
#' minimum number of Fourier frequencies used in test statistic. For T>500 it is recommended 
#' to use \code{epsilon=0.02}. Confer Sibbertsen, Leschinski, Holzhausen (2018) for further details.
#' @param coint.elements Vector specifying which elements in the vector series are in a cointegrating relationship. By default \code{NULL}. Cf details.
#' @param B cointegrating matrix, if known. Default is \code{B=NULL}.
#' @param prewhite specifies the form of pre-whitening applied. One of \code{c("none","uni","multi")}. 
#' If \code{uni} is selected the univariate a univariate of maximal order (1,d,1) is selected using the AIC. 
#' If \code{multi} is selected \code{VARFIMA_est} is used to fit a VARFIMA(1,d,1) in final equations form.
#' Default is \code{none}.
#' @param eta vector of weights. Default is \code{rep(1/sqrt(min(dim(X))),min(dim(X)))}.
#' @param rep if \code{prewhite="multi"} is selected, rep specifies whether the current parameter values are 
#' displayed to the user during optimization procedure. Default is \code{rep=FALSE}.
#' @param approx if \code{prewhite="multi"} is selected, approx specifies the order of the AR-approximation
#' used in \code{VARFIMA_est}. Default is \code{approx=100}.
#' @param split if \code{prewhite="multi"} is selected, \code{split} whether the sample should be split into
#' subsamples to speed up the estimation. Default is \code{split=1}, so that the whole sample is used.
#' @param T_limdist number of increments used in simulation if limit distribution. 
#' Only relevant for component-wise version of the test. Default is \code{T_limdist=1000}.
#' @param M_limdist number of replications for simulation of the limit distribution.
#' Default is \code{M_limdist=5000}.
#' @import fracdiff
#' @examples 
#' T<-500
#' m<-floor(1+T^0.75)
#' series<-FI.sim(T=T,q=2,rho=0.7,d=c(0.4,0.2))
#' ts.plot(series, col=1:2)
#' MLWS(X=series, m=m, epsilon=0.05)
#' 
#' shift.series<-series+ARRLS.sim(T=T, phi=0, sig.shift=2, prob=5/T)
#' ts.plot(shift.series, col=1:2)
#' MLWS(X=shift.series, m=m, epsilon=0.05)
#' 
#' T<-500
#' m<-floor(T^0.75)
#' series<-FI.sim(T=T,q=2,rho=0,d=c(0.1,0.4), B=rbind(c(1,-1),c(0,1)))
#' ts.plot(series, col=1:2)
#' MLWS(series, m=m)
#' MLWS(series, m=m, coint.elements=c(1,2))
#' @references Sibbertsen, P., Leschinski, C. H., Holzhausen, M., (2018): A Multivariate Test 
#'              Against Spurious Long Memory. Journal of Econometrics, Vol. 203, No. 1, pp. 33 - 49.
#' @export

MLWS<-function(X,m,epsilon=c(0.02,0.05), coint.elements=NULL, B=NULL, prewhite=c("none","uni","multi"), eta=rep(1/sqrt(min(dim(X))),min(dim(X))), rep=FALSE, approx=100, split=1, T_limdist=1000, M_limdist=5000){
  
  epsilon<-epsilon[1]
  prewhite<-prewhite[1]
  # transpose and get dimensions
  X<-as.matrix(X)
  if(which.max(dim(X))==1){X<-t(X)} 
  n<-ncol(X)                        
  q<-nrow(X)
  
  # demeaning
  means<-apply(X,1,mean)
  for(a in 1:q){X[a,]<-X[a,]-means[a]}

  #------ Estimation of d and B -----------#
  
  if(is.null(B)==FALSE){X<-apply(X,2,function(x){B%*%x})}
  
  if(is.null(coint.elements)){
    if(prewhite!="uni"){d.est<-as.vector(GSE(X=X,m=m))}
    B<-diag(q)
  }else{
    params<-as.vector(GSE_coint(X=X,m=m, elements=coint.elements))
    ncoint<-length(coint.elements)-1
    d.est<-as.vector(params[-(1:(ncoint))])
    B<-diag(q)
    for(i in 1:ncoint){
      B[coint.elements[1],coint.elements[(i+1)]]<--params[i]
    }
    X<-apply(X,2,function(x){B%*%x})
  }
  

  # check input parameters
  
  eta<-as.vector(eta)
  if(length(eta)!=q)stop("eta must be a vector of length q.")
  if((prewhite%in%c("none","uni","multi"))==FALSE)stop("prewhite must be either 'none', 'uni' or 'multi'.")
    
  
  #---- pre-whitening ----#
  if(prewhite=="multi"){
    if(rep==TRUE){print("Estimating VARFIMA")}
    theta.hat<-VARFIMA.est(data=t(X), approx=approx, split=split, rep=rep)
    if(rep==TRUE){print(theta.hat)}
    if(rep==TRUE){print("Pre-Whitening...")}
    filtered<-pre.White(theta=theta.hat, data=t(X), q=q, approx=approx)
  }
  if(prewhite=="uni"){
    for(aa in 1:q){
      prewhite_est<-list()
      suppressWarnings(
      for(i in 1:4){
        prewhite_est[[1]]<-fracdiff(X[aa,], nar=0, nma=0)
        prewhite_est[[2]]<-fracdiff(X[aa,], nar=1, nma=0)
        prewhite_est[[3]]<-fracdiff(X[aa,], nar=0, nma=1)
        prewhite_est[[4]]<-fracdiff(X[aa,], nar=1, nma=1)
      }
      )
      est<-prewhite_est[[which.min(unlist(lapply(prewhite_est, BIC)))]]
      X[aa,]<-(fdiff(residuals(est), -est$d))
    }
    d.est<-as.vector(GSE(X=X,m=m))
  }

  #-----------------------#


  
  #------ Calculate Test Statistic ------------------#
  

  W.stat<-W_multi(X=X, d_vec=d.est, m=m, epsilon=epsilon, eta=eta)
    
  
  #----------------------------------------#
  crit<-cbind(c(1.118,1.252,1.374,1.517),c(1.022,1.155,1.277,1.426))
  colnames(crit)<-c("eps=.02","eps=.05")
  rownames(crit)<-c("alpha=.1","alpha=.05","alpha=.025","alpha=.01")
  if(epsilon==0.02){crit<-crit[,1]}
  if(epsilon==0.05){crit<-crit[,2]}
  
  if(any(eta!=rep(1/sqrt(q),q))){
    G_est<-G.hat(X,d=d.est,m=m)
    cat("Simulating limit distribution...","\n")
    dist<-simMLWS(G=G_est, eta=eta, epsilon=epsilon, T=T_limdist, M=M_limdist)
    crit<-quantile(dist, probs=c(0.9,0.95,0.975,0.99))
    pval<-1-ecdf(dist)(W.stat)
    out<-list("B"=B,"d"=d.est,"W.stat"=W.stat, "CriticalValues"=crit, "pval"=pval)
  }else{
    out<-list("B"=B,"d"=d.est,"W.stat"=W.stat, "CriticalValues"=crit)
  }
  out
}



###########################################################################################
