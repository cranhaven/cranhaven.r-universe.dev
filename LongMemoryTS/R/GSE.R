
#################################################################################################
##    multivariate Gaussian Semiparametric Estimator (GSE) of Shimotsu (2007)
########################################################################################


#################################################
# Calculate multivariate periodogram


##########################################################
## Calculate G depending on d vector

#' @title Estimation of G matrix for multivariate long memory processes.
#' @description \code{G.hat} Estimates the matrix G of a multivariate long memory process 
#'              based on an estimate of the vector of memory parameters. The assumed spectral 
#'              density is that of Shimotsu (2007).
# #' @details add details here.
#' @param X data matrix with T observations of q-dimensional process.
#' @param d q-dimensional data vector.
#' @param m bandwith parameter specifying the number of Fourier frequencies.
#' used for the estimation usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @references Shimotsu, K. (2007): Gaussian semiparametric estimation of multivariate
#' fractionally integrated processes. Journal of Econometrics, Vol. 137, No. 2, pp. 277 - 310.
#' @examples
#' T<-500
#' d1<-0.4
#' d2<-0.2
#' data<-FI.sim(T, q=2, rho=0, d=c(d1,d2))
#' G.hat(X=data, d=c(d1,d2), m=floor(1+T^0.6))
#' #diagonal elements should equal 1/(2*pi)
#' @export

G.hat<-function(X,d,m){
  if(which.max(dim(X))==1){X<-t(X)} # convert matrix in q x n - dimensional matrix if it is n x q
  n<-ncol(X)                        # number of observations
  q<-nrow(X)                        # number of dimensions
  L1<-Lambda_j(q=q, n=floor(n/2), T=n, d_vec=d)                # create Lambda.j(d) vector with function Lamda.j
  I.j<-Peri(X)                      # calculate periodogram with function Peri()
  aux.M<-matrix(0,q,q)              # auxillary matrix to calculate mean of first m fourier frequencies
  for(j in 1:m){aux.M<-aux.M+Re(solve(L1[,,j])%*%I.j[,,j]%*%solve(t(Conj((L1[,,j])))))}   # calculate \hat{G}(d)
  1/m*(aux.M)
}

#G.hat(X,d.vec=0,m=50)


########################################################
### Wrapper Function for Minimization of R(d)

#' @title Multivariate local Whittle estimation of long memory parameters.
#' @description \code{GSE} Estimates the memory parameter of a vector valued long memory process.
# #' @details add details here.
#' @param X data matrix with T observations of q-dimensional process.
#' @param m bandwith parameter specifying the number of Fourier frequencies used for the estimation. Usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param l integer that specifies the number of Fourier frequencies (l-1) that are trimmed.
#' @references Shimotsu, K. (2007): Gaussian semiparametric estimation of multivariate
#' fractionally integrated processes. Journal of Econometrics, Vol. 137, No. 2, pp. 277 - 310.
#' @import stats
#' @examples
#' T<-500
#' d1<-0.4
#' d2<-0.2
#' data<-FI.sim(T, q=2, rho=0.5, d=c(d1,d2))
#' ts.plot(data, col=1:2)
#' GSE(data, m=floor(1+T^0.7))
#' @export

GSE<-function(X, m=m, l=1){
  X<-as.matrix(X)
  if(which.max(dim(X))==1){X<-t(X)} # convert matrix in q x n - dimensional matrix if it is n x q
  T<-ncol(X)                        # number of observations
  q<-nrow(X)                        # number of dimensions
  start.vec<-rep(0,q)
  for(a in 1:q){start.vec[a]<-local.W(X[a,],m=m, l=l)$d}
  PERI<-Peri(X)
  if(q==1){
    out<-optimize(f=R_d_multi_GSE, interval=c(-0.5,1), PERI=PERI, m=m, l=l, T=T, q=q, maximum=FALSE)
  }else{
    out<-optim(par=start.vec, fn=R_d_multi_GSE, PERI=PERI, m=m, l=l, T=T, q=q, method="Nelder-Mead", control=list(fnscale=1,trace=0, REPORT=1, maxit=10000))
    }
  erg<-out[[1]]
  names(erg)<-paste("d",1:q,sep="")
  erg
}



#' @title Multivariate local Whittle estimation of long memory parameters and cointegrating vector.
#' @description \code{GSE_coint} is an extended version of \code{GSE} that allows the joint 
#' estimation of the memory parameters and the cointegration vector for a vector valued process.
# #' @details add details here.
#' @param elements vector specifying which elements of the observation vector are cointegrated.
#' @inheritParams GSE
#' @examples 
#' #
#' # Cointegration:
#' #
#' T<-500
#' m<-floor(T^0.75)
#' series<-FI.sim(T=T,q=2,rho=0,d=c(0.1,0.4), B=rbind(c(1,-1),c(0,1)))
#' ts.plot(series, col=1:2)
#' GSE_coint(X=series,m=m, elements=c(1,2))
#' @export

GSE_coint<-function(X, m=m, elements, l=1){
  X<-as.matrix(X)
  if(which.max(dim(X))==1){X<-t(X)} # convert matrix in q x n - dimensional matrix if it is n x q
  T<-ncol(X)                        # number of observations
  q<-nrow(X)                        # number of dimensions
  start.vec<-rep(1,(q+length(elements)-1))
  for(a in 1:q){start.vec[length(elements)-1+a]<-local.W(X[a,],m=m, l=l)$d}
  PERI<-Peri(X)
  if(q==1){
    out<-optimize(f=R_d_multi_GSE_coint, interval=c(-0.5,1), PERI=PERI, m=m, T=T, q=q, maximum=FALSE)
  }else{
  out<-optim(par=start.vec, fn=R_d_multi_GSE_coint, PERI=PERI, m=m, l=l, T=T, q=q, elements=elements,
             method="L-BFGS-B", lower=c(-10,-0.49,-0.49), upper=c(10,1.5,1.5), control=list(fnscale=1,trace=0, REPORT=1, maxit=10000))
  }
  erg<-out[[1]]
  names(erg)<-c(paste("beta",1:(length(elements)-1), sep=""),paste("d",1:q,sep=""))
  erg
}


#GSE_coint<-function(X,m=m, elements){
#  X<-as.matrix(X)
#  if(which.max(dim(X))==1){X<-t(X)} # convert matrix in q x n - dimensional matrix if it is n x q
#  n<-ncol(X)                        # number of observations
#  q<-nrow(X)                        # number of dimensions
#  start.vec<-rep(0.01,(q+length(elements)-1))
#  for(a in 1:q){start.vec[a]<-local.W(X[a,],m=m)$d}
#  if(q==1)
#    out<-optimize(f=R_d_multi_GSE_coint, interval=c(-0.5,1), X=X, m=m, maximum=FALSE)
#  }else{
#    out<-optim(par=start.vec, fn=R_d_multi_GSE_coint, X=X, m=m, elements=elements,
#               method="Nelder-Mead", control=list(fnscale=1,trace=0, REPORT=1, maxit=10000))}
#  erg<-out[[1]]
#  names(erg)<-c(paste("beta",1:(length(elements)-1), sep=""),paste("d",1:q,sep=""))
#  erg
#}


#T<-1000
#X<-rbind(rnorm(T),rnorm(T))
#GSE(X,m=(T^0.7))

#GSE(as.matrix(rnorm(T)), m=(T^0.7))


