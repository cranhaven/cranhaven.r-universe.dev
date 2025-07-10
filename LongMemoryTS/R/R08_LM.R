##########################################################################################
####            Hausman-type test for fractional cointegration                     #######
####               Robinson (2008)                                                 #######                     
##########################################################################################

#' Real-valued element of derivative of objective function in R08.
#' @keywords internal
H.hat<-function(X,d,m){
  if(which.max(dim(X))==1){X<-t(X)} # convert matrix in q x n - dimensional matrix if it is n x q
  T<-ncol(X)                        # number of observations
  dim_series<-nrow(X)               # number of dimensions
  lambdaj<-2*pi*(1:T)/T
  logj.grid<-log(1:m)
  nu_j<-logj.grid-mean(logj.grid)
  I.j<-Peri(X)                      # calculate periodogram with function Peri()
  aux.M<-matrix(0,dim_series,dim_series)  # auxillary matrix to calculate mean of first m fourier frequencies
  for(j in 1:m){aux.M<-aux.M+Re(nu_j[j]*I.j[,,j]*lambdaj[j]^(2*d))}   # calculate \hat{H}(d)
  1/m*(aux.M)
}

#' Modified complex-valued element of derivative of objective function in R08.
#' @keywords internal
G.hat.star<-function(X,d,m){
 if(which.max(dim(X))==1){X<-t(X)} # convert matrix in q x n - dimensional matrix if it is n x q
  T<-ncol(X)                        # number of observations
  dim_series<-nrow(X)               # number of dimensions
  lambdaj<-2*pi*(1:T)/T
  I.j<-Peri(X)                      # calculate periodogram with function Peri()
  aux.M<-matrix(0,dim_series,dim_series)  # auxillary matrix to calculate mean of first m fourier frequencies
  for(j in 1:m){aux.M<-aux.M+ I.j[,,j]*lambdaj[j]^(2*d)}   # calculate \hat{G}^*(d)
  1/m*(aux.M)
}

#' Modified complex-valued element of derivative of objective function in R08.
#' @keywords internal
H.hat.star<-function(X,d,m){
  if(which.max(dim(X))==1){X<-t(X)} # convert matrix in q x n - dimensional matrix if it is n x q
  T<-ncol(X)                        # number of observations
  dim_series<-nrow(X)               # number of dimensions
  lambdaj<-2*pi*(1:T)/T
  logj.grid<-log(1:m)
  nu_j<-logj.grid-mean(logj.grid)
  I.j<-Peri(X)                      # calculate periodogram with function Peri()
  aux.M<-matrix(0,dim_series,dim_series)  # auxillary matrix to calculate mean of first m fourier frequencies
  for(j in 1:m){aux.M<-aux.M+ nu_j[j]*I.j[,,j]*lambdaj[j]^(2*d)}   # calculate \hat{H}^*(d)
  1/m*(aux.M)
}

#' Modified version of Lambda in spectral density in R08 
#' @keywords internal
Phi_j<-function(X,dvec){ 
 if(which.max(dim(X))==1){X<-t(X)} # convert matrix in q x n - dimensional matrix if it is n x q
 T<-ncol(X)                        # number of observations
 dim_series<-nrow(X)               # number of dimensions
 lambdaj<-2*pi*(1:T)/T
 Lambda<-array(0,dim=c(dim_series, dim_series, T))  # create array for Lambda values
 for(j in 1:T){Lambda[,,j]<-diag(lambdaj[j]^(dvec))}
 Lambda
}

#' Modified complex-valued element of derivative of objective function in R08 accounting for non-equal memory.
#' @keywords internal
G.hat.star.star<-function(X,d,m){
  if(which.max(dim(X))==1){X<-t(X)} # convert matrix in q x n - dimensional matrix if it is n x q
  T<-ncol(X)                        # number of observations
  dim_series<-nrow(X)               # number of dimensions
  L1<-Phi_j(X,dvec=d)               # Phi_j \neq Lambda_j (because no e^i)
  I.j<-Peri(X)                      # calculate periodogram with function Peri()
  aux.M<-matrix(0,dim_series,dim_series)   # auxillary matrix to calculate mean of first m fourier frequencies
  for(j in 1:m){aux.M<-aux.M+(L1[,,j]%*%I.j[,,j]%*%L1[,,j] )}   
  1/m*(aux.M)
}

#' Modified complex-valued element of derivative of objective function in R08 accounting for non-equal memory.
#' @keywords internal
H.hat.star.star<-function(X,d,m){
  if(which.max(dim(X))==1){X<-t(X)} # convert matrix in q x n - dimensional matrix if it is n x q
  T<-ncol(X)                        # number of observations
  dim_series<-nrow(X)               # number of dimensions
  logj.grid<-log(1:m)
  nu_j<-logj.grid-mean(logj.grid)
  L1<-Phi_j(X,dvec=d)               # Phi_j \neq Lambda_j (because no e^i)
  I.j<-Peri(X)                      # calculate periodogram with function Peri()
  aux.M<-matrix(0,dim_series,dim_series) # auxillary matrix to calculate mean of first m fourier frequencies
  for(j in 1:m){aux.M<-aux.M+( nu_j[j]* L1[,,j]%*%I.j[,,j]%*%L1[,,j] )}   
  1/m*(aux.M)
}

#' Derivative of multivariate local Whittle objective function in R08.
#' @keywords internal
s.delta<-function(G,H){sum(diag(solve(G)%*%H))}


#' @title Hausman-type test for fractional cointegration (Robinson (2008))
#' @description \code{FCI_R08} Semiparametric Hausmann-type test for fractional cointegration by Robinson (2008).
#'  Returns test statistic, critical value, testing decision and type. Null hypothesis: no fractional cointegration.
# #' @details add details here.
#' @param X data matrix.
#' @param m  bandwith parameter specifying the number of Fourier frequencies
#' used for the estimation, usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param type determines the implementation of the test statistic: \code{""} - real-valued, \code{"*"} - complex-valued, or \code{"**"} - complex-valued allowing for different memory parameters.
#' @param a.vec  weighting scheme for averaging univariate memory estimates, default is simple arithmetic mean.
#' @param alpha desired significance level. Default is \code{alpha=0.05}.
#' @references  Robinson, P. (2008): Diagnostic testing for cointegration. Journal of Econometrics, Vol. 143, No. 1, pp. 206 - 225.
#' @author Christian Leschinski, Michelle Voges
#' @examples
#' T<-1000
#' series<-FI.sim(T=T, q=2, rho=0.9, d=c(0.1,0.4), B=rbind(c(1,-1),c(0,1)))
#' FCI_R08(series, m=floor(T^0.75), type="*")
#' series<-FI.sim(T=T, q=2, rho=0.9, d=c(0.4,0.4))
#' FCI_R08(series,  m=floor(T^0.75), type="*")
#' @export

FCI_R08<-function(X, m, type=c("", "*", "**"), alpha=0.05,  a.vec=NULL){
  data_org<-as.matrix(X)
  if(which.max(dim(data_org))==2){data_org<-t(data_org)} # convert data matrix, so that all observations for the same unit are stacked in one col
  T<-max(dim(data_org))
  dim_series<-min(dim(data_org))
  if(is.null(a.vec)){a.vec<-rep(1/dim_series,dim_series)}
  type<-type[1]
  
  dvec<-apply(data_org,2,function(x){local.W(x,m=m)$d})       # use univariate LW estimates for "**" and "**h" (allows for different memory)
  if(type%in%c("","*")){dvec<-rep(mean(dvec),dim_series)} # take average over univariate LW estimates for "" and "*" (assumes equal memory)
    
  if(type==""){                  # type equal memory using Real valued estimates
  G<-G.hat(data_org,dvec,m)          # usual G matrix
  H<-H.hat(data_org,dvec,m)          # similar to G matrix but elements in sum weihted by Index of Fourier frequency
  }
  
  if(type=="*"){                 # do not use Re() in estimation but allow for complex valued estimates
  G<-G.hat.star(data_org,dvec,m)     # similar to G matrix but potenitally complex valued
  H<-H.hat.star(data_org,dvec,m)     # H matrix from above but ptentially complex valued
  }
 
  if(type=="**"){                # do not use Re() in estimation and do not average memory estimation
  G<-G.hat.star.star(data_org,dvec,m)
  H<-H.hat.star.star(data_org,dvec,m)  
  }

  D.hat<-diag(dim_series)        # generate diagonal matrix
   
   diag(D.hat)<-diag(Re(G))         # only diagonal of G estimate 
    R.hat<-solve(D.hat^0.5) %*% G %*% solve(D.hat^0.5)     
    # diagonal elements of G are real valued but because the off-diagonal elements might not, they might be saved as complex numbers with imaginary part =0
    
      if(type%in%c("","*")){
      A<-diag(dim_series)        # generate diagonal matrix
      diag(A)<-a.vec             # elements are weights for averaging the memory estimate
      trRARA<-sum(diag(R.hat%*%A%*%R.hat%*%A))
      teststat<-Re(m*s.delta(G,H)^2/(dim_series^2*trRARA-dim_series)) 
      }
      
        if(type=="**"){
        teststat<-Re(m*s.delta(G,H)^2/(sum(diag(R.hat%*%R.hat))-dim_series))
        }
  
  crit<-qchisq(p=(1-alpha), df=1)
  dec <- teststat>crit
  return(list("X.stat"=teststat, "crit"=crit, "reject"=dec, "type"=type))
}

