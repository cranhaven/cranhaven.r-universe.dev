#' GOF tests for general distributions using Sandwich estimation of covariance function
#'
#' @description
#' This function tests the hypothesis that data y come from
#' distribution \code{Fdist} with unknown parameter values theta.
#' Estimates of theta must be provided in \code{thetahat}.
#'
#' @details
#' It uses a large sample approximation to the limit distribution
#' based on the use of the score function components
#' to estimate the Fisher information and the limiting covariance
#' function of the empirical process.
#'
#' The estimates \code{thetahat} should be roots of the likelihood equations.
#'
#' @param y A random sample or the response of regression problem.
#' @param x The matrix of covariates.
#' @param Fdist User supplied function to compute probability integral transform of y.
#' @param thetahat Parameter estimates by MLE.
#' @param Score User supplied function to compute 3 components of the score function
#'   an n by p matrix with entries partial log f(y_i,\eqn{\theta})/ partial theta_j.
#' @param m Eigenvalues are extracted for an m by m grid of the covariance function.
#' @param ... Other inputs passed to \code{Fdist} and \code{Score} when needed.
#'
#' @return Cramér-von Mises, Anderson-Darling and Watson statistics and their P-values.
#'
#' @seealso
#' \code{\link{gof}} for generic functions using \code{\link[CompQuadForm]{imhof}} function;
#' \code{\link{gof.bootstrap}} for generic functions using bootstrap method.
#'
#' @examples
#' sample = rnorm(n=100,mean=0,sd=1)
#' mle = estimate.normal(sample)
#' cdf.normal.user = function(x,theta){
#'  pnorm(x,mean=theta[1],sd=theta[2])
#' }
#' score.normal.user = function(x,theta){
#'  sig=theta[2]
#'  mu=theta[1]
#'  s.mean= (x-mu)/sig
#'  s.sd= s.mean^2/sig-length(x)/sig
#'  cbind(s.mean/sig,s.sd)
#' }
#' output = gof.sandwich(y=sample,Fdist=cdf.normal.user,thetahat=mle,Score=score.normal.user,m=100)
#' output
#' @export
gof.sandwich=function(y,x=NULL,Fdist,thetahat,Score,m=max(n,100),...){
  n = length(y)                   # Sample size
  p=length(thetahat)              # Number of parameters
  if(is.null(x)){
    pit = Fdist(y,thetahat,...)   # Computes the probability integral transforms
  }else{
    pit=Fdist(y,x,thetahat,...)
  }
  if(is.null(x)){
    u =Score(y,thetahat,...)      # Components of the score: n by p matrix
  }else{
    u =Score(y,x,thetahat,...)    # Components of the score: n by p matrix
  }
  Fisher = t(u)%*% u / n          # Estimate of Fisher information in 1 point.
  s=(1:m)/(m+1)                   # Grid on which to compute covariance matrix.
  #
  ind=function(x,y){
    one = rep(1,length(x))
    one[x>y] = 0
    one
  }
  #
  Dfb = outer(pit,s,ind)
  #   Dfb is a matrix whose entry i,j is 1 if pit[i] > s[j]
  #       it is n by m
  Df=t(Dfb) %*% u/n  # This is an m by p matrix.
  m1 = outer(s,s,pmin)
  m2 = outer(s,s,"*")
  Sigma.CvM =  m1-m2-Df%*%solve(Fisher,t(Df))
  Sigma.AD =  Sigma.CvM/sqrt(outer(s*(1-s),s*(1-s),"*"))
  J = diag(m) - matrix(1/m,m,m)
  Sigma.Watson =  J %*% Sigma.CvM %*% J
  Evals.CvM = eigen(Sigma.CvM)$values/m
  Evals.AD = eigen(Sigma.AD)$values/m
  Evals.Watson = eigen(Sigma.Watson)$values/m
  stat=gof.statistics.only(pit)
  P.CvM = imhof(stat$CvM,Evals.CvM)$Qq
  P.AD = imhof(stat$AD,Evals.AD)$Qq
  P.Watson = imhof(stat$Watson,Evals.Watson)$Qq
  #
  list(CvM=list(W2=stat$CvM,P_value=P.CvM),
       AD=list(A2=stat$AD,P_value=P.AD),
       Watson=list(U2=stat$Watson,P_value=P.Watson))
}


# Helpers -----------------------------------------------------------------
gof.statistics.only=function(pit,AD=TRUE,CvM=TRUE,Watson=TRUE){
  if(any(pit<0))stop("Negative Probability Integral Transforms Not Allowed")
  if(any(pit>1))stop("Probability Integral Transforms More than 1 Not Allowed")
  p=sort(pit)
  out=list()
  if(AD){out$AD = AD(p)}
  if(CvM){out$CvM = CvM(p)}
  if(Watson){out$Watson = Watson(p)}
  return(out)
}

score.normal = function(x,theta){
  sig=theta[2]
  mu=theta[1]
  s.mean= (x-mu)/sig
  s.sd= s.mean^2/sig-length(x)/sig
  cbind(s.mean/sig,s.sd)
}

score.gamma=function(x,theta){
  scale=theta[2]
  shape=theta[1]
  s.shape= log(x/scale)-digamma(shape)
  s.scale= x/scale^2 -shape/scale
  cbind(s.shape,s.scale)
}

score.laplace = function(x,theta){
  sig=theta[2]
  mu=theta[1]
  signum<-function(x){
    y=x/abs(x)
    y[is.na(y)]=0
    return(y)
  }
  s.mean= signum(x-mu)/sig
  s.sd= s.mean^2/sig-length(x)/sig
  cbind(s.mean,s.sd)
}

score.weibull=function(x,theta){
  scale=theta[2]
  shape=theta[1]
  r=x/scale
  lr=log(r)
  s.shape= 1/shape+lr-lr*r^shape
  s.scale= shape*(r^shape-1)/scale
  cbind(s.shape,s.scale)
}

