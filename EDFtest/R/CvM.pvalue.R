#' P-value of Cramer-von Mises statistic
#'
#' @description
#' Compute the P-value of the given Cramér-von Mises statistic \eqn{W^2}
#' using \code{\link[CompQuadForm]{imhof}} function in \code{CompQuadForm}.
#'
#' @inherit AD.pvalue details
#'
#' @param w Cramér-von Mises statistic \eqn{W^2} with a given distribution.
#' @param neig Number of eigenvalues used for \code{\link[CompQuadForm]{imhof}}.
#' @param verbose Logical; if TRUE, print warning messages.
#' @param shape The shape parameter of Gamma distribution.
#'
#' @return P-value of the given Cramér-von Mises statistic.
#'
#' @seealso
#' \code{\link{CvM}} for calculating Cramér-von Mises statistic;
#' \code{\link{AD.pvalue}} for calculating P-value of Anderson-Darling statistic;
#' \code{\link{Watson.pvalue}} for calculating P-value of Watson statistic.
#'
#' @name CvM.pvalue
#' @examples
#' x0=runif(n=100,min=-1,max=1)
#' wsq0 = CvM.uniform(x0)
#' CvM.uniform.pvalue(wsq0)
#'
#' x1=rnorm(n=100,mean=0,sd=1)
#' wsq1 = CvM.normal(x1)
#' CvM.normal.pvalue(wsq1)
#'
#' x2=rgamma(n=100,shape=1,scale=1)
#' wsq2 = CvM.gamma(x2)
#' CvM.gamma.pvalue(wsq2,1)
#'
#' x3=rlogis(n=100,location=0,scale=1)
#' wsq3 = CvM.logistic(x3)
#' CvM.logistic.pvalue(wsq3)
#'
#' x4= rmutil::rlaplace(n=100,m=0,s=1)
#' wsq4 = CvM.laplace(x4)
#' CvM.laplace.pvalue(wsq4)
#'
#' x5=rweibull(n=100,shape=1,scale=1)
#' wsq5 = CvM.weibull(x5)
#' CvM.weibull.pvalue(wsq5)
#' x5_log=log(x5)
#' CvM.extremevalue.pvalue(CvM.extremevalue(x5_log))
#'
#' x6=rexp(n=100,rate=1/2)
#' wsq6 = CvM.exp(x6)
#' CvM.exp.pvalue(wsq6)
NULL

#' @export
#' @rdname CvM.pvalue
CvM.uniform.pvalue = function(w,neig=100,verbose=FALSE){
  e = CvM.uniform.eigen(neig)
  plb=pchisq(w/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(w,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for W = ",w," and neig = ",neig,
                      " imhof returned a negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for W = ",w," and neig = ",neig,
                    " p was replaced by a lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname CvM.pvalue
CvM.normal.pvalue = function(w,neig=100,verbose=FALSE){
  e = CvM.normal.eigen(neig)
  plb=pchisq(w/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(w,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for W = ",w," and neig = ",neig,
                      " imhof returned a negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for W = ",w," and neig = ",neig,
                    " p was replaced by a lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname CvM.pvalue
CvM.gamma.pvalue = function(w,shape , neig = 100,verbose=FALSE){
  e = CvM.gamma.eigen(neig,shape=shape)
  plb=pchisq(w/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(w,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for W = ",w," and neig = ",neig,
                      " imhof returned a negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for W = ",w," and neig = ",neig,
                    " p was replaced by a lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname CvM.pvalue
CvM.logistic.pvalue = function(w,neig=100,verbose=FALSE){
  e = CvM.logistic.eigen(neig)
  plb=pchisq(w/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(w,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for W = ",w," and neig = ",neig,
                      " imhof returned a negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for W = ",w," and neig = ",neig,
                    " p was replaced by a lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname CvM.pvalue
CvM.laplace.pvalue = function(w,neig=100,verbose=FALSE){
  e = CvM.laplace.eigen(neig)
  plb=pchisq(w/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(w,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for W = ",w," and neig = ",neig,
                      " imhof returned a negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for W = ",w," and neig = ",neig,
                    " p was replaced by a lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname CvM.pvalue
CvM.weibull.pvalue = function(w,neig=100,verbose=FALSE){
  e=CvM.weibull.eigen(neig)
  plb=pchisq(w/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(w,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for W = ",w," and neig = ",neig,
                      " imhof returned a negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for W = ",w," and neig = ",neig,
                    " p was replaced by a lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname CvM.pvalue
CvM.extremevalue.pvalue = function(w,neig=100,verbose=FALSE){
  CvM.weibull.pvalue(w=w,neig=neig,verbose=verbose)
}

#' @export
#' @rdname CvM.pvalue
CvM.exp.pvalue = function(w,neig=100,verbose=FALSE){
  e=CvM.exp.eigen(neig)
  plb=pchisq(w/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(w,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for W = ",w," and neig = ",neig,
                      " imhof returned a negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for W = ",w," and neig = ",neig,
                    " p was replaced by a lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

# Helpers -----------------------------------------------------------------

CvM.uniform.eigen = function(n){
  mean.wsq.uniform=1/6
  M=CvM.uniform.covmat(n)
  e=eigen(M)$values/n
  e*mean.wsq.uniform/sum(e)
}

CvM.uniform.covmat=function(n){
  s=1:n
  s=s/(n+1)
  M1=outer(s,s,pmin)-outer(s,s)
  M1
}

CvM.normal.eigen = function(n){
  mean.wsq.normal=1/6 -7*sqrt(3)/(36*pi)
  M=CvM.normal.covmat(n)
  e=eigen(M)$values/n
  e*mean.wsq.normal/sum(e)
}

CvM.normal.covmat=function(n){
  Fisher.normal = matrix(c(1,0,0,2),nrow=2)
  s=1:n
  s=s/(n+1)
  M1=outer(s,s,pmin)-outer(s,s)
  x = qnorm(s)
  G1 = dnorm(x)
  G2 = -x*G1
  M2 = cbind(G1,G2)
  M1-M2%*%solve(Fisher.normal,t(M2))
}

CvM.gamma.eigen = function(n,shape){
  # I don't have code for this yet
  # mean = 1/6 -?
  M=CvM.gamma.covmat(n,shape)
  e=eigen(M)$values/n
  e # *mean/sum(e)
}

CvM.gamma.covmat=function(n,shape){
  fisher.information.gamma=function(shape.hat){
    #
    # returns the estimated Fisher Information per point
    # for a gamma regression model in which the log mean is predicted
    # linearly from a matrix of covariates x
    # Normally x will contain an intercept term
    #
    FI=matrix(0,nrow=2,ncol=2)
    FI[1,1]=trigamma(shape.hat)
    FI[1,2]= 1
    FI[2,1]= 1
    FI[2,2]=shape.hat
    FI
  }
  g = gamma(shape)
  dg = digamma(shape)
  FI = fisher.information.gamma(shape.hat = shape)
  s=1:n
  s=s/(n+1)
  M1=outer(s,s,pmin)-outer(s,s)
  G1 = s*0
  Q = qgamma(s,shape=shape)
  D = dgamma(Q,shape=shape)
  G2 = - Q * D
  g1.integrand = function(x,shape){log(x)*x^(shape-1)*exp(-x)}
  for(i in 1:n){
    G1[i] = integrate(g1.integrand,0,Q[i],shape=shape)$value/g -s[i]*dg
  }
  M2 = cbind(G1,G2)
  M1-M2%*%solve(FI,t(M2))
}

CvM.logistic.eigen = function(n){
  mean.wsq.logistic= 1/6 -(4*pi^2-9)/(20*(pi^2+3))  # from Maple
  M=CvM.logistic.covmat(n)
  e=eigen(M)$values/n
  e*mean.wsq.logistic/sum(e)
}

CvM.logistic.covmat=function(n){
  Fisher.logistic = matrix(c(1/3,0,0,(pi^2+3)/9),nrow=2)
  s=1:n
  s=s/(n+1)
  t=s
  M1=outer(s,t,pmin)-outer(s,t)
  G1 = s*(1-s)
  G2 = G1*(log(s/(1-s)))
  M2 = cbind(G1,G2)
  M1-M2%*%solve(Fisher.logistic,t(M2))
}

CvM.laplace.eigen  = function(n){
  mean = 1/6 -1/12-1/54
  M=CvM.laplace.covmat(n)
  e=eigen(M)$values/n
  e * mean / sum(e)
}

CvM.laplace.covmat=function(n){
  Fisher.laplace = matrix(c(1,0,0,1),nrow=2)
  s=1:n
  s=s/(n+1)
  M1=outer(s,s,pmin)-outer(s,s)
  G1 = -s
  G1[s>0.5]=(s-1)[s>0.5]
  G2 = -s*log(2*s)
  G2[s>0.5] = ((1-s)*log(2*(1-s)))[s>0.5]
  M2 = cbind(G1,G2)
  M1-M2%*%solve(Fisher.laplace,t(M2))
}

CvM.weibull.eigen = function(n){
  mean.wsq.weibull=(1/54)-(4/9)*((log(3))^2-log(3)-1)/pi^2  # from Maple
  M=CvM.weibull.covmat(n)
  e=eigen(M)$values/n
  e*mean.wsq.weibull/sum(e)
}

CvM.weibull.covmat=function(n){
  Fisher.weibull = matrix(c(pi^2/6+(1+digamma(1))^2,-(1+digamma(1)),-(1+digamma(1)),1),nrow=2)
  s=1:n-0.5
  s=s/(n)
  t=s
  M1=outer(s,t,pmin)-outer(s,t)
  G1 = -log(1-s)*log(-log(1-s))*(1-s)
  G2 = log(1-s)*(1-s)
  M2 = cbind(G1,G2)
  M1-M2%*%solve(Fisher.weibull,t(M2))
}

CvM.exp.eigen = function(n){
  mean = 5/54 # from Maple
  M=CvM.exp.covmat(n)
  e=eigen(M)$values/n
  e  * mean / sum(e)
}

CvM.exp.covmat=function(n){
  Fisher.exp = 1 # fisher.information.exp=1
  s=1:n
  s=s/(n+1)
  M1=outer(s,s,pmin)-outer(s,s)
  G1 = -log(1-s)*(1-s)
  M2 = cbind(G1)
  M1-M2%*%solve(Fisher.exp,t(M2))
}








