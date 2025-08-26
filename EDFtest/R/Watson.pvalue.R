#' P-value of Watson statistic
#'
#' @description
#' Compute the P-value of the given Watson statistic \eqn{U^2}
#' using \code{\link[CompQuadForm]{imhof}} function in \code{CompQuadForm}.
#'
#' @inherit AD.pvalue details
#'
#' @param u Watson statistic \eqn{U^2} with a given distribution.
#' @param neig Number of eigenvalues used for \code{imhof()}.
#' @param verbose Logical; if TRUE, print warning messages.
#' @param shape The shape parameter of Gamma distribution.
#'
#' @return P-value of the given Watson statistic.
#'
#' @seealso
#' \code{\link{Watson}} for calculating Watson statistic;
#' \code{\link{CvM.pvalue}} for calculating P-value of Cramér-von Mises statistic;
#' \code{\link{AD.pvalue}} for calculating P-value of Anderson-Darling statistic.
#'
#' @name Watson.pvalue
#' @examples
#' x0=runif(n=100,min=-1,max=1)
#' usq0 = Watson.uniform(x0)
#' Watson.uniform.pvalue(usq0)
#'
#' x1=rnorm(n=100,mean=0,sd=1)
#' usq1 = Watson.normal(x1)
#' Watson.normal.pvalue(usq1)
#'
#' x2=rgamma(n=100,shape=1,scale=1)
#' usq2 = Watson.gamma(x2)
#' Watson.gamma.pvalue(usq2,1)
#'
#' x3=rlogis(n=100,location=0,scale=1)
#' usq3 = Watson.logistic(x3)
#' Watson.logistic.pvalue(usq3)
#'
#' x4= rmutil::rlaplace(n=100,m=0,s=1)
#' usq4 = Watson.laplace(x4)
#' Watson.laplace.pvalue(usq4)
#'
#' x5=rweibull(n=100,shape=1,scale=1)
#' usq5 = Watson.weibull(x5)
#' Watson.weibull.pvalue(usq5)
#' x5_log=log(x5)
#' Watson.extremevalue.pvalue(Watson.extremevalue(x5_log))
#'
#' x6=rexp(n=100,rate=1/2)
#' usq6 = Watson.exp(x6)
#' Watson.exp.pvalue(usq6)
NULL

#' @export
#' @rdname Watson.pvalue
Watson.uniform.pvalue = function(u,neig=100,verbose=FALSE){
  e = Watson.uniform.eigen(neig)
  plb=pchisq(u/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(u,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for U = ",u," and neig = ",neig,
                      " imhof returned u negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for U = ",u," and neig = ",neig,
                    " p was replaced by u lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname Watson.pvalue
Watson.normal.pvalue = function(u,neig=100,verbose=FALSE){
  e = Watson.normal.eigen(neig)
  plb=pchisq(u/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(u,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for U = ",u," and neig = ",neig,
                      " imhof returned u negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for U = ",u," and neig = ",neig,
                    " p was replaced by u lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname Watson.pvalue
Watson.gamma.pvalue = function(u,shape,neig = 100,verbose=FALSE){
  e = Watson.gamma.eigen(neig,shape=shape)
  plb=pchisq(u/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(u,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for U = ",u," and neig = ",neig,
                      " imhof returned u negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for U = ",u," and neig = ",neig,
                    " p was replaced by u lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname Watson.pvalue
Watson.logistic.pvalue = function(u,neig=100,verbose=FALSE){
  e = Watson.logistic.eigen(neig)
  plb=pchisq(u/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(u,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for U = ",u," and neig = ",neig,
                      " imhof returned u negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for U = ",u," and neig = ",neig,
                    " p was replaced by u lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname Watson.pvalue
Watson.laplace.pvalue = function(u,neig=100,verbose=FALSE){
  e = Watson.laplace.eigen(neig)
  plb=pchisq(u/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(u,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for U = ",u," and neig = ",neig,
                      " imhof returned u negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for U = ",u," and neig = ",neig,
                    " p was replaced by u lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname Watson.pvalue
Watson.weibull.pvalue = function(u,neig=100,verbose=FALSE){
  e = Watson.weibull.eigen(neig)
  plb=pchisq(u/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(u,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for U = ",u," and neig = ",neig,
                      " imhof returned u negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for U = ",u," and neig = ",neig,
                    " p was replaced by u lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

#' @export
#' @rdname Watson.pvalue
Watson.extremevalue.pvalue = function(u,neig=100,verbose=FALSE){
  Watson.weibull.pvalue(u=u,neig=neig,verbose=verbose)
}

#' @export
#' @rdname Watson.pvalue
Watson.exp.pvalue = function(u,neig=100,verbose=FALSE){
  e = Watson.exp.eigen(neig)
  plb=pchisq(u/max(e),df=1,lower.tail = FALSE)
  warn=getOption("warn")
  im = imhof(u,lambda=e,epsabs = 1e-9,limit=2^7)
  options(warn=warn)
  aerror=im$abserr
  p=im$Qq
  if(p<0&&verbose)cat("for U = ",u," and neig = ",neig,
                      " imhof returned u negative probability\n")
  if(p<plb){
    p=plb
    if(verbose) cat("for U = ",u," and neig = ",neig,
                    " p was replaced by u lower bound on p: ",plb, "\n")
  }
  list(P=p,error=aerror)
}

# Helpers -----------------------------------------------------------------

Watson.uniform.eigen = function(n){
  M=Watson.uniform.covmat(n)
  eigen(M)$values/n
}

Watson.uniform.covmat=function(n){
  (diag(n)-matrix(1/n,n,n)) %*% CvM.uniform.covmat(n) %*% (diag(n)-matrix(1/n,n,n))
}

Watson.normal.eigen = function(n){
  M=Watson.normal.covmat(n)
  eigen(M)$values/n
}

Watson.normal.covmat=function(n){
  (diag(n)-matrix(1/n,n,n)) %*% CvM.normal.covmat(n) %*% (diag(n)-matrix(1/n,n,n))
}

Watson.gamma.eigen = function(n,shape){
  M=Watson.gamma.covmat(n,shape)
  eigen(M)$values/n
}

Watson.gamma.covmat=function(n,shape){
  (diag(n)-matrix(1/n,n,n)) %*% CvM.gamma.covmat(n,shape=shape) %*% (diag(n)-matrix(1/n,n,n))
}

Watson.logistic.eigen = function(n){
  M=Watson.logistic.covmat(n)
  eigen(M)$values/n
}

Watson.logistic.covmat=function(n){
  (diag(n)-matrix(1/n,n,n)) %*% CvM.logistic.covmat(n) %*% (diag(n)-matrix(1/n,n,n))
}

Watson.laplace.eigen = function(n){
  M=Watson.laplace.covmat(n)
  eigen(M)$values/n
}

Watson.laplace.covmat=function(n){
  (diag(n)-matrix(1/n,n,n)) %*% CvM.laplace.covmat(n) %*% (diag(n)-matrix(1/n,n,n))
}

Watson.weibull.eigen = function(n){
  M=Watson.weibull.covmat(n)
  eigen(M)$values/n
}

Watson.weibull.covmat=function(n){
  (diag(n)-matrix(1/n,n,n)) %*% CvM.weibull.covmat(n) %*% (diag(n)-matrix(1/n,n,n))
}

Watson.exp.eigen = function(n){
  M=Watson.exp.covmat(n)
  eigen(M)$values/n
}

Watson.exp.covmat=function(n){
  (diag(n)-matrix(1/n,n,n)) %*% CvM.exp.covmat(n) %*% (diag(n)-matrix(1/n,n,n))
}

