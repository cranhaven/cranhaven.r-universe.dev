#' Generic GOF tests based on EDF
#'
#' @description
#' This function takes in an i.i.d. random sample, use MLE to estimate parameters of
#' the assumed distribution, compute probability integral transforms, and computes Cramér-von Mises,
#' Anderson-Darling and Watson statistics and their P-values using \code{\link[CompQuadForm]{imhof}}
#' function in \code{CompQuadForm}.
#'
#' @inherit AD.pvalue details
#'
#' @param x A random sample.
#' @param print Logical; if TRUE print both statistics and P-values; if FALSE the results are
#'   returned invisibly.
#' @param verbose verbose Logical; if TRUE, print warning messages.
#'
#' @return Cramér-von Mises, Anderson-Darling and Watson statistics and their P-values.
#'
#' @seealso
#' \code{\link{gof.sandwich}} for general distributions using Sandwich estimation
#'   of covariance function;
#' \code{\link{gof.bootstrap}} for generic functions using bootstrap method.
#'
#' @name gof
#' @examples
#' x0=runif(n=100,min=-1,max=1)
#' gof.uniform(x0,print=FALSE)
#'
#' x1=rnorm(n=100,mean=0,sd=1)
#' gof.normal(x1)
#'
#' x2=rgamma(n=100,shape=1,scale=1)
#' gof.gamma(x2)
#'
#' x3=rlogis(n=100,location=0,scale=1)
#' gof.logistic(x3)
#'
#' x4= rmutil::rlaplace(n=100,m=0,s=1)
#' gof.laplace(x4)
#'
#' x5=rweibull(n=100,shape=1,scale=1)
#' gof.weibull(x5)
#' x5_log=log(x5)
#' gof.extremevalue(x5_log)
#'
#' x6=rexp(n=100,rate=1/2)
#' gof.exp(x6)
NULL

#' @export
#' @rdname gof
gof.uniform=function(x,print=FALSE,verbose=FALSE){
  #  Compute three gof statistics
  w = CvM.uniform(x)
  a = AD.uniform(x)
  u = Watson.uniform(x)

  #  Compute their p-values
  w.p=CvM.uniform.pvalue(w,verbose=verbose)$P
  if(verbose){
    cat("Cramer-von Mises P-value output \n")
    print(w.p)
    cat("\n\n")
  }
  a.p=AD.uniform.pvalue(a,verbose=verbose)$P
  if(verbose){
    cat("Anderson-Darling P-value output \n")
    print(a.p)
    cat("\n\n")
  }
  u.p=Watson.uniform.pvalue(u,verbose=verbose)$P
  if(verbose){
    cat("Watson P-value output \n")
    print(u.p)
    cat("\n\n")
  }
  if(print){
    cat("Cramer-von Mises statistic is ",w,"with P-value is ",w.p,"\n")
    cat("Anderson-Darling statistic is ",a,"with P-value is ",a.p,"\n")
    cat("Watson statistic is ",u,"with P-value is ",u.p,"\n")
  }
  invisible(list(Wsq=w,Wsq.pvalue=w.p,Asq=a,Asq.pvalue=a.p,Usq=u,Usq.pvalue=u.p))
}

#' @export
#' @rdname gof
gof.normal=function(x,print=FALSE,verbose=FALSE){
  #  Estimate the parameters
  pars=estimate.normal(x)
  if(verbose){cat("Normal parameter estimates", pars, "\n")}
  pit=pnorm(x,mean=pars[1],sd=pars[2])
  if(verbose){cat("PITs are done \n \n")}
  w = CvM(pit)
  a = AD(pit)
  u = Watson(pit)
  w.p=CvM.normal.pvalue(w,verbose=verbose)$P
  if(verbose){
    cat("Cramer-von Mises P-value output \n")
    print(w.p)
    cat("\n\n")
  }
  a.p=AD.normal.pvalue(a,verbose=verbose)$P
  if(verbose){
    cat("Anderson-Darling P-value output \n")
    print(a.p)
    cat("\n\n")
  }
  u.p=Watson.normal.pvalue(u,verbose=verbose)$P
  if(verbose){
    cat("Watson P-value output \n")
    print(u.p)
    cat("\n\n")
  }
  if(print){
    cat("Cramer-von Mises statistic is ",w,"with P-value is ",w.p,"\n")
    cat("Anderson-Darling statistic is ",a,"with P-value is ",a.p,"\n")
    cat("Watson statistic is ",u,"with P-value is ",u.p,"\n")
  }
  invisible(list(Wsq=w,Wsq.pvalue=w.p,Asq=a,Asq.pvalue=a.p,Usq=u,Usq.pvalue=u.p))
}

#' @export
#' @rdname gof
gof.gamma=function(x,print=FALSE,verbose=FALSE){
  pars=estimate.gamma(x)
  if(verbose){cat("Gamma parameter estimates", pars, "\n")}
  pit=pgamma(x,shape=pars[1],scale=pars[2])
  if(verbose){cat("PITs are done \n \n")}
  w = CvM(pit)
  a = AD(pit)
  u = Watson(pit)
  w.p=CvM.gamma.pvalue(w,shape=pars[1])$P
  if(verbose){
    cat("Cramer-von Mises P-value output \n")
    print(w.p)
    cat("\n\n")
  }
  a.p=AD.gamma.pvalue(a,shape=pars[1])$P
  if(verbose){
    cat("Anderson-Darling P-value output \n")
    print(a.p)
    cat("\n\n")
  }
  u.p=Watson.gamma.pvalue(u,shape=pars[1])$P
  if(verbose){
    cat("Anderson-Darling P-value output \n")
    print(u.p)
    cat("\n\n")
  }
  if(print){
    cat("Cramer-von Mises statistic is ",w,"with P-value is ",w.p,"\n")
    cat("Anderson-Darling statistic is ",a,"with P-value is ",a.p,"\n")
    cat("Watson statistic is ",u,"with P-value is ",u.p,"\n")
  }
  invisible(list(Wsq=w,Wsq.pvalue=w.p,Asq=a,Asq.pvalue=a.p,Usq=u,Usq.pvalue=u.p))
}

#' @export
#' @rdname gof
gof.logistic=function(x,print=FALSE,verbose=FALSE){
  pars=estimate.logistic(x)
  if(verbose){cat("log-Logistic parameter estimates", pars, "\n")}
  pit=plogis(x,location=pars[1],scale=pars[2])
  if(verbose){cat("PITs are done \n \n")}
  w = CvM(pit)
  a = AD(pit)
  u = Watson(pit)
  if(verbose){cat("Statistics are ",w,a,u," \n \n")}
  w.p=CvM.logistic.pvalue(w,verbose=verbose)$P
  if(verbose){
    cat("Cramer-von Mises P-value output \n")
    print(w.p)
    cat("\n\n")
  }
  a.p=AD.logistic.pvalue(a,verbose=verbose)$P
  if(verbose){
    cat("Anderson-Darling P-value output \n")
    print(a.p)
    cat("\n\n")
  }
  u.p=Watson.logistic.pvalue(u,verbose=verbose)$P
  if(verbose){
    cat("Watson P-value output \n")
    print(u.p)
    cat("\n\n")
  }
  if(print){
    cat("Cramer-von Mises statistic is ",w,"with P-value is ",w.p,"\n")
    cat("Anderson-Darling statistic is ",a,"with P-value is ",a.p,"\n")
    cat("Watson statistic is ",u,"with P-value is ",u.p,"\n")
  }
  invisible(list(Wsq=w,Wsq.pvalue=w.p,Asq=a,Asq.pvalue=a.p,Usq=u,Usq.pvalue=u.p))
}

#' @export
#' @rdname gof
gof.laplace=function(x,print=FALSE,verbose=FALSE){
  pars=estimate.laplace(x,use.sd=FALSE)
  if(verbose){cat("Laplace parameter estimates", pars, "\n")}
  pit=cdf.laplace(x,theta =pars)
  if(verbose){cat("PITs are done \n \n")}
  w = CvM(pit)
  a = AD(pit)
  u = Watson(pit)
  w.p=CvM.laplace.pvalue(w,verbose=verbose)$P
  if(verbose){
    cat("Cramer-von Mises P-value output \n")
    print(w.p)
    cat("\n\n")
  }
  a.p=AD.laplace.pvalue(a,verbose=verbose)$P
  if(verbose){
    cat("Anderson-Darling P-value output \n")
    print(a.p)
    cat("\n\n")
  }
  u.p=Watson.logistic.pvalue(u,verbose=verbose)$P
  if(verbose){
    cat("Watson P-value output \n")
    print(u.p)
    cat("\n\n")
  }
  if(print){
    cat("Cramer-von Mises statistic is ",w,"with P-value is ",w.p,"\n")
    cat("Anderson-Darling statistic is ",a,"with P-value is ",a.p,"\n")
    cat("Watson statistic is ",u,"with P-value is ",u.p,"\n")
  }
  invisible(list(Wsq=w,Wsq.pvalue=w.p,Asq=a,Asq.pvalue=a.p,Usq=u,Usq.pvalue=u.p))
}

#' @export
#' @rdname gof
gof.weibull=function(x,print=FALSE,verbose=FALSE){
  pars=estimate.weibull(x)
  if(verbose){cat("Weibull parameter estimates", pars, "\n")}
  pit=pweibull(x,shape=pars[1],scale=pars[2])
  if(verbose){cat("PITs are done \n \n")}
  w = CvM(pit)
  a = AD(pit)
  u = Watson(pit)
  w.p=CvM.weibull.pvalue(w,verbose=verbose)$P
  if(verbose){
    cat("Cramer-von Mises P-value output \n")
    print(w.p)
    cat("\n\n")
  }
  a.p=AD.weibull.pvalue(a,verbose=verbose)$P
  if(verbose){
    cat("Anderson-Darling P-value output \n")
    print(a.p)
    cat("\n\n")
  }
  u.p=Watson.weibull.pvalue(u,verbose=verbose)$P
  if(verbose){
    cat("Watson P-value output \n")
    print(u.p)
    cat("\n\n")
  }
  if(print){
    cat("Cramer-von Mises statistic is ",w,"with P-value is ",w.p,"\n")
    cat("Anderson-Darling statistic is ",a,"with P-value is ",a.p,"\n")
    cat("Watson statistic is ",u,"with P-value is ",u.p,"\n")
  }
  invisible(list(Wsq=w,Wsq.pvalue=w.p,Asq=a,Asq.pvalue=a.p,Usq=u,Usq.pvalue=u.p))
}

#' @export
#' @rdname gof
gof.extremevalue=function(x,print=FALSE,verbose=FALSE){
  xx=(x-mean(x))/sd(x)
  ww=exp(xx)
  pars=estimate.weibull(ww)
  # if(verbose){cat("Weibull parameter estimates", pars, "\n")}
  pit=pweibull(ww,shape=pars[1],scale=pars[2])
  # if(verbose){cat("PITs are done \n \n")}
  w = CvM(pit)
  a = AD(pit)
  u = Watson(pit)
  w.p=CvM.weibull.pvalue(w,verbose=verbose)$P
  if(verbose){
    cat("Cramer-von Mises P-value output \n")
    print(w.p)
    cat("\n\n")
  }
  a.p=AD.weibull.pvalue(a,verbose=verbose)$P
  if(verbose){
    cat("Anderson-Darling P-value output \n")
    print(a.p)
    cat("\n\n")
  }
  u.p=Watson.weibull.pvalue(u,verbose=verbose)$P
  if(verbose){
    cat("Watson P-value output \n")
    print(u.p)
    cat("\n\n")
  }
  if(print){
    cat("Cramer-von Mises statistic is ",w,"with P-value is ",w.p,"\n")
    cat("Anderson-Darling statistic is ",a,"with P-value is ",a.p,"\n")
    cat("Watson statistic is ",u,"with P-value is ",u.p,"\n")
  }
  invisible(list(Wsq=w,Wsq.pvalue=w.p,Asq=a,Asq.pvalue=a.p,Usq=u,Usq.pvalue=u.p))
}

#' @export
#' @rdname gof
gof.exp=function(x,print=FALSE,verbose=FALSE){
  pars=estimate.exp(x)
  if(verbose){cat("Exponential parameter estimates", pars, "\n")}
  pit=pexp(x,rate=1/pars)
  if(verbose){cat("PITs are done \n \n")}
  w = CvM(pit)
  a = AD(pit)
  u = Watson(pit)
  w.p=CvM.exp.pvalue(w,verbose=verbose)$P
  if(verbose){
    cat("Cramer-von Mises P-value output \n")
    print(w.p)
    cat("\n\n")
  }
  a.p=AD.exp.pvalue(a,verbose=verbose)$P
  if(verbose){
    cat("Anderson-Darling P-value output \n")
    print(a.p)
    cat("\n\n")
  }
  u.p=Watson.exp.pvalue(u,verbose=verbose)$P
  if(verbose){
    cat("Watson P-value output \n")
    print(u.p)
    cat("\n\n")
  }
  if(print){
    cat("Cramer-von Mises statistic is ",w,"with P-value is ",w.p,"\n")
    cat("Anderson-Darling statistic is ",a,"with P-value is ",a.p,"\n")
    cat("Watson statistic is ",u,"with P-value is ",u.p,"\n")
  }
  invisible(list(Wsq=w,Wsq.pvalue=w.p,Asq=a,Asq.pvalue=a.p,Usq=u,Usq.pvalue=u.p))
}
