#' Generic GOF tests based on EDF using bootstrap
#'
#' @description
#' This function takes in an i.i.d. random sample, use MLE to estimate parameters of
#' the assumed distribution, compute probability integral transforms, and computes Cramér-von Mises,
#' Anderson-Darling and Watson statistics and their P-values using bootstrap method.
#'
#' @param x A random sample.
#' @param M Number of bootstrap, 10000 by default.
#'
#' @return Cramér-von Mises, Anderson-Darling and Watson statistics and their P-values.
#'
#' @seealso
#' \code{\link{gof.sandwich}} for general distributions using Sandwich estimation
#'   of covariance function;
#' \code{\link{gof}} for generic functions using \code{\link[CompQuadForm]{imhof}} function.
#'
#' @name gof.bootstrap
#' @examples
#' x0=runif(n=100,min=-1,max=1)
#' gof.uniform.bootstrap(x0,M=100)
#'
#' x1=rnorm(n=100,mean=0,sd=1)
#' gof.normal.bootstrap(x1,M=100)
#'
#' x2=rgamma(n=100,shape=1,scale=1)
#' gof.gamma.bootstrap(x2,M=100)
#'
#' x3=rlogis(n=100,location=0,scale=1)
#' gof.logistic.bootstrap(x3,M=100)
#'
#' x4= rmutil::rlaplace(n=100,m=0,s=1)
#' gof.laplace.bootstrap(x4,M=100)
#'
#' x5=rweibull(n=100,shape=1,scale=1)
#' gof.weibull.bootstrap(x5,M=100)
#' x5_log=log(x5)
#' gof.extremevalue.bootstrap(x5_log,M=100)
#'
#' x6=rexp(n=100,rate=1/2)
#' gof.exp.bootstrap(x6,M=100)
NULL

#' @export
#' @rdname gof.bootstrap
gof.uniform.bootstrap<-function(x, M=10000){
  a2 <- AD.uniform(x)
  w2 <- CvM.uniform(x)
  u2 <- Watson.uniform(x)
  n <- length(x)
  pars <- estimate.uniform(x)
  mi <- pars[1]
  ma <- pars[2]
  dat <- runif(n*M,min=mi,max=ma)
  dat <- matrix(dat,nrow=M)
  a2vals <- apply(dat,1,AD.uniform)
  w2vals <- apply(dat,1,CvM.uniform)
  u2vals <- apply(dat,1,Watson.uniform)
  a.pv <- length(a2vals[a2vals>a2])/M
  w.pv <- length(w2vals[w2vals>w2])/M
  u.pv <- length(u2vals[u2vals>u2])/M
  w2text <- paste("Cramer-von Mises statistic is ", as.character(round(w2,7)))
  w2text <- paste(w2text,"with P-value is ", as.character(round(w.pv,7)),"\n")
  a2text <- paste("Anderson-Darling statistic is ", as.character(round(a2,7)))
  a2text <- paste(a2text,"with P-value is ", as.character(round(a.pv,7)),"\n")
  u2text <- paste("Watson statistic is ", as.character(round(u2,7)))
  u2text <- paste(u2text,"with P-value is ", as.character(round(u.pv,7)),"\n")
  cat(w2text)
  cat(a2text)
  cat(u2text)
  invisible(list(Wsq=w2,Wsq.pvalue=w.pv,Asq=a2,Asq.pvalue=a.pv,Usq=u2,Usq.pvalue=u.pv))
}

#' @export
#' @rdname gof.bootstrap
gof.normal.bootstrap<-function(x, M=10000){
  a2 <- AD.normal(x)
  w2 <- CvM.normal(x)
  u2 <- Watson.normal(x)
  n <- length(x)
  pars <- estimate.normal(x)
  xbar <- pars[1]
  s <- pars[2]
  dat <- rnorm(n*M,mean=xbar,sd=s)
  dat <- matrix(dat,nrow=M)
  a2vals <- apply(dat,1,AD.normal)
  w2vals <- apply(dat,1,CvM.normal)
  u2vals <- apply(dat,1,Watson.normal)
  a.pv <- length(a2vals[a2vals>a2])/M
  w.pv <- length(w2vals[w2vals>w2])/M
  u.pv <- length(u2vals[w2vals>u2])/M
  w2text <- paste("Cramer-von Mises statistic is ", as.character(round(w2,7)))
  w2text <- paste(w2text,"with P-value is ", as.character(round(w.pv,7)),"\n")
  a2text <- paste("Anderson-Darling statistic is ", as.character(round(a2,7)))
  a2text <- paste(a2text,"with P-value is ", as.character(round(a.pv,7)),"\n")
  u2text <- paste("Watson statistic is ", as.character(round(u2,7)))
  u2text <- paste(u2text,"with P-value is ", as.character(round(u.pv,7)),"\n")
  cat(w2text)
  cat(a2text)
  cat(u2text)
  invisible(list(Wsq=w2,Wsq.pvalue=w.pv,Asq=a2,Asq.pvalue=a.pv,Usq=u2,Usq.pvalue=u.pv))
}

#' @export
#' @rdname gof.bootstrap
gof.gamma.bootstrap<-function(x, M=10000){
  a2 <- AD.gamma(x)
  w2 <- CvM.gamma(x)
  u2 <- Watson.gamma(x)
  n <- length(x)
  pars <- estimate.gamma(x)
  alpha <- pars[1]
  beta <- pars[2]
  dat <- rgamma(n*M,shape=alpha,scale=beta)
  dat <- matrix(dat,nrow=M)
  a2vals <- apply(dat,1,AD.gamma)
  w2vals <- apply(dat,1,CvM.gamma)
  u2vals <- apply(dat,1,Watson.gamma)
  a.pv <- length(a2vals[a2vals>a2])/M
  w.pv <- length(w2vals[w2vals>w2])/M
  u.pv <- length(u2vals[w2vals>u2])/M
  w2text <- paste("Cramer-von Mises statistic is ", as.character(round(w2,7)))
  w2text <- paste(w2text,"with P-value is ", as.character(round(w.pv,7)),"\n")
  a2text <- paste("Anderson-Darling statistic is ", as.character(round(a2,7)))
  a2text <- paste(a2text,"with P-value is ", as.character(round(a.pv,7)),"\n")
  u2text <- paste("Watson statistic is ", as.character(round(u2,7)))
  u2text <- paste(u2text,"with P-value is ", as.character(round(u.pv,7)),"\n")
  cat(w2text)
  cat(a2text)
  cat(u2text)
  invisible(list(Wsq=w2,Wsq.pvalue=w.pv,Asq=a2,Asq.pvalue=a.pv,Usq=u2,Usq.pvalue=u.pv))
}

#' @export
#' @rdname gof.bootstrap
gof.logistic.bootstrap=function(x, M=10000){
  a2 <- AD.logistic(x)
  w2 <- CvM.logistic(x)
  u2 <- Watson.logistic(x)
  n <- length(x)
  pars <- estimate.logistic(x)
  alpha <- pars[1]
  beta <- pars[2]
  dat <- rlogis(n*M,location=alpha,scale=beta)
  dat <- matrix(dat,nrow=M)
  a2vals <- apply(dat,1,AD.logistic)
  w2vals <- apply(dat,1,CvM.logistic)
  u2vals <- apply(dat,1,Watson.logistic)
  a.pv <- length(a2vals[a2vals>a2])/M
  w.pv <- length(w2vals[w2vals>w2])/M
  u.pv <- length(u2vals[w2vals>u2])/M
  w2text <- paste("Cramer-von Mises statistic is ", as.character(round(w2,7)))
  w2text <- paste(w2text,"with P-value is ", as.character(round(w.pv,7)),"\n")
  a2text <- paste("Anderson-Darling statistic is ", as.character(round(a2,7)))
  a2text <- paste(a2text,"with P-value is ", as.character(round(a.pv,7)),"\n")
  u2text <- paste("Watson statistic is ", as.character(round(u2,7)))
  u2text <- paste(u2text,"with P-value is ", as.character(round(u.pv,7)),"\n")
  cat(w2text)
  cat(a2text)
  cat(u2text)
  invisible(list(Wsq=w2,Wsq.pvalue=w.pv,Asq=a2,Asq.pvalue=a.pv,Usq=u2,Usq.pvalue=u.pv))
}

#' @export
#' @rdname gof.bootstrap
gof.laplace.bootstrap<-function(x, M=10000){
  a2 <- AD.laplace(x)
  w2 <- CvM.laplace(x)
  u2 <- Watson.laplace(x)
  n <- length(x)
  pars <- estimate.laplace(x)
  med <- pars[1]
  MAD <- pars[2]
  dat <- rmutil::rlaplace(n*M,m=med,s=MAD)
  dat <- matrix(dat,nrow=M)
  a2vals <- apply(dat,1,AD.laplace)
  w2vals <- apply(dat,1,CvM.laplace)
  u2vals <- apply(dat,1,Watson.laplace)
  a.pv <- length(a2vals[a2vals>a2])/M
  w.pv <- length(w2vals[w2vals>w2])/M
  u.pv <- length(u2vals[w2vals>u2])/M
  w2text <- paste("Cramer-von Mises statistic is ", as.character(round(w2,7)))
  w2text <- paste(w2text,"with P-value is ", as.character(round(w.pv,7)),"\n")
  a2text <- paste("Anderson-Darling statistic is ", as.character(round(a2,7)))
  a2text <- paste(a2text,"with P-value is ", as.character(round(a.pv,7)),"\n")
  u2text <- paste("Watson statistic is ", as.character(round(u2,7)))
  u2text <- paste(u2text,"with P-value is ", as.character(round(u.pv,7)),"\n")
  cat(w2text)
  cat(a2text)
  cat(u2text)
  invisible(list(Wsq=w2,Wsq.pvalue=w.pv,Asq=a2,Asq.pvalue=a.pv,Usq=u2,Usq.pvalue=u.pv))
}

#' @export
#' @rdname gof.bootstrap
gof.weibull.bootstrap<-function(x, M=10000){
  a2 <- AD.weibull(x)
  w2 <- CvM.weibull(x)
  u2 <- Watson.weibull(x)
  n <- length(x)
  pars <- estimate.weibull(x)
  alpha <- pars[1]
  beta <- pars[2]
  dat <- rweibull(n*M,shape=alpha,scale=beta)
  dat <- matrix(dat,nrow=M)
  a2vals <- apply(dat,1,AD.weibull)
  w2vals <- apply(dat,1,CvM.weibull)
  u2vals <- apply(dat,1,Watson.weibull)
  a.pv <- length(a2vals[a2vals>a2])/M
  w.pv <- length(w2vals[w2vals>w2])/M
  u.pv <- length(u2vals[w2vals>u2])/M
  w2text <- paste("Cramer-von Mises statistic is ", as.character(round(w2,7)))
  w2text <- paste(w2text,"with P-value is ", as.character(round(w.pv,7)),"\n")
  a2text <- paste("Anderson-Darling statistic is ", as.character(round(a2,7)))
  a2text <- paste(a2text,"with P-value is ", as.character(round(a.pv,7)),"\n")
  u2text <- paste("Watson statistic is ", as.character(round(u2,7)))
  u2text <- paste(u2text,"with P-value is ", as.character(round(u.pv,7)),"\n")
  cat(w2text)
  cat(a2text)
  cat(u2text)
  invisible(list(Wsq=w2,Wsq.pvalue=w.pv,Asq=a2,Asq.pvalue=a.pv,Usq=u2,Usq.pvalue=u.pv))
}

#' @export
#' @rdname gof.bootstrap
gof.extremevalue.bootstrap<-function(x, M=10000){
  xx=(x-mean(x))/sd(x)
  ww=exp(xx)
  a2 <- AD.weibull(ww)
  w2 <- CvM.weibull(ww)
  u2 <- Watson.weibull(ww)
  n <- length(ww)
  pars <- estimate.weibull(ww)
  alpha <- pars[1]
  beta <- pars[2]
  dat <- rweibull(n*M,shape=alpha,scale=beta)
  dat <- matrix(dat,nrow=M)
  a2vals <- apply(dat,1,AD.weibull)
  w2vals <- apply(dat,1,CvM.weibull)
  u2vals <- apply(dat,1,Watson.weibull)
  a.pv <- length(a2vals[a2vals>a2])/M
  w.pv <- length(w2vals[w2vals>w2])/M
  u.pv <- length(u2vals[w2vals>u2])/M
  w2text <- paste("Cramer-von Mises statistic is ", as.character(round(w2,7)))
  w2text <- paste(w2text,"with P-value is ", as.character(round(w.pv,7)),"\n")
  a2text <- paste("Anderson-Darling statistic is ", as.character(round(a2,7)))
  a2text <- paste(a2text,"with P-value is ", as.character(round(a.pv,7)),"\n")
  u2text <- paste("Watson statistic is ", as.character(round(u2,7)))
  u2text <- paste(u2text,"with P-value is ", as.character(round(u.pv,7)),"\n")
  cat(w2text)
  cat(a2text)
  cat(u2text)
  invisible(list(Wsq=w2,Wsq.pvalue=w.pv,Asq=a2,Asq.pvalue=a.pv,Usq=u2,Usq.pvalue=u.pv))
}

#' @export
#' @rdname gof.bootstrap
gof.exp.bootstrap<-function(x, M=10000){
  a2 <- AD.exp(x)
  w2 <- CvM.exp(x)
  u2 <- Watson.exp(x)
  n <- length(x)
  pars <- estimate.exp(x)
  scale <- pars
  dat <- rexp(n*M,rate=1/scale)
  dat <- matrix(dat,nrow=M)
  a2vals <- apply(dat,1,AD.exp)
  w2vals <- apply(dat,1,CvM.exp)
  u2vals <- apply(dat,1,Watson.exp)
  a.pv <- length(a2vals[a2vals>a2])/M
  w.pv <- length(w2vals[w2vals>w2])/M
  u.pv <- length(u2vals[w2vals>u2])/M
  w2text <- paste("Cramer-von Mises statistic is ", as.character(round(w2,7)))
  w2text <- paste(w2text,"with P-value is ", as.character(round(w.pv,7)),"\n")
  a2text <- paste("Anderson-Darling statistic is ", as.character(round(a2,7)))
  a2text <- paste(a2text,"with P-value is ", as.character(round(a.pv,7)),"\n")
  u2text <- paste("Watson statistic is ", as.character(round(u2,7)))
  u2text <- paste(u2text,"with P-value is ", as.character(round(u.pv,7)),"\n")
  cat(w2text)
  cat(a2text)
  cat(u2text)
  invisible(list(Wsq=w2,Wsq.pvalue=w.pv,Asq=a2,Asq.pvalue=a.pv,Usq=u2,Usq.pvalue=u.pv))
}
