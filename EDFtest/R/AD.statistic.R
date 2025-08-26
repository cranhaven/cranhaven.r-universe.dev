#' Anderson-Darling statistic
#'
#' @description
#' Compute the Anderson-Darling goodness-of-fit statistic \eqn{A^2} for an i.i.d sample,
#' x, to test for the given distribution with parameters unknown.Estimate parameters by ML
#' using \code{EDFtest} MLE function by default.
#'
#' @param x A random sample.
#' @param z A standard uniform random sample.
#' @param parameter Parameters of the given distribution, MLE by default.
#'
#' @return Anderson-Darling statistic of the given sample.
#'
#' @seealso
#' \code{\link{estimate}} for estimating distribution parameters by ML;
#' \code{\link{CvM}} for calculating Cramér-von Mises statistic;
#' \code{\link{Watson}} for calculating Watson statistic;
#' \code{\link{AD.pvalue}} for calculating P-value of Anderson-Darling statistic.
#'
#' @name AD
#' @examples
#' x0=runif(n=100,min=-1,max=1)
#' AD.uniform(x0)
#'
#' x1=rnorm(n=100,mean=0,sd=1)
#' AD.normal(x1)
#'
#' x2=rgamma(n=100,shape=1,scale=1)
#' AD.gamma(x2)
#'
#' x3=rlogis(n=100,location=0,scale=1)
#' AD.logistic(x3)
#'
#' x4= rmutil::rlaplace(n=100,m=0,s=1)
#' AD.laplace(x4)
#'
#' x5=rweibull(n=100,shape=1,scale=1)
#' AD.weibull(x5)
#' x5_log=log(x5)
#' AD.extremevalue(x5_log)
#'
#' x6=rexp(n=100,rate=1/2)
#' AD.exp(x6)
NULL

#' @export
#' @rdname AD
AD.uniform = function(x,parameter=estimate.uniform(x)){
  s <- sort(x)[-c(1,length(x))]
  z <- cdf.uniform(s,parameter)
  return(AD(z))
}

#' @export
#' @rdname AD
AD.normal = function(x,parameter=estimate.normal(x)){
  z <- cdf.normal(x,parameter)
  AD(z)
}

#' @export
#' @rdname AD
AD.gamma <- function(x,parameter=estimate.gamma(x)){
  z <- cdf.gamma(x,parameter)
  AD(z)
}

#' @export
#' @rdname AD
AD.logistic <- function(x,parameter=estimate.logistic(x)){
  z <- cdf.logistic(x,parameter)
  AD(z)
}

#' @export
#' @rdname AD
AD.laplace = function(x,parameter=estimate.laplace(x)){
  z = cdf.laplace(x,parameter)
  AD(z)
}

#' @export
#' @rdname AD
AD.weibull <- function(x,parameter=estimate.weibull(x)){
  z <- cdf.weibull(x,parameter)
  AD(z)
}

#' @export
#' @rdname AD
AD.extremevalue <- function(x){
  xx=(x-mean(x))/sd(x)
  ww=exp(xx)
  parameter=estimate.weibull(ww)
  z <- cdf.weibull(ww,parameter)
  AD(z)
}

#' @export
#' @rdname AD
AD.exp = function(x,parameter=estimate.exp(x)){
  z = cdf.exp(x,parameter)
  AD(z)
}

#' @export
#' @rdname AD
AD <- function(z){
  n <- length(z)
  u <- sort(z)
  i <- 2*(1:n)-1
  -n -sum(i*(log(u)+log(1-rev(u))))/n
}
