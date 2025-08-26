#' Watson statistic
#'
#' @description
#' Compute the Watson goodness-of-fit statistic \eqn{U^2} for an i.i.d. sample,
#' x, to test for the given distribution with parameters unknown. Estimate parameters by ML
#' using \code{EDFtest} MLE function by default.
#'
#' @param x A random sample.
#' @param z A standard uniform random sample.
#' @param parameter Parameters of the given distribution, MLE by default.
#'
#' @return Watson statistic of the given sample.
#'
#' @seealso
#' \code{\link{estimate}} for estimating distribution parameters by ML;
#' \code{\link{CvM}} for calculating Cramér-von Mises statistic;
#' \code{\link{AD}} for calculating Anderson-Darling statistic;
#' \code{\link{Watson.pvalue}} for calculating P-value of Watson statistic.
#'
#' @name Watson
#' @examples
#' x0=runif(n=100,min=-1,max=1)
#' Watson.uniform(x0)
#'
#' x1=rnorm(n=100,mean=0,sd=1)
#' Watson.normal(x1)
#'
#' x2=rgamma(n=100,shape=1,scale=1)
#' Watson.gamma(x2)
#'
#' x3=rlogis(n=100,location=0,scale=1)
#' Watson.logistic(x3)
#'
#' x4= rmutil::rlaplace(n=100,m=0,s=1)
#' Watson.laplace(x4)
#'
#' x5=rweibull(n=100,shape=1,scale=1)
#' Watson.weibull(x5)
#' x5_log=log(x5)
#' Watson.extremevalue(x5_log)
#'
#' x6=rexp(n=100,rate=1/2)
#' Watson.exp(x6)
NULL

#' @export
#' @rdname Watson
Watson.uniform <- function(x,parameter=estimate.uniform(x)){
    z <- cdf.uniform(x,parameter)
    Watson(z)
}

#' @export
#' @rdname Watson
Watson.normal = function(x,parameter=estimate.normal(x)){
    z <- cdf.normal(x, theta=parameter)
    Watson(z)
}

#' @export
#' @rdname Watson
Watson.gamma = function(x,parameter=estimate.gamma(x)){
    z <- cdf.gamma(x,parameter)
    Watson(z)
}

#' @export
#' @rdname Watson
Watson.logistic = function(x,parameter=estimate.logistic(x)){
    z <- cdf.logistic(x,parameter)
    Watson(z)
}

#' @export
#' @rdname Watson
Watson.laplace = function(x,parameter=estimate.laplace(x)){
    z <- cdf.laplace(x,parameter)
    Watson(z)
}

#' @export
#' @rdname Watson
Watson.weibull = function(x,parameter=estimate.weibull(x)){
    z <- cdf.weibull(x,parameter)
    Watson(z)
}

#' @export
#' @rdname Watson
Watson.extremevalue <- function(x){
    xx=(x-mean(x))/sd(x)
    ww=exp(xx)
    parameter=estimate.weibull(ww)
    z <- cdf.weibull(ww,parameter)
    Watson(z)
}

#' @export
#' @rdname Watson
Watson.exp = function(x,parameter=estimate.exp(x)){
    z <- cdf.exp(x,parameter)
    Watson(z)
}

#' @export
#' @rdname Watson
Watson <- function(z){
    # Given probability integral transforms, compute the Watson goodness-of-fit statistic
    # for testing uniformity on the unit interval.
    n <- length(z)
    u <- sort(z)
    i <- (2*(1:n)-1)/(2*n)
    w=sum((u-i)^2)+1/(12*n)
    corr=n*(mean(u)-0.5)^2 #correction
    w-corr
}
