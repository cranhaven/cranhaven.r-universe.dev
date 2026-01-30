#########1#########2#########3#########4#########5#########6#########7#########8
#' Cohen's d
#'
#' Calculate Cohen's d for one-sample t tests or two-sample independent tests or
#' two-sample paired t-tests
#'
#' @param x vector with (numeric) data
#' @param y for two-sample tests, a vector with (numeric) data for group 2
#' @param mu0 for one-sample tests, the number to test against
#' @param paired TRUE for a paired two-sample t-test, FALSE for an independent
#' sample t-test
#' @return value of Cohen's d
#' @examples
#' #one-sample
#' x=c(1:10,5,6,3:8)
#' dCohen(x,mu0=7)
#'
#' #two-sample independent
#' y=1:15
#' dCohen(x,y)
#'
#' #two-sample paired
#' dCohen(x,1:18,paired=TRUE)
#' @import stats
#' @export
################################################################################
dCohen<-function(x,y=NULL,mu0=0,paired=FALSE) {
  if (!inherits(x,c("numeric","integer"))) stop("x must be numeric")
  if (!inherits(mu0,c("numeric","integer"))) stop("mu0 must be numeric")
  if (length(mu0)>1) mu0=mu0[1]
  if (is.null(y)) return((mean(na.omit(x))-mu0)/(sd(na.omit(x))))
  if (!inherits(y,c("numeric","integer"))) stop("y must be numeric")
  paired=paired[1]
  if (!inherits(paired,"logical")) stop("paired must be TRUE or FALSE")
  if (paired) {
    if (length(x)!=length(y)) stop("x and y must have the same length")
    x2=x[complete.cases(x,y)];y2=y[complete.cases(x,y)]
    return(2*(mean(x2)-mean(y2))/(sd(x2)+sd(y2)))
  }
  x=na.omit(x);y=na.omit(y)
  df1=length(x)-1
  df2=length(y)-1
  (mean(x)-mean(y))/sqrt((df1*var(x)+df2*var(y))/(df1+df2))
}
