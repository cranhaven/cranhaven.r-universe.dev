#########1#########2#########3#########4#########5#########6#########7#########8
#' Normal Confidence Interval
#'
#' Confidence interval for a normally distributed sample mean
#'
#' @param x sample mean
#' @param s standard deviation
#' @param n sample size
#' @param level confidence level
#' @return vector with two values containing the confidence interval for the
#' sample mean
#' @examples
#' CI()
#' CI(150,5,30,.9)
#' @export
################################################################################
CI<-function(x=0,s=1,n=1,level=.95) {
  if (!inherits(x,c("integer","numeric"))) stop("x must be numeric")
  if (!inherits(s,c("integer","numeric"))) stop("s must be numeric")
  if (!isInt(n,FALSE)|n<1) stop("n must be a finite integer greater than 0")
  if (!inherits(level,c("integer","numeric"))) stop("level must be numeric")
  if (s<0) stop("s must be greater than or equal to zero")
  if (level<=0) stop("level must be greater than 0")
  if (level>1) stop("level must be less than or equal to 1")
  a=(1-level)/2
  return(c(stats::qnorm(a,x,s/sqrt(n)),stats::qnorm(1-a,x,s/sqrt(n))))
}
