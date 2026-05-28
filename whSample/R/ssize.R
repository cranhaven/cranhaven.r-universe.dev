#' Determine minimum sample size
#'
#' \code{ssize} takes a population size and returns a sample size
#'
#'@param N The population size
#'@param ci The desired confidence interval (default is 0.95)
#'@param me The margin of error (default: +/- 0.07)
#'@param p The expected rate of occurrence (default: 0.50)
#'@return Returns the estimated minimum sample size, rounded up to the nearest integer.
#'@examples
#'ssize(1000)
#'ssize(1000, ci=0.90, p=0.60)
#'@export
#'@section Details:
#'\code{ssize} uses a normal approximation of the hypergeomtric distribution approach.

ssize <- function(N, ci=0.95, me=0.07, p=0.50) {
  z <- qnorm((1-ci)/2,lower.tail=F)
  q <- 1-p
  n <- (N*z^2*p*q)/(me^2*(N-1)+z^2*p*q)
  return(ceiling(n))
}

