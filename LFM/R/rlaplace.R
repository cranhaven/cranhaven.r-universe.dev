#' @name rlaplace
#' @title Generate Random Variables from the Laplace Distribution
#'
#' @description
#' Generates random samples from the Laplace (double exponential)
#' distribution with location parameter 0 and scale parameter \code{b}.
#'
#' The probability density function is
#'
#' \deqn{
#' f(x)=\frac{1}{2b}\exp\left(-\frac{|x|}{b}\right),
#' }
#'
#' where \eqn{b>0} is the scale parameter.
#'
#' Random numbers are generated using the inverse transform method.
#'
#' @usage
#' rlaplace(n, b = 1)
#'
#' @param n An integer specifying the number of observations to generate.
#' @param b A positive numeric value specifying the scale parameter of the
#' Laplace distribution. The default value is \code{1}.
#'
#' @return
#' A numeric vector of length \code{n} containing random observations from
#' the Laplace distribution.
#'
#' @details
#' If \eqn{X \sim Laplace(0,b)}, then
#'
#' \deqn{
#' E(X)=0,
#' }
#'
#' and
#'
#' \deqn{
#' Var(X)=2b^2.
#' }
#'
#' This function is used internally by the Laplace Factor Model (LFM)
#' package for generating Laplace-distributed error terms.
#'
#' @examples
#' set.seed(123)
#'
#' x <- rlaplace(1000)
#'
#' mean(x)
#' var(x)
#'
#' hist(x,
#'      breaks = 30,
#'      main = "Laplace Random Sample",
#'      xlab = "x")
#'
#' @export
#' @importFrom stats runif
#'
rlaplace <- function(n, b = 1) {

  if (n <= 0) {
    stop("n must be positive")
  }

  if (b <= 0) {
    stop("b must be positive")
  }

  u <- runif(n, -0.5, 0.5)

  x <- -b * sign(u) * log(1 - 2 * abs(u))

  return(x)
}
