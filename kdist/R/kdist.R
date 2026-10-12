#' The K-distribution.
#'
#' Density, distribution function, quantile function and random generation for
#' the K-distribution with parameters \code{shape} and \code{scale}.
#'
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param shape,scale shape and scale parameters both defaulting to 1.
#' @param intensity logical; if TRUE, quantiles are intensities not amplitudes.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are P[X = x],
#'                     otherwise, P[X > x].
#' @details The K-distribution with \code{shape} parameter \eqn{\nu} and 
#'          \code{scale} parameter \eqn{b} has amplitude density given by
#'          \eqn{f(x) = [4 x^\nu / \Gamma(\nu)]
#'                      [(\nu / b)^(1+\nu/2)]
#'                      K(2 x \sqrt(\nu/b),\nu-1)}.
#'          Where \eqn{K} is a modified Bessel function of the second kind.
#'          For \eqn{\nu -> Inf}, the K-distrubution tends to a Rayleigh
#'          distribution, and for \eqn{\nu = 1} it is the Exponential
#'          distribution.
#'          The function \code{base::besselK} is used in the calculation, and
#'          care should be taken with large input arguements to this function,
#'          e.g. \eqn{b} very small or \eqn{x, \nu} very large.
#'          The cumulative distribution function for
#'          the amplitude, \eqn{x} is given by
#'          \eqn{F(x) = 1 - 2 x^\nu (\nu/b)^(\nu/2) K(2 x \sqrt(\nu/b), \nu)}.
#'          The K-Distribution is a compound distribution, with Rayleigh
#'          distributed amplitudes (exponential intensities) modulated by another
#'          underlying process whose amplitude is chi-distributed and whose
#'          intensity is Gamma distributed. An Exponential distributed number
#'          multiplied by a Gamma distributed random number is used to
#'          generate the random variates.
#'          The \eqn{m}th moments are given by \eqn{\mu_m = (b/\nu)^(m/2) \Gamma(0.5m + 1)
#'          \Gamma(0.5m + \nu) / \Gamma(\nu)}, so that the root mean square
#'          value of x is the \code{scale} factor, \eqn{<x^2> = b}. 
#' @return The function \code{dk} gives the density, \code{pk} gives the distribution
#'         function, \code{qk} gives the quantile function, and \code{rk}
#'         generates random variates.
#' @seealso \href{https://CRAN.R-project.org/view=Distributions}{Distributions}
#'          for other standard distributions, including \code{dweibull} for the Weibull
#'          distribution and \code{dexp} for the exponential distribution.
#' @references E Jakeman and R J A Tough, "Non-Gaussian models for the
#'             statistics of scattered waves", Adv. Phys., 1988, vol. 37, No. 5,
#'             pp471-529
#' @examples
#' #=====
#' r <- rk(10000, shape = 3, scale = 5, intensity = FALSE)
#' fn <- stats::ecdf(r)
#' x <- seq(0, 10, length = 100)
#' plot(x, fn(x))
#' lines(x, pk(x, shape = 3, scale = 5, intensity = FALSE))
#' #======
#' r <- rk(10000, shape = 3, scale = 5, intensity = FALSE)
#' d <- density(r)
#' x <- seq(0, 10, length = 100)
#' plot(d, xlim=c(0,10))
#' lines(x, dk(x, shape = 3, scale = 5, intensity = FALSE))
#' @name k
NULL

#' @rdname k
#' @export
# density function for K-distribution
dk <- function(x, shape = 1, scale = 1, intensity = FALSE,
               log = FALSE){

  # if K-distribution of intensity
  if (isTRUE(intensity)){
     # amplitude is sqrt of intensity value
     x <- sqrt(x)
  }

  # pdf of amplitude
  pdf <- ( (4 * x ^ shape) / gamma(shape) ) *
           ( (shape / scale) ^ ( (1 + shape) / 2) ) *
           (besselK(2 * x * sqrt(shape / scale), shape - 1) )

  # pdf = 0 for x < 0
  if (any(x <= 0)){
     pdf[which(x <= 0)] <- 0
  }

  if (isTRUE(log)){
     pdf = log(pdf)
  }

  return(pdf)
}

#' @rdname k
#' @export
# distribution function for K-distribution
pk <- function(q, shape = 1, scale = 1, intensity = FALSE,
               log.p = FALSE, lower.tail = TRUE){

  # if K-distribution of intensity
  if (isTRUE(intensity)){
     # amplitude is sqrt of intensity value
     q <- sqrt(q)
  }

  #if (2 * q * sqrt(shape / scale) > 1500) "besselK may have underflow problems"

  # cdf of amplitude
  cdf <- 1 - 2 * q ^ shape * (shape / scale) ^ (shape / 2) *
             besselK(2 * q * sqrt(shape / scale), shape) /
             gamma(shape)

  # cdf = 0 for q < 0
  if (any(q <= 0)){
     cdf[which(q <= 0)] <- 0
  }

  if (identical(lower.tail, FALSE)){
     cdf = 1 - cdf
  }

  if (log.p){
     cdf = log(cdf)
  }

  return(cdf)
}

#' @rdname k
#' @export
# quantile function for K-distribution
qk <- function(p, shape = 1, scale = 1, intensity = FALSE,
               log.p = FALSE){

  len <- length(p)
  q <- numeric(len)
  interval <- c(0, 10)

  f <- function(x, a){
     (pk(x, shape = shape, scale = scale, intensity = intensity) - a) ^ 2
  }

  if (isTRUE(log.p)){
     p = exp(p)
  }

  for (i in 1:len){
    q[i] <- stats::optimize(f, interval, tol = 0.0001, a = p[i])$minimum
  }

  if (isTRUE(intensity)){
     q = q^2
  }

  return(q)
}

#' @rdname k
#' @export
# random number generator for K-distribution
rk <- function(n, shape = 1, scale = 1, intensity = FALSE){

  # random variates from K-distribution of intensity
  rv <- scale * stats::rexp(n) * stats::rgamma(n, shape) / shape

  # if K-distribution of amplitude take the squareroot
  if (identical(intensity, FALSE)){
     rv <- sqrt(rv)
  }

  return(rv)
}
