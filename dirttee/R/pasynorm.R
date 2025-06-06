#' The asymmetric normal distribution.
#' 
#' Density, distribution function, quantile function and random generation for the asymmetric normal distribution with the parameters \code{mu}, \code{sigma} and \code{tau}.
#' 
#' 
#' @export
#' @family asynorm
#' @name asynorm
#' @aliases rasynorm dasynorm qasynorm
#' 
#' @usage 
#' dasynorm(x, mu = 0, sigma = 1, tau = 0.5)
#' pasynorm(q, mu = 0, sigma = 1, tau = 0.5)
#' qasynorm(p, mu = 0, sigma = 1, tau = 0.5)
#' rasynorm(n, mu = 0, sigma = 1, tau = 0.5)
#' 
#' @param x vector of locations.
#' @param q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If \eqn{length(n) > 1}, the length is taken to be the number required.
#' @param mu location parameter and mode of the distribution.
#' @param sigma comparable to the standard deviation. Must be positive.
#' @param tau asymmetry parameter.
#' 
#' @details 
#' The asymmetric normal distribution has the following density\cr
#' \eqn{f(x) =   (2\sqrt{\tau(1-\tau)/\pi}/\sigma)/(\sqrt{1-\tau} + \sqrt{\tau)}\exp(-|(\tau - (x <= \mu))|*(x - \mu)^2/\sigma^2)}
#' The cdf is derived by integration of the distribution function by using the \code{\link[stats]{integrate}} function.
#'  
#' @returns \code{dasynorm} gives the density, \code{pasynorm} gives the distribution function, \code{qasynorm} gives the quantile function, and \code{rasynorm} generates random deviates.
#' 
#' Corresponds to the normal distribution for \eqn{\tau = 0.5}.
#' 
#' The length of the result is determined by \code{n} for \code{rasynorm}, and is the maximum of the lengths of the numerical arguments for the other functions.
#' 
#' The numerical arguments other than \code{n} are recycled to the length of the result.
#'  
#' @examples
#'
#'hist(rasynorm(1000))
#'
#'qg <- qasynorm(0.1, 1, 2, 0.5)
#'
#'pasynorm(qg, 1, 2, 0.5)
#'
#'ax <- c(1:1000)/100-5
#'plot(ax,dasynorm(ax), type = 'l')


pasynorm <- function(q,mu=0,sigma=1,tau=0.5)
{
  if(any(sigma <= 0)) stop("standard deviation must be strictly > 0.")
  if(any(tau <= 0 | tau >= 1)) stop("tau must be between 0 and 1.")
  mapply(function(z,mu)integrate(dasynorm,-Inf,z,mu=mu,sigma=sigma,tau=tau)$value,q,mu)
}

