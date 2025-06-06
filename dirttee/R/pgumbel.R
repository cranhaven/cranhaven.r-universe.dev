#' The Gumbel Distribution.
#' 
#' Density, distribution function, quantile function and random generation for the gumbel distribution with the two parameters \code{location} and \code{scale}.
#' 
#' 
#' @export
#' @importFrom stats pnorm
#' @family gumbel
#' @name gumbel
#' @aliases rgumbel dgumbel qgumbel
#' 
#' @usage 
#' dgumbel(x, location = 0, scale = 1)
#' pgumbel(q, location = 0, scale = 1)
#' qgumbel(p, location = 0, scale = 1)
#' rgumbel(n, location = 0, scale = 1)
#' 
#' @param x vector of locations.
#' @param q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If \eqn{length(n) > 1}, the length is taken to be the number required.
#' @param location location parameter and mode of the distribution.
#' @param scale scaling parameter, has to be positive.
#' 
#' @details 
#' The gumbel distribution has the following density and cdf \cr
#' \eqn{f(x) = (1/scale)*exp((x - location)/scale - exp((x - location)/scale))},
#' \eqn{F(x) = 1 - exp(- exp((x - location)/scale))}.
#' The mode of the distribution is \code{location}, the variance is \eqn{\pi^{2/6} * \code{scale}}.
#'
#'
#'  
#' @returns \code{dgumbel} gives the density, \code{pgumbel} gives the distribution function, \code{qgumbel} gives the quantile function, and \code{rgumbel} generates random deviates.
#' 
#' The length of the result is determined by \code{n} for \code{rgumbel}, and is the maximum of the lengths of the numerical arguments for the other functions.
#' 
#' The numerical arguments other than \code{n} are recycled to the length of the result.
#' 
#' @references Collett, D. (2015). Modelling survival data in medical research, chapter 6. CRC press.
#' 
#' @examples 
#'
#'hist(rgumbel(1000))
#'
#'qg <- qgumbel(0.1, 1, 2)
#'
#'pgumbel(qg, 1, 2)
#'
#'ax <- c(1:1000)/100-5
#'plot(ax,dgumbel(ax), type = 'l')
#'

pgumbel <-
function(q, location = 0, scale = 1){
  if(any(scale <= 0)) stop("standard deviation must be strictly > 0.")
  1-exp(-exp((q - location)/scale))
}
