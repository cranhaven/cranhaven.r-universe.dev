#' @name sn.params
#' @title Skew-Normal parameter computation
#' @description Compute parameters and moments of skew normal distribution.
#' 
#' @param shape skew normal shape parameter.
#' @param scale skew normal scale parameter.
#' @param dp 3 element vector of (in order) location, scale, and shape parameters.
#' @param mode mode of skew normal distribution.
#' 
#' @return \tabular{ll}{
#'   \code{sn.location} \tab location parameter computed from mode, scale, and shape.\cr
#'   \code{sn.mean} \tab mean of the skew normal distribution.\cr
#'   \code{sn.mode} \tab mode of the skew normal distribution.\cr
#'   \code{sn.variance} \tab variance of the skew normal distribution.\cr
#'   \code{sn.skewness} \tab skewness of the skew normal distribution.\cr
#'   \code{sn.delta} \tab value used in other moment computations.\cr
#'   \code{sn.m0} \tab value used in mode computation.
#' }
#' 
#' @references \url{https://en.wikipedia.org/wiki/Skew_normal_distribution}
#' 
#' @seealso \code{sn} package by Adelchi Azzalini for skew normal 
#' PDF and CDF functions.\cr
#' Azzalini, A. with the collaboration of Capitanio, A. (2014). 
#' The Skew-Normal and Related Families. Cambridge University Press, 
#' IMS Monographs series.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}


#' @rdname sn.params
#' @export
#' 
sn.location <- function(mode, scale, shape) {
  mode - (scale * sn.m0(shape))
}

#' @rdname sn.params
#' @export
#' 
sn.mean <- function(dp) dp[1] + (dp[2] * sn.delta(dp[3]) * sqrt(2 / pi))

#' @rdname sn.params
#' @export
#' 
sn.mode <- function(dp) dp[1] + (dp[2] * sn.m0(dp[3]))

#' @rdname sn.params
#' @export
#' 
sn.variance <- function(scale, shape) {
  (scale ^ 2) * (1 - ((2 * sn.delta(shape)) / pi))
}

#' @rdname sn.params
#' @export
#' 
sn.skewness <- function(shape) {
  delta <- sn.delta(shape)
  term.1 <- (4 - pi) / 2
  term.2 <- (delta * sqrt(2 / pi)) ^ 3
  term.3 <-  (1 - ((2 * delta ^ 2) / pi)) ^ (3 / 2)
  term.1 * term.2 / term.3
}

#' @rdname sn.params
#' @export
#' 
sn.delta <- function(shape) shape / sqrt(1 + shape ^ 2)

#' @rdname sn.params
#' @export
#' 
sn.m0 <- function(shape) {
  # mu.z <- sqrt(2 / pi) * sn.delta(shape)
  # sigma.z <- sqrt(1 - mu.z ^ 2)
  # term.1 <- ((sn.skewness(shape) * sigma.z) / 2)
  # term.2 <-  (sign(shape) / 2) * exp((-2 * pi) / abs(shape))
  # mu.z - term.1 - term.2
  
  delta <- sn.delta(shape)
  term.1 <- sqrt(2 / pi) * delta
  term.2.1 <- 1 - (pi / 4)
  term.2.2 <- 1 - ((2 * delta ^ 2) / pi)
  term.2 <- term.2.1 * (term.1 ^ 3) / term.2.2
  term.3.1 <- (-2 * pi) / abs(shape)
  term.3 <- sign(shape) * exp(term.3.1) / 2
  term.1 - term.2 - term.3
}
