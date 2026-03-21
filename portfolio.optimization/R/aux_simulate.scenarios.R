#' @title Simulate a multivariate-normal scenario.set 
#'
#' @description
#' \code{aux_simulate.scenarios} simulates a scenario.set given a mean vector and
#' a covariance matrix using mvrnorm of the MASS package
#'
#' @param mu mean vector of asset returns 
#' @param Sigma covariance matrix of asset returns
#' @param n number of scenarios to simulate (default 1000)
#' @param seed random number seed (default 280277)
#' 
#' @return A scenario set `simulation` with mean `mu` and covariance `Sigma`
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
aux_simulate.scenarios <- function(mu, Sigma, n=1000, seed=280277) {
  set.seed(seed)
  simulation <- MASS::mvrnorm(n, mu, Sigma)
  return(simulation)
}
