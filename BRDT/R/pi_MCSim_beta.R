#' @title Beta Prior Simulation for Binomial RDT
#'
#' @description Define the simulation function to generate failure probability with Beta prior distributions as conjugate prior to binomial distributions (for binomial RDT).
#'
#' @param M Simulation sample size
#' @param seed Random seed for random sample
#' @param a Shape parameter 1 for beta distribution
#' @param b Shape parameter 2 for beta distribution
#' @return Vector of failure probability sample values
#' @examples
#' pi <- pi_MCSim_beta(M = 1000, seed = 10, a = 1, b = 1)
#' @export
#' @importFrom stats rbeta


pi_MCSim_beta <- function(M, seed, a, b){
  #requireNamespace("stats")
  set.seed(seed)
  return(rbeta(M, a, b))
}
