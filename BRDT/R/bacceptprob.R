#' @title Acceptance Probability for Binomial RDT
#'
#' @description Define the acceptance probability function which gets the probability of passing the test (for binomial RDT).
#'
#' @param n RDT sample size.
#' @param c Maximum allowable failures.
#' @param pi Failure probability.
#' @return Acceptance probability
#' @examples
#' pi <- pi_MCSim_beta(M = 5000, seed = 10, a = 1, b = 1)
#' bacceptprob(n = 10, c = 2, pi = pi);
#' @export
#' @importFrom stats pbinom


bacceptprob <- function(n, c, pi){
  return(mean(pbinom(c, n, pi)))
}
