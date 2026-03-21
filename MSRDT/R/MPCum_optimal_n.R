#' @title Optimal Test Sample Size for Multi-state RDT with Multiple Periods and Criteria for Cumulative Periods
#'
#' @description Define the optimal function to find the optimal test plan with minimum test sample size given an acceptable level of consumer's risk.
#' The maximum allowable failures for each cumulative period need to be satisfied to pass the test (for Multi-state RDT, Multiple Periods, Scenario I)
#'
#' @param cvec Maximum allowable failures for each separate period
#' @param pivec Failure probability for each seperate period
#' @param Rvec Lower level reliability requirements for each cumulative period from the begining of the test.
#' @param thres_CR Threshold (acceptable level) of consumer's risk
#' @return Minimum test sample size
#' @examples
#' \donttest{
#' pi <- pi_MCSim_dirichlet(M = 5000, seed = 10, par = c(1, 1, 1))
#' MPCum_optimal_n(cvec = c(1,1), pivec = pi, Rvec = c(0.8, 0.7), thres_CR = 0.05)
#' }
#' @export


MPCum_optimal_n <- function(cvec, pivec, Rvec, thres_CR){
  n <- sum(cvec) + 1
  CR <- MPCum_consumerrisk(n, cvec, pivec, Rvec)
  while (CR > thres_CR){
    n <- n + 1
    CR <- MPCum_consumerrisk(n, cvec, pivec, Rvec)
  }
  return(n)
}
