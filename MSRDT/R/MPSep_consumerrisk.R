#' @title Consumer's Risk for Multi-state RDT with Multiple Periods and Criteria for Separate Periods
#'
#' @description Define the consumer risk function hich gets the probability of passing the test when the lower level reliability requirements are not satisfied for any cumulative periods.
#' The maximum allowable failures for each separate period need to be satisfied to pass the test (for Multi-state RDT, Multiple Periods, Scenario I)
#'
#' @param n RDT sample size
#' @param cvec Maximum allowable failures for each separate period
#' @param pivec Failure probability for each seperate period
#' @param Rvec Lower level reliability requirements for each cumulative period from the begining of the test.
#' @return Probability for consumer's risk
#' @examples
#' pi <- pi_MCSim_dirichlet(M = 1000, seed = 10, par = c(1, 1, 1))
#' MPSep_consumerrisk(n = 10, cvec = c(1, 1), pi = pi, Rvec = c(0.8, 0.7))
#' @export




#
MPSep_consumerrisk <- function(n, cvec, pivec, Rvec){

  sum1 <- rep(NA, length(1:dim(pivec)[1]))
  sum2 <- rep(NA, length(1:dim(pivec)[1]))

  for(i in 1:dim(pivec)[1]){
    sum2[i] <- MPSep_core(n, cvec, pivec[i,])
    sum1[i] <- sum2[i] * MP_Indicator(pivec[i,], Rvec)
  }
  return(1 - (sum(sum1) / sum(sum2)))
}
