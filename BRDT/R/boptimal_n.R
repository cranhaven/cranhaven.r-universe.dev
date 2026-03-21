#' @title Optimal Test Sample Size for Binomial RDT
#'
#' @description Define the optimal function to find the optimal test plan with minimum test sample size given an acceptable
#' level of consumer's risk (for binomial RDT).
#'
#' @param c Maximum allowable failures
#' @param pi Failure probability
#' @param R Lower level reliability requirement
#' @param thres_CR Threshold (acceptable level) of consumer's risk
#' @return Minimum test sample size
#' @examples
#' \donttest{
#' pi <- pi_MCSim_beta(M = 5000, seed = 10, a = 1, b = 1)
#' boptimal_n(c = 2, pi = pi, R = 0.8, thres_CR = 0.05)
#' }
#' @export
#' @seealso
#' \code{\link{boptimal_cost}} for getting the optial test plan with minimum overall cost;
#' \code{\link{bdata_generator}} for generating optimal test plans dataset;

boptimal_n <- function(c, pi, R, thres_CR){
  n <- c + 1
  CR <- bconsumerrisk(n, c, pi, R)
  while (CR > thres_CR){
    n <- n + 1
    CR <- bconsumerrisk(n, c, pi, R)
  }
  return(n)
}
