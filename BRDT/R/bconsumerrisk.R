#' @title Consumer's Risk for Binomial RDT
#'
#' @description Define the consumer's risk function which gets the probability of passing the test when the lower level reliability requirement is not satisfied (for binomial RDT).
#'
#' @param n RDT sample size.
#' @param c Maximum allowable failures.
#' @param pi Failure probability.
#' @param R Lower level reliability requirement.
#' @return Probability of consumer's risk
#' @examples
#' pi <- pi_MCSim_beta(M = 1000, seed = 10, a = 1, b = 1)
#' bconsumerrisk(n = 10, c = 2, pi = pi, R = 0.8);
#' @export
#' @importFrom stats pbinom
#' @seealso \code{\link{bcore}} for getting the core probability of passting the test;
#' \code{\link{boptimal_n}} for getting the optimal test sample size;
#' \code{\link{bIndicator}} for getting the binary indicator;

bconsumerrisk <- function(n, c, pi, R){
  tmp <- pbinom(c, n, pi)
  sum1 <- t(matrix(tmp)) %*% matrix(sapply(pi, bIndicator, R))
  sum2 <- sum(tmp)
  return(1 - sum1/sum2)
}
