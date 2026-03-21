#' @title Probability Core for Binomial RDT
#'
#' @description  Define the summed core function inside of the integration which gets the probability of passing the test given specific failure probabilities (for binomial RDT).
#'
#' @param n RDT sample size.
#' @param c Maximum allowable failures.
#' @param pi Failure probability.
#' @return Core probability of passing the test given specific failure probabilities.
#' @examples
#' bcore(n = 10, c = 2, pi = 0.2)
#' @export
#' @importFrom stats dbinom
#' @seealso \code{\link{boptimal_n}} for getting the optimal test sample size;
#' \code{\link{bconsumerrisk}} for getting the consumer's risk;
#' \code{\link{bIndicator}} for getting the binary indicator;


bcore <- function(n, c, pi){
  return(sum(dbinom(c(0:c), n, pi)))
}
