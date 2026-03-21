#' @title Warranty Services Cost
#'
#' @description Define the cost function of warranty services (WS) after the decision of the test (for binomial RDT)
#'
#' @param Cw Average cost per warranty claim
#' @param N Sales volume
#' @param n RDT sample size
#' @param c Maximum allowable failures
#' @param pi Failure probability
#' @return The result is a vector with two values.
#' The first value is the expected failure probability in warranty period.
#' The second value is the expected warranty services cost.
#' @examples
#' #the n value can be the minimum test sample size obtained from \code{\link{boptimal_n}}.
#' n_optimal <- 20
#' pi <- pi_MCSim_beta(M = 1000, seed = 10, a = 1, b = 1)
#' WScost <- bcost_WS(Cw = 10, N = 1, n = n_optimal, c = 1, pi = pi);
#' print(WScost[1]) #expected failure probability
#' print(WScost[2]) #expected warranty services cost
#' @seealso \code{\link{bcost_RDT}}, \code{\link{bcost_RG}}, \code{\link{bcost_expected}}
#' @export
#' @importFrom stats pbinom

bcost_WS <- function(Cw, N, n, c, pi){
  failureprob <- sum(pbinom(c, n, pi) * pi) / sum(pbinom(c, n, pi))
  return(c(failureprob, Cw * N * failureprob))
}
