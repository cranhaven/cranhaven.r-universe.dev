#' @title Expected Overall Costs in Binomial RDT Design
#'
#' @description Define the cost function of expected overall cost including the RDT cost, expected reliabiltiy growth (RG) cost and expected warranty services (WS) cost (for binomial RDT design).
#'
#' @param Cf Fixed costs of RDT
#' @param Cv Variable unit costs of RDT
#' @param n RDT sample size
#' @param G Reliabilty growth cost
#' @param Cw Average cost per warranty claim
#' @param N Sales volume
#' @param c Maximum allowable failures
#' @param pi Failure probability
#' @return Overall expected cost
#' @examples
#' pi <- pi_MCSim_beta(M = 1000, seed = 10, a = 1, b = 1)
#' bcost_expected(Cf = 10, Cv = 10, n = 10, G = 100000, Cw = 10, N = 1, c = 1, pi = pi)
#' @seealso \code{\link{bcost_RDT}}, \code{\link{bcost_RG}}, \code{\link{bcost_WS}}
#' @export


bcost_expected <- function(Cf, Cv, n, G, Cw, N, c, pi){
  return(bcost_RDT(Cf, Cv, n) +
         bcost_RG(G) * (1 - bacceptprob(n, c, pi)) +
         bcost_WS(Cw, N, n, c, pi)[2] * bacceptprob(n, c, pi))
}
