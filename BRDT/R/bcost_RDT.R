#' @title Binomial RDT Cost
#'
#' @description Define the cost function of RDT, mainly determined by the test sample size (for binomial RDT)
#'
#' @param Cf Fixed costs
#' @param Cv Variable costs.
#' @param n Optimal test sample size
#' @return Binomial RDT cost
#' @examples
#' #the n value can be the minimum test sample size obtained from \code{\link{boptimal_n}}.
#' n_optimal <- 20
#' bcost_RDT(Cf = 0, Cv = 10, n = n_optimal);
#' @seealso \code{\link{bcost_RG}}, \code{\link{bcost_WS}}, \code{\link{bcost_expected}}
#' @export

bcost_RDT <- function(Cf, Cv, n){
  return(Cf + Cv * n)
}
