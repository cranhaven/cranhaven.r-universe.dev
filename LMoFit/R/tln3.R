#' Return period function of Lognormal-3 distribution
#'
#' @param x quantile/s
#' @param para parameters as c(zeta, mu, sigma) that is c(lower bound, mean on log scale, standard deviation on log scale).
#'
#' @return Return Period/s corresponding to quantile/s.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfln3
#'
#' @examples
#' 
#' RP <- tln3(x = 12, para = c(0, 0, 1))
#' 
tln3 <- function(x, para = c(0, 0, 1)){
  u <- cdfln3(x , para)
  RP <- 1/(1 - u)
  return(RP)
}
