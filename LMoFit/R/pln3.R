#' Cumulative distribution function of Lognormal-3 Distribution
#'
#' @param x quantile/s
#' @param para parameters as c(zeta, mu, sigma) that is c(lower bound, mean on log scale, standard deviation on log scale).
#'
#' @return Non-exceedance probability from the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfln3
#'
#' @examples
#' 
#' u <- pln3(x = 12, para = c(0, 0, 1))
#' 
pln3 <- function(x , para = c(0, 0, 1)) {
  u <- cdfln3(x , para)
  return(u)
}
