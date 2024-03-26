#' Cumulative distribution function of Gamma distribution
#'
#' @param x quantile/s
#' @param para parameters as c(shape, scale)
#'
#' @return Non-exceedance probability from the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfgam
#'
#' @examples
#' 
#' u <- pgam(x = 0.1, para = c(0.1, 0.2))
#' 
pgam <- function(x , para = c(1.5, 1)) {
  u <- cdfgam(x , para)
  return(u)
}
