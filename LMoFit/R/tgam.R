#' Return period function of Gamma distribution
#'
#' @param x quantile/s
#' @param para parameters as c(shape, scale)
#'
#' @return Return Period/s corresponding to quantile/s.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfgam
#'
#' @examples
#' 
#' RP <- tgam(x = 0.1, para = c(0.1, 0.2))
#' 
tgam <- function(x , para = c(1.5, 1)){
  u <- cdfgam(x , para)
  RP <- 1/(1 - u)
  return(RP)
}
