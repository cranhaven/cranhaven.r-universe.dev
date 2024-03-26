#' Return period function of Noramal distribution
#'
#' @param x quantile/s
#' @param para parameters as c(location, scale)
#'
#' @return Return Period/s corresponding to quantile/s.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfnor
#'
#' @examples
#' 
#' RP <- tnor(x = 11, para = c(10, 1.5))
#' 
tnor <- function(x, para = c(10, 1.5)){
  u <- cdfnor(x , para)
  RP <- 1/(1 - u)
  return(RP)
}
