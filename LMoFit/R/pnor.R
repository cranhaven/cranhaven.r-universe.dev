#' Cumulative distribution function of Noramal Distribution
#'
#' @param x quantile/s
#' @param para parameters as c(location, scale)
#'
#' @return Non-exceedance probability from the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfnor
#'
#' @examples
#' 
#' u <- pnor(x = 11, para = c(10, 1.5))
#' 
pnor <- function(x , para = c(10, 1.5)) {
  u <- cdfnor(x , para)
  return(u)
}
