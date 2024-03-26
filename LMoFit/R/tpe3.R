#' Return period function of Pearson type-3 distribution
#'
#' @param x quantile/s
#' @param para parameters as c(mu, sigma, gamma) that are c(location, scale, shape).
#'
#' @return Return Period/s corresponding to quantile/s.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfpe3
#'
#' @examples
#' 
#' RP <- tpe3(x = 12, para = c(10, 1, 1.5))
#' 
tpe3 <- function(x, para = c(10, 1, 1.5)){
  u <- cdfpe3(x , para)
  RP <- 1/(1 - u)
  return(RP)
}
