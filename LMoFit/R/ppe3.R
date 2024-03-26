#' Cumulative distribution function of Pearson type-3 Distribution
#'
#' @param x quantile/s
#' @param para parameters as c(mu, sigma, gamma) that are c(location, scale, shape).
#'
#' @return Non-exceedance probability from the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfpe3
#'
#' @examples
#' 
#' u <- ppe3(x = 12, para = c(10, 1, 1.5))
#' 
ppe3 <- function(x , para = c(10, 1, 1.5)) {
  u <- cdfpe3(x , para)
  return(u)
}
