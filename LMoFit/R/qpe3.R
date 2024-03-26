#' Quantile distribution function of Pearson type-3 Distribution
#'
#' @param u non-exceedance probability
#' @param RP Return Period "don't use in case u is used"
#' @param para parameters as c(mu, sigma, gamma) that is c(location, scale, shape).
#'
#' @return Quantile value/s using the inverse of the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom quape3
#'
#' @examples
#' 
#' x <- qpe3(u = 0.99, para = c(1, 1, 0))
#' x <- qpe3(RP = 100, para = c(1, 1, 0))
#' 
qpe3 <- function(u = NULL, RP = 1/(1 - u), para){
  if (is.null(u) & length(RP) >= 1) {u <- 1 - 1/RP}
  x <- quape3(f = u, para = para)
  return(x)
}
