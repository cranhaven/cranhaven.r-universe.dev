#' Quantile distribution function of Lognormal-3 Distribution
#'
#' @param u non-exceedance probability
#' @param RP Return Period "don't use in case u is used"
#' @param para parameters as c(zeta, mu, sigma) that is c(lower bound, mean on log scale, standard deviation on log scale).
#'
#' @return Quantile value/s using the inverse of the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom qualn3
#'
#' @examples
#' 
#' x <- qln3(u = 0.99, para = c(0, 0, 1))
#' x <- qln3(RP = 100, para = c(0, 0, 1))
#' 
qln3 <- function(u = NULL, RP = 1/(1 - u), para){
  if (is.null(u) & length(RP) >= 1) {u <- 1 - 1/RP}
  x <- qualn3(f = u, para = para)
  return(x)
}
