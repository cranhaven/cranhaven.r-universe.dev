#' Cumulative distribution function of Generalized Gamma (GG) distribution
#'
#' @param x quantile/s
#' @param para parameters as c(scale, shape1, shape2)
#'
#' @return Non-exceedance probability from the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @importFrom pracma gammainc
#' @export
#'
#' @examples
#' 
#' u <- pGG(x = 108.4992, para = c(10, 0.25, 0.5))
#' 
pGG <- function(x , para = c(10, 0.25, 0.5)) {
  scale <- para[1]; shape1 <- para[2]; shape2 <- para[3]
  z <- ((x/scale)^shape2)
  u <- (as.numeric(gammainc(z, (shape1/shape2))[1])) / gamma(shape1/shape2)
  return(u)
}
