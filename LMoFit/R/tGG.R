#' Return period function of Generalized Gamma distribution
#'
#' @param x quantile/s
#' @param para parameters as c(scale, shape1, shape2)
#'
#' @return Return Period/s corresponding to quantile/s.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#'
#' @examples
#' 
#' RP <- tGG(x = 108.4992, para = c(10, 0.25, 0.5))
#' 
tGG <- function(x, para = c(10, 0.25, 0.5)){
  scale <- para[1]; shape1 <- para[2]; shape2 <- para[3]
  z <- ((x/scale)^shape2)
  u <- (as.numeric(gammainc(z, (shape1/shape2))[1])) / gamma(shape1/shape2)
  RP <- 1/(1 - u)
  return(RP)
}
