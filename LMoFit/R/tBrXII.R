#' Return period function of BrXII distribution
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
#' RP <- tBrXII(x = 108.4992, para = c(10, 0.25, 0.5))
#' 
tBrXII <- function(x , para = c(1, 2, 0.5)) {
  scale <- para[1]; shape_1 <- para[2]; shape_2 <- para[3]
  u <- 1 - ((1 + shape_2*((x/scale)^shape_1))^(-1/(shape_1*shape_2)))
  RP <- 1/(1 - u)
  return(RP)
}
