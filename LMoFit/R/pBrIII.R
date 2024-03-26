#' Cumulative distribution function of BrIII distribution
#'
#' @param x quantile/s
#' @param para parameters as c(scale, shape1, shape2)
#'
#' @return Non-exceedance probability from the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#'
#' @examples
#' 
#' u <- pBrIII(x = 108.4992, para = c(10, 0.25, 0.5))
#' 
pBrIII <- function(x , para = c(1, 2, 0.5)) {
  scale <- para[1]; shape1 <- para[2]; shape2 <- para[3]
  u <- (1 + (1/shape1)*((x/scale)^(-1/shape2)))^(-shape1*shape2)
  return(u)
}
