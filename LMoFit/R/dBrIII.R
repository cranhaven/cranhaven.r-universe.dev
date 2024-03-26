#' Probability density function of BrIII distribution
#'
#' @param x quantile/s
#' @param para parameters as c(scale, shape1, shape2)
#'
#' @return Probability density function
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#'
#' @examples
#' 
#' d <- dBrIII(x = 108.4992, para = c(10, 0.25, 0.5))
#' 
dBrIII <- function(x , para = c(1, 2, 0.5)) {
  scale <- para[1]; shape1 <- para[2]; shape2 <- para[3]
  d <- (((1/(shape1*((x/scale)^(1/shape2)))) + 1)^(-shape1*shape2 - 1))/(x*((x/scale)^(1/shape2)))
  return(d)
}
