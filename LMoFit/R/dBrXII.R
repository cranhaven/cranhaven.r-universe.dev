#' Probability density function of BrXII distribution
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
#' d <- dBrXII(x = 108.4992, para = c(10, 0.25, 0.5))
#' 
dBrXII <- function(x , para = c(1, 2, 0.5)) {
  scale <- para[1]; shape_1 <- para[2]; shape_2 <- para[3]
  d <- (((x/scale)^shape_1)*((shape_2*((x/scale)^shape_1) + 1)^((-1/(shape_1*shape_2)) - 1)))/x
  return(d)
}
