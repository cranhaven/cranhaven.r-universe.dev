#' Probability density function of Generalized Gamma (GG) distribution
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
#' d <- dGG(x = 108.4992, para = c(10, 0.25, 0.5))
#' 
dGG <- function(x , para = c(10, 0.25, 0.5)) {
  scale <- para[1]; shape1 <- para[2]; shape2 <- para[3]
  d <- ifelse(x <= 0 | scale <= 0 | shape1 <= 0 | shape2 <= 0, NaN,
              ((shape2/(scale*gamma(shape1/shape2))) * ((x/scale)^(shape1 - 1)) ) * (exp(-((x/scale)^(shape2)))))
  #d <- (shape2/(scale^shape1))*(x^(shape1 - 1))*(exp(-(x/scale)^shape2)) / gamma(shape1/shape2) #the same but original formulation
  return(d)
}
