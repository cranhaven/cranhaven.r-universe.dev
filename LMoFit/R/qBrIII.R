#' Quantile distribution function of BrIII distribution
#'
#' @param u non-exceedance probability
#' @param RP Return Period "don't use in case u is used"
#' @param para parameters as c(scale, shape1, shape2)
#'
#' @return Quantile value/s using the inverse of the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#'
#' @examples
#' 
#' x <- qBrIII(u = 0.99, para = c(1, 10, 0.8))
#' x <- qBrIII(RP = 100, para = c(1, 10, 0.8))
#' 
qBrIII <- function(u = NULL, RP = 1/(1 - u), para){
  if (is.null(u) & length(RP) >= 1) {u <- 1 - 1/RP}
  scale <- para[1]; shape_1 <- para[2]; shape_2 <- para[3]
  x <- scale*((shape_1*((u^(-1/(shape_1*shape_2))) - 1))^(-1*shape_2)) #BrIII quantile distribution function
  return(x)
}

