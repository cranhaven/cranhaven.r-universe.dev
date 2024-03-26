#' Quantile distribution function of Gamma distribution
#'
#' @param u non-exceedance probability
#' @param RP Return Period "don't use in case u is used"
#' @param para parameters as c(shape, scale)
#'
#' @return Quantile value/s using the inverse of the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom quagam
#'
#' @examples
#' 
#' x <- qgam(u = 0.99, para = c(0.1, 0.2))
#' x <- qgam(RP = 100, para = c(0.1, 0.2))
#' 
qgam <- function(u = NULL, RP = 1/(1 - u), para){
  if (is.null(u) & length(RP) >= 1) {u <- 1 - 1/RP}
  x <- quagam(f = u , para = para)
  return(x)
}
