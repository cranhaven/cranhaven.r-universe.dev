#' Quantile distribution function of Generalized normal Distribution
#'
#' @param u non-exceedance probability
#' @param RP Return Period "don't use in case u is used"
#' @param para parameters as c(location, scale, shape)
#'
#' @return Quantile value/s using the inverse of the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom quagno
#'
#' @examples
#' 
#' x <- qgno(u = 0.99, para = c(10, 0.1, 0.2))
#' x <- qgno(RP = 100, para = c(10, 0.1, 0.2))
#' 
qgno <- function(u = NULL, RP = 1/(1 - u), para){
  if (is.null(u) & length(RP) >= 1) {u <- 1 - 1/RP}
  x <- quagno(f = u , para = para)
  return(x)
}
