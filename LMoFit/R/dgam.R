#' Probability density function of Gamma distribution
#'
#' @param x quantile/s
#' @param para parameters as c(shape, scale)
#'
#' @return Probability density function
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom stats dgamma
#'
#' @examples
#' 
#' d <- dgam(x = 0.1, para = c(0.1, 0.2))
#' 
dgam <- function(x , para = c(1, 2, 0.5)) {
  if (length(para) != 2) stop("parameter vector has wrong length")
  if (any(is.na(para))) stop("missing values in parameter vector")
  if (any(para <= 0)) stop("distribution parameters invalid")
  d <- dgamma(x/para[2],para[1])
  return(d)
}
