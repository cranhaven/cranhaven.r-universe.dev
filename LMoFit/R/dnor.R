#' Probability density function of Normal Distribution
#'
#' @param x quantile/s
#' @param para parameters as c(location, scale)
#'
#' @return Probability density function
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom stats dnorm
#'
#' @examples
#' 
#' d <- dnor(x = 1.5, para = c(1, 2))
#' 
dnor <- function(x , para = c(1, 2)) {
  if (length(para) != 2) stop("parameter vector has wrong length")
  if (any(is.na(para))) stop("missing values in parameter vector")
  if (para[2] <= 0) stop("distribution parameters invalid")
  y <- dnorm(x, para[1], para[2])
  return(y)
}
