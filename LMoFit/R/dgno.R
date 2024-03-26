#' Probability density function of Generalized normal Distribution
#'
#' @param x quantile/s
#' @param para parameters as c(location, scale, shape)
#'
#' @return Probability density function
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#'
#' @examples
#' 
#' d <- dgno(x = 0.1, para = c(1, 2, 0.5))
#' 
dgno <- function(x , para = c(1, 2, 0.5)) {
  if (length(para) != 3) stop("parameter vector has wrong length")
  if (any(is.na(para))) stop("missing values in parameter vector")
  if (para[2] <= 0) stop("distribution parameters invalid")
  if (para[3] == 0) y <- (exp((-(x - para[1])^2)/(2*((para[2])^2))))/(sqrt(2)*sqrt(pi)*para[2])
  else y <- (exp(((log(pmax(0, 1 - (para[3]*(x - para[1]))/(para[2]))))^2)/(-2*(para[3])^2)))/(-1*sqrt(2)*sqrt(pi)*(para[3]*x - para[1]*para[3] - para[2]))
  return(y)
}
