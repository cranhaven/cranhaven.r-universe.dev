#' Probability density function of Lognormal-3 Distribution
#'
#' @param x quantile/s
#' @param para parameters as c(zeta, mu, sigma) that is c(lower bound, mean on log scale, standard deviation on log scale).
#'
#' @return Probability density function
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#'
#' @examples
#' 
#' d <- dln3(x = 12, para = c(0, 0, 1))
#' 
dln3 <- function(x , para = c(0, 0, 1)) {
  if (length(para) != 3) stop("parameter vector has wrong length")
  if (any(is.na(para))) stop("missing values in parameter vector")
  if (para[3] <= 0) stop("distribution parameters invalid")
  y <- (exp((-1*(log(x - para[1]) - para[2])^2)/(2*(para[3])^2)))/(sqrt(2)*sqrt(pi)*para[3]*(x - para[1]))
  return(y)
}
