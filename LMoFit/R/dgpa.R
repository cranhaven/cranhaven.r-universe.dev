#' Probability density function of Generalized Pareto Distribution
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
#' d <- dgpa(x = 0.1, para = c(1, 2, 0.5))
#' 
dgpa <- function(x, para){
  if (length(para) != 3) stop("parameter vector has wrong length")
  if (any(is.na(para))) stop("missing values in parameter vector")
  if (para[2] <= 0) stop("distribution parameters invalid")
  if (para[3] == 0) y <- exp(-pmax(0, (x - para[1])/para[2]))/para[2]
  else y <- (exp((log(pmax(0, (1 - para[3]*(x - para[1])/para[2])) ))/para[3]) )/(-1*(para[3]*x - para[1]*para[3] - para[2]))
  return(y)
}
