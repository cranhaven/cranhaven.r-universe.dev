#' Cumulative distribution function of Generalized Logistic Distribution
#'
#' @param x quantile/s
#' @param para parameters as c(location, scale, shape)
#'
#' @return Non-exceedance probability from the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfglo
#'
#' @examples
#' 
#' u <- pglo(x = 0.1, para = c(10, 0.1, 0.2))
#' 
pglo <- function(x , para = c(10, 1.5, 1)) {
  u <- cdfglo(x , para)
  return(u)
}
