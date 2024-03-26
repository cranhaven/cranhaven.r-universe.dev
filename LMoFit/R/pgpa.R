#' Cumulative distribution function of Generalized Pareto Distribution
#'
#' @param x quantile/s
#' @param para parameters as c(location, scale, shape)
#'
#' @return Non-exceedance probability from the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfgpa
#'
#' @examples
#' 
#' u <- pgpa(x = 1.2, para = c(1, 2, 0.5))
#' 
pgpa <- function(x , para = c(1, 1, 1)) {
  u <- cdfgpa(x , para)
  return(u)
}
