#' Return period function of Generalized Pareto distribution
#'
#' @param x quantile/s
#' @param para parameters as c(location, scale, shape)
#'
#' @return Return Period/s corresponding to quantile/s.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfgpa
#'
#' @examples
#' 
#' RP <- tgpa(x = 1.2, para = c(1, 2, 0.5))
#' 
tgpa <- function(x, para = c(1, 1, 1)){
  u <- cdfgpa(x , para)
  RP <- 1/(1 - u)
  return(RP)
}
