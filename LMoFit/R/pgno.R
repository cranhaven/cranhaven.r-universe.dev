#' Cumulative distribution function of Generalized Normal Distribution
#'
#' @param x quantile/s
#' @param para parameters as c(location, scale, shape)
#'
#' @return Non-exceedance probability from the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfgno
#'
#' @examples
#' 
#' u <- pgno(x = 10.1, para = c(10, 0.1, 0.2))
#' 
pgno <- function(x , para = c(10, 1.5, 1)) {
  u <- cdfgno(x , para)
  return(u)
}
