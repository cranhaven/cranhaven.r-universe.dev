#' Return period function of Generalized Normal distribution
#'
#' @param x quantile/s
#' @param para parameters as c(location, scale, shape)
#'
#' @return Return Period/s corresponding to quantile/s.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom cdfgno
#'
#' @examples
#' 
#' RP <- tgno(x = 10.1, para = c(10, 0.1, 0.2))
#' 
tgno <- function(x, para = c(10, 1.5, 1)){
  u <- cdfgno(x , para)
  RP <- 1/(1 - u)
  return(RP)
}
