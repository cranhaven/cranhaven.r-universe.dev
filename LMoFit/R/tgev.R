#' Return period function of GEV distribution
#'
#' @param x quantile/s
#' @param para parameters as c(location, scale, shape)
#'
#' @return Return Period/s corresponding to quantile/s.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#'
#' @examples
#' 
#' RP <- tgev(x = 108.4992, para = c(10, 1, 1))
#' 
tgev <- function(x, para){
  a <- para[1]; b <- para[2]; c <- para[3]
  if (b <= 0) {stop("scale parameter cannot be <= 0")}
  if (c == 0) {
    u <- exp(-exp(-(x - a)/b)) #Gumble distribution
  } else {
    argument <- pmax((1 + c/b*(x - a)), 0)
    u <- exp(-(argument)^(-1/c)) #GEV distribution
  }
  RP <- 1/(1 - u)
  return(RP)
}
