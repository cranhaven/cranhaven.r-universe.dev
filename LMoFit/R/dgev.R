#' Probability density function of GEV distribution
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
#' d <- dgev(x = 108.4992, para = c(10, 1, 1))
#' 
dgev <- function(x, para){
  a <- para[1]; b <- para[2]; c <- para[3]
  if (b <= 0) {stop("scale parameter cannot be <= 0")}
  if (c == 0) {
    d <- (exp((-exp(-(x - a)/b)) - ((x - a)/b)))/b #Gumble distribution
  } else {
    d <- (((c/b*(x - a) + 1)^(-1/c - 1)) * (exp(-1/((c/b*(x - a) + 1)^(1/c)))))/b #GEV distribution
  }
  #if (1 + c/b*(x - a) < 0) {
  #  warning(paste("1 + shape/scale*(x - location) < 0", "GEV dist. is not recommended in this case", sep = " ... "))
  #}
  return(d)
}
