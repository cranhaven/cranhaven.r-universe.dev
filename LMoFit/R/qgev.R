#' Quantile distribution function of GEV distribution
#'
#' @param u non-exceedance probability
#' @param RP Return Period "don't use in case u is used"
#' @param para parameters as c(location, scale, shape)
#'
#' @return Quantile value/s using the inverse of the cumulative distribution function.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#'
#' @examples
#' 
#' x <- qgev(u = 0.99, para = c(10, 1, 1))
#' x <- qgev(RP = 100, para = c(10, 1, 1))
#' 
qgev <- function(u = NULL, RP = 1/(1 - u), para){
  if (is.null(u) & length(RP) >= 1) {u <- 1 - 1/RP}
  a <- para[1]; b <- para[2]; c <- para[3]
  if (b <= 0) {stop("scale parameter cannot be <= 0")}
  if (c == 0) {
    x <- a - b*(log(-log(u))) #Gumble quantile
  } else {
    #x <- a + b/c*(-1 - ((log(u))^(-c))) #GEV quantile
    x <- a + b/c*(-1 - (sign(log(u))*(abs(log(u)))^(-c))) #GEV quantile

  }
  return(x)
}
