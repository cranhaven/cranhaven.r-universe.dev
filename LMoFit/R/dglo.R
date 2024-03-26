#' Probability density function of Generalized Logestic Distribution
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
#' d <- dglo(x = 0.1, para = c(1, 2, 0.5))
#' 
dglo <- function(x , para = c(1, 2, 0.5)) {
  e <- para[1] #location
  a <- para[2] #scale
  k <- para[3] #shape
  x.correct <- (1 - (k*(x - e))/a) > 0  & !is.na(x)
  if (k == 0) {
    y <- (x - e)/a
  } else {
    y <- -k^(-1)*log(1 - (k*(x[x.correct] - e))/a)
  }
  top <- a^(-1)*exp(-(1 - k)*y)
  bottom <- (1 + exp(-y))^2
  pdf <- vector()
  pdf[x.correct] <- top/bottom
  pdf[x.correct == FALSE] <- .Machine$double.eps
  pdf[is.na(x)] <- NA
  return(pdf)
}
