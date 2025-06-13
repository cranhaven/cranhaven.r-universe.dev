predict.artfima <- function(object, n.ahead = 10, ...) {
  z <- object$z
  n <- length(z)
  r <- (object$sigmaSq)*(object$tacvf)
  zm <- object$constant
  r <- artfimaTACVF(maxlag=n+n.ahead, obj=object)
  TrenchForecast(z, r, zm, n, maxLead=n.ahead)
}
