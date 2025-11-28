Gini <- function(x) {
 x <- sort(x)
 n <- length(x)
 w <- rep(1, length=n)/n
 p <- cumsum(w)
 nu <- cumsum(w * x)
 nu <- nu/nu[n]
 sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
}
