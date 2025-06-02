L2 <- function(y, yhat, w){ 
  n <- length(y)
  if (missing(w)) w <- rep(1 / n, n)
  return(stats::weighted.mean((y-yhat)^2, w))
}
