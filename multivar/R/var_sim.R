#' Simulate a stationary Vector Autoregressive (VAR) time series.
#'
#' @param n An integer giving the number of timepoints.
#' @param phi A d x d transition matrix.
#' @param sigma A d x d innovation covariance matrix.
#' @param bubrn Burn-in period to be discarded. Default is 500.
#' @keywords var simulate internal
#' @noRd
var_sim = function(n, phi, sigma, burn = 500){
  k    <- dim(phi)[1]
  p    <- dim(phi)[2]/k
  inno <- MASS::mvrnorm(n=n+burn, rep(0, k), sigma)
  init <- MASS::mvrnorm(n=p, rep(0, k), sigma)
  init <- matrix(init, nrow=p)
	j    <- 1
	id   <- seq(from= j+p-1, to = j, by=-1)
  Y    <-  matrix(0, (n+burn), k)
  for(r in 1:(n+burn)){
  	Y[r,] <-  phi %*% as.vector(t(init[id,])) + inno[r,]
     init <- rbind(init[-1,], Y[r,])
  }
  return(Y[-(1:burn),])
}
