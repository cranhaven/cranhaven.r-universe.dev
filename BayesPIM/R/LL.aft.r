LL.aft <- function(par, t, Z, dist) {
  
  p <- length(par)
  beta <- par[1:(p-1)]
  log_sigma <- par[p]
  n <- length(t)
  V <- (log(t) - Z %*% as.matrix(beta))
  
  if (dist == 'weibull') {
    sigma <- exp(log_sigma)
    LL <- -n * log_sigma + sum((V / sigma) - exp(V / sigma))
  } else if (dist == 'loglog') {
    sigma <- exp(log_sigma)
    logexpV <- log(1 + exp(-V/sigma))
    logexpV[is.infinite(logexpV)] <- -V[is.infinite(logexpV)]
    LL <- -n * log_sigma - sum(V/sigma) - 2 * sum(logexpV)
  } else if (dist == 'lognormal') {
    sigma <- exp(log_sigma)
    LL <- -n * log_sigma - 1/2 * sum((V / sigma)^2)
  } else if (dist == 'gengamma') {
    LL <- log_likelihood_gengamma(par, t, Z)
  }
  
  LL
}
