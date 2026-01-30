ddist <- function(x, par, dist = "exp") {
  if (dist == "exp") {
    return(dexp(x, rate = par[, 1]))
  }
  if (dist == "gamma") {
    return(dgamma(x, shape = par[, 1], rate = par[, 2]))
  }
  if (dist == "weibull") {
    return(dweibull(x, shape = par[, 2], scale = par[, 1]))
  }
  if (dist == "loglog") {
    return(dloglog(x, lambda = par[, 1], gamma = par[, 2]))
  }
  if (dist == "lognormal") {
    return(dlnorm(x, meanlog = par[, 1], sdlog = par[, 2]))
  }
  if (dist == "gengamma") {
    return(dggamma(x, a = 1/par[, 1], b = par[, 2], k = par[, 3]))
  }
}