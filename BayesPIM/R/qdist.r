qdist <- function(p, par, dist = "exp") {
  if (dist == "exp") {
    return(qexp(p, rate = par[, 1]))
  }
  if (dist == "gamma") {
    return(qgamma(p, shape = par[, 1], rate = par[, 2]))
  }
  if (dist == "weibull") {
    return(qweibull(p, shape = par[, 2], scale = par[, 1]))
  }
  if (dist == "loglog") {
    return(qloglog(p, lambda = par[, 1], gamma = par[, 2]))
  }
  if (dist == "lognormal") {
    return(qlnorm(p, meanlog = par[, 1], sdlog = par[, 2]))
  }
  if (dist == "gengamma") {
    return(qggamma(p, a = 1/par[, 1], b = par[, 2], k = par[, 3]))
  }
}