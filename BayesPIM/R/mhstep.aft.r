mhstep.aft <- function(x, t, Z, tau, sig.prior, k.prior, prop.var = diag(1, length(x)), dist, fix.sigma = FALSE, fix.k = F, beta.prior = 't') {
  x_ <- mvrnorm(1, x, prop.var)
  if (fix.sigma) {
    if(dist != 'gengamma') x_[length(x_)] <- log(sig.prior)
    if(dist == 'gengamma') x_[length(x_)-1] <- log(sig.prior)
  }
  if (fix.k & dist == 'gengamma') {
    x_[length(x_)] <- log(k.prior)
  }
  p.Y <- pst.aft(par = x_, t = t, Z = Z, tau = tau, sig.prior = sig.prior, k.prior = k.prior, dist = dist, beta.prior = beta.prior)
  p.X <- pst.aft(par = x, t = t, Z = Z, tau = tau, sig.prior = sig.prior, k.prior = k.prior, dist = dist, beta.prior = beta.prior)
  r <- p.Y - p.X
  u <- runif(1, 0, 1)
  ret <- list()
  if (is.nan(r)) {
    r <- -Inf
  }
  if (log(u) <= r) {
    ret$s <- x_
    ret$p.X <- p.Y
  } else {
    ret$s <- x
    ret$p.X <- p.X
  }
  return(ret)
}