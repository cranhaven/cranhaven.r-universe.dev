pst.aft <- function(par, t, Z, tau, sig.prior, k.prior, dist, beta.prior = 't') {
  p <- length(par)
  beta <- par[1:(p-1)]
  si <- exp(par[p])
  
  if (dist == 'gengamma') {
    k <- exp(par[p-1])
    if (k == 0) k <- 1e-300
    prior_k <- dnorm(k, 0, k.prior)  # Prior probability for k (half-normal)
    LL <- LL.aft(par, t = t, Z = Z, dist = dist) + 
      dnorm(si, 0, sig.prior, log = TRUE) +
      sum(dt(beta[2:length(beta)], df = tau, log = TRUE)) +
      log(prior_k) + dnorm(beta)
  } else {
    if (beta.prior == 't') {
      LL <- LL.aft(par, t = t, Z = Z, dist = dist) + 
        dnorm(si, 0, sig.prior, log = TRUE) +
        sum(dt(beta, df = tau, log = TRUE))
    } else if (beta.prior == 'norm') {
      LL <- LL.aft(par, t = t, Z = Z, dist = dist) + 
        dnorm(si, 0, sig.prior, log = TRUE) +
        sum(dnorm(beta, 0, tau, log = TRUE))
    }
  }
  
  LL
}