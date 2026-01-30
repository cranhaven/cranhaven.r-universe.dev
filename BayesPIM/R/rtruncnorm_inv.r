rtruncnorm_inv <- function(n, mu = 0, sd = 1, a = -Inf, b = Inf) {
  a_cdf <- pnorm(a, mean = mu, sd = sd)
  b_cdf <- pnorm(b, mean = mu, sd = sd)
  
  u <- runif(n, min = a_cdf, max = b_cdf)
  
  return(qnorm(u, mean = mu, sd = sd))
}