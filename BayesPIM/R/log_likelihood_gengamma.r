log_likelihood_gengamma <- function(params, x, z) {
  # Extract beta, log_sigma, and k from params
  beta <- params[1:(length(params) - 2)]
  log_sigma <- params[length(params) - 1]
  log_k <- params[length(params)]
  
  # Compute sigma, gamma, and k
  sigma <- exp(log_sigma)
  gamma <- 1 / sigma
  k <- exp(log_k)
  if (k == 0) k <- 1e-300
  
  # Compute lambda
  lambda <- exp(-z %*% beta)
  
  # Compute the log-likelihood
  ll <- sum(log(lambda) + log(gamma) + (gamma * k - 1) * log(lambda * x) - (lambda * x)^gamma - log(gamma(k)))
  
  return(ll)
}