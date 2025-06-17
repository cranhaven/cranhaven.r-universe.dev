##########################################################################
# generate mean for alpha and beta, also transform alpha and beta from N_alpha, N_beta 
# u - mean vector
# sigma - covariance matrix
# n - # of samples
# debug - support debug print or not
##
## Note: should use rtmvnorm (truncated multivariate normal distribution) instead? (install.packages("tmvtnorm"))
.generate_u_param <- function(u, sigma, n, debug = FALSE){

  N_param <- rtmvnorm(n, mean = u, sigma = diag(sigma),lower=rep(0, length = length(u)), 
                      upper=rep( 10, length = length(u)))
  
  return(N_param)
}