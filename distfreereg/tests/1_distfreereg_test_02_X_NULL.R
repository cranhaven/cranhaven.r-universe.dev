library(distfreereg)
set.seed(20240123)
n <- 1e2

# Verify simple case in which X is NULL.

theta <- 2
ft <- function(theta) theta
Y <- distfreereg:::f2ftheta(f = ft, X = NULL, n = n)(theta) + rnorm(n)
dfr <- distfreereg(Y = Y, test_mean = ft, covariance = list(Sigma = 1), verbose = TRUE, 
                   theta_init = rep(1, length(theta)))
dfr

dfr[["mu"]]
dfr[["r"]]
dfr[["r_tilde"]]
