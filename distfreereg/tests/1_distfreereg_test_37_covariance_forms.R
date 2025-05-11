# Specifying the single value "5", the vector of n "5"s, and the diagonal matrix
# with diagonal elements "5" should yield the same results.

library(distfreereg)

n <- 100
X <- rnorm(n)
Y <- X + rnorm(n)
Sig1 <- 5
Sig2 <- rep(5,n)
Sig3 <- diag(5, n)
test_mean <- function(x, theta) x*theta

set.seed(20240711)
dfr1 <- distfreereg(Y = Y, X = X, test_mean = test_mean, B = 1e4,
                    covariance = list(Sigma = Sig1), theta_init = 1,
                    verbose = FALSE)
set.seed(20240711)
dfr2 <- distfreereg(Y = Y, X = X, test_mean = test_mean, B = 1e4,
                    covariance = list(Sigma = Sig2), theta_init = 1,
                    verbose = FALSE)
set.seed(20240711)
dfr3 <- distfreereg(Y = Y, X = X, test_mean = test_mean, B = 1e4,
                    covariance = list(Sigma = Sig3), theta_init = 1,
                    verbose = FALSE)

identical(dfr1$theta_hat, dfr2$theta_hat)
identical(dfr1$theta_hat, dfr3$theta_hat)
identical(dfr1$observed_stats, dfr2$observed_stats)
identical(dfr1$observed_stats, dfr3$observed_stats)
identical(dfr1$p, dfr2$p)
identical(dfr1$p, dfr3$p)