library(distfreereg)
set.seed(20240821)

n <- 1e2
true_mean <- function(X, theta) X[,1]^theta[1] + theta[2]*X[,2]
test_mean_upper <- true_mean
test_mean_lower <- function(x, theta) x[1]^theta[1] + theta[2]*x[2]
theta <- c(3,-2)
Sigma <- rWishart(1, df = n, Sigma = diag(n))[,,1]
X <- matrix(rexp(2*n), nrow = n)
Y <- distfreereg:::f2ftheta(true_mean, X)(theta) +
  distfreereg:::rmvnorm(n = n, reps = 1, SqrtSigma = distfreereg:::matsqrt(Sigma))

set.seed(20240821)
dfr_1 <- distfreereg(test_mean = test_mean_upper, Y = Y, X = X,
                     covariance = list(Sigma = Sigma),
                     theta_init = rep(1, length(theta)))

set.seed(20240821)
dfr_2 <- distfreereg(test_mean = test_mean_lower, Y = Y, X = X,
                     covariance = list(Sigma = Sigma),
                     theta_init = rep(1, length(theta)))

message('identical(dfr_1$theta_hat, dfr_2$theta_hat) (should be TRUE): ', identical(dfr_1$theta_hat, dfr_2$theta_hat))
message('identical(dfr_1$observed_stats, dfr_2$observed_stats) (should be TRUE): ', identical(dfr_1$observed_stats, dfr_2$observed_stats))
