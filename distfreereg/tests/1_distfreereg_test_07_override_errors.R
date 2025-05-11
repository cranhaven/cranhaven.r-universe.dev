library(distfreereg)
set.seed(20240123)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1), verbose = FALSE,
                     override = list(Sig)), error = function(e) warning(e))
tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1), verbose = FALSE,
                     override = list(Sig, res_order = sample(1:n))), error = function(e) warning(e))
tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1), verbose = FALSE,
                     override = list(order = sample(1:n))), error = function(e) warning(e))
