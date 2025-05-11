library(distfreereg)
set.seed(20240212)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2]
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5,1)
X <- matrix(rexp(2*n, rate = 1), nrow = n)
Y <- distfreereg:::f2ftheta(f = func, X = X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

dfr <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1),
                     verbose = FALSE)

plot(dfr, confband_args = NULL)
plot(dfr, confband_args = FALSE)

plot(dfr, which = "residuals")
plot(dfr, which = "epsp")
