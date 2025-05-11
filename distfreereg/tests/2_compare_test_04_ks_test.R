library(distfreereg)
set.seed(20240123)
n <- 20
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))

comp_dfr <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                    X = X, test_mean = func, covariance = list(Sigma = Sig),
                    true_covariance = list(Sigma = Sig), theta_init = rep(1, length(theta)))

ks.test(comp_dfr)
