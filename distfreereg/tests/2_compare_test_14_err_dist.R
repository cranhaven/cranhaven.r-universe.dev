library(distfreereg)
set.seed(20240228)

n <- 20
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))

cdfr_1 <- compare(reps = 5, prog = Inf, theta = theta, true_mean = func, true_X = X,
                  X = X, test_mean = func,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  theta_init = rep(1, length(theta)))

signif(cdfr_1[["observed_stats"]], digits = 4)

edf <- function(reps, n, sd = 1) matrix(rnorm(n * reps, sd = sd), nrow = n) 

cdfr_2 <- compare(reps = 5, prog = Inf, theta = theta, true_mean = func, true_X = X,
                  X = X, test_mean = func,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  theta_init = rep(1, length(theta)),
                  err_dist_fun = edf)

signif(cdfr_2[["observed_stats"]], digits = 4)

cdfr_3 <- compare(reps = 5, prog = Inf, theta = theta, true_mean = func, true_X = X,
                  X = X, test_mean = func,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  theta_init = rep(1, length(theta)),
                  err_dist_fun = edf, err_dist_args = list(sd = 10))

signif(cdfr_3[["observed_stats"]], digits = 4)
