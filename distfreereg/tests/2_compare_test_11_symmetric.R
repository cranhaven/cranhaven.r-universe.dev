library(distfreereg)
set.seed(20240227)

n <- 20
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))

set.seed(20240227)
cdfr_1 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                  X = X, test_mean = func,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  theta_init = rep(1, length(theta)))

set.seed(20240227)
cdfr_2 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                  X = X, test_mean = func,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  theta_init = rep(1, length(theta)), control = list(symmetric = FALSE))

message('identical(cdfr_1$observed_stats, cdfr_2$observed_stats) (should be TRUE): ', identical(cdfr_1$observed_stats, cdfr_2$observed_stats))

tryCatch(compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)), control = list(symmetric = TRUE)),
         error = function(e) warning(e))

tryCatch(compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 true_covariance = list(P = solve(Sig)),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)),
                 control = list(symmetric = list(tol = 1e-200))),
         error = function(e) warning(e))
