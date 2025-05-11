library(distfreereg)
set.seed(20240123)

# Test case where only true_mean contains X argument

n <- 20
true_mean_func <- function(x, theta) theta[1] + theta[2]*x[1]
test_mean_func <- function(theta) theta
theta <- c(2,5)
X <- rexp(n, rate = 1)

comp_dfr <- compare(reps = 10, B = 10, theta = theta, true_X = X, X = NULL, true_covariance = list(Sigma = 1),
                    covariance = list(Sigma = 1),
                    true_mean = true_mean_func, test_mean = test_mean_func, theta_init = 1)

comp_dfr
