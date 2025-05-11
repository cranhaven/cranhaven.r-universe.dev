library(distfreereg)
set.seed(20240123)

# Test case where only test_mean contains X argument

n <- 20
test_mean_func <- function(x, theta) theta[1] + theta[2]*x[1]
true_mean_func <- function(theta) theta
theta <- 2
X <- rexp(n, rate = 1)

cdfr <- compare(reps = 10, B = 10, theta = theta, true_X = NULL, X = X, true_covariance = list(Sigma = 1),
                covariance = list(Sigma = 1),
                true_mean = true_mean_func, test_mean = test_mean_func, theta_init = c(1,1))

signif(cdfr[["observed_stats"]][["KS"]], digits = 4)
signif(cdfr[["observed_stats"]][["CvM"]], digits = 4)
signif(cdfr[["mcsim_stats"]][["KS"]], digits = 4)
signif(cdfr[["mcsim_stats"]][["CvM"]], digits = 4)
