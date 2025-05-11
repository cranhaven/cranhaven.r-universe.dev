library(distfreereg)
set.seed(20240311)

# Test case where neither true_mean nor test_mean contains X argument

n <- 5
true_func <- function(theta) theta
test_func <- function(theta) theta
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- 2

cdfr_1 <- compare(true_mean = true_func,
                  true_X = NULL,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  theta = theta,
                  test_mean = test_func,
                  X = NULL,
                  theta_init = 1,
                  reps = 5,
                  B = 5,
                  n = n)
cdfr_1
