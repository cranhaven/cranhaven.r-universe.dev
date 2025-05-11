# All of the calls to compare() below should result in errors. They are grouped
# into three groups, depending on whether or not the true mean or test mean
# require covariates.

library(distfreereg)
set.seed(20240904)
n <- 5
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
true_func <- function(theta, x) theta[1]*x[1] + theta[2]*x[2]
test_func <- function(theta, x) theta[1]*x[1] + theta[2]*x[2]^2
true_X <- matrix(c(rexp(n, rate = 1), rnorm(n)), nrow = n)
X <- true_X
Y <- theta[1]*X[,1] + theta[2]*X[,2]

cdfr <- compare(true_mean = true_func,
        test_mean = test_func,
        true_X = true_X,
        X = X,
        true_covariance = list(Sigma = Sig),
        covariance = list(Sigma = Sig),
        theta_init = rep(1, length(theta)),
        reps = 10, prog = Inf, theta = theta,
        global_override = list(theta_hat = theta),
        manual = function(x) coef(x))

sapply(cdfr[["manual"]], function(x) identical(theta, unname(x)))# All TRUE
