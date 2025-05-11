library(distfreereg)
set.seed(20240123)
n <- 100
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))

comp_dfr <- compare(reps = 5e2, prog = Inf, theta = theta, true_mean = func, true_X = X,
                    X = X, test_mean = func,
                    true_covariance = list(Sigma = Sig),
                    covariance = list(Sigma = Sig),
                    theta_init = rep(1, length(theta)))

plot(comp_dfr, confband_args = FALSE)
plot(comp_dfr, confband_args = NULL)
plot(comp_dfr, which = "dens", confband_args = FALSE)
plot(comp_dfr, which = "dens", confband_args = NULL)
plot(comp_dfr, which = "qq")
plot(comp_dfr, which = "qqp")
