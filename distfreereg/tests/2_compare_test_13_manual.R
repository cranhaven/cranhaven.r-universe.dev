library(distfreereg)
set.seed(20240227)

n <- 20
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))

cdfr <- compare(reps = 5, prog = Inf, theta = theta, true_mean = func, true_X = X,
                  X = X, test_mean = func,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  theta_init = rep(1, length(theta)), manual = function(dfr) residuals(dfr))

lapply(cdfr[["manual"]], signif, digits = 4)

tryCatch(compare(reps = 5, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)), manual = "hi"),
         error = function(e) warning(e))
