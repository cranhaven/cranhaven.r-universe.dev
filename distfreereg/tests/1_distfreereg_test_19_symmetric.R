library(distfreereg)
set.seed(20240227)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2]
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5,1)
X <- matrix(rexp(2*n, rate = 1), ncol = 2)
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

set.seed(20240227)
dfr_1 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1), verbose = FALSE)
set.seed(20240227)
dfr_2 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1), verbose = FALSE,
                     control = list(symmetric = FALSE))

message('identical(dfr_1[["epsp"]], dfr_2[["epsp"]]) (should be TRUE): ', identical(dfr_1[["epsp"]], dfr_2[["epsp"]]))

tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1), verbose = FALSE,
                     control = list(symmetric = TRUE)),
         error = function(e) warning(e))

tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(P = solve(Sig)), theta_init = c(1,1,1), verbose = FALSE,
                     control = list(symmetric = list(tol = 1e-200))),
         error = function(e) warning(e))

tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(P = matrix(rnorm(n^2), nrow = n)),
                     theta_init = c(1,1,1), verbose = FALSE, control = list(symmetric = FALSE)),
         error = function(e) warning(e))

