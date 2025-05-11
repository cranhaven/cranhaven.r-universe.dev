library(distfreereg)
set.seed(20240227)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2]
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
P <- distfreereg:::matinv(Sig, tol = .Machine[["double.eps"]])
Q <- distfreereg:::matsqrt(P)
theta <- c(2,5,1)
X <- matrix(rexp(2*n, rate = 1), ncol = 2)
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

set.seed(20240227)
dfr_1 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1), verbose = FALSE)
set.seed(20240227)
dfr_2 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(P = P), theta_init = c(1,1,1), verbose = FALSE)
set.seed(20240227)
dfr_3 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Q = Q), theta_init = c(1,1,1), verbose = FALSE)

message('identical(dfr_1[["epsp"]], dfr_2[["epsp"]]) (should be TRUE): ', identical(dfr_1[["epsp"]], dfr_2[["epsp"]]))
message('identical(dfr_1[["epsp"]], dfr_3[["epsp"]]) (should be TRUE): ', identical(dfr_1[["epsp"]], dfr_3[["epsp"]]))

set.seed(20240227)
dfr_4 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = 7), theta_init = c(1,1,1), verbose = FALSE)
set.seed(20240227)
dfr_5 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(P = 1/7), theta_init = c(1,1,1), verbose = FALSE)
set.seed(20240227)
dfr_6 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Q = 1/sqrt(7)), theta_init = c(1,1,1), verbose = FALSE)
set.seed(20240227)
dfr_7 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = diag(7, nrow = n)), theta_init = c(1,1,1), verbose = FALSE)

message('identical(dfr_4[["epsp"]], dfr_5[["epsp"]]) (should be TRUE): ', identical(dfr_4[["epsp"]], dfr_5[["epsp"]]))
message('identical(dfr_4[["epsp"]], dfr_6[["epsp"]]) (should be TRUE): ', identical(dfr_4[["epsp"]], dfr_6[["epsp"]]))
message('identical(dfr_4[["epsp"]], dfr_7[["epsp"]]) (should be TRUE): ', identical(dfr_4[["epsp"]], dfr_7[["epsp"]]))
