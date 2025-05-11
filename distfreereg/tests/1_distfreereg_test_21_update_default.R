library(distfreereg)
set.seed(20240304)
n <- 1e2

func <- function(x, theta) sum(theta*x)
Sig <- diag(rexp(n))
theta <- c(2,5)
X <- cbind(`(Intercept)` = 1, matrix(rexp(n, rate = 1)))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
set.seed(20240303)
dfr_1a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                      theta_init = c(1,1), verbose = FALSE)

set.seed(20240303)
dfr_2a <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                      verbose = FALSE, J = dfr_1a[["J"]],
                      fitted_values = dfr_1a[["fitted_values"]])

data <- data.frame(a = X[,1], b = X[,2], c = Y)
set.seed(20240303)
dfr_3a <- distfreereg(test_mean = c ~ a + b - 1, data = data, covariance = list(Sigma = Sig),
                      verbose = FALSE,
                      override = list(res_order = dfr_1a[["res_order"]]))



dfr_1b <- update(dfr_1a, control = list(symmetric = FALSE))
dfr_2b <- update(dfr_2a, control = list(symmetric = FALSE))
dfr_3b <- update(dfr_3a, control = list(symmetric = FALSE))

message('identical(dfr_1a[["observed_stats"]], dfr_1b[["observed_stats"]]) (should be TRUE): ', identical(dfr_1a[["observed_stats"]], dfr_1b[["observed_stats"]]))
message('identical(dfr_1a[["observed_stats"]], dfr_2a[["observed_stats"]]) (should be TRUE): ', identical(dfr_1a[["observed_stats"]], dfr_2a[["observed_stats"]]))
message('identical(dfr_1a[["observed_stats"]], dfr_2b[["observed_stats"]]) (should be TRUE): ', identical(dfr_1a[["observed_stats"]], dfr_2b[["observed_stats"]]))
all.equal(dfr_1a[["observed_stats"]], dfr_3b[["observed_stats"]], tolerance = 1e-5)# nearly equal
message('identical(dfr_1a[["mcsim_stats"]], dfr_1b[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1a[["mcsim_stats"]], dfr_1b[["mcsim_stats"]]))
message('identical(dfr_1a[["mcsim_stats"]], dfr_2a[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1a[["mcsim_stats"]], dfr_2a[["mcsim_stats"]]))
message('identical(dfr_1a[["mcsim_stats"]], dfr_2b[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1a[["mcsim_stats"]], dfr_2b[["mcsim_stats"]]))
message('identical(dfr_1a[["mcsim_stats"]], dfr_3b[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1a[["mcsim_stats"]], dfr_3b[["mcsim_stats"]]))

set.seed(20240303)
dfr_1c <- update(dfr_1a, override = list(res_order = 1:n))
set.seed(20240303)
dfr_2c <- update(dfr_2a, override = list(res_order = 1:n))
set.seed(20240303)
dfr_3c <- update(dfr_3a, override = list(res_order = 1:n))

message('identical(dfr_1c[["observed_stats"]], dfr_2c[["observed_stats"]]) (should be TRUE): ', identical(dfr_1c[["observed_stats"]], dfr_2c[["observed_stats"]]))
all.equal(dfr_1c[["observed_stats"]], dfr_3c[["observed_stats"]], tolerance = 1e-5)# nearly equal
message('identical(dfr_1c[["mcsim_stats"]], dfr_2c[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1c[["mcsim_stats"]], dfr_2c[["mcsim_stats"]]))
message('identical(dfr_1c[["mcsim_stats"]], dfr_3c[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1c[["mcsim_stats"]], dfr_3c[["mcsim_stats"]]))

Y_new <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

set.seed(20240303)
dfr_1d <- update(dfr_1a, Y = Y_new)
set.seed(20240303)
dfr_2d <- update(dfr_2a, Y = Y_new, J = dfr_1d[["J"]],
                 fitted_values = dfr_1d[["fitted_values"]])
set.seed(20240303)
dfr_3d <- update(dfr_3a, data = data.frame(c = Y_new, a = X[,1], b = X[,2]))

message('identical(dfr_1d[["observed_stats"]], dfr_2d[["observed_stats"]]) (should be TRUE): ', identical(dfr_1d[["observed_stats"]], dfr_2d[["observed_stats"]]))
message('all.equal(dfr_1d[["theta_hat"]], dfr_3d[["theta_hat"]], check.attributes = FALSE, tolerance = 1e-5) (should be TRUE): ', all.equal(dfr_1d[["theta_hat"]], dfr_3d[["theta_hat"]], check.attributes = FALSE, tolerance = 1e-5))
message('identical(dfr_1d[["mcsim_stats"]], dfr_2d[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1d[["mcsim_stats"]], dfr_2d[["mcsim_stats"]]))
message('identical(dfr_1d[["mcsim_stats"]], dfr_3d[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1d[["mcsim_stats"]], dfr_3d[["mcsim_stats"]]))
