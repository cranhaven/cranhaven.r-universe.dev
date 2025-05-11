# Generally, equalities should be true for objects with same base number, and
# not for those with different numbers.

library(distfreereg)
set.seed(20240201)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

# Basic tests.
dfr_01a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1), verbose = FALSE)
dfr_01b <- update(dfr_01a, theta_init = c(3,3))

message('identical(dfr_01a[["r"]], dfr_01b[["r"]]) (should be TRUE): ', identical(dfr_01a[["r"]], dfr_01b[["r"]]))
message('all.equal(dfr_01a[["epsp"]], dfr_01b[["epsp"]], tolerance = 1e-4) (should be TRUE): ', all.equal(dfr_01a[["epsp"]], dfr_01b[["epsp"]], tolerance = 1e-4))

# Tests for theta_hat
dfr_01f <- update(dfr_01a, override = list(theta_hat = dfr_01a[["theta_hat"]]))
message('identical(dfr_01a[["epsp"]], dfr_01f[["epsp"]]) (should be TRUE): ', identical(dfr_01a[["epsp"]], dfr_01f[["epsp"]]))


# Verify that changing the test_mean function does not change mcsim_stats unless
# the dimension of the parameter space changes, too.
dfr_02a <- update(dfr_01a, test_mean = function(x, theta) theta[1] + theta[2]*x^2)
dfr_02b <- update(dfr_01a, test_mean = function(x, theta) theta[1] + theta[2]*x + theta[3]*x^2,
                  theta_init = c(1,1,1))
message('identical(dfr_01a[["mcsim_stats"]], dfr_02a[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_01a[["mcsim_stats"]], dfr_02a[["mcsim_stats"]]))
message('identical(dfr_01a[["mcsim_stats"]], dfr_02b[["mcsim_stats"]]) (should be FALSE): ', identical(dfr_01a[["mcsim_stats"]], dfr_02b[["mcsim_stats"]]))


dfr_03a <- update(dfr_01a, theta_init = c(3,3))
message('identical(dfr_01a[["theta_hat"]], dfr_03a[["theta_hat"]]) (should be FALSE): ', identical(dfr_01a[["theta_hat"]], dfr_03a[["theta_hat"]]))
message('identical(dfr_01a[["epsp"]], dfr_03a[["epsp"]]) (should be FALSE): ', identical(dfr_01a[["epsp"]], dfr_03a[["epsp"]]))

dfr_01g <- update(dfr_01a, theta_init = c(3,3), override = list(theta_hat = dfr_01a[["theta_hat"]]))
message('identical(dfr_01a[["epsp"]], dfr_01g[["epsp"]]) (should be TRUE): ', identical(dfr_01a[["epsp"]], dfr_01g[["epsp"]]))

# Tests for res_order
dfr_04a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1),
                       verbose = FALSE, ordering = "optimal")
dfr_04b <- update(dfr_01a, ordering = "optimal")
message('identical(dfr_01a[["res_order"]], dfr_04a[["res_order"]]) (should be FALSE): ', identical(dfr_01a[["res_order"]], dfr_04a[["res_order"]]))
message('identical(dfr_04a[["res_order"]], dfr_04b[["res_order"]]) (should be TRUE): ', identical(dfr_04a[["res_order"]], dfr_04b[["res_order"]]))
message('identical(dfr_04a[["r"]], dfr_04b[["r"]]) (should be TRUE): ', identical(dfr_04a[["r"]], dfr_04b[["r"]]))

dfr_04c <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1),
                       verbose = FALSE, override = list(res_order = dfr_04a[["res_order"]]))
message('identical(dfr_04a[["r"]], dfr_04c[["r"]]) (should be TRUE): ', identical(dfr_04a[["r"]], dfr_04c[["r"]]))

dfr_05a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1),
                       verbose = FALSE, ordering = "asis")
dfr_05b <- update(dfr_04c, ordering = "asis")# presence of "asis" should clear override from dfr_04c
message('identical(dfr_05a[["r"]], dfr_05b[["r"]]) (should be TRUE): ', identical(dfr_05a[["r"]], dfr_05b[["r"]]))

# Tests for clearing only one entry from override
dfr_06a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1),
                       verbose = FALSE, override = list(theta_hat = c(2,5),
                                                        res_order = dfr_04a[["res_order"]]))
dfr_06b <- update(dfr_06a, ordering = "asis")
message('identical(dfr_06a[["theta_hat"]], dfr_06b[["theta_hat"]]) (should be TRUE): ', identical(dfr_06a[["theta_hat"]], dfr_06b[["theta_hat"]]))
message('identical(dfr_06a[["res_order"]], dfr_04a[["res_order"]]) (should be TRUE): ', identical(dfr_06a[["res_order"]], dfr_04a[["res_order"]]))
message('identical(dfr_06a[["res_order"]], dfr_06b[["res_order"]]) (should be FALSE): ', identical(dfr_06a[["res_order"]], dfr_06b[["res_order"]]))


# Tests for r and mcsim_stats
dfr_07a <- distfreereg(Y = Y, covariance = list(Sigma = 1), test_mean = function(theta) theta,
                       theta_init = 1, verbose = FALSE)
dfr_07b <- update(dfr_07a, Y = rnorm(n))
dfr_07c <- update(dfr_07a, Y = rnorm(n+1))

message('identical(dfr_07a[["r"]], dfr_07b[["r"]]) (should be TRUE): ', identical(dfr_07a[["r"]], dfr_07b[["r"]]))
message('identical(dfr_07a[["mcsim_stats"]], dfr_07b[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_07a[["mcsim_stats"]], dfr_07b[["mcsim_stats"]]))
message('identical(dfr_07a[["r"]], dfr_07c[["r"]]) (should be FALSE): ', identical(dfr_07a[["r"]], dfr_07c[["r"]]))
message('identical(dfr_07a[["mcsim_stats"]], dfr_07c[["mcsim_stats"]]) (should be FALSE): ', identical(dfr_07a[["mcsim_stats"]], dfr_07c[["mcsim_stats"]]))

dfr_08a <- distfreereg(Y = Y, covariance = list(Sigma = 1), X = X, test_mean = func,
                       theta_init = c(1,1), verbose = FALSE)
dfr_08b <- update(dfr_08a, X = X + 1)
dfr_08c <- update(dfr_08a,
                  test_mean = function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[1]^2,
                  theta_init = c(1,1,1))
message('identical(dfr_08a[["r"]], dfr_08b[["r"]]) (should be TRUE): ', identical(dfr_08a[["r"]], dfr_08b[["r"]]))
message('identical(dfr_08a[["mcsim_stats"]], dfr_08b[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_08a[["mcsim_stats"]], dfr_08b[["mcsim_stats"]]))
message('identical(dfr_08a[["r"]], dfr_08c[["r"]]) (should be FALSE): ', identical(dfr_08a[["r"]], dfr_08c[["r"]]))
message('identical(dfr_08a[["mcsim_stats"]], dfr_08c[["mcsim_stats"]]) (should be FALSE): ', identical(dfr_08a[["mcsim_stats"]], dfr_08c[["mcsim_stats"]]))
