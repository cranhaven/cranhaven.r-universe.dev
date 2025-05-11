library(distfreereg)
set.seed(20240319)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- diag(rexp(n))
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
data <- data.frame(a = X, b = Y)
form <- b ~ a

# Basic tests.
dfr_01a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig), verbose = FALSE)
tryCatch(update(dfr_01a, theta_init = c(3,3)), error = function(e) warning(e))
tryCatch(update(dfr_01a, override = list(theta_hat = dfr_01a[["theta_hat"]])),
         error = function(e) warning(e))

# Tests for res_order
dfr_04a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
                       verbose = FALSE, ordering = "optimal")
dfr_04b <- update(dfr_01a, ordering = "optimal")
message('identical(dfr_01a[["res_order"]], dfr_04a[["res_order"]]) (should be FALSE): ', identical(dfr_01a[["res_order"]], dfr_04a[["res_order"]]))
message('identical(dfr_04a[["res_order"]], dfr_04b[["res_order"]]) (should be TRUE): ', identical(dfr_04a[["res_order"]], dfr_04b[["res_order"]]))
message('identical(dfr_04a[["r"]], dfr_04b[["r"]]) (should be TRUE): ', identical(dfr_04a[["r"]], dfr_04b[["r"]]))

dfr_04c <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
                       verbose = FALSE, override = list(res_order = dfr_04a[["res_order"]]))
message('identical(dfr_04a[["r"]], dfr_04c[["r"]]) (should be TRUE): ', identical(dfr_04a[["r"]], dfr_04c[["r"]]))

dfr_05a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
                       verbose = FALSE, ordering = "asis")
dfr_05b <- update(dfr_04c, ordering = "asis")# presence of "asis" should clear override from dfr_04c
message('identical(dfr_05a[["r"]], dfr_05b[["r"]]) (should be TRUE): ', identical(dfr_05a[["r"]], dfr_05b[["r"]]))

# Tests for clearing only one entry from override
dfr_06a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
                       verbose = FALSE, override = list(mcsim_stats = dfr_04a[["mcsim_stats"]],
                                                        res_order = dfr_04a[["res_order"]]))
dfr_06b <- update(dfr_06a, ordering = "asis")
message('identical(dfr_06a[["mcsim_stats"]], dfr_06b[["mcsim_stats"]]) (should be FALSE): ', identical(dfr_06a[["mcsim_stats"]], dfr_06b[["mcsim_stats"]]))
message('identical(dfr_06a[["res_order"]], dfr_04a[["res_order"]]) (should be TRUE): ', identical(dfr_06a[["res_order"]], dfr_04a[["res_order"]]))
message('identical(dfr_06a[["res_order"]], dfr_06b[["res_order"]]) (should be FALSE): ', identical(dfr_06a[["res_order"]], dfr_06b[["res_order"]]))
