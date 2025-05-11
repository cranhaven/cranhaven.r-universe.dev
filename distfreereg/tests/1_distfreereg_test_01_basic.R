library(distfreereg)
set.seed(20240303)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- diag(rexp(n))
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
set.seed(20240303)
dfr_1 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1), verbose = FALSE)
dfr_2 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(P = solve(Sig)), theta_init = c(1,1), verbose = FALSE)
dfr_1
dfr_2

message('all.equal(dfr_1[["epsp"]], dfr_2[["epsp"]], tolerance = 1e-5) (should be TRUE): ', all.equal(dfr_1[["epsp"]], dfr_2[["epsp"]], tolerance = 1e-5))

# Verify that r and mu are orthogonal.
message('all.equal(crossprod(dfr_1[["r"]]), diag(length(dfr_1[["theta_hat"]])), tolerance = 1e-5) (should be TRUE): ', all.equal(crossprod(dfr_1[["r"]]), diag(length(dfr_1[["theta_hat"]])), tolerance = 1e-5))
message('all.equal(crossprod(dfr_1[["mu"]]), diag(length(dfr_1[["theta_hat"]])), tolerance = 1e-5) (should be TRUE): ', all.equal(crossprod(dfr_1[["mu"]]), diag(length(dfr_1[["theta_hat"]])), tolerance = 1e-5))

signif(fitted(dfr_1), digits = 5)

message('all.equal(residuals(dfr_1), residuals(dfr_1, type = "raw"), tolerance = 1e-5) (should be TRUE): ', all.equal(residuals(dfr_1), residuals(dfr_1, type = "raw"), tolerance = 1e-5))
signif(residuals(dfr_1, type = "sphered"), digits = 5)
signif(residuals(dfr_1, type = "transformed"), digits = 5)



# Compare results of different methods; should all be equivalent.

set.seed(20240303)
dfr_3 <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                     verbose = FALSE, J = dfr_1[["J"]],
                     fitted_values = dfr_1[["fitted_values"]])


data <- data.frame(a = X, b = Y)

set.seed(20240303)
dfr_4 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                     verbose = FALSE, override = list(res_order = dfr_1[["res_order"]]))

m <- lm(b ~ a, data = data, weights = 1/diag(Sig))

set.seed(20240303)
dfr_5 <- distfreereg(test_mean = m, verbose = FALSE,
                     override = list(res_order = dfr_1[["res_order"]]))

set.seed(20240303)
form <- b ~ d + e*a
dfr_6 <- distfreereg(test_mean = form, data = data, covariance = list(Sigma = Sig),
                     method = "nls", verbose = FALSE,
                     override = list(res_order = dfr_1[["res_order"]]))

message('identical(dfr_1[["epsp"]], dfr_3[["epsp"]]) (should be TRUE): ', identical(dfr_1[["epsp"]], dfr_3[["epsp"]]))
message('all.equal(dfr_1[["theta_hat"]], dfr_4[["theta_hat"]], check.attributes = FALSE, tolerance = 1e-5) (should be TRUE): ', all.equal(dfr_1[["theta_hat"]], dfr_4[["theta_hat"]], check.attributes = FALSE, tolerance = 1e-5))
message('all.equal(dfr_1[["epsp"]], dfr_4[["epsp"]], check.attributes = FALSE, tolerance = 1e-5) (should be TRUE): ', all.equal(dfr_1[["epsp"]], dfr_4[["epsp"]], check.attributes = FALSE, tolerance = 1e-5))
message('all.equal(dfr_1[["epsp"]], dfr_5[["epsp"]], check.attributes = FALSE, tolerance = 1e-5) (should be TRUE): ', all.equal(dfr_1[["epsp"]], dfr_5[["epsp"]], check.attributes = FALSE, tolerance = 1e-5))
message('all.equal(dfr_1[["epsp"]], dfr_6[["epsp"]], tolerance = 1e-5) (should be TRUE): ', all.equal(dfr_1[["epsp"]], dfr_6[["epsp"]], tolerance = 1e-5))
message('identical(dfr_1[["observed_stats"]], dfr_3[["observed_stats"]]) (should be TRUE): ', identical(dfr_1[["observed_stats"]], dfr_3[["observed_stats"]]))
all.equal(dfr_1[["observed_stats"]], dfr_4[["observed_stats"]],
          check.attributes = FALSE, tolerance = 1e-5)# TRUE, or small relative difference
all.equal(dfr_1[["observed_stats"]], dfr_5[["observed_stats"]],
          check.attributes = FALSE, tolerance = 1e-5)# TRUE, or small relative difference
message('all.equal(dfr_1[["observed_stats"]], dfr_6[["observed_stats"]], tolerance = 1e-5) (should be TRUE): ', all.equal(dfr_1[["observed_stats"]], dfr_6[["observed_stats"]], tolerance = 1e-5))
message('identical(dfr_1[["mcsim_stats"]], dfr_3[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1[["mcsim_stats"]], dfr_3[["mcsim_stats"]]))
message('identical(dfr_1[["mcsim_stats"]], dfr_4[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1[["mcsim_stats"]], dfr_4[["mcsim_stats"]]))
message('identical(dfr_1[["mcsim_stats"]], dfr_5[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1[["mcsim_stats"]], dfr_5[["mcsim_stats"]]))
message('identical(dfr_1[["mcsim_stats"]], dfr_6[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1[["mcsim_stats"]], dfr_6[["mcsim_stats"]]))
message('identical(dfr_1[["p"]], dfr_3[["p"]]) (should be TRUE): ', identical(dfr_1[["p"]], dfr_3[["p"]]))
message('identical(dfr_1[["p"]], dfr_4[["p"]]) (should be TRUE): ', identical(dfr_1[["p"]], dfr_4[["p"]]))
message('identical(dfr_1[["p"]], dfr_5[["p"]]) (should be TRUE): ', identical(dfr_1[["p"]], dfr_5[["p"]]))
message('identical(dfr_1[["p"]], dfr_6[["p"]]) (should be TRUE): ', identical(dfr_1[["p"]], dfr_6[["p"]]))


m_nls <- nls(form, data = data, weights = 1/diag(Sig))
set.seed(20240303)
dfr_7 <- distfreereg(test_mean = m_nls,
                     override = list(res_order = dfr_1[["res_order"]]))
message('identical(dfr_6[["epsp"]], dfr_7[["epsp"]]) (should be TRUE): ', identical(dfr_6[["epsp"]], dfr_7[["epsp"]]))
message('identical(dfr_6[["observed_stats"]], dfr_7[["observed_stats"]]) (should be TRUE): ', identical(dfr_6[["observed_stats"]], dfr_7[["observed_stats"]]))
message('identical(dfr_6[["mcsim_stats"]], dfr_7[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_6[["mcsim_stats"]], dfr_7[["mcsim_stats"]]))
message('identical(dfr_6[["p"]], dfr_7[["p"]]) (should be TRUE): ', identical(dfr_6[["p"]], dfr_7[["p"]]))
