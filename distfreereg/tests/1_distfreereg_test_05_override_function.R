library(distfreereg)
set.seed(20240319)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
set.seed(20240319)
dfrfunc_1 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1), verbose = FALSE)
set.seed(20240319)
dfrfunc_2 <- distfreereg(Y = Y, X = X, test_mean = func, theta_init = c(1,1), verbose = FALSE,
                         covariance = list(Sigma = Sig), override = list(res_order = dfrfunc_1[["res_order"]]))
set.seed(20240319)
dfrfunc_3 <- distfreereg(Y = Y, X = X, test_mean = func, verbose = FALSE,
                         covariance = list(Sigma = Sig), override = list(theta_hat = dfrfunc_1[["theta_hat"]]))
set.seed(20240319)
dfrfunc_4 <- distfreereg(Y = Y, X = X, test_mean = func, theta_init = c(1,1), verbose = FALSE,
                         covariance = list(Sigma = Sig), override = list(r = dfrfunc_1[["r"]]))
set.seed(20240319)
dfrfunc_5 <- distfreereg(Y = Y, X = X, test_mean = func, theta_init = c(1,1), verbose = FALSE,
                         covariance = list(Sigma = Sig), override = list(mcsim_stats = dfrfunc_1[["mcsim_stats"]]))

message('identical(dfrfunc_1[["epsp"]], dfrfunc_2[["epsp"]]) (should be TRUE): ', identical(dfrfunc_1[["epsp"]], dfrfunc_2[["epsp"]]))
message('identical(dfrfunc_1[["epsp"]], dfrfunc_3[["epsp"]]) (should be TRUE): ', identical(dfrfunc_1[["epsp"]], dfrfunc_3[["epsp"]]))
message('identical(dfrfunc_1[["epsp"]], dfrfunc_4[["epsp"]]) (should be TRUE): ', identical(dfrfunc_1[["epsp"]], dfrfunc_4[["epsp"]]))
message('identical(dfrfunc_1[["epsp"]], dfrfunc_5[["epsp"]]) (should be TRUE): ', identical(dfrfunc_1[["epsp"]], dfrfunc_5[["epsp"]]))
message('identical(dfrfunc_1[["p"]], dfrfunc_5[["p"]]) (should be TRUE): ', identical(dfrfunc_1[["p"]], dfrfunc_5[["p"]]))





my_res_order <- sample(1:n)
my_theta_hat <- c(7,13)
my_r <- dfrfunc_1[["r"]][sample(1:n),]
my_mcsim_stats <- list(KS = dfrfunc_1[["mcsim_stats"]][["KS"]]^2,
                       CvM = dfrfunc_1[["mcsim_stats"]][["CvM"]]^2)
set.seed(20240319)
dfrfunc_5 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                         theta_init = c(1,1), verbose = FALSE,
                         override = list(res_order = my_res_order))
set.seed(20240319)
dfrfunc_6 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                         theta_init = c(1,1), verbose = FALSE,
                         override = list(theta_hat = my_theta_hat))
set.seed(20240319)
dfrfunc_7 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                         theta_init = c(1,1), verbose = FALSE,
                         override = list(r = my_r))
set.seed(20240319)
dfrfunc_8 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                         theta_init = c(1,1), verbose = FALSE,
                         override = list(mcsim_stats = my_mcsim_stats))

# Verify that specified overrides are correct.
message('identical(dfrfunc_1[["res_order"]], dfrfunc_5[["res_order"]]) (should be FALSE): ', identical(dfrfunc_1[["res_order"]], dfrfunc_5[["res_order"]]))
message('identical(dfrfunc_1[["theta_hat"]], dfrfunc_6[["theta_hat"]]) (should be FALSE): ', identical(dfrfunc_1[["theta_hat"]], dfrfunc_6[["theta_hat"]]))
message('identical(dfrfunc_1[["r"]], dfrfunc_7[["r"]]) (should be FALSE): ', identical(dfrfunc_1[["r"]], dfrfunc_7[["r"]]))
message('identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_8[["mcsim_stats"]]) (should be FALSE): ', identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_8[["mcsim_stats"]]))
message('identical(my_res_order, dfrfunc_5[["res_order"]]) (should be TRUE): ', identical(my_res_order, dfrfunc_5[["res_order"]]))
message('identical(my_theta_hat, dfrfunc_6[["theta_hat"]]) (should be TRUE): ', identical(my_theta_hat, dfrfunc_6[["theta_hat"]]))
message('identical(my_r, dfrfunc_7[["r"]]) (should be TRUE): ', identical(my_r, dfrfunc_7[["r"]]))
message('identical(my_mcsim_stats, dfrfunc_8[["mcsim_stats"]]) (should be TRUE): ', identical(my_mcsim_stats, dfrfunc_8[["mcsim_stats"]]))

# Verify that other things changed or not, as appropriate.
message('identical(dfrfunc_1[["theta_hat"]], dfrfunc_5[["theta_hat"]]) (should be TRUE): ', identical(dfrfunc_1[["theta_hat"]], dfrfunc_5[["theta_hat"]]))
message('identical(dfrfunc_1[["optimization_output"]], dfrfunc_5[["optimization_output"]]) (should be TRUE): ', identical(dfrfunc_1[["optimization_output"]], dfrfunc_5[["optimization_output"]]))
message('identical(dfrfunc_1[["fitted_values"]], dfrfunc_5[["fitted_values"]]) (should be TRUE): ', identical(dfrfunc_1[["fitted_values"]], dfrfunc_5[["fitted_values"]]))
message('identical(dfrfunc_1[["J"]], dfrfunc_5[["J"]]) (should be TRUE): ', identical(dfrfunc_1[["J"]], dfrfunc_5[["J"]]))
message('identical(dfrfunc_1[["r"]], dfrfunc_5[["r"]]) (should be FALSE): ', identical(dfrfunc_1[["r"]], dfrfunc_5[["r"]]))
message('identical(dfrfunc_1[["r_tilde"]], dfrfunc_5[["r_tilde"]]) (should be FALSE): ', identical(dfrfunc_1[["r_tilde"]], dfrfunc_5[["r_tilde"]]))
message('identical(dfrfunc_1[["mu"]], dfrfunc_5[["mu"]]) (should be TRUE): ', identical(dfrfunc_1[["mu"]], dfrfunc_5[["mu"]]))
message('identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_5[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_5[["residuals"]][["raw"]]))
message('identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_5[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_5[["residuals"]][["sphered"]]))
message('identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_5[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_5[["residuals"]][["transformed"]]))
message('identical(dfrfunc_1[["epsp"]], dfrfunc_5[["epsp"]]) (should be FALSE): ', identical(dfrfunc_1[["epsp"]], dfrfunc_5[["epsp"]]))
message('identical(dfrfunc_1[["observed_stats"]], dfrfunc_5[["observed_stats"]]) (should be FALSE): ', identical(dfrfunc_1[["observed_stats"]], dfrfunc_5[["observed_stats"]]))
message('identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_5[["mcsim_stats"]]) (should be FALSE): ', identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_5[["mcsim_stats"]]))
message('identical(dfrfunc_1[["p"]], dfrfunc_5[["p"]]) (should be FALSE): ', identical(dfrfunc_1[["p"]], dfrfunc_5[["p"]]))

message('identical(dfrfunc_1[["theta_hat"]], dfrfunc_6[["theta_hat"]]) (should be FALSE): ', identical(dfrfunc_1[["theta_hat"]], dfrfunc_6[["theta_hat"]]))
message('identical(dfrfunc_1[["optimization_output"]], dfrfunc_6[["optimization_output"]]) (should be FALSE): ', identical(dfrfunc_1[["optimization_output"]], dfrfunc_6[["optimization_output"]]))
message('identical(dfrfunc_1[["fitted_values"]], dfrfunc_6[["fitted_values"]]) (should be FALSE): ', identical(dfrfunc_1[["fitted_values"]], dfrfunc_6[["fitted_values"]]))
message('identical(dfrfunc_1[["J"]], dfrfunc_6[["J"]]) (should be FALSE): ', identical(dfrfunc_1[["J"]], dfrfunc_6[["J"]]))
message('identical(dfrfunc_1[["r"]], dfrfunc_6[["r"]]) (should be TRUE): ', identical(dfrfunc_1[["r"]], dfrfunc_6[["r"]]))
message('identical(dfrfunc_1[["r_tilde"]], dfrfunc_6[["r_tilde"]]) (should be FALSE): ', identical(dfrfunc_1[["r_tilde"]], dfrfunc_6[["r_tilde"]]))
message('identical(dfrfunc_1[["mu"]], dfrfunc_6[["mu"]]) (should be FALSE): ', identical(dfrfunc_1[["mu"]], dfrfunc_6[["mu"]]))
message('identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_6[["residuals"]][["raw"]]) (should be FALSE): ', identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_6[["residuals"]][["raw"]]))
message('identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_6[["residuals"]][["sphered"]]) (should be FALSE): ', identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_6[["residuals"]][["sphered"]]))
message('identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_6[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_6[["residuals"]][["transformed"]]))
message('identical(dfrfunc_1[["epsp"]], dfrfunc_6[["epsp"]]) (should be FALSE): ', identical(dfrfunc_1[["epsp"]], dfrfunc_6[["epsp"]]))
message('identical(dfrfunc_1[["observed_stats"]], dfrfunc_6[["observed_stats"]]) (should be FALSE): ', identical(dfrfunc_1[["observed_stats"]], dfrfunc_6[["observed_stats"]]))
message('identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_6[["mcsim_stats"]]) (should be TRUE): ', identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_6[["mcsim_stats"]]))
message('identical(dfrfunc_1[["p"]], dfrfunc_6[["p"]]) (should be FALSE): ', identical(dfrfunc_1[["p"]], dfrfunc_6[["p"]]))

message('identical(dfrfunc_1[["theta_hat"]], dfrfunc_7[["theta_hat"]]) (should be TRUE): ', identical(dfrfunc_1[["theta_hat"]], dfrfunc_7[["theta_hat"]]))
message('identical(dfrfunc_1[["optimization_output"]], dfrfunc_7[["optimization_output"]]) (should be TRUE): ', identical(dfrfunc_1[["optimization_output"]], dfrfunc_7[["optimization_output"]]))
message('identical(dfrfunc_1[["fitted_values"]], dfrfunc_7[["fitted_values"]]) (should be TRUE): ', identical(dfrfunc_1[["fitted_values"]], dfrfunc_7[["fitted_values"]]))
message('identical(dfrfunc_1[["J"]], dfrfunc_7[["J"]]) (should be TRUE): ', identical(dfrfunc_1[["J"]], dfrfunc_7[["J"]]))
message('identical(dfrfunc_1[["r"]], dfrfunc_7[["r"]]) (should be FALSE): ', identical(dfrfunc_1[["r"]], dfrfunc_7[["r"]]))
message('identical(dfrfunc_1[["r_tilde"]], dfrfunc_7[["r_tilde"]]) (should be FALSE): ', identical(dfrfunc_1[["r_tilde"]], dfrfunc_7[["r_tilde"]]))
message('identical(dfrfunc_1[["mu"]], dfrfunc_7[["mu"]]) (should be TRUE): ', identical(dfrfunc_1[["mu"]], dfrfunc_7[["mu"]]))
message('identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_7[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_7[["residuals"]][["raw"]]))
message('identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_7[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_7[["residuals"]][["sphered"]]))
message('identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_7[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_7[["residuals"]][["transformed"]]))
message('identical(dfrfunc_1[["epsp"]], dfrfunc_7[["epsp"]]) (should be FALSE): ', identical(dfrfunc_1[["epsp"]], dfrfunc_7[["epsp"]]))
message('identical(dfrfunc_1[["observed_stats"]], dfrfunc_7[["observed_stats"]]) (should be FALSE): ', identical(dfrfunc_1[["observed_stats"]], dfrfunc_7[["observed_stats"]]))
message('identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_7[["mcsim_stats"]]) (should be FALSE): ', identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_7[["mcsim_stats"]]))
message('identical(dfrfunc_1[["p"]], dfrfunc_7[["p"]]) (should be FALSE): ', identical(dfrfunc_1[["p"]], dfrfunc_7[["p"]]))

message('identical(dfrfunc_1[["theta_hat"]], dfrfunc_8[["theta_hat"]]) (should be TRUE): ', identical(dfrfunc_1[["theta_hat"]], dfrfunc_8[["theta_hat"]]))
message('identical(dfrfunc_1[["optimization_output"]], dfrfunc_8[["optimization_output"]]) (should be TRUE): ', identical(dfrfunc_1[["optimization_output"]], dfrfunc_8[["optimization_output"]]))
message('identical(dfrfunc_1[["fitted_values"]], dfrfunc_8[["fitted_values"]]) (should be TRUE): ', identical(dfrfunc_1[["fitted_values"]], dfrfunc_8[["fitted_values"]]))
message('identical(dfrfunc_1[["J"]], dfrfunc_8[["J"]]) (should be TRUE): ', identical(dfrfunc_1[["J"]], dfrfunc_8[["J"]]))
message('identical(dfrfunc_1[["r"]], dfrfunc_8[["r"]]) (should be TRUE): ', identical(dfrfunc_1[["r"]], dfrfunc_8[["r"]]))
message('identical(dfrfunc_1[["r_tilde"]], dfrfunc_8[["r_tilde"]]) (should be TRUE): ', identical(dfrfunc_1[["r_tilde"]], dfrfunc_8[["r_tilde"]]))
message('identical(dfrfunc_1[["mu"]], dfrfunc_8[["mu"]]) (should be TRUE): ', identical(dfrfunc_1[["mu"]], dfrfunc_8[["mu"]]))
message('identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_8[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_8[["residuals"]][["raw"]]))
message('identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_8[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_8[["residuals"]][["sphered"]]))
message('identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_8[["residuals"]][["transformed"]]) (should be TRUE): ', identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_8[["residuals"]][["transformed"]]))
message('identical(dfrfunc_1[["epsp"]], dfrfunc_8[["epsp"]]) (should be TRUE): ', identical(dfrfunc_1[["epsp"]], dfrfunc_8[["epsp"]]))
message('identical(dfrfunc_1[["observed_stats"]], dfrfunc_8[["observed_stats"]]) (should be TRUE): ', identical(dfrfunc_1[["observed_stats"]], dfrfunc_8[["observed_stats"]]))
message('identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_8[["mcsim_stats"]]) (should be FALSE): ', identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_8[["mcsim_stats"]]))
message('identical(dfrfunc_1[["p"]], dfrfunc_8[["p"]]) (should be FALSE): ', identical(dfrfunc_1[["p"]], dfrfunc_8[["p"]]))
