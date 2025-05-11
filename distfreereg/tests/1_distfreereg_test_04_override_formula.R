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


# First, method = "lm".

set.seed(20240319)
dfrform_lm_1 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE)
set.seed(20240319)
dfrform_lm_2 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(res_order = dfrform_lm_1[["res_order"]]))
set.seed(20240319)
dfrform_lm_4 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(r = dfrform_lm_1[["r"]]))
set.seed(20240319)
dfrform_lm_5 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(mcsim_stats = dfrform_lm_1[["mcsim_stats"]]))


message('identical(dfrform_lm_1[["epsp"]], dfrform_lm_2[["epsp"]]) (should be TRUE): ', identical(dfrform_lm_1[["epsp"]], dfrform_lm_2[["epsp"]]))
message('identical(dfrform_lm_1[["epsp"]], dfrform_lm_4[["epsp"]]) (should be TRUE): ', identical(dfrform_lm_1[["epsp"]], dfrform_lm_4[["epsp"]]))
message('identical(dfrform_lm_1[["epsp"]], dfrform_lm_5[["epsp"]]) (should be TRUE): ', identical(dfrform_lm_1[["epsp"]], dfrform_lm_5[["epsp"]]))
message('identical(dfrform_lm_1[["p"]], dfrform_lm_5[["p"]]) (should be TRUE): ', identical(dfrform_lm_1[["p"]], dfrform_lm_5[["p"]]))






my_res_order <- sample(1:n)
my_theta_hat <- c(7,13)
my_r <- dfrform_lm_1[["r"]][sample(1:n),]
my_mcsim_stats <- list(KS = dfrform_lm_1[["mcsim_stats"]][["KS"]]^2,
                       CvM = dfrform_lm_1[["mcsim_stats"]][["CvM"]]^2)
set.seed(20240319)
dfrform_lm_5 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(res_order = my_res_order))
set.seed(20240319)
dfrform_lm_7 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(r = my_r))
set.seed(20240319)
dfrform_lm_8 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(mcsim_stats = my_mcsim_stats))

# Verify that specified overrides are correct.
message('identical(dfrform_lm_1[["res_order"]], dfrform_lm_5[["res_order"]]) (should be FALSE): ', identical(dfrform_lm_1[["res_order"]], dfrform_lm_5[["res_order"]]))
message('identical(dfrform_lm_1[["r"]], dfrform_lm_7[["r"]]) (should be FALSE): ', identical(dfrform_lm_1[["r"]], dfrform_lm_7[["r"]]))
message('identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_8[["mcsim_stats"]]) (should be FALSE): ', identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_8[["mcsim_stats"]]))
message('identical(my_res_order, dfrform_lm_5[["res_order"]]) (should be TRUE): ', identical(my_res_order, dfrform_lm_5[["res_order"]]))
message('identical(my_r, dfrform_lm_7[["r"]]) (should be TRUE): ', identical(my_r, dfrform_lm_7[["r"]]))
message('identical(my_mcsim_stats, dfrform_lm_8[["mcsim_stats"]]) (should be TRUE): ', identical(my_mcsim_stats, dfrform_lm_8[["mcsim_stats"]]))

# Verify that other things changed or not, as appropriate.
message('identical(dfrform_lm_1[["theta_hat"]], dfrform_lm_5[["theta_hat"]]) (should be TRUE): ', identical(dfrform_lm_1[["theta_hat"]], dfrform_lm_5[["theta_hat"]]))
message('identical(dfrform_lm_1[["optimization_output"]], dfrform_lm_5[["optimization_output"]]) (should be TRUE): ', identical(dfrform_lm_1[["optimization_output"]], dfrform_lm_5[["optimization_output"]]))
message('identical(dfrform_lm_1[["fitted_values"]], dfrform_lm_5[["fitted_values"]]) (should be TRUE): ', identical(dfrform_lm_1[["fitted_values"]], dfrform_lm_5[["fitted_values"]]))
message('identical(dfrform_lm_1[["J"]], dfrform_lm_5[["J"]]) (should be TRUE): ', identical(dfrform_lm_1[["J"]], dfrform_lm_5[["J"]]))
message('identical(dfrform_lm_1[["r"]], dfrform_lm_5[["r"]]) (should be FALSE): ', identical(dfrform_lm_1[["r"]], dfrform_lm_5[["r"]]))
message('identical(dfrform_lm_1[["r_tilde"]], dfrform_lm_5[["r_tilde"]]) (should be FALSE): ', identical(dfrform_lm_1[["r_tilde"]], dfrform_lm_5[["r_tilde"]]))
message('identical(dfrform_lm_1[["mu"]], dfrform_lm_5[["mu"]]) (should be TRUE): ', identical(dfrform_lm_1[["mu"]], dfrform_lm_5[["mu"]]))
message('identical(dfrform_lm_1[["residuals"]][["raw"]], dfrform_lm_5[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrform_lm_1[["residuals"]][["raw"]], dfrform_lm_5[["residuals"]][["raw"]]))
message('identical(dfrform_lm_1[["residuals"]][["sphered"]], dfrform_lm_5[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrform_lm_1[["residuals"]][["sphered"]], dfrform_lm_5[["residuals"]][["sphered"]]))
message('identical(dfrform_lm_1[["residuals"]][["transformed"]], dfrform_lm_5[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrform_lm_1[["residuals"]][["transformed"]], dfrform_lm_5[["residuals"]][["transformed"]]))
message('identical(dfrform_lm_1[["epsp"]], dfrform_lm_5[["epsp"]]) (should be FALSE): ', identical(dfrform_lm_1[["epsp"]], dfrform_lm_5[["epsp"]]))
message('identical(dfrform_lm_1[["observed_stats"]], dfrform_lm_5[["observed_stats"]]) (should be FALSE): ', identical(dfrform_lm_1[["observed_stats"]], dfrform_lm_5[["observed_stats"]]))
message('identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_5[["mcsim_stats"]]) (should be FALSE): ', identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_5[["mcsim_stats"]]))
message('identical(dfrform_lm_1[["p"]], dfrform_lm_5[["p"]]) (should be FALSE): ', identical(dfrform_lm_1[["p"]], dfrform_lm_5[["p"]]))

message('identical(dfrform_lm_1[["theta_hat"]], dfrform_lm_7[["theta_hat"]]) (should be TRUE): ', identical(dfrform_lm_1[["theta_hat"]], dfrform_lm_7[["theta_hat"]]))
message('identical(dfrform_lm_1[["optimization_output"]], dfrform_lm_7[["optimization_output"]]) (should be TRUE): ', identical(dfrform_lm_1[["optimization_output"]], dfrform_lm_7[["optimization_output"]]))
message('identical(dfrform_lm_1[["fitted_values"]], dfrform_lm_7[["fitted_values"]]) (should be TRUE): ', identical(dfrform_lm_1[["fitted_values"]], dfrform_lm_7[["fitted_values"]]))
message('identical(dfrform_lm_1[["J"]], dfrform_lm_7[["J"]]) (should be TRUE): ', identical(dfrform_lm_1[["J"]], dfrform_lm_7[["J"]]))
message('identical(dfrform_lm_1[["r"]], dfrform_lm_7[["r"]]) (should be FALSE): ', identical(dfrform_lm_1[["r"]], dfrform_lm_7[["r"]]))
message('identical(dfrform_lm_1[["r_tilde"]], dfrform_lm_7[["r_tilde"]]) (should be FALSE): ', identical(dfrform_lm_1[["r_tilde"]], dfrform_lm_7[["r_tilde"]]))
message('identical(dfrform_lm_1[["mu"]], dfrform_lm_7[["mu"]]) (should be TRUE): ', identical(dfrform_lm_1[["mu"]], dfrform_lm_7[["mu"]]))
message('identical(dfrform_lm_1[["residuals"]][["raw"]], dfrform_lm_7[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrform_lm_1[["residuals"]][["raw"]], dfrform_lm_7[["residuals"]][["raw"]]))
message('identical(dfrform_lm_1[["residuals"]][["sphered"]], dfrform_lm_7[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrform_lm_1[["residuals"]][["sphered"]], dfrform_lm_7[["residuals"]][["sphered"]]))
message('identical(dfrform_lm_1[["residuals"]][["transformed"]], dfrform_lm_7[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrform_lm_1[["residuals"]][["transformed"]], dfrform_lm_7[["residuals"]][["transformed"]]))
message('identical(dfrform_lm_1[["epsp"]], dfrform_lm_7[["epsp"]]) (should be FALSE): ', identical(dfrform_lm_1[["epsp"]], dfrform_lm_7[["epsp"]]))
message('identical(dfrform_lm_1[["observed_stats"]], dfrform_lm_7[["observed_stats"]]) (should be FALSE): ', identical(dfrform_lm_1[["observed_stats"]], dfrform_lm_7[["observed_stats"]]))
message('identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_7[["mcsim_stats"]]) (should be FALSE): ', identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_7[["mcsim_stats"]]))
message('identical(dfrform_lm_1[["p"]], dfrform_lm_7[["p"]]) (should be FALSE): ', identical(dfrform_lm_1[["p"]], dfrform_lm_7[["p"]]))

message('identical(dfrform_lm_1[["theta_hat"]], dfrform_lm_8[["theta_hat"]]) (should be TRUE): ', identical(dfrform_lm_1[["theta_hat"]], dfrform_lm_8[["theta_hat"]]))
message('identical(dfrform_lm_1[["optimization_output"]], dfrform_lm_8[["optimization_output"]]) (should be TRUE): ', identical(dfrform_lm_1[["optimization_output"]], dfrform_lm_8[["optimization_output"]]))
message('identical(dfrform_lm_1[["fitted_values"]], dfrform_lm_8[["fitted_values"]]) (should be TRUE): ', identical(dfrform_lm_1[["fitted_values"]], dfrform_lm_8[["fitted_values"]]))
message('identical(dfrform_lm_1[["J"]], dfrform_lm_8[["J"]]) (should be TRUE): ', identical(dfrform_lm_1[["J"]], dfrform_lm_8[["J"]]))
message('identical(dfrform_lm_1[["r"]], dfrform_lm_8[["r"]]) (should be TRUE): ', identical(dfrform_lm_1[["r"]], dfrform_lm_8[["r"]]))
message('identical(dfrform_lm_1[["r_tilde"]], dfrform_lm_8[["r_tilde"]]) (should be TRUE): ', identical(dfrform_lm_1[["r_tilde"]], dfrform_lm_8[["r_tilde"]]))
message('identical(dfrform_lm_1[["mu"]], dfrform_lm_8[["mu"]]) (should be TRUE): ', identical(dfrform_lm_1[["mu"]], dfrform_lm_8[["mu"]]))
message('identical(dfrform_lm_1[["residuals"]][["raw"]], dfrform_lm_8[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrform_lm_1[["residuals"]][["raw"]], dfrform_lm_8[["residuals"]][["raw"]]))
message('identical(dfrform_lm_1[["residuals"]][["sphered"]], dfrform_lm_8[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrform_lm_1[["residuals"]][["sphered"]], dfrform_lm_8[["residuals"]][["sphered"]]))
message('identical(dfrform_lm_1[["residuals"]][["transformed"]], dfrform_lm_8[["residuals"]][["transformed"]]) (should be TRUE): ', identical(dfrform_lm_1[["residuals"]][["transformed"]], dfrform_lm_8[["residuals"]][["transformed"]]))
message('identical(dfrform_lm_1[["epsp"]], dfrform_lm_8[["epsp"]]) (should be TRUE): ', identical(dfrform_lm_1[["epsp"]], dfrform_lm_8[["epsp"]]))
message('identical(dfrform_lm_1[["observed_stats"]], dfrform_lm_8[["observed_stats"]]) (should be TRUE): ', identical(dfrform_lm_1[["observed_stats"]], dfrform_lm_8[["observed_stats"]]))
message('identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_8[["mcsim_stats"]]) (should be FALSE): ', identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_8[["mcsim_stats"]]))
message('identical(dfrform_lm_1[["p"]], dfrform_lm_8[["p"]]) (should be FALSE): ', identical(dfrform_lm_1[["p"]], dfrform_lm_8[["p"]]))










# Second, method = "nls".

set.seed(20240319)
dfrform_nls_1 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls")
set.seed(20240319)
dfrform_nls_2 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(res_order = dfrform_nls_1[["res_order"]]))
set.seed(20240319)
tryCatch(dfrform_nls_3 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                                      verbose = FALSE, method = "nls",
                                      override = list(theta_hat = dfrform_nls_1[["theta_hat"]])),
         error = function(e) warning(e))
set.seed(20240319)
dfrform_nls_4 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(r = dfrform_nls_1[["r"]]))
set.seed(20240319)
dfrform_nls_5 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(mcsim_stats = dfrform_nls_1[["mcsim_stats"]]))


message('identical(dfrform_nls_1[["epsp"]], dfrform_nls_2[["epsp"]]) (should be TRUE): ', identical(dfrform_nls_1[["epsp"]], dfrform_nls_2[["epsp"]]))
message('identical(dfrform_nls_1[["epsp"]], dfrform_nls_4[["epsp"]]) (should be TRUE): ', identical(dfrform_nls_1[["epsp"]], dfrform_nls_4[["epsp"]]))
message('identical(dfrform_nls_1[["epsp"]], dfrform_nls_5[["epsp"]]) (should be TRUE): ', identical(dfrform_nls_1[["epsp"]], dfrform_nls_5[["epsp"]]))
message('identical(dfrform_nls_1[["p"]], dfrform_nls_5[["p"]]) (should be TRUE): ', identical(dfrform_nls_1[["p"]], dfrform_nls_5[["p"]]))






my_res_order <- sample(1:n)
my_theta_hat <- c(7,13)
my_r <- dfrform_nls_1[["r"]][sample(1:n),]
my_mcsim_stats <- list(KS = dfrform_nls_1[["mcsim_stats"]][["KS"]]^2,
                       CvM = dfrform_nls_1[["mcsim_stats"]][["CvM"]]^2)
set.seed(20240319)
dfrform_nls_5 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(res_order = my_res_order))
set.seed(20240319)
tryCatch(dfrform_nls_6 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                                      verbose = FALSE, method = "nls",
                                      override = list(theta_hat = my_theta_hat)),
         error = function(e) e)
set.seed(20240319)
dfrform_nls_7 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(r = my_r))
set.seed(20240319)
dfrform_nls_8 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(mcsim_stats = my_mcsim_stats))

# Verify that specified overrides are correct.
message('identical(dfrform_nls_1[["res_order"]], dfrform_nls_5[["res_order"]]) (should be FALSE): ', identical(dfrform_nls_1[["res_order"]], dfrform_nls_5[["res_order"]]))
# message('identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_6[["theta_hat"]]) (should be FALSE): ', identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_6[["theta_hat"]]))
message('identical(dfrform_nls_1[["r"]], dfrform_nls_7[["r"]]) (should be FALSE): ', identical(dfrform_nls_1[["r"]], dfrform_nls_7[["r"]]))
message('identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_8[["mcsim_stats"]]) (should be FALSE): ', identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_8[["mcsim_stats"]]))
message('identical(my_res_order, dfrform_nls_5[["res_order"]]) (should be TRUE): ', identical(my_res_order, dfrform_nls_5[["res_order"]]))
# message('identical(my_theta_hat, dfrform_nls_6[["theta_hat"]]) (should be TRUE): ', identical(my_theta_hat, dfrform_nls_6[["theta_hat"]]))
message('identical(my_r, dfrform_nls_7[["r"]]) (should be TRUE): ', identical(my_r, dfrform_nls_7[["r"]]))
message('identical(my_mcsim_stats, dfrform_nls_8[["mcsim_stats"]]) (should be TRUE): ', identical(my_mcsim_stats, dfrform_nls_8[["mcsim_stats"]]))

# Verify that other things changed or not, as appropriate.
message('identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_5[["theta_hat"]]) (should be TRUE): ', identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_5[["theta_hat"]]))
message('identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_5[["fitted_values"]]) (should be TRUE): ', identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_5[["fitted_values"]]))
message('identical(dfrform_nls_1[["J"]], dfrform_nls_5[["J"]]) (should be TRUE): ', identical(dfrform_nls_1[["J"]], dfrform_nls_5[["J"]]))
message('identical(dfrform_nls_1[["r"]], dfrform_nls_5[["r"]]) (should be FALSE): ', identical(dfrform_nls_1[["r"]], dfrform_nls_5[["r"]]))
message('identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_5[["r_tilde"]]) (should be FALSE): ', identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_5[["r_tilde"]]))
message('identical(dfrform_nls_1[["mu"]], dfrform_nls_5[["mu"]]) (should be TRUE): ', identical(dfrform_nls_1[["mu"]], dfrform_nls_5[["mu"]]))
message('identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_5[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_5[["residuals"]][["raw"]]))
message('identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_5[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_5[["residuals"]][["sphered"]]))
message('identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_5[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_5[["residuals"]][["transformed"]]))
message('identical(dfrform_nls_1[["epsp"]], dfrform_nls_5[["epsp"]]) (should be FALSE): ', identical(dfrform_nls_1[["epsp"]], dfrform_nls_5[["epsp"]]))
message('identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_5[["observed_stats"]]) (should be FALSE): ', identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_5[["observed_stats"]]))
message('identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_5[["mcsim_stats"]]) (should be FALSE): ', identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_5[["mcsim_stats"]]))
message('identical(dfrform_nls_1[["p"]], dfrform_nls_5[["p"]]) (should be FALSE): ', identical(dfrform_nls_1[["p"]], dfrform_nls_5[["p"]]))

# message('identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_6[["theta_hat"]]) (should be FALSE): ', identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_6[["theta_hat"]]))
# message('identical(dfrform_nls_1[["optimization_output"]], dfrform_nls_6[["optimization_output"]]) (should be FALSE): ', identical(dfrform_nls_1[["optimization_output"]], dfrform_nls_6[["optimization_output"]]))
# message('identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_6[["fitted_values"]]) (should be FALSE): ', identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_6[["fitted_values"]]))
# message('identical(dfrform_nls_1[["J"]], dfrform_nls_6[["J"]]) (should be FALSE): ', identical(dfrform_nls_1[["J"]], dfrform_nls_6[["J"]]))
# message('identical(dfrform_nls_1[["r"]], dfrform_nls_6[["r"]]) (should be TRUE): ', identical(dfrform_nls_1[["r"]], dfrform_nls_6[["r"]]))
# message('identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_6[["r_tilde"]]) (should be FALSE): ', identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_6[["r_tilde"]]))
# message('identical(dfrform_nls_1[["mu"]], dfrform_nls_6[["mu"]]) (should be FALSE): ', identical(dfrform_nls_1[["mu"]], dfrform_nls_6[["mu"]]))
# message('identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_6[["residuals"]][["raw"]]) (should be FALSE): ', identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_6[["residuals"]][["raw"]]))
# message('identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_6[["residuals"]][["sphered"]]) (should be FALSE): ', identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_6[["residuals"]][["sphered"]]))
# message('identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_6[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_6[["residuals"]][["transformed"]]))
# message('identical(dfrform_nls_1[["epsp"]], dfrform_nls_6[["epsp"]]) (should be FALSE): ', identical(dfrform_nls_1[["epsp"]], dfrform_nls_6[["epsp"]]))
# message('identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_6[["observed_stats"]]) (should be FALSE): ', identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_6[["observed_stats"]]))
# message('identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_6[["mcsim_stats"]]) (should be TRUE): ', identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_6[["mcsim_stats"]]))
# message('identical(dfrform_nls_1[["p"]], dfrform_nls_6[["p"]]) (should be FALSE): ', identical(dfrform_nls_1[["p"]], dfrform_nls_6[["p"]]))

message('identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_7[["theta_hat"]]) (should be TRUE): ', identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_7[["theta_hat"]]))
message('identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_7[["fitted_values"]]) (should be TRUE): ', identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_7[["fitted_values"]]))
message('identical(dfrform_nls_1[["J"]], dfrform_nls_7[["J"]]) (should be TRUE): ', identical(dfrform_nls_1[["J"]], dfrform_nls_7[["J"]]))
message('identical(dfrform_nls_1[["r"]], dfrform_nls_7[["r"]]) (should be FALSE): ', identical(dfrform_nls_1[["r"]], dfrform_nls_7[["r"]]))
message('identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_7[["r_tilde"]]) (should be FALSE): ', identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_7[["r_tilde"]]))
message('identical(dfrform_nls_1[["mu"]], dfrform_nls_7[["mu"]]) (should be TRUE): ', identical(dfrform_nls_1[["mu"]], dfrform_nls_7[["mu"]]))
message('identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_7[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_7[["residuals"]][["raw"]]))
message('identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_7[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_7[["residuals"]][["sphered"]]))
message('identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_7[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_7[["residuals"]][["transformed"]]))
message('identical(dfrform_nls_1[["epsp"]], dfrform_nls_7[["epsp"]]) (should be FALSE): ', identical(dfrform_nls_1[["epsp"]], dfrform_nls_7[["epsp"]]))
message('identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_7[["observed_stats"]]) (should be FALSE): ', identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_7[["observed_stats"]]))
message('identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_7[["mcsim_stats"]]) (should be FALSE): ', identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_7[["mcsim_stats"]]))
message('identical(dfrform_nls_1[["p"]], dfrform_nls_7[["p"]]) (should be FALSE): ', identical(dfrform_nls_1[["p"]], dfrform_nls_7[["p"]]))

message('identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_8[["theta_hat"]]) (should be TRUE): ', identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_8[["theta_hat"]]))
message('identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_8[["fitted_values"]]) (should be TRUE): ', identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_8[["fitted_values"]]))
message('identical(dfrform_nls_1[["J"]], dfrform_nls_8[["J"]]) (should be TRUE): ', identical(dfrform_nls_1[["J"]], dfrform_nls_8[["J"]]))
message('identical(dfrform_nls_1[["r"]], dfrform_nls_8[["r"]]) (should be TRUE): ', identical(dfrform_nls_1[["r"]], dfrform_nls_8[["r"]]))
message('identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_8[["r_tilde"]]) (should be TRUE): ', identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_8[["r_tilde"]]))
message('identical(dfrform_nls_1[["mu"]], dfrform_nls_8[["mu"]]) (should be TRUE): ', identical(dfrform_nls_1[["mu"]], dfrform_nls_8[["mu"]]))
message('identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_8[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_8[["residuals"]][["raw"]]))
message('identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_8[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_8[["residuals"]][["sphered"]]))
message('identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_8[["residuals"]][["transformed"]]) (should be TRUE): ', identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_8[["residuals"]][["transformed"]]))
message('identical(dfrform_nls_1[["epsp"]], dfrform_nls_8[["epsp"]]) (should be TRUE): ', identical(dfrform_nls_1[["epsp"]], dfrform_nls_8[["epsp"]]))
message('identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_8[["observed_stats"]]) (should be TRUE): ', identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_8[["observed_stats"]]))
message('identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_8[["mcsim_stats"]]) (should be FALSE): ', identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_8[["mcsim_stats"]]))
message('identical(dfrform_nls_1[["p"]], dfrform_nls_8[["p"]]) (should be FALSE): ', identical(dfrform_nls_1[["p"]], dfrform_nls_8[["p"]]))
