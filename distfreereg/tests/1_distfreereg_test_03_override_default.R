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
dfrdef_0 <- distfreereg(Y = Y, X = X, test_mean = func,covariance = list(Sigma = Sig), theta_init = c(1,1), verbose = FALSE)
set.seed(20240319)
dfrdef_1 <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                        J = dfrdef_0[["J"]], fitted_values = dfrdef_0[["fitted_values"]],
                        verbose = FALSE)
set.seed(20240319)
dfrdef_2 <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                        J = dfrdef_0[["J"]], fitted_values = dfrdef_0[["fitted_values"]],
                        verbose = FALSE, override = list(res_order = dfrdef_1[["res_order"]]))
tryCatch(distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                        J = dfrdef_0[["J"]], fitted_values = dfrdef_0[["fitted_values"]],
                        verbose = FALSE, override = list(theta_hat = dfrdef_1[["theta_hat"]])),
         error = function(e) warning(e))
set.seed(20240319)
dfrdef_3 <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                        J = dfrdef_0[["J"]], fitted_values = dfrdef_0[["fitted_values"]],
                        verbose = FALSE, override = list(r = dfrdef_1[["r"]]))
set.seed(20240319)
dfrdef_4 <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                        J = dfrdef_0[["J"]], fitted_values = dfrdef_0[["fitted_values"]],
                        verbose = FALSE, override = list(mcsim_stats = dfrdef_1[["mcsim_stats"]]))

message('identical(dfrdef_1[["epsp"]], dfrdef_2[["epsp"]]) (should be TRUE): ', identical(dfrdef_1[["epsp"]], dfrdef_2[["epsp"]]))
message('identical(dfrdef_1[["epsp"]], dfrdef_3[["epsp"]]) (should be TRUE): ', identical(dfrdef_1[["epsp"]], dfrdef_3[["epsp"]]))
message('identical(dfrdef_1[["epsp"]], dfrdef_4[["epsp"]]) (should be TRUE): ', identical(dfrdef_1[["epsp"]], dfrdef_4[["epsp"]]))
message('identical(dfrdef_1[["p"]], dfrdef_4[["p"]]) (should be TRUE): ', identical(dfrdef_1[["p"]], dfrdef_4[["p"]]))



my_res_order <- sample(1:n)
my_r <- dfrdef_0[["r"]][sample(1:n),]
my_mcsim_stats <- list(KS = dfrdef_1[["mcsim_stats"]][["KS"]]^2,
                       CvM = dfrdef_1[["mcsim_stats"]][["CvM"]]^2)
set.seed(20240319)
dfrdef_5 <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                        J = dfrdef_0[["J"]], fitted_values = dfrdef_0[["fitted_values"]],
                        verbose = FALSE, override = list(res_order = my_res_order))
set.seed(20240319)
dfrdef_6 <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                        J = dfrdef_0[["J"]], fitted_values = dfrdef_0[["fitted_values"]],
                        verbose = FALSE, override = list(r = my_r))
set.seed(20240319)
dfrdef_7 <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                        J = dfrdef_0[["J"]], fitted_values = dfrdef_0[["fitted_values"]],
                        verbose = FALSE, override = list(mcsim_stats = my_mcsim_stats))

# Verify that specified overrides are correct.
message('identical(dfrdef_1[["res_order"]], dfrdef_5[["res_order"]]) (should be FALSE): ', identical(dfrdef_1[["res_order"]], dfrdef_5[["res_order"]]))
message('identical(dfrdef_1[["r"]], dfrdef_6[["r"]]) (should be FALSE): ', identical(dfrdef_1[["r"]], dfrdef_6[["r"]]))
message('identical(dfrdef_1[["mcsim_stats"]], dfrdef_7[["mcsim_stats"]]) (should be FALSE): ', identical(dfrdef_1[["mcsim_stats"]], dfrdef_7[["mcsim_stats"]]))
message('identical(my_res_order, dfrdef_5[["res_order"]]) (should be TRUE): ', identical(my_res_order, dfrdef_5[["res_order"]]))
message('identical(my_r, dfrdef_6[["r"]]) (should be TRUE): ', identical(my_r, dfrdef_6[["r"]]))
message('identical(my_mcsim_stats, dfrdef_7[["mcsim_stats"]]) (should be TRUE): ', identical(my_mcsim_stats, dfrdef_7[["mcsim_stats"]]))

# Verify that other things changed or not, as appropriate.
message('identical(dfrdef_1[["fitted_values"]], dfrdef_5[["fitted_values"]]) (should be TRUE): ', identical(dfrdef_1[["fitted_values"]], dfrdef_5[["fitted_values"]]))
message('identical(dfrdef_1[["J"]], dfrdef_5[["J"]]) (should be TRUE): ', identical(dfrdef_1[["J"]], dfrdef_5[["J"]]))
message('identical(dfrdef_1[["r"]], dfrdef_5[["r"]]) (should be FALSE): ', identical(dfrdef_1[["r"]], dfrdef_5[["r"]]))
message('identical(dfrdef_1[["r_tilde"]], dfrdef_5[["r_tilde"]]) (should be FALSE): ', identical(dfrdef_1[["r_tilde"]], dfrdef_5[["r_tilde"]]))
message('identical(dfrdef_1[["mu"]], dfrdef_5[["mu"]]) (should be TRUE): ', identical(dfrdef_1[["mu"]], dfrdef_5[["mu"]]))
message('identical(dfrdef_1[["residuals"]][["raw"]], dfrdef_5[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrdef_1[["residuals"]][["raw"]], dfrdef_5[["residuals"]][["raw"]]))
message('identical(dfrdef_1[["residuals"]][["sphered"]], dfrdef_5[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrdef_1[["residuals"]][["sphered"]], dfrdef_5[["residuals"]][["sphered"]]))
message('identical(dfrdef_1[["residuals"]][["transformed"]], dfrdef_5[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrdef_1[["residuals"]][["transformed"]], dfrdef_5[["residuals"]][["transformed"]]))
message('identical(dfrdef_1[["epsp"]], dfrdef_5[["epsp"]]) (should be FALSE): ', identical(dfrdef_1[["epsp"]], dfrdef_5[["epsp"]]))
message('identical(dfrdef_1[["observed_stats"]], dfrdef_5[["observed_stats"]]) (should be FALSE): ', identical(dfrdef_1[["observed_stats"]], dfrdef_5[["observed_stats"]]))
message('identical(dfrdef_1[["mcsim_stats"]], dfrdef_5[["mcsim_stats"]]) (should be FALSE): ', identical(dfrdef_1[["mcsim_stats"]], dfrdef_5[["mcsim_stats"]]))
message('identical(dfrdef_1[["p"]], dfrdef_5[["p"]]) (should be FALSE): ', identical(dfrdef_1[["p"]], dfrdef_5[["p"]]))

message('identical(dfrdef_1[["fitted_values"]], dfrdef_6[["fitted_values"]]) (should be TRUE): ', identical(dfrdef_1[["fitted_values"]], dfrdef_6[["fitted_values"]]))
message('identical(dfrdef_1[["J"]], dfrdef_6[["J"]]) (should be TRUE): ', identical(dfrdef_1[["J"]], dfrdef_6[["J"]]))
message('identical(dfrdef_1[["r"]], dfrdef_6[["r"]]) (should be FALSE): ', identical(dfrdef_1[["r"]], dfrdef_6[["r"]]))
message('identical(dfrdef_1[["r_tilde"]], dfrdef_6[["r_tilde"]]) (should be FALSE): ', identical(dfrdef_1[["r_tilde"]], dfrdef_6[["r_tilde"]]))
message('identical(dfrdef_1[["mu"]], dfrdef_6[["mu"]]) (should be TRUE): ', identical(dfrdef_1[["mu"]], dfrdef_6[["mu"]]))
message('identical(dfrdef_1[["residuals"]][["raw"]], dfrdef_6[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrdef_1[["residuals"]][["raw"]], dfrdef_6[["residuals"]][["raw"]]))
message('identical(dfrdef_1[["residuals"]][["sphered"]], dfrdef_6[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrdef_1[["residuals"]][["sphered"]], dfrdef_6[["residuals"]][["sphered"]]))
message('identical(dfrdef_1[["residuals"]][["transformed"]], dfrdef_6[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrdef_1[["residuals"]][["transformed"]], dfrdef_6[["residuals"]][["transformed"]]))
message('identical(dfrdef_1[["epsp"]], dfrdef_6[["epsp"]]) (should be FALSE): ', identical(dfrdef_1[["epsp"]], dfrdef_6[["epsp"]]))
message('identical(dfrdef_1[["observed_stats"]], dfrdef_6[["observed_stats"]]) (should be FALSE): ', identical(dfrdef_1[["observed_stats"]], dfrdef_6[["observed_stats"]]))
message('identical(dfrdef_1[["mcsim_stats"]], dfrdef_6[["mcsim_stats"]]) (should be FALSE): ', identical(dfrdef_1[["mcsim_stats"]], dfrdef_6[["mcsim_stats"]]))
message('identical(dfrdef_1[["p"]], dfrdef_6[["p"]]) (should be FALSE): ', identical(dfrdef_1[["p"]], dfrdef_6[["p"]]))

message('identical(dfrdef_1[["fitted_values"]], dfrdef_7[["fitted_values"]]) (should be TRUE): ', identical(dfrdef_1[["fitted_values"]], dfrdef_7[["fitted_values"]]))
message('identical(dfrdef_1[["J"]], dfrdef_7[["J"]]) (should be TRUE): ', identical(dfrdef_1[["J"]], dfrdef_7[["J"]]))
message('identical(dfrdef_1[["r"]], dfrdef_7[["r"]]) (should be TRUE): ', identical(dfrdef_1[["r"]], dfrdef_7[["r"]]))
message('identical(dfrdef_1[["r_tilde"]], dfrdef_7[["r_tilde"]]) (should be TRUE): ', identical(dfrdef_1[["r_tilde"]], dfrdef_7[["r_tilde"]]))
message('identical(dfrdef_1[["mu"]], dfrdef_7[["mu"]]) (should be TRUE): ', identical(dfrdef_1[["mu"]], dfrdef_7[["mu"]]))
message('identical(dfrdef_1[["residuals"]][["raw"]], dfrdef_7[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrdef_1[["residuals"]][["raw"]], dfrdef_7[["residuals"]][["raw"]]))
message('identical(dfrdef_1[["residuals"]][["sphered"]], dfrdef_7[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrdef_1[["residuals"]][["sphered"]], dfrdef_7[["residuals"]][["sphered"]]))
message('identical(dfrdef_1[["residuals"]][["transformed"]], dfrdef_7[["residuals"]][["transformed"]]) (should be TRUE): ', identical(dfrdef_1[["residuals"]][["transformed"]], dfrdef_7[["residuals"]][["transformed"]]))
message('identical(dfrdef_1[["epsp"]], dfrdef_7[["epsp"]]) (should be TRUE): ', identical(dfrdef_1[["epsp"]], dfrdef_7[["epsp"]]))
message('identical(dfrdef_1[["observed_stats"]], dfrdef_7[["observed_stats"]]) (should be TRUE): ', identical(dfrdef_1[["observed_stats"]], dfrdef_7[["observed_stats"]]))
message('identical(dfrdef_1[["mcsim_stats"]], dfrdef_7[["mcsim_stats"]]) (should be FALSE): ', identical(dfrdef_1[["mcsim_stats"]], dfrdef_7[["mcsim_stats"]]))
message('identical(dfrdef_1[["p"]], dfrdef_7[["p"]]) (should be FALSE): ', identical(dfrdef_1[["p"]], dfrdef_7[["p"]]))
