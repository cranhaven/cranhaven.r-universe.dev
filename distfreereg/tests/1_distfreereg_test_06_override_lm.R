library(distfreereg)
set.seed(20240319)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

data <- data.frame(a = X, b = Y)
m <- lm(b ~ a, data = data)

set.seed(20240319)
dfrm_1 <- distfreereg(test_mean = m, verbose = FALSE)
set.seed(20240319)
dfrm_2 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(res_order = dfrm_1[["res_order"]]))
set.seed(20240319)
dfrm_4 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(r = dfrm_1[["r"]]))
set.seed(20240319)
dfrm_5 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(mcsim_stats = dfrm_1[["mcsim_stats"]]))


message('identical(dfrm_1[["epsp"]], dfrm_2[["epsp"]]) (should be TRUE): ', identical(dfrm_1[["epsp"]], dfrm_2[["epsp"]]))
message('identical(dfrm_1[["epsp"]], dfrm_4[["epsp"]]) (should be TRUE): ', identical(dfrm_1[["epsp"]], dfrm_4[["epsp"]]))
message('identical(dfrm_1[["epsp"]], dfrm_5[["epsp"]]) (should be TRUE): ', identical(dfrm_1[["epsp"]], dfrm_5[["epsp"]]))
message('identical(dfrm_1[["p"]], dfrm_5[["p"]]) (should be TRUE): ', identical(dfrm_1[["p"]], dfrm_5[["p"]]))






my_res_order <- sample(1:n)
my_theta_hat <- c(7,13)
my_r <- dfrm_1[["r"]][sample(1:n),]
my_mcsim_stats <- list(KS = dfrm_1[["mcsim_stats"]][["KS"]]^2,
                       CvM = dfrm_1[["mcsim_stats"]][["CvM"]]^2)
set.seed(20240319)
dfrm_5 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(res_order = my_res_order))
set.seed(20240319)
dfrm_7 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(r = my_r))
set.seed(20240319)
dfrm_8 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(mcsim_stats = my_mcsim_stats))

# Verify that specified overrides are correct.
message('identical(dfrm_1[["res_order"]], dfrm_5[["res_order"]]) (should be FALSE): ', identical(dfrm_1[["res_order"]], dfrm_5[["res_order"]]))
message('identical(dfrm_1[["r"]], dfrm_7[["r"]]) (should be FALSE): ', identical(dfrm_1[["r"]], dfrm_7[["r"]]))
message('identical(dfrm_1[["mcsim_stats"]], dfrm_8[["mcsim_stats"]]) (should be FALSE): ', identical(dfrm_1[["mcsim_stats"]], dfrm_8[["mcsim_stats"]]))
message('identical(my_res_order, dfrm_5[["res_order"]]) (should be TRUE): ', identical(my_res_order, dfrm_5[["res_order"]]))
message('identical(my_r, dfrm_7[["r"]]) (should be TRUE): ', identical(my_r, dfrm_7[["r"]]))
message('identical(my_mcsim_stats, dfrm_8[["mcsim_stats"]]) (should be TRUE): ', identical(my_mcsim_stats, dfrm_8[["mcsim_stats"]]))

# Verify that other things changed or not, as appropriate.
message('identical(dfrm_1[["theta_hat"]], dfrm_5[["theta_hat"]]) (should be TRUE): ', identical(dfrm_1[["theta_hat"]], dfrm_5[["theta_hat"]]))
message('identical(dfrm_1[["optimization_output"]], dfrm_5[["optimization_output"]]) (should be TRUE): ', identical(dfrm_1[["optimization_output"]], dfrm_5[["optimization_output"]]))
message('identical(dfrm_1[["fitted_values"]], dfrm_5[["fitted_values"]]) (should be TRUE): ', identical(dfrm_1[["fitted_values"]], dfrm_5[["fitted_values"]]))
message('identical(dfrm_1[["J"]], dfrm_5[["J"]]) (should be TRUE): ', identical(dfrm_1[["J"]], dfrm_5[["J"]]))
message('identical(dfrm_1[["r"]], dfrm_5[["r"]]) (should be FALSE): ', identical(dfrm_1[["r"]], dfrm_5[["r"]]))
message('identical(dfrm_1[["r_tilde"]], dfrm_5[["r_tilde"]]) (should be FALSE): ', identical(dfrm_1[["r_tilde"]], dfrm_5[["r_tilde"]]))
message('identical(dfrm_1[["mu"]], dfrm_5[["mu"]]) (should be TRUE): ', identical(dfrm_1[["mu"]], dfrm_5[["mu"]]))
message('identical(dfrm_1[["residuals"]][["raw"]], dfrm_5[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrm_1[["residuals"]][["raw"]], dfrm_5[["residuals"]][["raw"]]))
message('identical(dfrm_1[["residuals"]][["sphered"]], dfrm_5[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrm_1[["residuals"]][["sphered"]], dfrm_5[["residuals"]][["sphered"]]))
message('identical(dfrm_1[["residuals"]][["transformed"]], dfrm_5[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrm_1[["residuals"]][["transformed"]], dfrm_5[["residuals"]][["transformed"]]))
message('identical(dfrm_1[["epsp"]], dfrm_5[["epsp"]]) (should be FALSE): ', identical(dfrm_1[["epsp"]], dfrm_5[["epsp"]]))
message('identical(dfrm_1[["observed_stats"]], dfrm_5[["observed_stats"]]) (should be FALSE): ', identical(dfrm_1[["observed_stats"]], dfrm_5[["observed_stats"]]))
message('identical(dfrm_1[["mcsim_stats"]], dfrm_5[["mcsim_stats"]]) (should be FALSE): ', identical(dfrm_1[["mcsim_stats"]], dfrm_5[["mcsim_stats"]]))
message('identical(dfrm_1[["p"]], dfrm_5[["p"]]) (should be FALSE): ', identical(dfrm_1[["p"]], dfrm_5[["p"]]))

message('identical(dfrm_1[["theta_hat"]], dfrm_7[["theta_hat"]]) (should be TRUE): ', identical(dfrm_1[["theta_hat"]], dfrm_7[["theta_hat"]]))
message('identical(dfrm_1[["optimization_output"]], dfrm_7[["optimization_output"]]) (should be TRUE): ', identical(dfrm_1[["optimization_output"]], dfrm_7[["optimization_output"]]))
message('identical(dfrm_1[["fitted_values"]], dfrm_7[["fitted_values"]]) (should be TRUE): ', identical(dfrm_1[["fitted_values"]], dfrm_7[["fitted_values"]]))
message('identical(dfrm_1[["J"]], dfrm_7[["J"]]) (should be TRUE): ', identical(dfrm_1[["J"]], dfrm_7[["J"]]))
message('identical(dfrm_1[["r"]], dfrm_7[["r"]]) (should be FALSE): ', identical(dfrm_1[["r"]], dfrm_7[["r"]]))
message('identical(dfrm_1[["r_tilde"]], dfrm_7[["r_tilde"]]) (should be FALSE): ', identical(dfrm_1[["r_tilde"]], dfrm_7[["r_tilde"]]))
message('identical(dfrm_1[["mu"]], dfrm_7[["mu"]]) (should be TRUE): ', identical(dfrm_1[["mu"]], dfrm_7[["mu"]]))
message('identical(dfrm_1[["residuals"]][["raw"]], dfrm_7[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrm_1[["residuals"]][["raw"]], dfrm_7[["residuals"]][["raw"]]))
message('identical(dfrm_1[["residuals"]][["sphered"]], dfrm_7[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrm_1[["residuals"]][["sphered"]], dfrm_7[["residuals"]][["sphered"]]))
message('identical(dfrm_1[["residuals"]][["transformed"]], dfrm_7[["residuals"]][["transformed"]]) (should be FALSE): ', identical(dfrm_1[["residuals"]][["transformed"]], dfrm_7[["residuals"]][["transformed"]]))
message('identical(dfrm_1[["epsp"]], dfrm_7[["epsp"]]) (should be FALSE): ', identical(dfrm_1[["epsp"]], dfrm_7[["epsp"]]))
message('identical(dfrm_1[["observed_stats"]], dfrm_7[["observed_stats"]]) (should be FALSE): ', identical(dfrm_1[["observed_stats"]], dfrm_7[["observed_stats"]]))
message('identical(dfrm_1[["mcsim_stats"]], dfrm_7[["mcsim_stats"]]) (should be FALSE): ', identical(dfrm_1[["mcsim_stats"]], dfrm_7[["mcsim_stats"]]))
message('identical(dfrm_1[["p"]], dfrm_7[["p"]]) (should be FALSE): ', identical(dfrm_1[["p"]], dfrm_7[["p"]]))

message('identical(dfrm_1[["theta_hat"]], dfrm_8[["theta_hat"]]) (should be TRUE): ', identical(dfrm_1[["theta_hat"]], dfrm_8[["theta_hat"]]))
message('identical(dfrm_1[["optimization_output"]], dfrm_8[["optimization_output"]]) (should be TRUE): ', identical(dfrm_1[["optimization_output"]], dfrm_8[["optimization_output"]]))
message('identical(dfrm_1[["fitted_values"]], dfrm_8[["fitted_values"]]) (should be TRUE): ', identical(dfrm_1[["fitted_values"]], dfrm_8[["fitted_values"]]))
message('identical(dfrm_1[["J"]], dfrm_8[["J"]]) (should be TRUE): ', identical(dfrm_1[["J"]], dfrm_8[["J"]]))
message('identical(dfrm_1[["r"]], dfrm_8[["r"]]) (should be TRUE): ', identical(dfrm_1[["r"]], dfrm_8[["r"]]))
message('identical(dfrm_1[["r_tilde"]], dfrm_8[["r_tilde"]]) (should be TRUE): ', identical(dfrm_1[["r_tilde"]], dfrm_8[["r_tilde"]]))
message('identical(dfrm_1[["mu"]], dfrm_8[["mu"]]) (should be TRUE): ', identical(dfrm_1[["mu"]], dfrm_8[["mu"]]))
message('identical(dfrm_1[["residuals"]][["raw"]], dfrm_8[["residuals"]][["raw"]]) (should be TRUE): ', identical(dfrm_1[["residuals"]][["raw"]], dfrm_8[["residuals"]][["raw"]]))
message('identical(dfrm_1[["residuals"]][["sphered"]], dfrm_8[["residuals"]][["sphered"]]) (should be TRUE): ', identical(dfrm_1[["residuals"]][["sphered"]], dfrm_8[["residuals"]][["sphered"]]))
message('identical(dfrm_1[["residuals"]][["transformed"]], dfrm_8[["residuals"]][["transformed"]]) (should be TRUE): ', identical(dfrm_1[["residuals"]][["transformed"]], dfrm_8[["residuals"]][["transformed"]]))
message('identical(dfrm_1[["epsp"]], dfrm_8[["epsp"]]) (should be TRUE): ', identical(dfrm_1[["epsp"]], dfrm_8[["epsp"]]))
message('identical(dfrm_1[["observed_stats"]], dfrm_8[["observed_stats"]]) (should be TRUE): ', identical(dfrm_1[["observed_stats"]], dfrm_8[["observed_stats"]]))
message('identical(dfrm_1[["mcsim_stats"]], dfrm_8[["mcsim_stats"]]) (should be FALSE): ', identical(dfrm_1[["mcsim_stats"]], dfrm_8[["mcsim_stats"]]))
message('identical(dfrm_1[["p"]], dfrm_8[["p"]]) (should be FALSE): ', identical(dfrm_1[["p"]], dfrm_8[["p"]]))
