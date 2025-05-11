library(distfreereg)
set.seed(20240320)
n <- 1e2
func <- function(x, theta) sum(theta*x)
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- cbind(`(Intercept)` = 1, matrix(rexp(n, rate = 1)))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

set.seed(20240320)
dfr_ref <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                       theta_init = c(1,1), verbose = FALSE)

J <- dfr_ref[["J"]]
fitted_values <- dfr_ref[["fitted_values"]]

set.seed(20240320)
dfr_0 <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                     verbose = FALSE, J = J, fitted_values = fitted_values)


set.seed(20240320)
dfr_1a <- distfreereg(Y = Y*Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                      verbose = FALSE, J = J, fitted_values = fitted_values)
dfr_1b <- update(dfr_0, Y = Y*Y)
message('identical(dfr_1a[-1], dfr_1b[-1]) (should be TRUE): ', identical(dfr_1a[-1], dfr_1b[-1]))


set.seed(20240320)
dfr_2a <- distfreereg(Y = Y, X = X*X, test_mean = NULL, covariance = list(Sigma = Sig),
                      verbose = FALSE, J = J, fitted_values = fitted_values)
dfr_2b <- update(dfr_0, X = X*X)
message('identical(dfr_2a[-c(1,13,14)], dfr_2b[-c(1,13,14)]) (should be TRUE): ', identical(dfr_2a[-c(1,13,14)], dfr_2b[-c(1,13,14)]))

set.seed(20240320)
dfr_3a <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig %*% Sig),
                      verbose = FALSE, J = J, fitted_values = fitted_values)
dfr_3b <- update(dfr_0, covariance = list(Sigma = Sig %*% Sig))
message('identical(dfr_3a[-1], dfr_3b[-1]) (should be TRUE): ', identical(dfr_3a[-1], dfr_3b[-1]))


set.seed(20240320)
dfr_4a <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                      verbose = FALSE, J = J*J, fitted_values = fitted_values)
dfr_4b <- update(dfr_0, J = J*J)
message('identical(dfr_4a[-1], dfr_4b[-1]) (should be TRUE): ', identical(dfr_4a[-1], dfr_4b[-1]))


set.seed(20240320)
dfr_5a <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                      verbose = FALSE, J = J, fitted_values = fitted_values^2)
dfr_5b <- update(dfr_0, fitted_values = fitted_values^2)
message('identical(dfr_5a[-1], dfr_5b[-1]) (should be TRUE): ', identical(dfr_5a[-1], dfr_5b[-1]))


set.seed(20240320)
dfr_6a <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                      verbose = FALSE, J = J, fitted_values = fitted_values, ordering = "natural")
dfr_6b <- update(dfr_0, ordering = "natural")
set.seed(20240320)
dfr_6c <- update(dfr_0, ordering = "natural")
message('identical(dfr_6a[-1], dfr_6b[-1]) (should be FALSE): ', identical(dfr_6a[-1], dfr_6b[-1]))
message('identical(dfr_6a[-1], dfr_6c[-1]) (should be TRUE): ', identical(dfr_6a[-1], dfr_6c[-1]))


set.seed(20240320)
dfr_7a <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                      verbose = FALSE, J = J, fitted_values = fitted_values, B = 1e2)
dfr_7b <- update(dfr_0, B = 1e2)
set.seed(20240320)
dfr_7c <- update(dfr_0, B = 1e2)
message('identical(dfr_7a[-1], dfr_7b[-1]) (should be FALSE): ', identical(dfr_7a[-1], dfr_7b[-1]))
message('identical(dfr_7a[-1], dfr_7c[-1]) (should be TRUE): ', identical(dfr_7a[-1], dfr_7c[-1]))


set.seed(20240320)
new_ordering <- sample(1:n)
set.seed(20240320)
dfr_8a <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                      verbose = FALSE, J = J, fitted_values = fitted_values,
                      override = list(res_order = new_ordering))
dfr_8b <- update(dfr_0, override = list(res_order = new_ordering))
set.seed(20240320)
dfr_8c <- update(dfr_0, override = list(res_order = new_ordering))
message('identical(dfr_8a[-1], dfr_8b[-1]) (should be FALSE): ', identical(dfr_8a[-1], dfr_8b[-1]))
message('identical(dfr_8a[-1], dfr_8c[-1]) (should be TRUE): ', identical(dfr_8a[-1], dfr_8c[-1]))


set.seed(20240320)
new_r <- dfr_0[["r"]][,2:1]
set.seed(20240320)
dfr_9a <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                      verbose = FALSE, J = J, fitted_values = fitted_values,
                      override = list(r = new_r))
dfr_9b <- update(dfr_0, override = list(r = new_r))
set.seed(20240320)
dfr_9c <- update(dfr_0, override = list(r = new_r))
message('identical(dfr_9a[-1], dfr_9b[-1]) (should be FALSE): ', identical(dfr_9a[-1], dfr_9b[-1]))
message('identical(dfr_9a[-1], dfr_9c[-1]) (should be TRUE): ', identical(dfr_9a[-1], dfr_9c[-1]))


set.seed(20240320)
new_mc <- lapply(dfr_0[["mcsim_stats"]], FUN = function(x) x^2)
set.seed(20240320)
dfr_10a <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                      verbose = FALSE, J = J, fitted_values = fitted_values,
                      override = list(mcsim_stats = new_mc))
dfr_10b <- update(dfr_0, override = list(mcsim_stats = new_mc))
message('identical(dfr_10a[-1], dfr_10b[-1]) (should be TRUE): ', identical(dfr_10a[-1], dfr_10b[-1]))


set.seed(20240320)
dfr_11a <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                       verbose = FALSE, J = J, fitted_values = fitted_values,
                       stat = "KSmin")
dfr_11b <- update(dfr_0, stat = "KSmin")
set.seed(20240320)
dfr_11c <- update(dfr_0, stat = "KSmin")
message('identical(dfr_11a[-1], dfr_11b[-1]) (should be FALSE): ', identical(dfr_11a[-1], dfr_11b[-1]))
message('identical(dfr_11a[-1], dfr_11c[-1]) (should be TRUE): ', identical(dfr_11a[-1], dfr_11c[-1]))
