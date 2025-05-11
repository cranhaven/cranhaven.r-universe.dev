library(distfreereg)
set.seed(20240320)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2]
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5,1)
X <- matrix(rexp(2*n, rate = 1), ncol = 2)
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))


set.seed(20240320)
dfr_0 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     verbose = FALSE, theta_init = c(1,1,1))


set.seed(20240320)
new_Y <- Y^2
dfr_1a <- distfreereg(Y = new_Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                      verbose = FALSE, theta_init = c(1,1,1))
dfr_1b <- update(dfr_0, Y = new_Y)
message('identical(dfr_1a[-1], dfr_1b[-1]) (should be TRUE): ', identical(dfr_1a[-1], dfr_1b[-1]))


set.seed(20240320)
new_X <- X*X
dfr_2a <- distfreereg(Y = Y, X = new_X, test_mean = func, covariance = list(Sigma = Sig),
                      verbose = FALSE, theta_init = c(1,1,1))
dfr_2b <- update(dfr_0, X = new_X)
message('identical(dfr_2a[-c(1,17,18)], dfr_2b[-c(1,17,18)]) (should be TRUE): ', identical(dfr_2a[-c(1,17,18)], dfr_2b[-c(1,17,18)]))


set.seed(20240320)
dfr_3a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig %*% Sig),
                      verbose = FALSE, theta_init = c(1,1,1))
dfr_3b <- update(dfr_0, covariance = list(Sigma = Sig %*% Sig))
message('identical(dfr_3a[-1], dfr_3b[-1]) (should be TRUE): ', identical(dfr_3a[-1], dfr_3b[-1]))


set.seed(20240320)
dfr_4a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                      verbose = FALSE, theta_init = c(1,1,1), override = list(J = dfr_0[["J"]]^2))
dfr_4b <- update(dfr_0, override = list(J = dfr_0[["J"]]^2))
message('identical(dfr_4a[-c(1,6)], dfr_4b[-c(1,6)]) (should be TRUE): ', identical(dfr_4a[-c(1,6)], dfr_4b[-c(1,6)]))


set.seed(20240320)
dfr_5a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                      verbose = FALSE, theta_init = c(1,1,1), override = list(fitted_values = dfr_0[["fitted_values"]]^2))
dfr_5b <- update(dfr_0, override = list(fitted_values = dfr_0[["fitted_values"]]^2))
message('identical(dfr_5a[-c(1,6)], dfr_5b[-c(1,6)]) (should be TRUE): ', identical(dfr_5a[-c(1,6)], dfr_5b[-c(1,6)]))


set.seed(20240320)
dfr_6a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                      verbose = FALSE, theta_init = c(1,1,1), ordering = "natural")
dfr_6b <- update(dfr_0, ordering = "natural")
set.seed(20240320)
dfr_6c <- update(dfr_0, ordering = "natural")
message('identical(dfr_6a[-c(1,6)], dfr_6b[-c(1,6)]) (should be FALSE): ', identical(dfr_6a[-c(1,6)], dfr_6b[-c(1,6)]))
message('identical(dfr_6a[-c(1,6)], dfr_6c[-c(1,6)]) (should be TRUE): ', identical(dfr_6a[-c(1,6)], dfr_6c[-c(1,6)]))


set.seed(20240320)
dfr_7a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                      verbose = FALSE, theta_init = c(1,1,1), B = 1e2)
dfr_7b <- update(dfr_0, B = 1e2)
set.seed(20240320)
dfr_7c <- update(dfr_0, B = 1e2)
message('identical(dfr_7a[-c(1,6)], dfr_7b[-c(1,6)]) (should be FALSE): ', identical(dfr_7a[-c(1,6)], dfr_7b[-c(1,6)]))
message('identical(dfr_7a[-c(1,6)], dfr_7c[-c(1,6)]) (should be TRUE): ', identical(dfr_7a[-c(1,6)], dfr_7c[-c(1,6)]))


set.seed(20240320)
new_ordering <- sample(1:n)
set.seed(20240320)
dfr_8a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                      verbose = FALSE, theta_init = c(1,1,1), override = list(res_order = new_ordering))
dfr_8b <- update(dfr_0, override = list(res_order = new_ordering))
set.seed(20240320)
dfr_8c <- update(dfr_0, override = list(res_order = new_ordering))
message('identical(dfr_8a[-c(1,6)], dfr_8b[-c(1,6)]) (should be FALSE): ', identical(dfr_8a[-c(1,6)], dfr_8b[-c(1,6)]))
message('identical(dfr_8a[-c(1,6)], dfr_8c[-c(1,6)]) (should be TRUE): ', identical(dfr_8a[-c(1,6)], dfr_8c[-c(1,6)]))


set.seed(20240320)
new_r <- dfr_0[["r"]][,3:1]
set.seed(20240320)
dfr_9a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                      verbose = FALSE, theta_init = c(1,1,1), override = list(r = new_r))
dfr_9b <- update(dfr_0, override = list(r = new_r))
set.seed(20240320)
dfr_9c <- update(dfr_0, override = list(r = new_r))
message('identical(dfr_9a[-c(1,6)], dfr_9b[-c(1,6)]) (should be FALSE): ', identical(dfr_9a[-c(1,6)], dfr_9b[-c(1,6)]))
message('identical(dfr_9a[-c(1,6)], dfr_9c[-c(1,6)]) (should be TRUE): ', identical(dfr_9a[-c(1,6)], dfr_9c[-c(1,6)]))


set.seed(20240320)
new_mc <- lapply(dfr_0[["mcsim_stats"]], FUN = function(x) x^2)
set.seed(20240320)
dfr_10a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                       verbose = FALSE, theta_init = c(1,1,1), override = list(mcsim_stats = new_mc))
dfr_10b <- update(dfr_0, override = list(mcsim_stats = new_mc))
message('identical(dfr_10a[-c(1,6)], dfr_10b[-c(1,6)]) (should be TRUE): ', identical(dfr_10a[-c(1,6)], dfr_10b[-c(1,6)]))


set.seed(20240320)
new_func <- function(x, theta) theta[1] + theta[2]*x[1]^2 + theta[3]*x[2]^2
set.seed(20240320)
dfr_11a <- distfreereg(Y = Y, X = X, test_mean = new_func, covariance = list(Sigma = Sig),
                       verbose = FALSE, theta_init = c(1,1,1))
dfr_11b <- update(dfr_0, test_mean = new_func)
message('identical(dfr_11a[-1], dfr_11b[-1]) (should be TRUE): ', identical(dfr_11a[-1], dfr_11b[-1]))


set.seed(20240320)
dfr_12a <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                       verbose = FALSE, theta_init = c(1,1,1), stat = "KSmin")
dfr_12b <- update(dfr_0, stat = "KSmin")
set.seed(20240320)
dfr_12c <- update(dfr_0, stat = "KSmin")
message('identical(dfr_12a[-c(1,6)], dfr_12b[-c(1,6)]) (should be FALSE): ', identical(dfr_12a[-c(1,6)], dfr_12b[-c(1,6)]))
message('identical(dfr_12a[-c(1,6)], dfr_12c[-c(1,6)]) (should be TRUE): ', identical(dfr_12a[-c(1,6)], dfr_12c[-c(1,6)]))
