library(distfreereg)
set.seed(20240206)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2]
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5,1)
X <- matrix(rexp(2*n, rate = 1), nrow = n)
Y <- distfreereg:::f2ftheta(f = func, X = X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

# The following three distfreereg objects should have identical simulated
# results.
set.seed(20240206)
dfr_1 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1),
                     verbose = FALSE, B = 10)
set.seed(20240206)
dfr_2 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1),
                     verbose = FALSE, B = 10.5)
set.seed(20240206)
dfr_3 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1),
                     verbose = FALSE, B = 10.6)

message('identical(dfr_1[["mcsim_stats"]], dfr_2[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1[["mcsim_stats"]], dfr_2[["mcsim_stats"]]))
message('identical(dfr_1[["mcsim_stats"]], dfr_3[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1[["mcsim_stats"]], dfr_3[["mcsim_stats"]]))


# Verify that the code works for B = 1.
distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1),
            verbose = FALSE, B = 1)

# All of the following should result in errors.
tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1),
                     verbose = FALSE, B = 0),
         error = function(e) warning("There was an error: ", e))
tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1),
                     verbose = FALSE, B = 0.7),
         error = function(e) warning("There was an error: ", e))
tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1),
                     verbose = FALSE, B = -1),
         error = function(e) warning("There was an error: ", e))
tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1),
                     verbose = FALSE, B = 1i),
         error = function(e) warning("There was an error: ", e))
tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1),
                     verbose = FALSE, B = "hi"),
         error = function(e) warning("There was an error: ", e))
