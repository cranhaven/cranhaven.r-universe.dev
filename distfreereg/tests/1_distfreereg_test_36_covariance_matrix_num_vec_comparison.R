library(distfreereg)
set.seed(20240715)

n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2] + theta[4]*x[3] + theta[5]*x[4]
Sig_number <- 5
Sig_vector <- rep(5, n)
Sig_matrix <- diag(Sig_vector)
theta <- c(2,5,-1,7,3)
X <- matrix(rexp(4*n, rate = 1), ncol = 4)
Y <- distfreereg:::f2ftheta(f = func, X)(theta) + rnorm(n, sd = sqrt(Sig_number))

set.seed(20240715)
dfr_num <- distfreereg(test_mean = func, Y = Y, X = X, theta_init = c(1,1,1,1,1),
                       covariance = list(Sigma = Sig_number), verbose = FALSE)

set.seed(20240715)
dfr_vec <- distfreereg(test_mean = func, Y = Y, X = X, theta_init = c(1,1,1,1,1),
                       covariance = list(Sigma = Sig_vector), verbose = FALSE)

set.seed(20240715)
dfr_mat <- distfreereg(test_mean = func, Y = Y, X = X, theta_init = c(1,1,1,1,1),
                       covariance = list(Sigma = Sig_matrix), verbose = FALSE)

message('identical(dfr_num$covariance, dfr_vec$covariance) (should be FALSE): ', identical(dfr_num$covariance, dfr_vec$covariance))
message('identical(dfr_num$covariance, dfr_mat$covariance) (should be FALSE): ', identical(dfr_num$covariance, dfr_mat$covariance))
message('identical(dfr_vec$covariance, dfr_mat$covariance) (should be FALSE): ', identical(dfr_vec$covariance, dfr_mat$covariance))

message('identical(dfr_num$theta_hat, dfr_vec$theta_hat) (should be TRUE): ', identical(dfr_num$theta_hat, dfr_vec$theta_hat))
message('identical(dfr_num$theta_hat, dfr_mat$theta_hat) (should be TRUE): ', identical(dfr_num$theta_hat, dfr_mat$theta_hat))

message('identical(dfr_num$observed_stats, dfr_vec$observed_stats) (should be TRUE): ', identical(dfr_num$observed_stats, dfr_vec$observed_stats))
message('identical(dfr_num$observed_stats, dfr_mat$observed_stats) (should be TRUE): ', identical(dfr_num$observed_stats, dfr_mat$observed_stats))

message('identical(dfr_num$p, dfr_vec$p) (should be TRUE): ', identical(dfr_num$p, dfr_vec$p))
message('identical(dfr_num$p, dfr_mat$p) (should be TRUE): ', identical(dfr_num$p, dfr_mat$p))



# Test a non-constant vector
set.seed(20240715)
Sig_vector <- rexp(n)
Sig_matrix <- diag(Sig_vector)

set.seed(20240715)
dfr_vec <- distfreereg(test_mean = func, Y = Y, X = X, theta_init = c(1,1,1,1,1),
                       covariance = list(Sigma = Sig_vector), verbose = FALSE)

set.seed(20240715)
dfr_mat <- distfreereg(test_mean = func, Y = Y, X = X, theta_init = c(1,1,1,1,1),
                       covariance = list(Sigma = Sig_matrix), verbose = FALSE)

message('identical(dfr_vec$covariance, dfr_mat$covariance) (should be FALSE): ', identical(dfr_vec$covariance, dfr_mat$covariance))
message('identical(dfr_vec$theta_hat, dfr_mat$theta_hat) (should be TRUE): ', identical(dfr_vec$theta_hat, dfr_mat$theta_hat))
message('identical(dfr_vec$observed_stats, dfr_mat$observed_stats) (should be TRUE): ', identical(dfr_vec$observed_stats, dfr_mat$observed_stats))
message('identical(dfr_vec$p, dfr_mat$p) (should be TRUE): ', identical(dfr_vec$p, dfr_mat$p))
