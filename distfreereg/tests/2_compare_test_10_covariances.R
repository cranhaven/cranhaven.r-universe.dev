library(distfreereg)
set.seed(20240123)

n <- 20
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
P <- solve(Sig)
SqrtSigma <- distfreereg:::matsqrt(Sig)
Q <- distfreereg:::matsqrt(solve(Sig))
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))

# Verify that true_covariance is validated correctly.

# Missing
tryCatch(compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta))),
         error = function(e) warning(e))

# Bad name
tryCatch(compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 true_covariance = list(SqrtSigm = Sig),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta))),
         error = function(e) warning(e))

# No name
tryCatch(compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 true_covariance = list(Sig),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta))),
         error = function(e) warning(e))

# Non-matrix value
tryCatch(compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 true_covariance = list(Sigma = 1:10),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta))),
         error = function(e) warning(e))

# Non-numeric value
tryCatch(compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 covariance = list(Sigma = Sig),
                 true_covariance = list(Sigma = matrix(letters[sample(1:26, size = 400,
                                                                      replace = TRUE)],
                                                       nrow = 20)),
                 theta_init = rep(1, length(theta))),
         error = function(e) warning(e))

# Matrix with incorrect dimensions
tryCatch(compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 true_covariance = list(Sigma = matrix(rnorm(400))),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta))),
         error = function(e) warning(e))

# Function (only covariance, not true_covariance, should accept a function)
tryCatch(compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 true_covariance = list(Sigma = function(Y) 1),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta))),
         error = function(e) warning(e))


# Verify that any of the four true_covariance specifications result in the same
# observed statistics.
set.seed(20240222)
comp_dfr_1 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                      X = X, test_mean = func, covariance = list(Sigma = Sig),
                      true_covariance = list(Sigma = Sig), theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_2 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                      X = X, test_mean = func, covariance = list(Sigma = Sig),
                      true_covariance = list(P = P), theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_3 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                      X = X, test_mean = func, covariance = list(Sigma = Sig),
                      true_covariance = list(Q = Q), theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_4 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                      X = X, test_mean = func, covariance = list(Sigma = Sig),
                      true_covariance = list(SqrtSigma = SqrtSigma), theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_5 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                      X = X, test_mean = func, covariance = list(Sigma = Sig),
                      true_covariance = list(Sigma = Sig, P = P), theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_6 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                      X = X, test_mean = func, covariance = list(Sigma = Sig),
                      true_covariance = list(Sigma = Sig, SqrtSigma = SqrtSigma), theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_7 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                      X = X, test_mean = func, covariance = list(Sigma = Sig),
                      true_covariance = list(Sigma = Sig, Q = Q), theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_8 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                      X = X, test_mean = func, covariance = list(Sigma = Sig),
                      true_covariance = list(P = P, SqrtSigma = SqrtSigma), theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_9 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                      X = X, test_mean = func, covariance = list(Sigma = Sig),
                      true_covariance = list(P = P, Q = Q), theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_10 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                       X = X, test_mean = func, covariance = list(Sigma = Sig),
                       true_covariance = list(SqrtSigma = SqrtSigma, Q = Q), theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_11 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                       X = X, test_mean = func, covariance = list(Sigma = Sig),
                       true_covariance = list(P = P, SqrtSigma = SqrtSigma, Q = Q), theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_12 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                       X = X, test_mean = func, covariance = list(Sigma = Sig),
                       true_covariance = list(Sigma = Sig, SqrtSigma = SqrtSigma, Q = Q),
                       theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_13 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                       X = X, test_mean = func, covariance = list(Sigma = Sig),
                       true_covariance = list(Sigma = Sig, P = P, Q = Q),
                       theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_14 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                       X = X, test_mean = func, covariance = list(Sigma = Sig),
                       true_covariance = list(Sigma = Sig, P = P, SqrtSigma = SqrtSigma),
                       theta_init = rep(1, length(theta)))

set.seed(20240222)
comp_dfr_15 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                       X = X, test_mean = func, covariance = list(Sigma = Sig),
                       true_covariance = list(Sigma = Sig, P = P, SqrtSigma = SqrtSigma, Q = Q),
                       theta_init = rep(1, length(theta)))

message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_2[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_2[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_3[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_3[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_4[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_4[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_5[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_5[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_6[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_6[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_7[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_7[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_8[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_8[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_9[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_9[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_10[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_10[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_11[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_11[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_12[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_12[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_13[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_13[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_14[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_14[["observed_stats"]]))
message('all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_15[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_1[["observed_stats"]], comp_dfr_15[["observed_stats"]]))



# # Verify that covariance list can accept a function for anything other than
# # SqrtSigma. All of the named cdfr objects below should be equivalent.
# Sigma_func <- function(Y, X){
#   set.seed(20240222)
#   rWishart(1, df = length(Y), Sigma = diag(length(Y)))[,,1]
# }
# P_func <- function(Y, X){
#   set.seed(20240222)
#   solve(rWishart(1, df = length(Y), Sigma = diag(length(Y)))[,,1])
# }
# Q_func <- function(Y, X){
#   set.seed(20240222)
#   distfreereg:::matsqrt(solve(rWishart(1, df = length(Y), Sigma = diag(length(Y)))[,,1]))
# }
# set.seed(20240222)
# Sigma_same_as_func <- rWishart(1, df = n, Sigma = diag(n))[,,1]
# 
# set.seed(20240222)
# comp_dfr_16 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
#                        X = X, test_mean = func,
#                        true_covariance = list(Sigma = Sig),
#                        covariance = list(Sigma = Sigma_same_as_func),
#                        theta_init = rep(1, length(theta)))
# 
# set.seed(20240222)
# comp_dfr_17 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
#                        X = X, test_mean = func,
#                        true_covariance = list(Sigma = Sig),
#                        covariance = list(Sigma = Sigma_func),
#                        theta_init = rep(1, length(theta)))
# 
# set.seed(20240222)
# comp_dfr_18 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
#                        X = X, test_mean = func,
#                        true_covariance = list(Sigma = Sig),
#                        covariance = list(P = P_func),
#                        theta_init = rep(1, length(theta)))
# 
# set.seed(20240222)
# comp_dfr_19 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
#                        X = X, test_mean = func,
#                        true_covariance = list(Sigma = Sig),
#                        covariance = list(Q = Q_func),
#                        theta_init = rep(1, length(theta)))
# 
# message('all.equal(comp_dfr_16[["observed_stats"]], comp_dfr_17[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_16[["observed_stats"]], comp_dfr_17[["observed_stats"]]))
# message('all.equal(comp_dfr_16[["observed_stats"]], comp_dfr_18[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_16[["observed_stats"]], comp_dfr_18[["observed_stats"]]))
# message('all.equal(comp_dfr_16[["observed_stats"]], comp_dfr_19[["observed_stats"]]) (should be TRUE): ', all.equal(comp_dfr_16[["observed_stats"]], comp_dfr_19[["observed_stats"]]))
