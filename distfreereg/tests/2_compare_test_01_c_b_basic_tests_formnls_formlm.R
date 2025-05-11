library(distfreereg)
set.seed(20240829)
n <- 5
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(j = 2, k = 5)
true_func <- function(theta, x) theta[1]*x[1] + theta[2]*x[2]
test_func <- function(theta, x) theta[1]*x[1] + theta[2]*x[2]^2
true_form <- c ~ j*a + k*b
test_form <- c ~ a + I(b^2) - 1
true_data <- data.frame(a = rexp(n, rate = 1), b = rnorm(n))
true_X <- as.matrix(true_data)
X <- true_X + rexp(length(true_X))
data <- as.data.frame(X)
true_data[["c"]] <- theta[1]*true_data[["a"]] + theta[2]*true_data[["b"]] + rnorm(n)
data[["c"]] <- 5*data[["a"]] - 4*data[["b"]]^2 + rnorm(n)
true_m <- nls(true_form, data = true_data)
test_m <- lm(test_form, data = data)

# Each true/test mean pair has 16 possibilities for which X/data values are
# given. These are specified below by a four-digit binary sequence indicating
# the presence/absence of each term in the call, in the following order:
# true_data, data, true_X, X. For example, "0011" indicates that only
# true_X and X are provided.

###################################################
#### true_mean form (nls), test_mean form (lm) ####
####       should work exactly for 1100        ####
###################################################

# 0000
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 # true_data = true_data,
                 # data = data,
                 # true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1000
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 true_data = true_data,
                 # data = data,
                 # true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0100
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 # true_data = true_data,
                 data = data,
                 # true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1100
set.seed(20240308)
cdfr <- compare(true_mean = true_form,
                true_method = "nls",
                test_mean = test_form,
                true_data = true_data,
                data = data,
                # true_X = true_X,
                # X = X,
                true_covariance = list(Sigma = Sig),
                covariance = list(Sigma = diag(Sig)),
                reps = 10, B = 10, prog = Inf, theta = theta)

signif(cdfr[["observed_stats"]][["KS"]], digits = 4)
signif(cdfr[["observed_stats"]][["CvM"]], digits = 4)
signif(cdfr[["mcsim_stats"]][["KS"]], digits = 4)
signif(cdfr[["mcsim_stats"]][["CvM"]], digits = 4)

# 0010
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 # true_data = true_data,
                 # data = data,
                 true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1010
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 true_data = true_data,
                 # data = data,
                 true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0110
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 # true_data = true_data,
                 data = data,
                 true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1110
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 true_data = true_data,
                 data = data,
                 true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0001
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 # true_data = true_data,
                 # data = data,
                 # true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))
# 1001
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 true_data = true_data,
                 # data = data,
                 # true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0101
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 # true_data = true_data,
                 data = data,
                 # true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1101
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 true_data = true_data,
                 data = data,
                 # true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0011
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 # true_data = true_data,
                 # data = data,
                 true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1011
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 true_data = true_data,
                 # data = data,
                 true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0111
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 # true_data = true_data,
                 data = data,
                 true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1111
set.seed(20240308)
tryCatch(compare(true_mean = true_form,
                 true_method = "nls",
                 test_mean = test_form,
                 true_data = true_data,
                 data = data,
                 true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = diag(Sig)),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))
