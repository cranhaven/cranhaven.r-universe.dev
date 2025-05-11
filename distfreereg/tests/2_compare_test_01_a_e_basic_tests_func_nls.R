library(distfreereg)
set.seed(20240829)
n <- 5
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
true_func <- function(theta, x) theta[1]*x[1] + theta[2]*x[2]
test_func <- function(theta, x) theta[1]*x[1] + theta[2]*x[2]^2
true_form <- c ~ a + b - 1
test_form <- c ~ k*a + j*b
true_data <- data.frame(a = rexp(n, rate = 1), b = rnorm(n))
true_X <- as.matrix(true_data)
X <- true_X + rexp(length(true_X))
data <- as.data.frame(X)
true_data[["c"]] <- theta[1]*true_data[["a"]] + theta[2]*true_data[["b"]]
data[["c"]] <- 5*data[["a"]] - 4*data[["b"]]^2
true_m <- lm(true_form, data = true_data)
test_m <- nls(test_form, data = data)

# Each true/test mean pair has 16 possibilities for which X/data values are
# given. These are specified below by a four-digit binary sequence indicating
# the presence/absence of each term in the call, in the following order:
# true_data, data, true_X, X. For example, "0011" indicates that only
# true_X and X are provided.

#######################################
#### true_mean func, test_mean nls ####
####  should work exactly for 0010 ####
#######################################

# 0000
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 # true_data = true_data,
                 # data = data,
                 # true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1000
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 true_data = true_data,
                 # data = data,
                 # true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0100
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 # true_data = true_data,
                 data = data,
                 # true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1100
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 true_data = true_data,
                 data = data,
                 # true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0010
set.seed(20240829)
cdfr <- compare(true_mean = true_func,
                test_mean = test_m,
                # true_data = true_data,
                # data = data,
                true_X = true_X,
                # X = X,
                true_covariance = list(Sigma = Sig),
                reps = 10, B = 10, prog = Inf, theta = theta)

signif(cdfr[["observed_stats"]][["KS"]], digits = 4)
signif(cdfr[["observed_stats"]][["CvM"]], digits = 4)
signif(cdfr[["mcsim_stats"]][["KS"]], digits = 4)
signif(cdfr[["mcsim_stats"]][["CvM"]], digits = 4)

# 1010
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 true_data = true_data,
                 # data = data,
                 true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0110
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 # true_data = true_data,
                 data = data,
                 true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1110
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 true_data = true_data,
                 data = data,
                 true_X = true_X,
                 # X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0001
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 # true_data = true_data,
                 # data = data,
                 # true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))
# 1001
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 true_data = true_data,
                 # data = data,
                 # true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0101
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 # true_data = true_data,
                 data = data,
                 # true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1101
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 true_data = true_data,
                 data = data,
                 # true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0011
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 # true_data = true_data,
                 # data = data,
                 true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1011
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 true_data = true_data,
                 # data = data,
                 true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 0111
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 # true_data = true_data,
                 data = data,
                 true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# 1111
set.seed(20240829)
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 true_data = true_data,
                 data = data,
                 true_X = true_X,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 reps = 10, B = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))
