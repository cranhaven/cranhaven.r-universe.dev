# All of the calls to compare() below should result in errors. They are grouped
# into three groups, depending on whether or not the true mean or test mean
# require covariates.

library(distfreereg)
set.seed(20240312)
n <- 5
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
true_func <- function(theta, x) theta[1]*x[1] + theta[2]*x[2]
test_func <- function(theta, x) theta[1]*x[1] + theta[2]*x[2]^2
true_form <- c ~ a + b - 1
test_form <- c ~ a + I(b^2) - 1
true_data <- data.frame(a = rexp(n, rate = 1), b = rnorm(n))
true_X <- as.matrix(true_data)
X <- true_X + rexp(length(true_X))
data <- as.data.frame(X)
true_data[["c"]] <- theta[1]*true_data[["a"]] + theta[2]*true_data[["b"]]
data[["c"]] <- 5*data[["a"]] - 4*data[["b"]]^2
true_m <- lm(true_form, data = true_data)
test_m <- lm(test_form, data = data)

true_X_longer <- rbind(true_X, true_X)
X_longer <- rbind(X, X)
true_data_longer <- rbind(true_data, true_data)
data_longer <- rbind(data, data)
true_m_longer <- lm(true_form, data = true_data_longer)
test_m_longer <- lm(test_form, data = data_longer)
Sig_longer <- rWishart(1, df = 2*n, Sigma = diag(2*n))[,,1]

### No n required

# True func, test func
tryCatch(compare(true_mean = true_func,
                 test_mean = test_func,
                 true_X = true_X_longer,
                 X = X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True func, test form
tryCatch(compare(true_mean = true_func,
                 test_mean = test_form,
                 data = data_longer,
                 true_X = true_X,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = Sig_longer),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True func, test lm
tryCatch(compare(true_mean = true_func,
                 test_mean = test_m,
                 true_X = true_X_longer,
                 true_covariance = list(Sigma = Sig_longer),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True form, test func
tryCatch(compare(true_mean = true_form,
                 true_method = "lm",
                 test_mean = test_func,
                 true_data = true_data_longer,
                 X = X,
                 true_covariance = list(Sigma = Sig_longer),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True form, test form
tryCatch(compare(true_mean = true_form,
                 true_method = "lm",
                 test_mean = test_form,
                 true_data = true_data_longer,
                 data = data,
                 true_covariance = list(Sigma = Sig_longer),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True form, test lm
tryCatch(compare(true_mean = true_form,
                 true_method = "lm",
                 test_mean = test_m,
                 true_data = true_data_longer,
                 true_covariance = list(Sigma = Sig_longer),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True lm, test func
tryCatch(compare(true_mean = true_m,
                 test_mean = test_func,
                 X = X_longer,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = Sig_longer),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True lm, test form
tryCatch(compare(true_mean = true_m,
                 test_mean = test_form,
                 data = data_longer,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = Sig_longer),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True lm, test lm
tryCatch(compare(true_mean = true_m_longer,
                 test_mean = test_m,
                 true_covariance = list(Sigma = Sig_longer),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))





### True_mean requires n

# True func, test func
tryCatch(compare(true_mean = function(theta) theta,
                 test_mean = test_func,
                 n = 2*n,
                 X = X,
                 true_covariance = list(Sigma = Sig_longer),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True func, test form
tryCatch(compare(true_mean = function(theta) theta,
                 test_mean = test_form,
                 data = data_longer,
                 n = 2*n,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = Sig_longer),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True func, test lm
tryCatch(compare(true_mean = function(theta) theta,
                 test_mean = test_m,
                 n = 2*n,
                 true_covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))




### Test_mean requires n

# True func, test func
tryCatch(compare(true_mean = true_func,
                 test_mean = function(theta) theta,
                 true_X = true_X_longer,
                 n = n,
                 true_covariance = list(Sigma = Sig_longer),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True form, test func
tryCatch(compare(true_mean = true_form,
                 true_method = "lm",
                 test_mean = function(theta) theta,
                 true_data = true_data_longer,
                 n = n,
                 true_covariance = list(Sigma = Sig_longer),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))

# True lm, test func
tryCatch(compare(true_mean = true_m_longer,
                 test_mean = function(theta) theta,
                 n = n,
                 true_covariance = list(Sigma = Sig_longer),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)),
                 reps = 10, prog = Inf, theta = theta),
         error = function(e) warning(e))
