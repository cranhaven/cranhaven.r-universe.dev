# The following call to compare() results in an error. The error results from
# the call to nls(). Something about having the same seeds causes nls() to fail
# to find a solution. Making the seeds different solves this problem, but this
# test is here just to keep an eye on the situation in case something changes.

library(distfreereg)
set.seed(20240903)
n <- 100
theta <- c(j = 2, k = 5, l = 1)
true_form <- w ~ j*x + y^k + l*z
test_form <- true_form
x <- rnorm(n)
y <- rexp(n, rate = 1)
z <- rnorm(n)
w <- theta[1]*x + y^theta[2] + theta[3]*z + rnorm(n)
true_data <- data.frame(x = x, y = y, z = z, w = w)
data <- true_data

set.seed(20240903)
tryCatch(compare(true_mean = true_form,
                 err_dist_fun = function(n, reps) matrix(rnorm(n*reps), nrow = n),
                 true_method = "nls",
                 test_mean = test_form,
                 method = "nls",
                 true_data = true_data,
                 data = data,
                 true_covariance = list(Sigma = 1),
                 # covariance = list(Sigma = 1),
                 theta = theta,
                 theta_init = c(j = 1, k = 1, l = 1),
                 reps = 1000,
                 prog = 100),
         error = function(e) warning(e))
