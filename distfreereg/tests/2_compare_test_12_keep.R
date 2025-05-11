library(distfreereg)
set.seed(20240227)

n <- 20
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))

set.seed(20240227)
cdfr_1 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                  X = X, test_mean = func,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  theta_init = rep(1, length(theta)), keep = "all")

set.seed(20240227)
cdfr_2 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                  X = X, test_mean = func,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  theta_init = rep(1, length(theta)), keep = 1:10)

message('identical(cdfr_1$dfrs, cdfr_2$dfrs) (should be TRUE): ', identical(cdfr_1$dfrs, cdfr_2$dfrs))
message('identical(length(cdfr_1$dfrs), 10L) (should be TRUE): ', identical(length(cdfr_1$dfrs), 10L))
class(cdfr_1$dfrs)

cdfr_3 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                  X = X, test_mean = func,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  theta_init = rep(1, length(theta)), keep = c(2,5,9))
names(cdfr_3$dfrs)

tryCatch(compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)), keep = 1:15),
         error = function(e) warning(e))

tryCatch(compare(reps = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                 X = X, test_mean = func,
                 true_covariance = list(Sigma = Sig),
                 covariance = list(Sigma = Sig),
                 theta_init = rep(1, length(theta)), keep = 15),
         error = function(e) warning(e))
